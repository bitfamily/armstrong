-module(armstrong_pub_socket).

-behaviour(gen_server).

-record(state, {timeout,
                pub_updated_at,
                sub_updated_at,
                parent,
                mgr_pid,
                module,
                socket_state,
                listen_socket,
                active_option,
                raw_socket}).

%% APIs
-export([start_link/5,
         send/2,
         stop/1]).

%% Callbacks
-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).

%% =========================================================
%% API implementations
%% =========================================================

start_link(MgrPid, Module, SocketArgs, ListenSocket, Options) ->
    gen_server:start_link(
      ?MODULE, [self(), MgrPid, Module, SocketArgs, ListenSocket, Options], []).

send(PubSocketPid, Reply) ->
    gen_server:cast(PubSocketPid, {send, Reply}).

stop(PubSocketPid) ->
    gen_server:cast(PubSocketPid, stop).

%% =========================================================
%% Callback implementations
%% =========================================================

init([Parent, MgrPid, Module, SocketArgs, ListenSocket, Options]) ->
    lager:debug(
      "Initializing pub socket: parent: ~p, module: ~p, socket_args: ~p",
      [Parent, Module, SocketArgs]),

    {ok, SocketState, Timeout} = Module:init_socket(SocketArgs),
    ActiveOption = lists:keyfind(active, 1, Options),
    State = #state{timeout = Timeout,
                   pub_updated_at = 0,
                   sub_updated_at = 0,
                   parent = Parent,
                   mgr_pid = MgrPid,
                   module = Module,
                   socket_state = SocketState,
                   listen_socket = ListenSocket,
                   active_option = ActiveOption},

    gen_server:cast(self(), accept),

    {ok, State}.

handle_call(Request, _From, State) ->
    lager:error("Unknown call: request: ~p, state: ~p",
                [Request, State]),
    {stop, {error, {unknown_call, Request}}, State}.

handle_cast(accept, State) ->
    {ok, RawSocket} = gen_tcp:accept(State#state.listen_socket),
    case inet:peername(RawSocket) of
        {ok, Endpoint} ->
            lager:debug("Sub socket accepted: endpoint: ~p",
                        [Endpoint]);
        {error, Reason} ->
            lager:warning(
              "Sub socket accepted: endpoint: undefined, reason: ~p",
              [Reason])
    end,
    armstrong_pub_socket_sup:start_child(State#state.parent),
    Current = armstrong_misc:current(),
    {noreply,
     State#state{
       pub_updated_at = Current,
       sub_updated_at = Current,
       raw_socket = RawSocket},
     State#state.timeout}
        ;
handle_cast({send, Reply}, State) ->
    ok = gen_tcp:send(State#state.raw_socket, Reply),
    {noreply,
     State#state{pub_updated_at = armstrong_misc:current()},
     State#state.timeout}
        ;
handle_cast(stop, State) ->
    {stop, shutdown, State}.

handle_info({tcp, _RawSocket, Request}, State) ->
    Module = State#state.module,
    SocketState = State#state.socket_state,
    <<IntRequestHeader:8, RequestBody/binary>> = Request,

    case IntRequestHeader of
        1 -> % 'SUBSCRIBE'
            case Module:on_subscribe(RequestBody, SocketState) of
                {ok, Filter, NewSocketState} ->
                    ok = armstrong_subscription_mgr:add(
                           State#state.mgr_pid, Filter, self()),
                    {noreply,
                     State#state{
                       sub_updated_at = armstrong_misc:current(),
                       socket_state = NewSocketState},
                     State#state.timeout};
                {error, Reason} ->
                    {stop, {error, Reason}, State}
            end;
        2 -> % 'UNSUBSCRIBE'
            case Module:on_unsubscribe(RequestBody, SocketState) of
                {ok, Filter, NewSocketState} ->
                    ok = armstrong_subscription_mgr:delete(
                           State#state.mgr_pid, Filter, self()),
                    {noreply,
                     State#state{
                       sub_updated_at = armstrong_misc:current(),
                       socket_state = NewSocketState},
                     State#state.timeout};
                {error, Reason} ->
                    {stop, {error, Reason}, State}
            end;
        3 -> % 'REQUEST'
            case Module:on_request(RequestBody, SocketState) of
                {ok, NewSocketState} ->
                    {noreply,
                     State#state{
                       sub_updated_at = armstrong_misc:current(),
                       socket_state = NewSocketState},
                     State#state.timeout};
                {error, Reason} ->
                    {stop, {error, Reason}, State}
            end
    end
        ;
handle_info({tcp_passive, RawSocket}, State) ->
    ok = inet:setopts(RawSocket, [State#state.active_option]),
    {noreply, State, State#state.timeout}
        ;
handle_info({tcp_closed, _RawSocket}, State) ->
    {stop, {shutdown, tcp_closed}, State}
        ;
handle_info({tcp_error, _RawSocket, Reason}, State) ->
    lager:error("Error occurred: reason: ~p, state: ~p",
                [Reason, State]),
    {stop, {error, {tcp_error, Reason}}, State}
        ;
handle_info(timeout, State) ->
    case (State#state.module):on_timeout(State#state.pub_updated_at,
                                         State#state.sub_updated_at,
                                         State#state.socket_state) of
        {ok, SocketState, Timeout} ->
            {noreply,
             State#state{
               timeout = Timeout,
               socket_state = SocketState},
             Timeout};
        {error, Reason} ->
            {stop, {error, Reason}, State}
    end.

terminate(Reason, State) ->
    lager:debug("Terminating pub socket: reason: ~p, state: ~p",
               [Reason, State]),
    case (catch (State#state.module):terminate_socket(
             Reason, State#state.socket_state)) of
        ok ->
            ignore;
        {'EXIT', TraceStack} ->
            lager:error(
              "Terminate socket failed: trace stack: ~p", [TraceStack])
    end,
    ok = armstrong_subscription_mgr:delete(State#state.mgr_pid, self()).

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
