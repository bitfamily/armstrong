-module(armstrong_pull_socket).

-behaviour(gen_server).

-record(state, {parent,
                module,
                socket_state,
                listen_socket,
                active_option,
                raw_socket}).

%% APIs
-export([start_link/4]).

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

start_link(Module, SocketArgs, ListenSocket, Options) ->
    gen_server:start_link(
      ?MODULE, [self(), Module, SocketArgs, ListenSocket, Options], []).

%% =========================================================
%% Callback implementations
%% =========================================================

init([Parent, Module, SocketArgs, ListenSocket, Options]) ->
    lager:debug(
      "Initializing pull socket: parent: ~p, module: ~p, socket_args: ~p",
      [Parent, Module, SocketArgs]),

    {ok, SocketState} = Module:init_socket(SocketArgs),

    ActiveOption = lists:keyfind(active, 1, Options),
    State = #state{parent = Parent,
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
            lager:debug("Push socket accepted: endpoint: ~p",
                        [Endpoint]);
        {error, Reason} ->
            lager:warning(
              "Push socket accepted: endpoint: undefined, reason: ~p",
              [Reason])
    end,
    armstrong_pull_socket_sup:start_child(State#state.parent),
    {noreply, State#state{raw_socket = RawSocket}}.

handle_info({tcp, _RawSocket, Request}, State) ->
    Module = State#state.module,
    SocketState = State#state.socket_state,
    {ok, NewSocketState} = Module:on_request(Request, SocketState),
    {noreply, State#state{socket_state = NewSocketState}}
        ;
handle_info({tcp_passive, RawSocket}, State) ->
    ok = inet:setopts(RawSocket, [State#state.active_option]),
    {noreply, State}
        ;
handle_info({tcp_closed, _RawSocket}, State) ->
    {stop, {shutdown, tcp_closed}, State}
        ;
handle_info({tcp_error, _RawSocket, Reason}, State) ->
    lager:error("Error occurred: reason: ~p, state: ~p",
                [Reason, State]),
    {stop, {error, {tcp_error, Reason}}, State}.

terminate(Reason, State) ->
    lager:debug("Terminating pull socket: reason: ~p, state: ~p",
                [Reason, State]),
    ok = (State#state.module):terminate_socket(Reason,
                                               State#state.socket_state),
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
