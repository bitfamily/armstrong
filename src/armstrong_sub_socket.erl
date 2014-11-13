-module(armstrong_sub_socket).

-behaviour(gen_server).

-record(state, {timeout,
                pub_updated_at,
                sub_updated_at,
                module,
                module_state,
                raw_socket}).

%% APIs
-export([start_link/4,
         start_link/5,
         subscribe/2,
         unsubscribe/2,
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

start_link(Module, Args, Endpoint, Options) ->
    gen_server:start_link(?MODULE, [Module, Args, Endpoint, Options], []).

start_link(SocketName, Module, Args, Endpoint, Options) ->
    gen_server:start_link({local, SocketName},
                          ?MODULE,
                          [Module, Args, Endpoint, Options],
                          []).

subscribe(ClientRef, Request) ->
    gen_server:cast(ClientRef, {subscribe, Request}).

unsubscribe(ClientRef, Request) ->
    gen_server:cast(ClientRef, {unsubscribe, Request}).

send(SocketRef, Request) ->
    gen_server:cast(SocketRef, {send, Request}).

stop(SocketRef) ->
    gen_server:cast(SocketRef, stop).

%% =========================================================
%% Callback implementations
%% =========================================================

init([Module, Args, {Address, Port} = Endpoint, Options]) ->
    NewOptions = armstrong_misc:formalize_active_connect_options(Options),
    lager:debug("Initializing sub socket:
                 module: ~p, args: ~p,
                 endpoint: ~p,
                 input options: ~p,
                 final options: ~p",
                [Module, Args, Endpoint, Options, NewOptions]),
    ConnectTimeout = armstrong_misc:parse_option(connect_timeout, Options),
    {ok, RawSocket} = gen_tcp:connect(Address,
                                      Port,
                                      NewOptions,
                                      ConnectTimeout),
    {ok, ModuleState, Timeout} = Module:init(Args),
    Current = armstrong_misc:current(),
    {ok, #state{timeout = Timeout,
                pub_updated_at = Current,
                sub_updated_at = Current,
                module = Module,
                module_state = ModuleState,
                raw_socket = RawSocket}, Timeout}.

handle_call(Request, _From, State) ->
    lager:error("Unknown call: request: ~p, state: ~p",
                [Request, State]),
    {stop, {error, {unknown_call, Request}}, State}.

handle_cast({subscribe, Request}, State) ->
    ok = gen_tcp:send(State#state.raw_socket, [<<1:8>> | Request]),
    {noreply,
     State#state{sub_updated_at = armstrong_misc:current()},
     State#state.timeout}
        ;
handle_cast({unsubscribe, Request}, State) ->
    ok = gen_tcp:send(State#state.raw_socket, [<<2:8>> | Request]),
    {noreply,
     State#state{sub_updated_at = armstrong_misc:current()},
     State#state.timeout}
        ;
handle_cast({send, Request}, State) ->
    ok = gen_tcp:send(State#state.raw_socket, [<<3:8>> | Request]),
    {noreply,
     State#state{sub_updated_at = armstrong_misc:current()},
     State#state.timeout}
        ;
handle_cast(stop, State) ->
    {stop, normal, State}.

handle_info({tcp, _RawSocket, Request}, State) ->
    Module = State#state.module,
    ModuleState = State#state.module_state,
    {ok, NewModuleState} = Module:on_reply(Request, ModuleState),
    {noreply,
     State#state{
       pub_updated_at = armstrong_misc:current(),
       module_state = NewModuleState},
     State#state.timeout}
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
                                         State#state.module_state) of
        {ok, ModuleState, Timeout} ->
            {noreply,
             State#state{
               timeout = Timeout,
               module_state = ModuleState},
             Timeout};
        {error, Reason} ->
            {stop, {error, Reason}, State}
    end.

terminate(Reason, State) ->
    lager:debug("Terminating sub socket: reason: ~p, state: ~p",
                [Reason, State]),
    ok = (State#state.module):terminate(Reason, State#state.module_state).

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
