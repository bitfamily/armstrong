-module(armstrong_push_socket).

-behaviour(gen_server).

-record(state, {module,
                module_state,
                raw_socket,
                connect_args}).

%% APIs
-export([start_link/4,
         start_link/5,
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

send(SocketRef, Request) ->
    gen_server:call(SocketRef, {send, Request}).

stop(SocketRef) ->
    gen_server:cast(SocketRef, stop).

%% =========================================================
%% Callback implementations
%% =========================================================

init([Module, Args, {Address, Port} = Endpoint, Options]) ->
    NewOptions = armstrong_misc:formalize_active_connect_options(Options),
    lager:debug("Initializing push socket:
                 module: ~p, args: ~p,
                 endpoint: ~p,
                 input options: ~p,
                 final options: ~p",
                [Module, Args, Endpoint, Options, NewOptions]),

    {ok, ModuleState} = Module:init(Args),

    ConnectTimeout = armstrong_misc:parse_option(connect_timeout, Options),
    {RawSocket, ConnectArgs}
        = case armstrong_misc:parse_option(delay_connect, Options) of
              true ->
                  {undefined, {Address, Port, NewOptions, ConnectTimeout}};
              false ->
                  {connect({Address, Port, NewOptions, ConnectTimeout}),
                   undefined}
          end,

    {ok, #state{module = Module,
                module_state = ModuleState,
                raw_socket = RawSocket,
                connect_args = ConnectArgs}}.

handle_call({send, Request}, _From, State) ->
    if
        State#state.raw_socket =:= undefined ->
            RawSocket = connect(State#state.connect_args),
            ok = gen_tcp:send(RawSocket, Request),
            {reply, ok, State#state{raw_socket = RawSocket}};
        true ->
            ok = gen_tcp:send(State#state.raw_socket, Request),
            {reply, ok, State}
    end.

handle_cast(stop, State) ->
    {stop, normal, State}.

handle_info({tcp_closed, _RawSocket}, State) ->
    {stop, {shutdown, tcp_closed}, State}
        ;
handle_info({tcp_error, _RawSocket, Reason}, State) ->
    lager:error("Error occurred: reason: ~p, state: ~p",
                [Reason, State]),
    {stop, {error, {tcp_error, Reason}}, State}.

terminate(Reason, State) ->
    lager:debug("Terminating push socket: reason: ~p, state: ~p",
                [Reason, State]),
    ok = (State#state.module):terminate(Reason, State#state.module_state),
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% =========================================================
%% Internal functions
%% =========================================================

connect({Address, Port, Options, ConnectTimeout}) ->
    {ok, RawSocket} = gen_tcp:connect(Address, Port, Options, ConnectTimeout),
    RawSocket.
