-module(armstrong_req_socket).

-behaviour(gen_server).

-record(state, {module,
                module_state,
                raw_socket,
                connect_args,
                reconnect,
                recv_timeout}).

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
    NewOptions = armstrong_misc:formalize_passive_connect_options(Options),
    lager:debug("Initializing req socket:
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
                   {Address, Port, NewOptions, ConnectTimeout}}
          end,

    Reconnect = armstrong_misc:parse_option(reconnect, Options),
    RecvTimeout = armstrong_misc:parse_option(recv_timeout, Options),

    {ok, #state{module = Module,
                module_state = ModuleState,
                raw_socket = RawSocket,
                connect_args = ConnectArgs,
                reconnect = Reconnect,
                recv_timeout = RecvTimeout}}.

handle_call({send, Request}, _From, State) ->
    RawSocket =
        if
            State#state.raw_socket =/= undefined ->
                State#state.raw_socket;
            true ->
                connect(State#state.connect_args)
        end,

    case request(RawSocket, Request, State#state.recv_timeout) of
        {ok, Reply} ->
            {reply,
             {ok, Reply},
             State#state{raw_socket = RawSocket}};
        {error, closed} ->
            case State#state.reconnect of
                true ->
                    lager:debug("Reconnecting: ~p", [State#state.connect_args]),
                    gen_tcp:close(RawSocket),
                    NewRawSocket = connect(State#state.connect_args),
                    {ok, Reply} = request(NewRawSocket,
                                          Request,
                                          State#state.recv_timeout),
                    {reply,
                     {ok, Reply},
                     State#state{raw_socket = NewRawSocket}};
                false ->
                    {stop,
                     {error, closed},
                     State#state{raw_socket = RawSocket}}
            end;
        {error, Reason} ->
            {stop,
             {error, Reason},
             State#state{raw_socket = RawSocket}}
    end.

handle_cast(stop, State) ->
    {stop, normal, State}.

handle_info(Info, State) ->
    lager:error("Unknown info: info: ~p, state: ~p",
                [Info, State]),
    {stop, {error, {unknown_info, Info}}, State}.

terminate(Reason, State) ->
    lager:debug("Terminating req socket: reason: ~p, state: ~p",
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

request(RawSocket, Request, Timeout) ->
    case gen_tcp:send(RawSocket, Request) of
        ok ->
            gen_tcp:recv(RawSocket, 0, Timeout);
        {error, Reason} ->
            {error, Reason}
    end.
