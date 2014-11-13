-module(armstrong_pub_server_impl).

-behaviour(gen_server).

-record(state, {module,
                module_state,
                mgr_pid,
                sup_pid}).

%% APIs
-export([start_link/5,
         start_link/6,
         send/3,
         delete_filter/2]).

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

start_link(Module, Args, SocketArgs, Endpoint, Options) ->
    gen_server:start_link(?MODULE,
                          [Module, Args, SocketArgs, Endpoint, Options],
                          []).

start_link(ServerName, Module, Args, SocketArgs, Endpoint, Options) ->
    gen_server:start_link({local, ServerName},
                          ?MODULE,
                          [Module, Args, SocketArgs, Endpoint, Options],
                          []).

send(ServerRef, Filter, Reply) ->
    gen_server:call(ServerRef, {send, Filter, Reply}).

delete_filter(ServerRef, Filter) ->
    gen_server:call(ServerRef, {delete_filter, Filter}).

%% =========================================================
%% Callback implementations
%% =========================================================

init([Module, Args, SocketArgs, Endpoint, Options]) ->
    NewOptions = armstrong_misc:formalize_listen_options(Options),
    lager:debug("Initializing pub server:
                 module: ~p, args: ~p,
                 endpoint: ~p,
                 input options: ~p,
                 final options: ~p",
                [Module, Args, Endpoint, Options, NewOptions]),

    {ok, ModuleState} = Module:init(Args),
    {ok, MgrPid} = armstrong_subscription_mgr:start_link(),
    {ok, SupPid} = armstrong_pub_socket_sup:start_link(
                     MgrPid, Module, SocketArgs, Endpoint, NewOptions),
    armstrong_misc:start_children(
      SupPid, armstrong_misc:parse_option(concurrency, Options)),

    State = #state{module = Module,
                   module_state = ModuleState,
                   mgr_pid = MgrPid,
                   sup_pid = SupPid},
    {ok, State}.

handle_call({send, Filter, Reply}, _From, State) ->
    {ok, PubSocketPids}
        = armstrong_subscription_mgr:find(State#state.mgr_pid, Filter),
    lists:foreach(
      fun(PubSocketPid) ->
              armstrong_pub_socket:send(PubSocketPid, Reply)
      end, PubSocketPids),
    {reply, ok, State}
        ;
handle_call({delete_filter, Filter}, _From, State) ->
    {ok, PubSocketPids}
        = armstrong_subscription_mgr:find(State#state.mgr_pid, Filter),
    lists:foreach(
      fun(PubSocketPid) ->
              armstrong_pub_socket:stop(PubSocketPid)
      end, PubSocketPids),
    {reply, ok, State}.

handle_cast(Request, State) ->
    lager:error("Unknown cast: request: ~p, state: ~p",
                [Request, State]),
    {stop, {error, {unknown_cast, Request}}, State}.

handle_info(Info, State) ->
    lager:error("Unknown info: info: ~p, state: ~p",
                [Info, State]),
    {stop, {error, {unknown_info, Info}}, State}.

terminate(Reason, State) ->
    lager:debug("Terminating pub server: reason: ~p, state: ~p",
                [Reason, State]),
    exit(State#state.sup_pid, Reason),
    exit(State#state.mgr_pid, Reason),
    (State#state.module):terminate(Reason, State#state.module_state),
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
