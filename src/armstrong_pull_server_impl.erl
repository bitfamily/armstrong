-module(armstrong_pull_server_impl).

-behaviour(gen_server).

-record(state, {module,
                module_state,
                sup_pid}).

%% APIs
-export([start_link/5,
         start_link/6]).

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

%% =========================================================
%% Callback implementations
%% =========================================================

init([Module, Args, SocketArgs, Endpoint, Options]) ->
    NewOptions = armstrong_misc:formalize_listen_options(Options),
    lager:debug("Initializing pull server:
                 module: ~p, args: ~p,
                 endpoint: ~p,
                 input options: ~p,
                 final options: ~p",
                [Module, Args, Endpoint, Options, NewOptions]),

    {ok, ModuleState} = Module:init(Args),
    {ok, SupPid} = armstrong_pull_socket_sup:start_link(
                     Module, SocketArgs, Endpoint, NewOptions),
    armstrong_misc:start_children(
      SupPid, armstrong_misc:parse_option(concurrency, Options)),

    State = #state{module = Module,
                   module_state = ModuleState,
                   sup_pid = SupPid},
    {ok, State}.

handle_call(Request, _From, State) ->
    lager:error("Unknown cast: request: ~p, state: ~p",
                [Request, State]),
    {stop, {error, {unknown_call, Request}}, State}.

handle_cast(Request, State) ->
    lager:error("Unknown cast: request: ~p, state: ~p",
                [Request, State]),
    {stop, {error, {unknown_cast, Request}}, State}.

handle_info(Info, State) ->
    lager:error("Unknown info: info: ~p, state: ~p",
                [Info, State]),
    {stop, {error, {unknown_info, Info}}, State}.

terminate(Reason, State) ->
    lager:debug("Terminating pull server: reason: ~p, state: ~p",
                [Reason, State]),
    exit(State#state.sup_pid, Reason),
    (State#state.module):terminate(Reason, State#state.module_state),
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
