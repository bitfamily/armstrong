-module(armstrong_subscription_mgr).

-behaviour(gen_server).

-record(state, {table_id}).

%% APIs
-export([start_link/0,
         add/3,
         delete/2,
         delete/3,
         find/2]).

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

start_link() ->
    gen_server:start_link(?MODULE, [], []).

add(MgrPid, Filter, PubSocketPid) ->
    gen_server:call(MgrPid, {add, Filter, PubSocketPid}).

delete(MgrPid, PubSocketPid) ->
    gen_server:cast(MgrPid, {delete, PubSocketPid}).

delete(MgrPid, Filter, PubSocketPid) ->
    gen_server:cast(MgrPid, {delete, Filter, PubSocketPid}).

find(MgrPid, Filter) ->
    gen_server:call(MgrPid, {find, Filter}).

%% =========================================================
%% Callback implementations
%% =========================================================

init([]) ->
    lager:debug("Initializing subscription manager..."),
    TableId = armstrong_subscription_store:init(),
    {ok, #state{table_id=TableId}}.

handle_call({add, Filter, PubSocketPid}, _From, State) ->
    ok = armstrong_subscription_store:insert(State#state.table_id,
                                             Filter,
                                             PubSocketPid),
    {reply, ok, State}
        ;
handle_call({find, Filter}, _From, State) ->
    Result = armstrong_subscription_store:find(State#state.table_id, Filter),
    {reply, {ok, Result}, State}.

handle_cast({delete, PubSocketPid}, State) ->
    ok = armstrong_subscription_store:delete(State#state.table_id,
                                             PubSocketPid),
    {noreply, State}
        ;
handle_cast({delete, Filter, PubSocketPid}, State) ->
    ok = armstrong_subscription_store:delete(State#state.table_id,
                                             Filter,
                                             PubSocketPid),
    {noreply, State}.

handle_info(Info, State) ->
    lager:error("Unknown info: info: ~p, state: ~p",
                [Info, State]),
    {stop, {error, {unknown_info, Info}}, State}.

terminate(Reason, State) ->
    lager:debug(
      "Terminating subscription manager: reason: ~p, state: ~p",
      [Reason, State]),
    armstrong_subscription_store:terminate(State#state.table_id),
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
