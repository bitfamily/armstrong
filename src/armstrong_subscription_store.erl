-module(armstrong_subscription_store).

%% APIs
-export([init/0,
         insert/3,
         delete/2,
         delete/3,
         find/2,
         terminate/1]).

%% =========================================================
%% API implementations
%% =========================================================

init() ->
    ets:new(table, [bag, public, {read_concurrency, true}]).

insert(TableId, Filter, PubSocketPid) ->
    ets:insert(TableId, {Filter, PubSocketPid}),
    ok.

delete(TableId, PubSocketPid) ->
    ets:match_delete(TableId, {'_', PubSocketPid}),
    ok.

delete(TableId, Filter, PubSocketPid) ->
    ets:match_delete(TableId, {Filter, PubSocketPid}),
    ok.

find(TableId, Filter) ->
    Result = ets:lookup(TableId, Filter),
    [PubSocketPid || {_Filter, PubSocketPid} <- Result].

terminate(TableId) ->
    ets:delete(TableId).
