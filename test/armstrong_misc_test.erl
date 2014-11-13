-module(armstrong_misc_test).

-include_lib("eunit/include/eunit.hrl").

build_name_test() ->
    T0 = current(),
    for(fun armstrong_misc:build_name/1, 10),
    T1 = current(),
    SpentTime = T1 - T0,
    io:format("Spent Time: ~p~n", [SpentTime]).

build_existing_name_test() ->
    T0 = current(),
    for(fun armstrong_misc:build_existing_name/1, 10),
    T1 = current(),
    SpentTime = T1 - T0,
    io:format("Spent Time: ~p~n", [SpentTime]).

current() ->
    {MegaSecs, Secs, _MicroSecs} = erlang:now(),
    MegaSecs * 1000000 + Secs.

for(_F, 0) -> ok;
for(F, I) -> F([armstrong, "_table"]), for(F, I-1).


