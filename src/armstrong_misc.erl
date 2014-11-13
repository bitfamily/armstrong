-module(armstrong_misc).

%% APIs
-export([parse_option/2,
         formalize_listen_options/1,
         formalize_passive_connect_options/1,
         formalize_active_connect_options/1,
         start_children/2,
         build_name/1,
         build_existing_name/1,
         current/0]).

%% =========================================================
%% API implementations
%% =========================================================

parse_option(reconnect, Options) ->
    case lists:keyfind(reconnect, 1, Options) of
        false -> false;
        {reconnect, Bool} -> Bool
    end
        ;
parse_option(connect_timeout, Options) ->
    case lists:keyfind(connect_timeout, 1, Options) of
        false -> 100;
        {connect_timeout, N} -> N
    end
        ;
parse_option(recv_timeout, Options) ->
    case lists:keyfind(recv_timeout, 1, Options) of
        false -> 100;
        {recv_timeout, N} -> N
    end
        ;
parse_option(delay_connect, Options) ->
    case lists:keyfind(delay_connect, 1, Options) of
        false -> false;
        {delay_connect, Bool} -> Bool
    end
        ;
parse_option(concurrency, Options) ->
    case lists:keyfind(concurrency, 1, Options) of
        false -> 1;
        {concurrency, N} -> N
    end.

formalize_listen_options(Options) ->
    ActiveOption = lists:keyfind(active, 1, Options),
    DropKeys = [concurrency, active],
    NewOptions = drop_options(DropKeys, Options),
    ActiveValue = case ActiveOption of
                      false -> 512;
                      {active, Value} ->
                          if
                              Value =:= true -> 512;
                              true -> Value
                          end
                  end,
    [{active, ActiveValue} | NewOptions].

formalize_passive_connect_options(Options) ->
    [{active, false} | formalize_connect_options(Options)].

formalize_active_connect_options(Options) ->
    [{active, true} | formalize_connect_options(Options)].

formalize_connect_options(Options) ->
    DropKeys = [reconnect,
                connect_timeout,
                recv_timeout,
                delay_connect,
                active],
    drop_options(DropKeys, Options).

start_children(SupPid, N) ->
    [supervisor:start_child(SupPid, []) || _ <- lists:seq(1, N)].

build_name(Things) ->
    list_to_atom(lists:concat(Things)).

build_existing_name(Things) ->
    list_to_existing_atom(lists:concat(Things)).

current() ->
    {MegaSecs, Secs, MicroSecs} = os:timestamp(),
    (MegaSecs*1000000 + Secs)*1000 + round(MicroSecs/1000).

%% =========================================================
%% Internal functions
%% =========================================================

drop_options(DropKeys, Options) ->
    lists:filter(
      fun(Option) ->
              Key = case Option of
                        {Key2, _Value} -> Key2;
                        Other -> Other
                     end,
              IsMember = lists:member(Key, DropKeys),
              if
                  IsMember =:= true -> false;
                  true -> true
              end
      end, Options).
