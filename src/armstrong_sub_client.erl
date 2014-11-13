-module(armstrong_sub_client).

%% APIs
-export([start_link/4,
         start_link/5,
         subscribe/2,
         unsubscribe/2,
         send/2,
         stop/1]).

-callback init(Args :: [term()]) ->
    {ok, State :: term()} | {error, Reason :: string()}.

-callback on_reply(Reply :: term(), State :: term()) ->
    {ok, State :: term()} | {error, Reason :: string()}.

-callback on_timeout(PubUpdatedAt :: integer(),
                     SubUpdatedAt :: integer(),
                     State :: term()) ->
    {ok, State :: term(), Timeout :: integer()}
        | {error, Reason :: string()}.

-callback terminate(Reason :: term(), State :: term()) ->
    ok.

%% =========================================================
%% API implementations
%% =========================================================

start_link(Module, Args, Endpoint, Options) ->
    armstrong_sub_socket:start_link(Module, Args, Endpoint, Options).

start_link(ClientName, Module, Args, Endpoint, Options) ->
    armstrong_sub_socket:start_link(
      ClientName, Module, Args, Endpoint, Options).

subscribe(ClientRef, Request) ->
    armstrong_sub_socket:subscribe(ClientRef, Request).

unsubscribe(ClientRef, Request) ->
    armstrong_sub_socket:unsubscribe(ClientRef, Request).

send(ClientRef, Request) ->
    armstrong_sub_socket:send(ClientRef, Request).

stop(ClientRef) ->
    armstrong_sub_socket:stop(ClientRef).
