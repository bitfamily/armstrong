-module(armstrong_push_client).

%% APIs
-export([start_link/4,
         start_link/5,
         send/2,
         stop/1]).

-callback init(Args :: [term()]) ->
    {ok, State :: term()} | {error, Reason :: string()}.

-callback terminate(Reason :: term(), State :: term()) ->
    ok.

%% =========================================================
%% API implementations
%% =========================================================

start_link(Module, Args, Endpoint, Options) ->
    armstrong_push_socket:start_link(Module, Args, Endpoint, Options).

start_link(ClientName, Module, Args, Endpoint, Options) ->
    armstrong_push_socket:start_link(
      ClientName, Module, Args, Endpoint, Options).

send(ClientRef, Request) ->
    armstrong_push_socket:send(ClientRef, Request).

stop(ClientRef) ->
    armstrong_push_socket:stop(ClientRef).
