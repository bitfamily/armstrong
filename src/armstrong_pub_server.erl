-module(armstrong_pub_server).

%% APIs
-export([start_link/5,
         start_link/6,
         send/3,
         delete_filter/2]).

-callback init(Args :: list(term())) ->
    {ok, State :: term()} | {error, Reason :: string()}.

-callback init_socket(SocketArgs :: list(term())) ->
    {ok, SocketState :: term(), Timeout :: integer()}
        | {error, Reason :: string()}.

-callback on_subscribe(Request :: term(), SocketState :: term()) ->
    {ok, Filter :: term(), SocketState :: term()} | {error, Reason :: string()}.

-callback on_unsubscribe(Request :: term(), SocketState :: term()) ->
    {ok, Filter :: term(), SocketState :: term()} | {error, Reason :: string()}.

-callback on_request(Request :: term(), SocketState :: term()) ->
    {ok, SocketState :: term()} | {error, Reason :: string()}.

-callback on_timeout(PubUpdatedAt :: integer(),
                     SubUpdatedAt :: integer(),
                     SocketState :: term()) ->
    {ok, SocketState :: term(), Timeout :: integer()}
        | {error, Reason :: string()}.

-callback terminate_socket(Reason :: term(), SocketState :: term()) ->
    ok.

-callback terminate(Reason :: term(), State :: term()) ->
    ok.

%% =========================================================
%% API implementations
%% =========================================================

start_link(Module, Args, SocketArgs, Endpoint, Options) ->
    armstrong_pub_server_impl:start_link(
      Module, Args, SocketArgs, Endpoint, Options).

start_link(ServerName, Module, Args, SocketArgs, Endpoint, Options) ->
    armstrong_pub_server_impl:start_link(
      ServerName, Module, Args, SocketArgs, Endpoint, Options).

send(ServerRef, Filter, Reply) ->
    armstrong_pub_server_impl:send(ServerRef, Filter, Reply).

delete_filter(ServerRef, Filter) ->
    armstrong_pub_server_impl:delete_filter(ServerRef, Filter).
