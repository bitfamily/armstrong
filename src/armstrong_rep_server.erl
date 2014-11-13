-module(armstrong_rep_server).

%% APIs
-export([start_link/5,
         start_link/6]).

-callback init(Args :: list(term())) ->
    {ok, State :: term()} | {error, Reason :: string()}.

-callback init_socket(SocketArgs :: list(term())) ->
        {ok, SocketState :: term()} | {error, Reason :: string()}.

-callback on_request(Request :: term(), State :: term()) ->
    {ok, Reply :: term(), State :: term()} | {error, Reason :: string()}.

-callback terminate_socket(Reason :: term(), SocketState :: term()) ->
    ok.

-callback terminate(Reason :: term(), State :: term()) ->
    ok.

%% =========================================================
%% API implementations
%% =========================================================

start_link(Module, Args, SocketArgs, Endpoint, Options) ->
    armstrong_rep_server_impl:start_link(
      Module, Args, SocketArgs, Endpoint, Options).

start_link(ServerName, Module, Args, SocketArgs, Endpoint, Options) ->
    armstrong_rep_server_impl:start_link(
      ServerName, Module, Args, SocketArgs, Endpoint, Options).
