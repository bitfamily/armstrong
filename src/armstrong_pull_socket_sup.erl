-module(armstrong_pull_socket_sup).

-behaviour(supervisor).

%% APIs
-export([start_link/4,
         start_child/1]).

%% Callbacks
-export([init/1]).

%% =========================================================
%% API implementations
%% =========================================================

start_link(Module, SocketArgs, Endpoint, Options) ->
    supervisor:start_link(?MODULE, [Module, SocketArgs, Endpoint, Options]).

start_child(Pid) ->
    supervisor:start_child(Pid, []).

%% =========================================================
%% Callback implementations
%% =========================================================

init([Module, SocketArgs, {Port}, Options]) ->
    {ok, ListenSocket} = gen_tcp:listen(Port, Options),
    PullSocketSpec = {armstrong_pull_socket,
                      {armstrong_pull_socket,
                       start_link,
                       [Module, SocketArgs, ListenSocket, Options]},
                      temporary,
                      100,
                      worker,
                      [armstrong_pull_socket]},
    ChildSpecs = [PullSocketSpec],
    RestartStrategy = {simple_one_for_one, 0, 1},
    {ok, {RestartStrategy, ChildSpecs}}.
