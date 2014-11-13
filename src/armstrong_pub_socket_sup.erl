-module(armstrong_pub_socket_sup).

-behaviour(supervisor).

%% APIs
-export([start_link/5,
         start_child/1]).

%% Callbacks
-export([init/1]).

%% =========================================================
%% API implementations
%% =========================================================

start_link(MgrPid, Module, SocketArgs, Endpoint, Options) ->
    supervisor:start_link(
      ?MODULE, [MgrPid, Module, SocketArgs, Endpoint, Options]).

start_child(Pid) ->
    supervisor:start_child(Pid, []).

%% =========================================================
%% Callback implementations
%% =========================================================

init([MgrPid, Module, SocketArgs, {Port}, Options]) ->
   {ok, ListenSocket} = gen_tcp:listen(Port, Options),
    PubSocketSpec = {armstrong_pub_socket,
                     {armstrong_pub_socket,
                      start_link,
                      [MgrPid, Module, SocketArgs, ListenSocket, Options]},
                     temporary,
                     100,
                     worker,
                     [armstrong_pub_socket]},
    ChildSpecs = [PubSocketSpec],
    RestartStrategy = {simple_one_for_one, 0, 1},
    {ok, {RestartStrategy, ChildSpecs}}.
