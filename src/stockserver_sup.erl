-module(stockserver_sup).
-author('Eldar Abdrazakov').

-behaviour(supervisor).

-export([start_link/0]).

-export([init/1]).

-define(SERVER, ?MODULE).

start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).
    
init([]) ->
    Server = {stockserver, {stockserver, start_link, []},
              permanent, 2000, worker, [stockserver]},
    Children = [Server],
    RestartStrategy = {one_for_one, 0, 1},
    {ok, {RestartStrategy, Children}}. 