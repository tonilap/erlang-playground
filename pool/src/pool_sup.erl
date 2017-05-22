-module(pool_sup).

-behaviour(supervisor).

-export([start_link/0]).

%% supervisor callbacks.
-export([init/1]).

start_link() ->
    supervisor:start_link({local,?MODULE}, ?MODULE, []).

init([]) ->
    RestartStrategy = one_for_one,
    MaxRestarts = 3,
    MaxSecondsBetweenRestarts = 30,

    SupFlags = {RestartStrategy, MaxRestarts, MaxSecondsBetweenRestarts},

    {ok, {SupFlags, [{worker_sup, {worker_sup, start_link, []},
                      permanent, infinity, supervisor, [worker_sup]},
                     {pool, {pool, start_link, []},
                      permanent, infinity, worker, [pool]}]}}.
