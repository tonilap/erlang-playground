-module(worker_sup).

-behaviour(supervisor).

-export([start_link/0]).

-export([init/1]).

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->
    % Start withouth children, they will be added dinamically
    {ok, {{one_for_one, 10, 10}, []}}.
