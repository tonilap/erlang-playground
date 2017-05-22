-module(pool).

-behaviour(gen_server).

-export([start_link/0]).

-export([add_worker/2]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-define(SERVER, ?MODULE).

-record(state, {limit = 5, waiting_queue}).

start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

add_worker(Name, Duration) ->
    gen_server:cast(?SERVER, {add_worker, {Name, Duration}}).

init([]) ->
    gen_server:cast(?SERVER, startup),
    {ok, #state{}}.

handle_call(_Request, _From, State) ->
    {reply, ok, State}.

handle_cast({add_worker, {Name, Duration}}, #state{limit = Limit} = State) ->
    WorkerSpec =  {Name, {worker, start_link, [Duration]},
                   temporary, 1000, worker, [worker]},
    {ok, Pid} = supervisor:start_child(worker_sup, WorkerSpec),
    erlang:monitor(process, Pid),
    {noreply, State#state{limit = Limit - 1}};

handle_cast(startup, State) ->
    {noreply, State#state{waiting_queue = queue:new()}};

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info({'DOWN', Ref, process, Pid, normal}, State) ->
    lager:info("Termination: [~p,~p]",[Ref,Pid]),
    {noreply, State};

handle_info(Unknown, State) ->
    lager:info("Unkmown: [~p]",[Unknown]),
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
