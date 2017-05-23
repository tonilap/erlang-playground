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

handle_cast({add_worker, {Name, Duration}},
            #state{limit = Limit} = State) when Limit > 0 ->
    %% transient: child process is restarted only if it terminates abnormally,
    %% that is, with an exit reason other than normal, shutdown, or {shutdown,Term}.
    WorkerSpec = {Name, {worker, start_link, [Name, Duration]},
                  transient, 1000, worker,  [worker]},

    case supervisor:start_child(worker_sup, WorkerSpec) of
        {error,already_present} ->
            %% Child is already present so restarting
            {ok, Pid} = supervisor:restart_child(worker_sup, Name),
            erlang:monitor(process, Pid);
        {ok, Pid} ->
            erlang:monitor(process, Pid)
    end,
    {noreply, State#state{limit = Limit - 1}};

handle_cast({add_worker, {Name, Duration}},
            #state{limit = Limit, waiting_queue = Queue} = State) when Limit =< 0 ->
    lager:debug("Limit reached. Queue in ~p", [Name]),
    NewQueue = queue:in({Name, Duration}, Queue),
    {noreply, State#state{waiting_queue = NewQueue}};

handle_cast(startup, State) ->
    {noreply, State#state{waiting_queue = queue:new()}};

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info({'DOWN', _Ref, process, _Pid, {shutdown, {Name, Duration}}},
            #state{limit = Limit, waiting_queue = Queue} = State) ->
    case queue:out(Queue) of
        {empty,_} ->
            lager:debug("Queue is empty. Restarting worker"),
            %% No-one is waiting. Let's start the same worker then.
            supervisor:restart_child(worker_sup, Name),
            {noreply, State};
        {{value,Worker}, RemainingQueue} ->
            %% Start the waiting worker and queue in the one is finished
            {WaitName, WaitDuration} = Worker,
            lager:debug("Starting waiting worker ~p", [WaitName]),
            add_worker(WaitName, WaitDuration),
            NewQueue = queue:in({Name, Duration}, RemainingQueue),
            {noreply, State#state{limit = Limit + 1, waiting_queue = NewQueue}}
    end.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
