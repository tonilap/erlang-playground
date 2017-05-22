-module(worker).

-behaviour(gen_server).

-export([start_link/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-define(SERVER, ?MODULE).

-record(state, {duration}).

start_link(Duration) ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [Duration], []).

init([Duration]) ->
    gen_server:cast(?SERVER, startup),
    {ok, #state{duration = Duration}}.

handle_call(_Request, _From, State) ->
    {reply, ok, State}.

handle_cast(startup, #state{duration = Duration} = State) ->
    %% simulates it is doing some stuff
    timer:sleep(Duration),
    %% terminates itself
    {stop, normal, State};

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
