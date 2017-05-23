-module(worker).

-behaviour(gen_server).

-export([start_link/2]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-define(SERVER, ?MODULE).

-record(state, {name, duration}).

start_link(Name, Duration) ->
    gen_server:start_link({local, Name}, ?MODULE, [Name, Duration], []).

init([Name, Duration]) ->
    gen_server:cast(Name, startup),
    {ok, #state{name = Name, duration = Duration}}.

handle_call(_Request, _From, State) ->
    {reply, ok, State}.

handle_cast(startup, #state{name = Name, duration = Duration} = State) ->
    %% simulates it is doing some stuff
    timer:sleep(Duration),
    %% terminates itself
    {stop, {shutdown, {Name, Duration}}, State};

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
