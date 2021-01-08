-module(sellaprime_prime_server).
-compile(export_all).

-behaviour(gen_server).

-record(state, {
    id :: atom(), 
    requests=queue:new(),
    busy=false,
    callbackModule=default_prime_tester
}).

-spec accept_work(atom(), #{n => number(), from => {pid(), reference()}}) -> ok.
accept_work(WorkerId, Request) ->
    gen_server:call(WorkerId,{accept, Request}).

-spec utilization(atom()) -> ok.
utilization(WorkerId) ->
    gen_server:call(WorkerId, {utilization}).

-spec free(atom()) -> ok.
free(WorkerId) ->
    gen_server:call(WorkerId, {free}).

start_link(Name) ->
    gen_server:start_link({local, Name}, ?MODULE, [Name], []).

init([Name | _] = Args) ->
    io:format("[~p] starting with args: ~p~n", [Name, Args]),
    {ok, #state{id=Name}}.

handle_call({accept, Request}, _From, #state{id=Id, requests=Queue, busy=Busy, callbackModule=CbMod} = State) ->
    case Busy of 
        true -> 
            NewQueue = queue:in(Request, Queue),
            {reply, ok, State#state{requests=NewQueue}};
        false ->
            do_async_work(Id, CbMod, Request),
            {reply, ok, State#state{busy=true}}
    end;

handle_call({free}, _From, #state{requests={[],[]}, busy=Busy} = State) ->
    case Busy of
        true ->
            {reply, ok, State#state{busy=false}};
        false ->
            {reply, ok, State}
    end;

handle_call({free}, _From, #state{id=Id, requests=Queue, busy=true, callbackModule=Cbm} = State) ->
    {{value, NextRequest}, NewQueue} = queue:out(Queue),
    do_async_work(Id, Cbm, NextRequest),
    {reply, ok, State#state{requests=NewQueue}};

handle_call({utilization}, _From, State) ->
    QueuedRequests = queue:len(State#state.requests),
    {reply, #{queued => QueuedRequests, busy => State#state.busy}, State};

handle_call(kill, _From, State) ->
    {stop, kill, killed, State};

handle_call(_Request, _From, State) ->
    {reply, ok, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(Info, State) ->
    io:format("Got info: ~p~n", [Info]),
    {noreply, State}.

terminate(Reason, _State) ->
    io:format("Gotta terminate due to: ~p", [Reason]),
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

do_async_work(WorkerId, CbMod, #{n := CheckPrime, from := {Pid, Ref}}) ->
    spawn_link(fun() ->
        io:format("[~p](~p) Checking if ~p is prime~n", [WorkerId, self(), CheckPrime]),
        Result = CbMod:is_prime(CheckPrime),
        timer:sleep(5000),
        free(WorkerId),
        io:format("[~p](~p) Done checking: ~p~n", [WorkerId, self(), CheckPrime]),
        Pid ! {Ref, {ok, {CheckPrime, Result}}}
    end).
    