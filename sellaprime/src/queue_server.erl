-module(queue_server).

-behaviour(gen_server).
-compile(export_all).

%% Queues is_prime requests untile one of the prime servers become free.
%% Satisfies Chapter 23 Exercise 2.
%%
%% This is a rather naive implementation. A wiser and more correct approach would be using 
%% events and fsm to decouple the workers from the queue server. The queue_server would then only
%% be a producer whereas the worker would be the subscriber.

-record(state, {queue=queue:new(), waiting_workers=[], busy_workers=[], blacklisted=[]}).


statistics() ->
    gen_server:call(?MODULE, {stats}).

set_busy(Identifier) -> 
    gen_server:cast(?MODULE, {busy, Identifier}).

set_waiting(Identifier) -> 
    gen_server:cast(?MODULE, {waiting, Identifier}).

fulfil(ReplyTo, {ok, _} = Result) ->
    gen_server:call(?MODULE, {fulfil, ReplyTo, Result});

fulfil(ReplyTo, {error, timeout, N}) ->
    gen_server:call(?MODULE, {requeue, {ReplyTo, N}}).

is_prime(N) ->
    gen_server:cast(?MODULE, {is_prime, self(), N}),
    receive
        Result -> Result
    after
        5000 -> 
            gen_server:cast(?MODULE, {blacklist, self()}),
            {error, timeout}
    end.

start_link(Workers) ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, Workers, []).

init(Workers) ->
    io:format("~p started~n", [?MODULE]),
    {ok, #state{waiting_workers=Workers}}.

handle_call({stats}, _From, State) ->
    {reply, State, State};

handle_call({requeue, {ReplyTo, _ } = Request}, _From, #state{queue=Queue, blacklisted=Blacklisted} = State) ->
    case lists:any(fun(BlacklistedItem) -> BlacklistedItem =:= ReplyTo end, Blacklisted) of
        true -> {reply, ok, State};
        false ->
            NewQueue = queue:in(Request, Queue),
            {reply, ok, State#state{queue=NewQueue}}
    end;

handle_call({fulfil, ReplyTo, Result}, _From, State) ->
    ReplyTo ! Result,
    {reply, ok, State};

handle_call(_Request, _From, State) -> 
    {reply, ok, State}.

handle_cast({busy, Identifier}, #state{waiting_workers=WaitingWorkers, busy_workers=BusyWorkers} = State) ->
    NewWaiting = lists:filter(fun(Id) -> Id =/= Identifier end, WaitingWorkers),
    NewBusy = [Identifier | BusyWorkers ],
    {noreply, State#state{waiting_workers=NewWaiting, busy_workers=NewBusy}};

handle_cast({waiting, Identifier}, #state{queue={[], []}, waiting_workers=WaitingWorkers, busy_workers=BusyWorkers} = State) ->
    NewBusy = lists:filter(fun(Id) -> Id =/= Identifier end, BusyWorkers),
    NewWaiting = [Identifier | WaitingWorkers],
    {noreply, State#state{waiting_workers=NewWaiting, busy_workers=NewBusy}};

handle_cast({waiting, Identifier}, #state{queue=Queue, waiting_workers=WaitingWorkers, busy_workers=BusyWorkers} = State) ->
    NewBusy = lists:filter(fun(Id) -> Id =/= Identifier end, BusyWorkers),
    NewWaiting = [Identifier | WaitingWorkers],
    {{value, {ReplyTo, N}}, NewQueue} = queue:out(Queue),
    io:format("[~p] Getting ~p from queue~n",[Identifier, {ReplyTo, N}]),
    prime_tester_server:is_prime(Identifier, ReplyTo, N),
    {noreply, State#state{queue=NewQueue, waiting_workers=NewWaiting, busy_workers=NewBusy}};

handle_cast({is_prime, ReplyTo, N}, #state{queue=Queue, waiting_workers=[]} = State) ->
    io:format("All workers are busy - Queing request ~p~n", [{ReplyTo, N}]),
    NewQueue = queue:in({ReplyTo, N}, Queue),
    {noreply, State#state{queue=NewQueue}};

handle_cast({is_prime, ReplyTo, N}, #state{waiting_workers=[Worker | _]} = State) ->
    prime_tester_server:is_prime(Worker, ReplyTo, N),
    {noreply, State};

handle_cast({blacklist, Pid}, #state{blacklisted=Blacklisted} = State) ->
    % an ever growing list of Pids is probably not the best idea :)
    NewBlacklisted = [Pid | Blacklisted],
    {noreply, State#state{blacklisted=NewBlacklisted}};

handle_cast(_Message, State) -> 
    {noreply, State}.

