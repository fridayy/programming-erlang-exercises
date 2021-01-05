-module(sellaprime_loadbalancer).
-compile(export_all).

-behaviour(gen_server).

%%
%% Worker implementation for Chapter 23 Exercise 3
%%

-record(state, {workers=[]}).

is_prime(N) ->
    Ref = make_ref(),
    gen_server:call(?MODULE, {is_prime, #{n => N, from => {self(), Ref}}}),
    receive
        {Ref, Result} ->
            io:format("Result=~p~n", [Result]),
            Result
    end.

kill() ->
    gen_server:call(?MODULE, kill).

start_link(Workers) ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, Workers, []).

init(Workers) ->
    io:format("~p starting with args: ~p~n",
              [?MODULE, Workers]),
    {ok, #state{workers=Workers}}.

handle_call({is_prime, Request}, _From, #state{workers=Workers} = State) ->
    {Worker, _} = get_least_utilized(Workers),
    io:format("[~p] Sending work to ~p~n", [?MODULE, Worker]),
    sellaprime_prime_server:accept_work(Worker, Request),
    {reply, ok, State};

handle_call(stop, _From, State) ->
    {stop, normal, stopped, State};

handle_call(_Request, _From, State) ->
    {reply, ok, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

get_least_utilized(Workers) ->
    WorkerUtilization = lists:map(fun(Worker) -> {Worker, sellaprime_prime_server:utilization(Worker)} end, Workers),
    OrderedWorkerUtilization = lists:sort(fun sort_by_queue/2, WorkerUtilization),
    io:format("Worker Utilization = ~p~n", [OrderedWorkerUtilization]),
    [Worker | _] = OrderedWorkerUtilization,
    Worker.

sort_by_queue({_WorkerA, #{queued := QA}}, {_WorkerB, #{queued := QB }}) ->
    QA =< QB.