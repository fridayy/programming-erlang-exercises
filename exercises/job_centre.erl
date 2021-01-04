-module(job_centre).
-behaviour(gen_server).

%% API
-export([start/0, stop/0, start_link/0]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).
-export([add_job/1, work_wanted/0, job_done/1, statistics/0, test/0, extended_stats/0]).

-record(job, {number = 0, fn, jobtime=2}).
-record(state, {queue = queue:new(), 
                inflight = maps:new(),
                masters = maps:new(),
                finished = 0}).

add_job(Fun) ->
    gen_server:call(?MODULE, {add, Fun}).

work_wanted() ->
    gen_server:call(?MODULE, {get}).

job_done(JobNumber) ->
    gen_server:call(?MODULE, {done, JobNumber}).

statistics() ->
    gen_server:call(?MODULE, {stats}).

extended_stats() ->
    gen_server:call(?MODULE, {estats}).

clear_master(JobNumber) ->
    gen_server:call(?MODULE, {clear_master, JobNumber}).

start() ->
    start_link().

stop() ->
    gen_server:call(?MODULE, stop).

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

init(_Args) ->
    {ok, #state{}}.

handle_call(stop, _From, State) ->
    {stop, normal, stopped, State};

handle_call({add, Fun}, _From, #state{queue=JobQueue} = State) ->
    JobNumber = generate_number(Fun),
    Job = #job{number = JobNumber, fn = Fun},
    NewJobQueue = queue:in(Job, JobQueue),
    {reply, Job#job.number, State#state{queue=NewJobQueue}};

handle_call({get}, {Pid, _Ref}, #state{queue=JobQueue, inflight=Inflight, masters=Masters} = State) ->
    case queue:out(JobQueue) of
        {{value, Job}, NewQueue} ->
            monitor(process, Pid),
            MasterPid = create_master(Pid, Job),
            NewMasters = maps:put(Job#job.number, MasterPid, Masters),
            NewInflight = add_inflight(Pid, Job, Inflight),
            {reply, {Job#job.number, Job#job.fn}, State#state{queue=NewQueue, inflight=NewInflight, masters=NewMasters}};
        {empty, _} ->
            {reply, no, State}
    end;

handle_call({stats}, _From, #state{queue=JobQueue, inflight=Inflight, finished=Finished} = State) ->
    Queued = queue:len(JobQueue),
    InProgress = erlang:length(lists:flatten(maps:values(Inflight))),
    {reply, [{queued, Queued}, {in_progress, InProgress}, {finished, Finished}], State};

handle_call({estats}, _From, State) ->
    {reply, State, State};

handle_call({clear_master, JobNumber}, From, #state{masters=Masters} = State) ->
    io:format("Clearing master for ~p PID: ~p~n", [JobNumber, From]),
    NewMasters = maps:remove(JobNumber, Masters),
    {reply, ok, State#state{masters=NewMasters}};

handle_call({done, JobNumber}, {Pid, _Ref}, #state{inflight=Inflight, finished=Finished, masters=Masters} = State) ->
    MasterPid = maps:get(JobNumber, Masters),
    MasterPid ! done,
    case remove_inflight(Pid, JobNumber, Inflight) of
        {ok, NewInflight} ->
             NewFinished = Finished + 1,
             {reply, ok, State#state{inflight=NewInflight, finished=NewFinished}};
        Error ->
            {reply, Error, State}
    end.

handle_cast(_Msg, State) -> 
    {noreply, State}.


handle_info({'DOWN', _Ref, process, _WorkerPid, normal}, State) -> 
    {noreply, State};

handle_info({'DOWN', _Ref, process, WorkerPid, Reason}, #state{queue=JobQueue, inflight=Inflight} = State) ->
    io:format("Worker ~p died due to ~p - Requeing jobs. ~n", [WorkerPid, Reason]),
    UncompletableJobs = maps:get(WorkerPid, Inflight, []),
    NewQueue = lists:foldl(fun(Job, Acc) -> queue:in(Job, Acc) end, JobQueue, UncompletableJobs),
    NewInflight = maps:remove(WorkerPid, Inflight),
    io:format("Successfully requeued ~p ~n", [WorkerPid]),
    {noreply, State#state{queue=NewQueue, inflight=NewInflight}};

handle_info(Info, State) ->
    io:format("Got info: ~p~n", [Info]),
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.


create_master(WorkerPid, Job) ->
    spawn(fun() -> master(WorkerPid, Job) end).

master(WorkerPid, #job{number=JobNumber, jobtime=Jobtime}) ->
    WarningTimer = (Jobtime - 1) * 1000,
    KillTimer = (Jobtime + 1) * 1000,
    receive
        done -> clear_master(JobNumber)
    after 
        WarningTimer -> 
            WorkerPid ! hurry_up,
            io:format("Worker ~p better hurry! ~n", [WorkerPid]),
            receive
                done -> clear_master(JobNumber)
            after
                KillTimer -> 
                exit(WorkerPid, youre_fired),
                clear_master(JobNumber),
                io:format("Worker ~p did not finish in time: youre_fired! ~n", [WorkerPid])
            end
    end.


generate_number(Fun) ->
    erlang:phash2(Fun) + os:system_time().

add_inflight(WorkerPid, Job, Inflight) ->
    case maps:is_key(WorkerPid, Inflight) of
        true ->
            maps:update_with(WorkerPid, fun(Jobs) -> [Job | Jobs] end, Inflight);
        false ->
            maps:put(WorkerPid, [Job | []], Inflight)
    end.

remove_inflight(WorkerPid, JobNumber, Inflight) ->
    FilterByJobNumber = fun(Jobs) ->
        lists:filter(fun(Job) -> Job#job.number =/= JobNumber end, Jobs)
    end,
    UpdateResult = maps:update_with(WorkerPid, FilterByJobNumber, Inflight),
    case UpdateResult of
        {badkey, _} -> {error, wrong_pid};
        Result -> {ok, Result}
    end.

test() ->
    test0(),
    test1(),
    test3(),
    test4().
test0() ->
    start(),
    No = add_job(fun() -> 1 + 1 end),
    [{queued, 1}, {in_progress, 0}, {finished, 0}] = statistics(),
    {No, Fn} = work_wanted(),
    [{queued, 0}, {in_progress, 1}, {finished, 0}] = statistics(),
    2 = Fn(),
    job_done(No),
    [{queued, 0}, {in_progress, 0}, {finished, 1}] = statistics(),
    stop(),
    io:format("Test0 success~n").

test1() ->
    start(),
    No0 = add_job(fun() -> 1 + 1 end),
    No1 = add_job(fun() -> 4 + 4 end),
    [{queued, 2}, {in_progress, 0}, {finished, 0}] = statistics(),
    {No0, Fn0} = work_wanted(),
    [{queued, 1}, {in_progress, 1}, {finished, 0}] = statistics(),
    {No1, Fn1} = work_wanted(),
    [{queued, 0}, {in_progress, 2}, {finished, 0}] = statistics(),
    no = work_wanted(),
    2 = Fn0(),
    job_done(No0),
    [{queued, 0}, {in_progress, 1}, {finished, 1}] = statistics(),
    8 = Fn1(),
    job_done(No1),
    [{queued, 0}, {in_progress, 0}, {finished, 2}] = statistics(),
    stop(),
    io:format("Test1 success~n").


test3() ->
    start(),
    add_job(fun() -> 1 + 1 end),
    [{queued, 1}, {in_progress, 0}, {finished, 0}] = statistics(),
    spawn(fun() -> work_wanted(), throw(kaputt) end),
    [{queued, 1}, {in_progress, 0}, {finished, 0}] = statistics(),
    stop(),
    io:format("Test3 success~n").


test4() -> 
    start(),
    add_job(fun() -> 1 + 1 end),
    spawn(fun() -> 
        {JobNumber, _Fn} = work_wanted(),
        io:format("Waiting..."),
        timer:sleep(8000),
        job_done(JobNumber)
    end
    ),
    timer:sleep(1000),
    [{queued, 0}, {in_progress, 1}, {finished, 0}] = statistics(),
    timer:sleep(4000),
    [{queued, 1}, {in_progress, 0}, {finished, 0}] = statistics(),
    stop(),
    io:format("Test4 success~n").
