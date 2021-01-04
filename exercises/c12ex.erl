-module(c12ex).
-export([start/2]).

% Exercise 3 Ring Benchmark
start(NumberProcesses, MaxHops) ->
    Pids = lists:map(fun(ProcessNo) -> {ProcessNo, spawn(?MODULE, ring_process, [nil, ProcessNo, MaxHops, 0])} end, range(0, NumberProcesses)),
    lists:foreach(fun({ProcessNo, Pid}) -> send_next(ProcessNo, Pid, Pids) end, Pids),
    [{_, Start} | _T] = Pids,
    recv(Start, {self(), "Hello World!"}).

send_next(ProcessNo, Pid, Pids) when ProcessNo < length(Pids) - 1 ->
    {_, NextPid} = lists:nth(ProcessNo + 2, Pids),
    Pid ! {self(), {next, NextPid}};

send_next(_ProcessNo, Pid, [{_, H} | _T]) ->
    Pid ! {self(), {next, H}}.

ring_process(NextPid, ProcessNo, MaxHops, Hops) ->
    receive
        {_From, {next, NPid}} ->
            io:format("[<<][~p][~p] Setting next pid ~p ~n",[ProcessNo, self(), NPid]),
            ring_process(NPid, ProcessNo, MaxHops, 0);
        {_From, {Origin, Message}} when MaxHops =:= Hops->
            io:format("[<<][~p] Roundtrip completed we are done ~n", [ProcessNo]),
            Origin ! {self(), {done, Message}};
        {_From, {Origin, Message}} ->
            io:format("[<<][~p] received message - Hop: ~p ~n",
                      [ProcessNo, Hops]),
            NextPid ! {self(), {Origin, Message}},
            ring_process(NextPid, ProcessNo, MaxHops, Hops + 1);
        Message -> throw(Message)
    end.

% Sends a message and waits for the response
recv(Pid, Message) ->
    Start = os:perf_counter(1000),
    Pid ! {self(), Message},
    receive {Pid, M} -> 
        Stop = os:perf_counter(1000),
        io:format("Received response after: ~p ms", [Stop - Start]),
        M
    end.


range(From, To) -> 
    do_range(From, To, []).

do_range(From, Iter, Acc) when Iter =:= From ->
    Acc;

do_range(From, Iter, Acc) -> 
    NewIter = Iter - 1,
    do_range(From, NewIter, [NewIter | Acc]).