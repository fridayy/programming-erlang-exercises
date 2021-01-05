-module(prime_tester_server).

-behaviour(gen_server).

-compile(export_all).

-define(TIMEOUT, 10000).


%% Prime Tester Server that satisfies the requires of Chapter 23 exercise 2.
%%

-record(state, {id, primetester}).

is_prime(Worker, ReplyTo, N) ->
    gen_server:cast(Worker, {ReplyTo, N}).

start_link(Id, PrimeTesterModule) ->
    gen_server:start_link({local, Id},
                          ?MODULE,
                          [Id, PrimeTesterModule],
                          []).

init([Id, PrimeTesterModule] = Args) ->
    io:format("~p starting with args: ~p~n",
              [Id, Args]),
    {ok, #state{id = Id, primetester = PrimeTesterModule}}.

handle_call(_Request, _From, State) ->
    {reply, ok, State}.

handle_cast({ReplyTo, N},
            #state{id = Id, primetester = Tester} = State) ->
    queue_server:set_busy(Id),
    calculate_is_prime(Tester, N, ReplyTo, Id),
    {noreply, State}.

%
% Tries to calculate a prime number using the given prime tester callback module.
% If the calculation does not finish within TIMEOUT the calculating process is killed.
calculate_is_prime(PrimeTester, N, ReplyTo, Id) ->
    spawn(fun () ->
                  Self = self(),
                  CalcPid = spawn(fun () ->
                                          io:format("[~p] Calculating prime...~n", [Id]),
                                          IsPrime = PrimeTester:is_prime(N),
                                          SleepFor = rand:uniform(10) * 1000,
                                          timer:sleep(SleepFor),
                                          io:format("[~p] Successfully calculated ~n",
                                                    [Id]),
                                          Self ! IsPrime
                                  end),
                  receive
                      Result -> queue_server:fulfil(ReplyTo, {ok, Result})
                      after 3000 ->
                                io:format("[~p] Killing calculation ~p too slow~n",
                                          [Id, CalcPid]),
                                exit(CalcPid, bye),
                                queue_server:fulfil(ReplyTo, {error, timeout, N})
                  end,
                  queue_server:set_waiting(Id)
          end).
