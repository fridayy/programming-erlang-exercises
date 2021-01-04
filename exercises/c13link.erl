-module(c13link).
-export([boot/0]).

% Chapter 13 exercises and error handling stuff
boot() -> 
    ParentPid = spawn(c13link, parent, []),
    register(parent, ParentPid).

% the parent process is the supervisor process of the child process
% by trapping the exit signal and restarting the process using the firewall approach
% see 13.5 p. 205
% Whereby this implementation should be done using a monitor instead of a link
parent() ->
    process_flag(trap_exit, true),
    spawn_child(),
    ploop().

spawn_child() ->
    ChildPid = spawn_link(c13link, cloop, []),
    register(child, ChildPid).

% the recv loop of the parent (supervisor) process.
ploop() -> 
    receive
        {'EXIT', _KilledPid, Reason} ->
            spawn_child(),
            io:format("Child restarted - Reason: ~p ~n", [Reason]);
        Any -> 
            io:format("[<<][~p]~p~n", [self(), Any]),
            ploop()
    end.

% the recv loop of the child (supervised) process.
cloop() -> 
    receive
        {_From, kill} -> 
            throw(goodbyte);
        Any -> 
            io:format("[<<][~p]~p~n", [self(), Any]),
            cloop()
    end.