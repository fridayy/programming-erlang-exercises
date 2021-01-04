-module(c13monitor).

-compile(export_all).

boot() ->
    Parent = spawn(c13monitor, parent, []),
    register(parent, Parent),
    ok.

parent() ->
    spawn_child(),
    ploop().

spawn_child() ->
    {Child, _} = spawn_monitor(c13monitor, child, []),
    register(child, Child),
    io:format("[parent] child spawned ~n").

ploop() ->
    receive
        {'DOWN', _, _Process, _Cpid, normal} ->
            io:format("[parent] child did finish normally ~n"),
            ploop();
        {'DOWN', _, _Process, _CPid, {{nocatch, Reason}, _}} ->
            io:format("[parent] child crashed: ~p~n", [Reason]),
            spawn_child(),
            ploop();
        Any ->
            io:format("[parent] received ~p~n", [Any]),
            ploop()
    end.

child() ->
    receive
        {_From, kill} -> throw(killed);
        Any -> 
            io:format("[child] received ~p~n", [Any]),
            child()
    end.
