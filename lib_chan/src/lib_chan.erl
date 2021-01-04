-module(lib_chan).

-export([start/1, start/2, start/3, stop/0, connect/3, send/2, disconnect/1]).


% ~ client
connect(Addr, Port, Fun) ->
    {ok, Socket} = gen_tcp:connect(Addr, Port, [binary, {active, false}, {packet, 0}]),
    lib_chan_conn:new_connection(Socket, Fun).

send(ProxyPid, Message) ->
    lib_chan_conn:send(ProxyPid, Message).

disconnect(ProxyPid) ->
    lib_chan_conn:send(ProxyPid, close).

% ~ server
default_handler(ProxyPid, Message) ->
    io:format("~p ! ~p ~n", [ProxyPid, Message]),
    lib_chan_conn:send(ProxyPid, Message).

start(Port) ->
    start(Port, fun default_handler/2).

start(Port, Mod, Fun) ->
    start(Port, fun Mod:Fun/2).

start(Port, Fun) ->
    spawn(fun () -> do_start_server(Port, Fun) end),
    io:format("Server started~n"),
    ok.

stop() -> 
    lib_chan_exit ! close,
    unregister(lib_chan_exit),
    ok.

do_start_server(Port, Fun) ->
    {ok, LSocket} = gen_tcp:listen(Port,
                                   [binary, {active, false}, {packet, 0}]),
    ExitListener = spawn(fun () -> exit_listener(LSocket) end),
    register(lib_chan_exit, ExitListener),
    loop(LSocket, Fun).

loop(LSocket, Fun) ->
    case gen_tcp:accept(LSocket) of
        {ok, Socket} -> 
            lib_chan_conn:new_connection(Socket, Fun),
            loop(LSocket, Fun);
        {error, _Reason} ->
            io:format("Server closed ~n")
    end.

exit_listener(LSocket) ->
    receive
        close -> 
            gen_tcp:close(LSocket),
            io:format("LSocket closed~n")
    end.