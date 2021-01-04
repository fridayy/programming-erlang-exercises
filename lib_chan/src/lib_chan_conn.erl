-module(lib_chan_conn).
-export([new_connection/2, send/2]).

send(ProxyPid, Message) ->
    ProxyPid ! {self(), Message}.

new_connection(Socket, Fun) -> 
    io:format("New connection established ~n"),
    ProxyPid = spawn(fun () -> client_proxy(Socket) end),
    spawn(fun () -> acceptor(Socket, ProxyPid, Fun) end),
    {ok, ProxyPid}.


acceptor(Socket, ProxyPid, Fun) ->
     case gen_tcp:recv(Socket, 0) of
         {ok, Message} -> 
             io:format("Received ~p~n", [Message]),
             spawn(fun () -> Fun(ProxyPid, Message) end),
             acceptor(Socket, ProxyPid, Fun);
        {error, closed} ->
            io:format("Connection closed~n"),
            send(ProxyPid, close),
            ok
        end.


client_proxy(ClientSocket) ->
    receive 
        {_From, close} -> 
            io:format("Client Proxy closed bye ~n");
        {_From, Message} ->
            io:format("Sending ~p to ~p~n", [Message, ClientSocket]),
            case gen_tcp:send(ClientSocket, Message) of 
                ok -> client_proxy(ClientSocket);
                {error, Reason} ->
                    io:format("ClientSocket closed due to: '~p'. Could not send message ~p~n", [Reason, Message])
            end;
        Any -> 
            io:format("Recv unknown message ~p~n", [Any]),
            client_proxy(ClientSocket)
    end.