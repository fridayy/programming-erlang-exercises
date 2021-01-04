-module(lib_chan_fs).
-export([start/1, stop/0]).

start(Dir) -> 
    FsPid = spawn(fun() -> loop(Dir) end),
    register(?MODULE, FsPid),
    lib_chan:start(9999, fun(ProxyPid, Message) -> 
        Term = binary_to_term(Message, [safe]),
        FsPid ! {self(), Term},
        receive
            {ok, Response} ->
                lib_chan:send(ProxyPid, term_to_binary(Response))
            end
        end),
    ok.

stop() ->
    ?MODULE ! {self(), stop},
    unregister(?MODULE),
    lib_chan:stop(),
    io:format("Stopped~n").

loop(Dir) -> 
  receive
    {Client, {list_dir}} -> 
      Client ! {ok, file:list_dir(Dir)},
      loop(Dir);
    {Client, {get_file, File}} ->
      Full = filename:join(Dir, File),
      Client ! {ok, file:read_file(Full)},
      loop(Dir);
    {_Client, stop} ->
        io:format("Stopping~n")
  end.

