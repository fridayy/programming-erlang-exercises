-module(lib_chan_fc).
-compile(export_all).

connect(Addr, Port) ->
    Self = self(),
    {ok, ProxyPid} = lib_chan:connect(Addr, Port, fun(_ProxyPid, Message) -> 
            Term = binary_to_term(Message),
            Self ! Term
        end),
    register(?MODULE, ProxyPid).

ls() -> rpc({list_dir}).
cat(Filename) -> rpc({get_file, Filename}).

rpc(Request) -> 
    lib_chan:send(?MODULE, term_to_binary(Request)),
    receive 
        Response -> Response
    after 
        5000 ->
            {error, timeout}
    end.

disconnect() -> 
    unregister(?MODULE),
    lib_chan:disconnect().