-module(afile_client).
-export([ls/1]).

ls(Server) ->
    Server ! {self(), list_dir},
    receive
        {_Server, {ok, Files}} ->
            lists:foreach(fun(File) -> io:format("~s~n", [File]) end, Files);
        _ -> io:format("Something went wrong ~n")
    end.