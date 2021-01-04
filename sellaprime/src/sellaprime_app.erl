%%%-------------------------------------------------------------------
%% @doc sellaprime public API
%% @end
%%%-------------------------------------------------------------------

-module(sellaprime_app).

-behaviour(application).

-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
    sellaprime_sup:start_link().

stop(_State) ->
    ok.

%% internal functions
