%%%-------------------------------------------------------------------
%% @doc sellaprime prime worker supervisor
%% @end
%%%-------------------------------------------------------------------

-module(sellaprime_prime_worker_sup).

-behaviour(supervisor).

-export([start_link/0]).

-export([init/1]).

-define(SERVER, ?MODULE).

start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

%% sup_flags() = #{strategy => strategy(),         % optional
%%                 intensity => non_neg_integer(), % optional
%%                 period => pos_integer()}        % optional
%% child_spec() = #{id => child_id(),       % mandatory
%%                  start => mfargs(),      % mandatory
%%                  restart => restart(),   % optional
%%                  shutdown => shutdown(), % optional
%%                  type => worker(),       % optional
%%                  modules => modules()}   % optional
init([]) ->
    SupFlags = #{strategy => one_for_one,
                 intensity => 1,
                 period => 1},
    ChildSpecs = [
        %exercise 3
        #{
            id => prime_worker_1,
            start => {sellaprime_prime_server, start_link, [prime_worker_1]}
          },
        #{
            id => prime_worker_2,
            start => {sellaprime_prime_server, start_link, [prime_worker_2]}
          }
        ],
    {ok, {SupFlags, ChildSpecs}}.

%% internal functions
