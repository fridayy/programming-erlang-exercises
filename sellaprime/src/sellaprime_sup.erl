%%%-------------------------------------------------------------------
%% @doc sellaprime top level supervisor.
%% @end
%%%-------------------------------------------------------------------

-module(sellaprime_sup).

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
    SupFlags = #{strategy => one_for_all, % if the loadbalancer crashes, lets crash the worker supervisor as well
                 intensity => 1,
                 period => 1},
    ChildSpecs = [
        % exercise 2
        % #{
        %     id => prime_tester_server1,
        %     start => {prime_tester_server, start_link, [prime_tester_server1, default_prime_tester]}
        % },
        % #{
        %     id => prime_tester_server2,
        %     start => {prime_tester_server, start_link, [prime_tester_server2, default_prime_tester]}
        % },
        % #{
        %   id => queue_server,
        %   start => {queue_server, start_link, [[prime_tester_server1, prime_tester_server2]]}  
        %  }
        
        %exercise 3
        #{
            id => sellaprime_loadbalancer,
            start => {sellaprime_loadbalancer, start_link, [[prime_worker_1, prime_worker_2]]}   
         },
        #{
            id => sellaprime_prime_worker_sup,
            start => {sellaprime_prime_worker_sup, start_link, []},
            type => supervisor
        }
        ],
    {ok, {SupFlags, ChildSpecs}}.

%% internal functions
