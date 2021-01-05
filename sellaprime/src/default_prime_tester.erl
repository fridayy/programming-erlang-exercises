-module(default_prime_tester).
-behaviour(prime_tester).
-export([is_prime/1]).

is_prime(N) when N == 2 -> true;
is_prime(N) when N < 2 orelse N rem 2 == 0 -> false;
is_prime(N) -> is_prime(N,3).
 
is_prime(N,K) when K*K > N -> true;
is_prime(N,K) when N rem K == 0 -> false;
is_prime(N,K) -> is_prime(N,K+2).