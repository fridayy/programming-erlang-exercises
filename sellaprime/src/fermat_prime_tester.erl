-module(fermat_prime_tester).
-behaviour(prime_tester).
-export([is_prime/1]).


is_prime(N) -> 
    fermat(N).

% fermats little theorem to check if the given number N is probably a prime number.
fermat(N) when N < 3 -> composite;
fermat(N) -> do_fermat(N, 0, 100).

do_fermat(_N, Iteration, K) when Iteration =:= K ->
    true;
do_fermat(N, Iteration, K) ->
    A = make_random(N - 2),
    R0 = pow(A, N - 1) rem N,
    R1 = 1 rem N,
    case R0 =:= R1 of
        true -> do_fermat(N, Iteration + 1, K);
        false -> false
    end.

make_random(Max) ->
    R = rand:uniform(Max),
    if R < 3 -> make_random(Max);
       true -> R
    end.

% calcuates the pow using exponentation by squaring
% https://en.wikipedia.org/wiki/Exponentiation_by_squaring
pow(X, N) ->
    do_pow(1, X, N).

do_pow(Y, X, N) when N < 0 ->
    do_pow(Y, 1 div X, -N);
do_pow(_Y,_X, 0) -> 1;
do_pow(Y,X, 1) -> Y * X;
do_pow(Y,X, N) ->
    if
        N rem 2 =:= 0 -> %even
            do_pow(Y, X * X, N div 2);
        true ->
            do_pow(X * Y, X * X, (N - 1) div 2)
    end.