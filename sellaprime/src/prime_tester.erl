-module(prime_tester).

-callback is_prime(number()) -> B :: true | false.