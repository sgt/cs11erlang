-module(lab1_mobius).
-export([is_prime/1]).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

is_divisible(N, X) when X*X>N ->
    false;
is_divisible(N, X) ->
    N rem X == 0.

is_prime(1) -> true;
is_prime(2) -> true;
is_prime(N) when N > 2 ->
    L = [X || X <- lists:seq(2, N), is_divisible(N, X)],
    length(L) == 0.

-ifdef(TEST).

is_prime_test() ->
    [?assert(is_prime(2) =:= true),
     ?assert(is_prime(28) =:= false),
     ?assert(is_prime(57) =:= false),
     ?assert(is_prime(61) =:= true)
    ].

-endif.
