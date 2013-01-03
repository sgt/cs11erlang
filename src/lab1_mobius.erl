-module(lab1_mobius).
-export([is_prime/1, prime_factors/1]).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

%%is_divisible(N, X) when X*X > N -> false;
is_divisible(N, X) ->
    N rem X == 0.

is_prime(1) -> true;
is_prime(2) -> true;
is_prime(N) when N > 2 ->
    L = [X || X <- lists:seq(2, N-1), is_divisible(N, X)],
    io:format("~p ~w~n", [N, L]),
    length(L) == 0.

prime_factors_recur(1, Acc) -> 
    Acc;
prime_factors_recur(N, Acc) when N > 1 ->
    case is_prime(N) of
        true ->
            [N|Acc];
        _ ->
            L = [X || X <- lists:seq(N-1, 2, -1), is_prime(X) and is_divisible(N, X)],
            [Divisor|_] = L,
            prime_factors_recur(N div Divisor, [Divisor|Acc])
    end.

prime_factors(N) ->
    prime_factors_recur(N, []).

-ifdef(TEST).

is_prime_test() ->
    [?assert(is_prime(2)),
     ?assertNot(is_prime(28)),
     ?assertNot(is_prime(57)),
     ?assert(is_prime(61))
    ].

prime_factors_test() ->
    [?assertEqual([2], prime_factors(2)),
     ?assertEqual([2,3], prime_factors(6)),
     ?assertEqual([2,2,2,3], prime_factors(24))
    ].
-endif.
