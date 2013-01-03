-module(lab1_mobius).
-export([is_prime/1, prime_factors/1, find_square_multiples/2]).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

%% an optimised is_divisible that runs checks only on X < sqrt(N)
is_divisible_opt(N, X) when X*X > N -> false;
is_divisible_opt(N, X) ->
    N rem X == 0.

is_prime_recur(1, _) -> true;
is_prime_recur(2, _) -> true;
is_prime_recur(_, 1) -> true;
is_prime_recur(N, X) when N > 2, X > 1 ->
    case is_divisible_opt(N, X) of
        true -> false; % not a prime
        _ -> is_prime_recur(N, X-1)
    end.
            
%% is N a prime number?
is_prime(N) ->
    is_prime_recur(N, N-1).

prime_factors_recur(1, Acc) -> Acc;
prime_factors_recur(N, Acc) when N > 1 ->
    L = lists:dropwhile(fun(X) -> not is_prime(X) or (N rem X =/= 0) end,
                        lists:seq(2, N)),
    [Divisor|_] = L,
    prime_factors_recur(N div Divisor, [Divisor|Acc]).

%% list prime factors of N
prime_factors(1) -> [1];
prime_factors(N) ->
    lists:reverse(prime_factors_recur(N, [])).

is_square_multiple(N) ->
    Factors = prime_factors(N),
    UniqueFactors = sets:to_list(sets:from_list(Factors)),
    length(Factors) =/= length(UniqueFactors).

count_square_multiples(N, MaxN) ->
    L = lists:takewhile(fun(X) -> is_square_multiple(X) end, lists:seq(N, MaxN)),
    length(L).

find_square_multiples_recur(Start, MaxN, _Count) when Start > MaxN ->
    fail;
find_square_multiples_recur(Start, MaxN, Count) ->
    ActualCount = count_square_multiples(Start, MaxN),
    case ActualCount >= Count of
        true ->
            Start;
        _ ->
            Skip = max(1, ActualCount),
            find_square_multiples_recur(Start + Skip, MaxN, Count)
    end.

find_square_multiples(Count, MaxN) ->
    find_square_multiples_recur(1, MaxN, Count).

-ifdef(TEST).

is_prime_test() ->
    [?assert(is_prime(2)),
     ?assertNot(is_prime(28)),
     ?assertNot(is_prime(57)),
     ?assert(is_prime(61))
    ].

prime_factors_test() ->
    [?assertEqual([1], prime_factors(1)),
     ?assertEqual([2], prime_factors(2)),
     ?assertEqual([2,3], prime_factors(6)),
     ?assertEqual([2,2,2,3], prime_factors(24))
    ].

is_square_multiple_test() ->
    [?assert(is_square_multiple(24)),
     ?assertNot(is_square_multiple(15))
    ].

find_square_multiples_test() ->
    [?assertEqual(48, find_square_multiples(3, 50)),
     ?assertEqual(fail, find_square_multiples(3, 20))
    ].

-endif.
