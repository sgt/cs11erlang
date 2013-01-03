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
        true ->
            false; % not a prime
        _ ->
            is_prime_recur(N, X-1)
    end.
            
%% is N a prime number?
is_prime(N) ->
    is_prime_recur(N, N-1).

prime_factors_recur(1, _X, Acc) -> Acc;
prime_factors_recur(N, X, Acc) when N > 1, X =< N ->
    case is_prime(X) and (N rem X == 0) of
        true ->
            %% found a factor, reset X to 2 and go up again
            prime_factors_recur(N div X, 2, [X|Acc]);
        _ ->
            prime_factors_recur(N, X+1, Acc)
    end.

%% list prime factors of N
prime_factors(1) -> [1];
prime_factors(N) ->
    lists:reverse(prime_factors_recur(N, 2, [])).

is_square_multiple(N) ->
    Factors = prime_factors(N),
    UniqueFactors = sets:to_list(sets:from_list(Factors)),
    length(Factors) =/= length(UniqueFactors).

count_square_multiples_recur(N, MaxN, Acc) when N > MaxN -> Acc;
count_square_multiples_recur(N, MaxN, Acc) ->
    case is_square_multiple(N) of
        false ->
            Acc;
        _ ->
            count_square_multiples_recur(N+1, MaxN, Acc+1)
    end.

count_square_multiples(N, MaxN) ->
    count_square_multiples_recur(N, MaxN, 0).

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

count_square_multiples_test() ->
    [?assertEqual(1, count_square_multiples(4, 5)),
     ?assertEqual(0, count_square_multiples(1, 5)),
     ?assertEqual(1, count_square_multiples(4, 4))
    ].

find_square_multiples_test() ->
    [?assertEqual(48, find_square_multiples(3, 50)),
     ?assertEqual(fail, find_square_multiples(3, 20))
    ].

-endif.
