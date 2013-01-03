-module(lab1_fib).
-export([fact/1, fact_tail/1, fib_p/1, fib_g/1, tail_fib/1]).

%% simple factorial
fact(0) -> 1;
fact(N) -> N*fact(N-1).

fact_tail_rec(0, Acc) -> Acc;
fact_tail_rec(N, Acc) ->
    fact_tail_rec(N-1, Acc*N).

%% tail-recursive factorial
fact_tail(N) ->
    fact_tail_rec(N, 1).

%% simple fibonacci
fib_p(0) -> 0;
fib_p(1) -> 1;
fib_p(N) -> fib_p(N-1) + fib_p(N-2).

%% simple fibonacci with guards
fib_g(N) when N == 0 -> 0;
fib_g(N) when N == 1 -> 1;
fib_g(N) when N > 1 -> fib_g(N-1) + fib_g(N-2).

tail_fib_rec(0, Result, _Next) ->
    Result;
tail_fib_rec(Iter, Result, Next) when Iter > 0 ->
    tail_fib_rec(Iter - 1, Next, Result + Next).

%% tail-recursive fibonacci
tail_fib(N) ->
    tail_fib_rec(N, 0, 1).
