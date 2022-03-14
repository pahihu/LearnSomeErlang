-module(factprg).
-export([fact/1, fib/1]).

fact(0) ->
   1;
fact(N) ->
   N * fact(N-1).

fib(N) when N < 2 ->
   N;
fib(N) ->
   fib(N-1) + fib(N-2).
