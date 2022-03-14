-module(sieve2).
-export([sieve/1]).

sieve2([H|T], N, R) when H =< N ->
   L = lists:filter(fun (X) -> X rem H /= 0 end, T),
   sieve2(L, N, [H|R]);
sieve2(L, _, R) ->
   lists:reverse(R) ++ L.

sieve2(N) ->
   sieve2(lists:seq(2, N), math:pow(N, 0.5), []).

