-module(shop3).
-export([total/1, sum/1, map/2]).

total(L) ->
   sum(map(fun({What,N}) -> shop:cost(What) * N end, L)).

sum(L) ->
   sum(L, 0).

sum([N|T], Sum) ->
   sum(T, N + Sum);
sum([], Sum) ->
   Sum.

map(F, L) ->
   map(F, L, []).

map(F, [H|T], L) ->
   map(F, T, [F(H) | L]);
map(F, [], L) ->
   L.
   
