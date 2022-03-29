-module(shop3).
-export([total/1]).
-import(lists, [sum/1]).

total(L) ->
   sum([shop:cost(What) * N || {What, N} <- L]).

map(F, L) -> [F(X) || X <- L].
