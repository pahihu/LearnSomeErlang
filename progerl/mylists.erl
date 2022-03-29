-module(mylists).
-export([sum/1, map/2]).

sum([N | T]) ->
   N + sum(T);
sum([]) ->
   0.

map(F, [H|T]) ->
   [F(H) | map(F, T)];
map(F, []) ->
   [].
