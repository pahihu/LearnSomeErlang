-module(mylists).
-export([sum/1, map/2, filter/2, patfilter/2]).

sum([N | T]) ->
   N + sum(T);
sum([]) ->
   0.

map(F, [H|T]) ->
   [F(H) | map(F, T)];
map(F, []) ->
   [].

filter(P, [H|T]) ->
   case P(H) of
      true  -> [H | filter(P, T)];
      false -> filter(P, T)
   end;
filter(P, []) ->
   [].

%% pattern-matching only
patfilter(P, [H|T]) ->
   patfilter1(P(H), H, P, T);
patfilter(P, []) ->
   [].

patfilter1(true, H, P, T) ->
   [H | patfilter(P, T)];
patfilter1(false, H, P, T) ->
   patfilter(P, T).
