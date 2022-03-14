-module(stats).
-export([minimum/1, maximum/1, range/1]).

minimum([H|T]) ->
   minimum(T,H).

% minimum(L) ->
%    minimum(tl(L), hd(L)).

minimum([], M) ->
   M;
minimum([H|T], M) when H < M ->
   minimum(T, H);
minimum([H|T], M) ->
   minimum(T, M).


maximum([H|T]) ->
   maximum(T, H).

maximum([], M) ->
   M;
maximum([H|T], M) when H > M ->
   maximum(T, H);
maximum([H|T], M) ->
   maximum(T, M).


range(L) ->
   [minimum(L), maximum(L)].
