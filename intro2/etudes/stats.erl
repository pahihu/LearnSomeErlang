-module(stats).
-export([minimum/1, maximum/1, mean/1, stdv/1]).

minimum(L) ->
   try minimum(hd(L), tl(L))
   catch
      error:Error -> {error, Error}
   end.

minimum(M, []) ->
   M;
minimum(M, [H|T]) when M =< H ->
   minimum(M, T);
minimum(M, [H|T]) ->
   minimum(H, T).

maximum(L) ->
   try maximum(hd(L), tl(L))
   catch
      error:Error -> {error, Error}
   end.

maximum(M, []) ->
   M;
maximum(M, [H|T]) when M >= H ->
   maximum(M, T);
maximum(M, [H|T]) ->
   maximum(H, T).

avg(L) ->
   lists:sum(L) / length(L).

mean(L) ->
   try avg(L)
   catch
      error:Error -> {error, Error}
   end.

stdv(L) ->
   try
      N = length(L),
      Mu = avg(L),
      S = [(X - Mu) * (X - Mu) || X <- L],
      math:sqrt(lists:sum(S) / (N - 1))
   catch
      error:Error -> {error, Error}
   end.
