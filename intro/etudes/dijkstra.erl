-module(dijkstra).
-export([gcd/2, gcd_if/2]).

gcd(M, N) when M == N ->
   M;

gcd(M, N) when M >  N ->
   gcd(M-N, N);

gcd(M, N) ->
   gcd(M, N-M).


gcd_if(M, N) ->
   if
      M == N ->
         M;
      M >  N ->
         gcd(M-N, N);
      true ->
         gcd(M, N-M)
   end.
