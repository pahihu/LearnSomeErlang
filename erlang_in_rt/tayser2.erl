-module(tayser2).
-export([exp/1, sin/1, taylorterm/2, tayterms/2]).

epsilon() ->
   1.0e-8.

fact(0) ->
   1;
fact(N) ->
   N * fact(N-1).

taylorterm(Z, N) ->
   math:pow(Z, N) / fact(N).

exp(Z) ->
   exp(Z, 1, epsilon(), taylorterm(Z, 1)).

exp(Z, N, Epsilon, Sum) ->
   NextSum = Sum + taylorterm(Z, N),
   if
      abs(NextSum - Sum) < Epsilon ->
         NextSum;
      true ->
         exp(Z, N+1, Epsilon, NextSum)
   end.

sin(Z) ->
   sin(Z, 1, -1, epsilon(), taylorterm(Z, 1)).

sin(Z, N, 1, Epsilon, S) ->
   NextS = S + taylorterm(Z, (2*N)+1),
   if
      abs(NextS - S) < Epsilon ->
         {N, NextS};
      true ->
         sin(Z, N+1, -1, Epsilon, NextS)
   end;
sin(Z, N, -1, Epsilon, S) ->
   NextS = S - taylorterm(Z, (2*N)+1),
   if
      abs(NextS - S) < Epsilon ->
         {N, NextS};
      true ->
         sin(Z, N+1, 1, Epsilon, NextS)
   end.

tayterms(_,L,L) ->
   ok;
tayterms(Z,N,L) ->
   io:format("~w ~17.15.0f~n",[N,taylorterm(Z,(2*N)+1)]),
   tayterms(Z,N+1,L).
   
tayterms(Z,L) ->
   tayterms(Z,0,L).
