-module(powers).
-export([raise/2, nth_root/2]).
-define(EPSILON, 1.0e-8).

raise(X, 0) ->
   1;
raise(X, 1) ->
   X;
raise(X, N) when N > 0 ->
   % X * raise(X, N-1);
   raise(X, N, 1);
raise(X, N) ->
   1.0 / raise(X, -N).

raise(X, 0, Acc) ->
   Acc;
raise(X, N, Acc) ->
   raise(X, N-1, X*Acc).


%% Newton-Raphson method
nth_root(X, N) ->
   nth_root(X, N, X/2.0).

nth_root(X, N, A) ->
   io:format("Current guess is ~w~n", [A]),
   F = raise(A, N) - X,
   Fprime = N * raise(A, N-1),
   Next = A - F / Fprime,
   Change = abs(Next -A),
   if
      Change < ?EPSILON ->
         Next;
      true ->
         nth_root(X, N, Next)
   end.
