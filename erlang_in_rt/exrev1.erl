-module(exrev1).
-export([f/1, r/1]).

f(X) ->
   1.0 + math:sqrt(X).

r(X) ->
   Y = X - 1.0,
   Y * Y.
