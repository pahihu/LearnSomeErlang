-module(exrev2).
-export([f/1, r/1, deg/1, rad/1]).

f(X) ->
   0.8 * math:cos(X).

r(X) ->
   Y = X / 0.8,
   math:acos(Y).

deg(X) ->
   X / math:pi() * 180.0.

rad(X) ->
   X / 180.0 * math:pi().
