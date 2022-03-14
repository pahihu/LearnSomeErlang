-module(ot).
-export([ot/1]).

ot(X) when is_integer(X), X > 0 ->
   'positive natural';
ot(X) when is_integer(X), X < 0 ->
   'negative integer';
ot(X) when is_integer(X) ->
   integer;
ot(X) when is_float(X) ->
   float;
ot(X) when is_list(X) ->
   list;
ot(X) when is_tuple(X) ->
   tuple;
ot(X) when is_atom(X) ->
   atom.
