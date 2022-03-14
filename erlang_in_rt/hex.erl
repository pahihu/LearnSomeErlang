-module(hex).
-export([hexdec/1, hexadd/0]).

hexadd() ->
   X = (catch adder()),
   case X of
      {'EXIT', Reason} ->
         io:format("Error ~w~n", [X]);
      {fail, badchar} ->
         io:format("Error hex digits must are 0-9 a-f~n");
      {fail, nullarg} ->
         io:format("Error value required~n");
      Y -> Y
   end.

adder() ->
   {ok, [A]} = io:fread('Enter first number > ', "~a"),
   {ok, [B]} = io:fread('Enter second number > ', "~a"),
   hexdec(A) + hexdec(B).

hexdec(Atom) ->
   L = atom_to_list(Atom),
   case (catch hexcvt(L)) of
      {'EXIT', {function_clause, _}} ->
         throw({fail, badchar});
      {fail, X} ->
         throw({fail, X});
      R -> R
   end.

hexcvt([]) ->
   throw({fail, nullarg});
hexcvt(L) ->
   hexcvt(L, 0).

hexcvt([], N) ->
   N;
hexcvt([H|T], N) ->
   V = decval(H),
   hexcvt(T, N * 16 + V).

decval(C) when $0 =< C, C =< $9 ->
   C - $0;
decval(C) when $a =< C, C =< $f ->
   C - $a + 10.
