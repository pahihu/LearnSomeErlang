-module(day2).
-export([value/2, total/1, ttt/1]).

%% foreach(F,L), map(F,L), filter(F,L), all(F,L), any(F,L),
%% takewhile(F,L) dropwhile(F,L) foldl(F,E,L)

-spec value(any(), [{any(), any()}]) -> any().
value(Key,[{Key,Val}|T]) ->
   Val;
value(Key,[H|T]) ->
   value(Key,T);
value(Key,[]) ->
   not_found.

%% {Item,Quant,Price} -> {Item,TotalPrice}

-spec total([{atom(), non_neg_integer(), number()}]) -> [{atom(), number()}].
total(L) ->
   [{Item,Quant*Price} || {Item,Quant,Price} <- L].

%% has empty space in TTT table
-spec has_empty(1..9, tuple()) -> boolean().
has_empty(0,T) ->
   false;
has_empty(N,T) ->
   if
      element(N, T) == '_' ->
         true;
      true ->
         has_empty(N-1,T)
   end.

%% check winner positions
-type ttt_shape() :: 'x' | 'o' | '_'.
-type ttt_table() :: {ttt_shape(), ttt_shape(), ttt_shape(),
                      ttt_shape(), ttt_shape(), ttt_shape(),
                      ttt_shape(), ttt_shape(), ttt_shape()}.
-spec winner(ttt_table()) -> ttt_shape().

winner({X, X, X,
        _, _, _,
        _, _, _}) when X /= '_' -> X;
winner({_, _, _,
        X, X, X,
        _, _, _}) when X /= '_' -> X;
winner({_, _, _,
        _, _, _,
        X, X, X}) when X /= '_' -> X;
winner({X, _, _,
        X, _, _,
        X, _, _}) when X /= '_' -> X;
winner({_, X, _,
        _, X, _,
        _, X, _}) when X /= '_' -> X;
winner({_, _, X,
        _, _, X,
        _, _, X}) when X /= '_' -> X;
winner({X, _, _,
        _, X, _,
        _, _, X}) when X /= '_' -> X;
winner({_, _, X,
        _, X, _,
        X, _, _}) when X /= '_' -> X;
winner(_) ->
   '_'.

%% check tic-tac-toe state
-spec ttt(ttt_table()) -> 'no_winner' | 'cat' | 'x' | 'o'.
ttt(T) ->
   case winner(T) of
      '_' ->
         E = has_empty(9, T),
         if 
            E    -> no_winner;
            true -> cat
         end;
      X ->
         X
   end.

