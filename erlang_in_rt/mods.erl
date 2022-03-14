-module(mods).
-export([list/0]).

list() ->
   lists:filter(fun filter/1, code:all_loaded()).

filter(X) ->
   case X of
      {Int, interpreted} ->
         true;
      {PreLd, preloaded} ->
         true;
      _ ->
         false
   end.
