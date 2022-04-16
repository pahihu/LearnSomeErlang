-module(myc).
-export([fonts_in/1]).
-spec fonts_in(a:rich_text()) -> [a:font()].

fonts_in(Str) ->
   X = a:make_text(Str),
   [F || {F,_} <- X].
