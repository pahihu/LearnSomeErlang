-module(b).
-export([x/0]).
-export([rich_text_length/1]).
-export([do_this/0]).
-spec rich_text_length(a:t_rich_text()) -> integer().

do_this() ->
   X = a:make_text("hello world"),
   {W, H} = a:bounding_box(X),
   [W, H].

rich_text_length(RichText) ->
   lists:foldl(fun({_Font, Text}, A) ->
                  A + length(Text)
               end,
               0,
               RichText).

x() -> 3.
