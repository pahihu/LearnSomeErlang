-module(a).
-compile(export_all).
-type t_rich_text()  :: [{font(), [char(),...]}].
-opaque rich_text()  :: [{font(), [char(),...]}].
-type font()      :: integer().
-export_type([t_rich_text/0, rich_text/0, font/0]).

-export([make_text/1, make_text/2, bounding_box/1]).
-spec make_text(string()) -> rich_text().
-spec make_text(font(),string()) -> rich_text().
-spec bounding_box(rich_text()) -> {Height::integer, Width::integer()}.

-define(TimesRoman, 1).
-define(Courier, 2).

default_font() ->
   ?TimesRoman.

make_text(S) ->
   make_text(default_font(), S).

make_text(F, S) -> [{F, S}].

font_height(F) ->
   case F of
      ?TimesRoman -> 11;
      ?Courier    -> 10;
      _Font       -> 12
   end.

font_width(F) ->
   case F of
      ?TimesRoman ->  9;
      ?Courier    ->  8;
      _Font       -> 10
   end.

bounding_box(RichText) ->
   lists:foldl(fun({F, S}, {Height, Width}) ->
                  {max(Height, font_height(F)),
                   max(Width,  length(S) * font_width(F))}
               end,
               {0, 0},
               RichText).


start(Tag) ->
   spawn(fun() -> loop(Tag) end).

loop(Tag) ->
   sleep(),
   Val = b:x(),
   io:format("Vsn3 (~p) b:x() = ~p~n",[Tag,Val]),
   loop(Tag).

sleep() ->
   receive
      after 3000 -> true
   end.
