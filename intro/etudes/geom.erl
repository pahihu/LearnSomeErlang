-module(geom).
-export([area/2]).

%% @doc Calculates the area of a rectangle, given the
%% length and width. Returns the product
%% of its arguments.

-spec(area(number(), number()) -> number()).

area(L, W) -> L * W.
