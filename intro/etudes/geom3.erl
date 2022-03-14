-module(geom3).
-export([area/3, area/2, area/1, carea/3]).

area({Shape, A, B}) -> area(Shape, A, B).

-spec(area(atom(), number()) -> number()).

area(square, A) when A >= 0 -> area(rectangle, A, A);

area(circle, R) when R >= 0 -> area(ellipse, R, R).

%% @doc Calculates the area of a shape, given the
%% shape and two of the dimensions. Returns the product
%% of its arguments for a rectangle, one half the
%% product of the arguments for a triangle, and
%% math:pi times the product of the arguments for
%% an ellipse.

-spec(area(atom(), number(), number()) -> number()).

area(rectangle, L, W) when W >= 0, L >= 0 -> L * W;

area(triangle, B, H) when B >= 0, H >= 0 -> (B * H) / 2.0;

area(ellipse, A, B) when A >= 0, B >= 0 -> math:pi() * A * B;

area(_Other, _A, _B) -> 0.


-type shape() :: 'rectangle' | 'triangle' | 'ellipse' | atom().
-spec carea(shape(), number(), number()) -> number().

carea(Shape, A, B) when A >= 0, B >= 0 ->
   Scale = case Shape of
      rectangle -> 1.0;
      triangle  -> 0.5;
      ellipse   -> math:pi();
      Other     -> 0
   end,
   Scale * A * B.
