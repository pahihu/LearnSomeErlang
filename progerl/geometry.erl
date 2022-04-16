-module(geometry).
-export([test/0, area/1, perimeter/1, pa_ratio/1]).

area({rectangle, Width, Height}) ->
   Width * Height;
area({circle, Radius}) ->
   math:pi() * Radius * Radius;
area({right_triangle, A, B, C}) when A + B > C,
                                     B + C > A,
                                     C + A > B ->
   A * B / 2;
area(T = {triangle, A, B, C}) when A + B > C,
                                   B + C > A,
                                   C + A > B ->
   S = perimeter(T) / 2.0,
   math:sqrt(S * (S - A) * (S - B) * (S - C));
area({square, Side}) ->
   Side * Side.

perimeter({rectangle, Width, Height}) ->
   2 * (Width + Height);
perimeter({circle, Radius}) ->
   2 * math:pi() * Radius;
perimeter({Triangle, A, B, C}) when Triangle =:= triangle; 
                                    Triangle =:= right_triangle ->
   A + B + C;
perimeter({square, Side}) ->
   4 * Side.

pa_ratio(Shape) ->
   perimeter(Shape) / area(Shape).


test() ->
   12  = area({rectangle, 3, 4}),
   144 = area({square, 12}),
   16  = perimeter({triangle, 7, 6, 3}),
   14  = perimeter({rectangle, 3, 4}),
   48  = perimeter({square, 12}),
   tests_worked.
