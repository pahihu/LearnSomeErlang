-module(ask_area).
-export([area/0]).

area() ->
   Shape = get_shape(),
   if
      Shape == unknown ->
         ok;
      true ->
         {Prompt1, Prompt2} = get_prompt(Shape),
         {N1, N2} = get_dimensions(Prompt1, Prompt2),
         calculate(Shape, N1, N2)
   end.

get_shape() ->
   Input = io:get_line("R)ectangle, T)riangle, or E)llipse > "),
   Value = string:strip(Input, right, $\n),
   char_to_shape(hd(Value)).

get_prompt(rectangle) -> {"width", "height"};
get_prompt(triangle)  -> {"base", "height"};
get_prompt(ellipse)   -> {"major axis", "minor axis"}.

char_to_shape($r) -> rectangle;
char_to_shape($t) -> triangle;
char_to_shape($e) -> ellipse;

char_to_shape($R) -> rectangle;
char_to_shape($T) -> triangle;
char_to_shape($E) -> ellipse;

char_to_shape(X) ->
   io:format("Unknown shape ~s~n", [[X]]),
   unknown.

get_number(Prompt) ->
   io:format("Enter ~s > ", [Prompt]),
   Input = io:get_line(""),
   Value = string:strip(Input, right, $\n),
   case string:to_float(Value) of
      {error, no_float} ->
         {I, _} = string:to_integer(Value),
         I;
      {F, _} ->
         F
   end.

get_dimensions(Prompt1, Prompt2) ->
   N1 = get_number(Prompt1),
   N2 = get_number(Prompt2),
   {N1, N2}.

calculate(Shape, N1, N2) ->
   if
      Shape == unknown ->
         io:format("Unknown shape.~n");
      not is_number(N1) ->
         io:format("Error in first number.~n");
      not is_number(N2) ->
         io:format("Error in second number.~n");
      N1 < 0; N2 < 0 ->
         io:format("Both numbers must be greater than or equal to zero.~n");
      true ->
         geom3:carea(Shape, N1, N2)
   end. 
