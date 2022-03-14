-module(non_fp).
-export([generate_teeth/2]).

generate_teeth(Teeth, P) ->
   rand:seed(default),
   generate_teeth(Teeth, P, []).

generate_teeth([], P, L) ->
   lists:reverse(L);
generate_teeth([$F | Tail], P, L) ->
   generate_teeth(Tail, P, [[0] | L]);
generate_teeth([$T | Tail], P, L) ->
   generate_teeth(Tail, P, [generate_tooth(P) | L]).

generate_tooth(P) ->
   R = rand:uniform(),
   if
      R < P ->
         BaseDepth = 2;
      true ->
         BaseDepth = 3
   end,
   generate_tooth(BaseDepth, 6, []).

generate_tooth(BaseDepth, 0, L) ->
   lists:reverse(L);
generate_tooth(BaseDepth, N, L) ->
   generate_tooth(BaseDepth, N-1, [BaseDepth + rand:uniform(2) - 1 | L]).
