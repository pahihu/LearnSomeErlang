-module(mymaps).
-export([count_characters/1, test/0]).
-export([to_json/1]).

count_characters(Str) ->
   count_characters(Str, #{}).

%% count_characters([H | T], #{ H := N } = X) ->
%%    count_characters(T, X#{ H := N+1 });
%% count_characters([H | T], X) ->
%%    count_characters(T, X#{ H => 1});
%% count_characters([], X) ->
%%    X.

count_characters([H | T], X) when is_map(X) ->
   NewN = case maps:find(H, X) of
      {ok, N} ->
         N+1;
      error ->
         1
   end,
   count_characters(T, X#{ H => NewN });
count_characters([], X) ->
   X.

comma(L) when length(L) > 0 ->
   ["," | L];
comma(L) ->
   L.

to_json(X) ->
   lists:flatten(any_to_json(X)).

any_to_json(X) ->
   if
      is_atom(X) ->
         atom_to_list(X);
      is_integer(X) ->
         integer_to_list(X);
      is_binary(X) ->
         "\"" ++ binary_to_list(X) ++ "\"";
      is_float(X) ->
         float_to_list(X, [compact]);
      is_list(X) ->
         L = lists:foldr(fun(E,A) ->
                           [any_to_json(E) | comma(A)]
                         end,
                         [], X),
         [ "[", L, "]" ];
      is_map(X) ->
         M = maps:fold(fun(K,V,A) ->
                         ["\"",any_to_json(K),"\": ",any_to_json(V) | comma(A)]
                       end,
                       [], X),
         [ "{", M, "}" ]
   end.


test() ->
   #{101 := 1, 104 := 1, 108 := 2, 111 := 1} = count_characters("hello"),
   ok.
