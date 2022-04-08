-module(mymaps).
-export([count_characters/1, test/0]).
-export([to_json/1, from_json/1]).

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
      is_float(X) ->
         float_to_list(X, [compact]);
      is_binary(X) ->
         "\"" ++ binary_to_list(X) ++ "\"";
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

skip(X) -> string:trim(X, leading).

from_json([], A) ->
   lists:reverse(A);
from_json([$ | T], A) ->
   from_json(skip(T), A);
from_json(X, A) ->
   {V, R} = json_to_any(X),
   from_json(R, [V | A]).

from_json(X) ->
   from_json(string:trim(X), []).

char_letter(X) -> ($a =< X) andalso (X =< $z).
char_number(X) ->
   (($0 =< X) andalso (X =< $9))
   orelse (X =:= $+)
   orelse (X =:= $-)
   orelse (X =:= $E)
   orelse (X =:= $e)
   orelse (X =:= $.).
char_atom(X)  -> char_letter(X) orelse char_number(X).
char_notquote(X) -> $" /= X.


xtakewhile(P, L) ->
   xtakewhile(P, skip(L), []).

xtakewhile(_P, [], A) ->
   {lists:reverse(A), []};
xtakewhile(P, L = [H|T], A) ->
   case P(H) of
      true  -> xtakewhile(P, T, [H|A]);
      false -> {lists:reverse(A), L}
   end.
   
to_number(X) ->
   case string:to_float(X) of
      {error, _Reason} ->
         {I, []} = string:to_integer(X),
         I;
      {F, []} ->
         F
   end.

parse_atom(L) ->
   {S, R} = xtakewhile(fun char_atom/1, L),
   {list_to_existing_atom(S), R}.

parse_number(L) ->
   {S, R} = xtakewhile(fun char_number/1, L),
   {to_number(S), R}.

parse_binary(L) ->
   {S, [$" | R]} = xtakewhile(fun char_notquote/1, L),
   {list_to_binary(S), R}.

parse_key(L) ->
   {S, [$" | R]} = xtakewhile(fun char_notquote/1, L),
   {list_to_existing_atom(S), R}.


parse_list([$] | T], A) ->
   {lists:reverse(A), T};
parse_list([$ | T], A) ->
   parse_list(skip(T), A);
parse_list([$, | T], A) ->
   parse_list(T, A);
parse_list(L, A) ->
   {V, R} = json_to_any(L),
   parse_list(R, [V | A]).


parse_map([$} | T], M) ->
   {M, T};
parse_map([$ | T], M) ->
   parse_map(skip(T), M);
parse_map([$, | T], M) ->
   parse_map(T, M);
parse_map([$" | T], M) ->
   {K, R1} = parse_key(T),
   [$: | R2] = skip(R1),
   {V, R3} = json_to_any(skip(R2)),
   parse_map(R3, M#{ K => V}).
   

json_to_any(L = [H|T]) ->
   if
      ($a =< H) andalso (H =< $z) ->
         parse_atom(L);
      ($0 =< H andalso H =< $9) orelse (H =:= $-) ->
         parse_number(L);
      $" =:= H ->
         parse_binary(T);
      $[ =:= H ->
         parse_list(T, []);
      ${ =:= H ->
         parse_map(T, #{})
   end;
json_to_any([]) ->
   ok.


test() ->
   #{101 := 1, 104 := 1, 108 := 2, 111 := 1} = count_characters("hello"),
   ok.
