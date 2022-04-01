-module(math_functions).
-export([even/1, odd/1, filter/2, split1/1, split2/1]).
-export([test/0]).

even(X) ->
   0 =:= (X rem 2).

odd(X) ->
   1 =:= (X rem 2).


filter(F,L) -> [X || X <- L, F(X)].

split1(L) ->
   split1(L, {[], []}).

split1([], {Even, Odd}) ->
   {lists:reverse(Even), lists:reverse(Odd)};
split1([H|T], {Even, Odd}) ->
   NewResult = case even(H) of
      true  -> {[H|Even], Odd};
      false -> {Even, [H | Odd]}
   end,
   split1(T, NewResult).

split2(L) ->
   {filter(fun even/1, L), filter(fun odd/1, L)}.


test() ->
   true = even(2),
   false = even(3),
   true = odd(3),
   false = odd(2),
   [2,4] = filter(fun even/1, lists:seq(1,5)),
   [1,3,5] = filter(fun odd/1, lists:seq(1,5)),
   {[2,4], [1,3,5]} = split1(lists:seq(1,5)),
   {[2,4], [1,3,5]} = split2(lists:seq(1,5)),
   ok.
