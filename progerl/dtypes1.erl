-module(dtypes1).
-export([f1/1, f2/1, f3/1]).
-export([myand1/2]).

bug1(X, Y) ->
   case myand1(X, Y) of
      true ->
         X + Y
   end.

myand1(true, true) -> true;
myand1(false,   _) -> false;
myand1(   _,false) -> false.

-spec f1({number(), string(), number()}) -> number().
f1({H,M,S}) ->
   (H*60+M)*60+S.

-spec f2({integer(), number(), number()}) -> number().
f2({H,M,S}) when is_integer(H) ->
   (H*60+M)*60+S.

-spec f3({integer(), integer(), integer()}) -> integer().
f3({H,M,S}) ->
   print(H,M,S),
   (H*60+M)*60+S.

-spec print(integer(), integer(), integer()) -> ok.
print(H,M,S) ->
   Str = integer_to_list(H) ++ ":" ++ integer_to_list(M) ++ ":" ++
         integer_to_list(S),
   io:format("~s", [Str]).
