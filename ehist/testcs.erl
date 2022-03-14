%%% Parameterizin the server

-module(testcs).
-export([start/0, add/2, lookup/1, test/0]).

start() -> cs:start(keydb, [], fun handler/2).

add(Key, Val) -> cs:rpc(keydb, {add, Key, Val}).
lookup(Key)   -> cs:rpc(keydb, {lookup, Key}).

handler({add, Key, Val}, Data) ->
   {ok, add(Key,Val,Data)};
handler({lookup, Key}, Data) ->
   {find(Key,Data), Data}.

add(Key,Val,[{Key, _}|T])  -> [{Key,Val}|T];
add(Key,Val,[H|T])         -> [H | add(Key,Val,T)];
add(Key,Val,[])            -> [{Key,Val}].

find(Key,[{Key,Val}|_]) -> {found, Val};
find(Key,[H|T])         -> find(Key,T);
find(Key,[])            -> error.

test() ->
   start(),
   add(xx, 1),
   add(yy, 2),
   io:format("~p~n", [lookup(xx)]),
   io:format("~p~n", [lookup(zz)]).
