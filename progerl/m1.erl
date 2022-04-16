-module(m1).
-export([loop/1]).
-export([some_func/1]).

-ifdef(debug_flag).
-define(DEBUG(X), io:format("DEBUG ~p:~p ~p~n",[?MODULE,?LINE,X])).
-else.
-define(DEBUG(X), void).
-endif.

loop(0) ->
   done;
loop(N) ->
   ?DEBUG(N),
   loop(N-1).

some_func(X) ->
   {P, _Q} = some_other_func(X),
   io:format("_Q = ~p~n",[_Q]),
   P.

some_other_func(X) ->
   {X, erlang:timestamp()}.
