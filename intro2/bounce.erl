-module(bounce).
-export([report/0, fragile/0]).

report() ->
   report(1).

report(State) ->
   NewState = receive
      X ->
         io:format("Received #~p: ~p~n",[State,X]),
         State+1
   end,
   report(NewState).

fragile() ->
   receive
      X ->
         io:format("Divided to ~p~n",[X/2]),
         fragile()
   end.
