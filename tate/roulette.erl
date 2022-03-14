-module(roulette).
-export([loop/0]).

% send a number, 1-6
loop() ->
   loop(rand:uniform(6)).

loop(N) ->
   receive
      N ->
         io:format("bang.~n"),
         exit({roulette,die,at,erlang:time()});
      _ ->
         io:format("click~n"),
         loop(N)
   end.
