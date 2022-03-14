-module(linkr).
-export([start/0, child/0]).

start() ->
   process_flag(trap_exit, true),
   Child = spawn_link(linkr, child, []),
   io:format("Child is ~w~n",[Child]),
   receive
      {'EXIT', ExPID, Reason} ->
         io:format("~w exited because of ~w~n",[ExPID, Reason]),
         Reason;
      X -> X
   end.

child() ->
   receive
      X -> X
   after
      10000 -> timeout
   end,
   exit(kill).
