-module(doctor2).
-export([loop/0]).

restart() ->
   Doc = spawn_link(fun doctor:loop/0),
   Doc ! new,
   io:format("The doctor is ~p.~n", [Doc]).

loop() ->
   process_flag(trap_exit, true),
   receive
      new ->
         io:format("Creating and monitoring process.~n"),
         restart(),
         loop();
      {'EXIT', From, Reason} ->
         case Reason of
            doctor2 ->
               io:format("doctorov: dieing with reason ~p.",[Reason]),
               exit(doctor2);
            doctor ->
               io:format("doctorov: doctor ~p died with reason ~p.", [From, Reason]),
               io:format("doctorov: restarting doctor.~n"),
               restart(),
               loop();
            _ ->
               io:format("The process ~p died with reason ~p.", [From, Reason])
         end
   end.
