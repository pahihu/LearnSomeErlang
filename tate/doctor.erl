-module(doctor).
-export([loop/0]).

%% roulette runs when doctor exited, linked trap_exit-ed processes do NOT exit
%% exit() runs asynchronously, so we should wait before register()-ing a new
%% process

wait(undefined) ->
   ok;
wait(Any) ->
   wait(whereis(revolver)).

loop() ->
   process_flag(trap_exit, true),
   receive
      new ->
         io:format("Creating and monitoring process.~n"),
         _ = (catch exit(whereis(revolver),kill)),
         wait(whereis(revolver)),
         register(revolver, spawn_link(fun roulette:loop/0)),
         loop();
      {'EXIT', From, Reason} ->
         case Reason of
            doctor2 ->
               io:format("doctor: doctorov ~p died with reason ~p.", [From,Reason]),
               io:format("doctor: restarting doctorov.~n"),
               Doc2 = spawn(fun doctor2:loop/0),
               Doc2 ! new,
               io:format("doctor: exiting.~n");
            doctor ->
               io:format("doctor: myself ~p died with reason ~p.", [From, Reason]),
               io:format("doctor: exiting.~n"),
               exit(doctor);
            _ ->
               io:format("doctor: shooter ~p died with reason ~p.", [From, Reason]),
               io:format("doctor: restarting revolver.~n"),
               self() ! new,
               loop()
            end
   end.
