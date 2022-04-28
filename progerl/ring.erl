-module(ring).
-export([start/2, action/1]).

-ifdef(debug_flag).
-define(DEBUG(X), io:format("DEBUG ~p:~p ~p~n",[?MODULE,?LINE,X])).
-else.
-define(DEBUG(X), void).
-endif.

start(N, M) ->
   statistics(runtime),
   statistics(wall_clock),
   Last = lists:foldl(fun(_X, Pid) ->
                  spawn(ring, action, [Pid])
               end,
               self(),
               lists:seq(1, N-1)),
   self() ! N * M + 1,
   action(Last),
   {_, Time1} = statistics(runtime),
   {_, Time2} = statistics(wall_clock),
   io:format("Elapsed time ~p (~p) ms~n", [Time1,Time2]).
   

action(Pid) ->
   ?DEBUG({self(), started}),
   process(Pid).

process(Pid) ->
   receive
      Msg ->
         ?DEBUG({self(), got, Msg}),
         NewMsg = Msg - 1,
         if
            NewMsg > 0 ->
               ?DEBUG({self(), send, NewMsg, to, Pid}),
               Pid ! NewMsg,
               process(Pid);
            true ->
               Pid ! NewMsg,
               ok
         end
   end,
   ?DEBUG({self(), done}).
