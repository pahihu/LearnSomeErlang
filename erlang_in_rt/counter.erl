-module(counter).
-export([timer/1, timer/2]).

loop(F, 0) ->
   ok;
loop(F, N) ->
   F(),
   loop(F, N-1).

timer(F,N) when N > 1 ->
   T1 = erlang:timestamp(),
   loop(F,N),
   T2 = erlang:timestamp(),
   timer:now_diff(T2, T1) / N.

timer(F) ->
   T1 = erlang:timestamp(),
   F(),
   T2 = erlang:timestamp(),
   timer:now_diff(T2, T1).

   
