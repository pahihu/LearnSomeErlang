-module(processes).
-export([imax/1, max/1]).

%% max(N)

%%  Create N processes then destroy them
%%  See how much time this takes

imax(N) ->
   statistics(runtime),
   statistics(wall_clock),
   L = for(1, N, fun() -> spawn(fun() -> wait() end) end),
   {_, Time1} = statistics(runtime),
   {_, Time2} = statistics(wall_clock),
   lists:foreach(fun(Pid) -> Pid ! die end, L),
   U1 = Time1 * 1000 / N,
   U2 = Time2 * 1000 / N,
   [U1, U2].

max(N) ->
   Max = erlang:system_info(process_limit),
   io:format("Maximum allowed processes:~p~n",[Max]),
   io:format("Process spawn time=~p (~p) microseconds~n",max(N)).

wait() ->
   receive
      die -> void
   end.

for(N, N, F) -> [F()];
for(I, N, F) -> [F() | for(I+1, N, F)]. 
