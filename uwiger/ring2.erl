-module(ring2).
-export([send/2]).

%% @doc Send M messages through a ring of N processes.
send(M, N) ->
  statistics(runtime),
%  H = lists:foldl(
%    fun(Id, Pid) -> spawn_link(fun() -> loop(Id, Pid, M) end) end,
%    self(),
%    lists:seq(N, 2, -1)),
  H = pspawn(4,N,M),
  {_, Time} = statistics(runtime),
  io:format("~p processes spawned in ~p ms~n", [N, Time]),
  statistics(runtime),
  H ! M,
  loop(H, M).

loop(Pid, M) ->
  receive
    {prev, PrevPid} ->
      loop(PrevPid, M);
    1 ->
      {_, Time} = statistics(runtime),
      io:format("~p messages sent in ~p ms~n", [M, Time]),
      exit(self(), ok);
    Index ->
      Pid ! Index - 1,
      loop(Pid, M)
  end.

pspawn(NumWorkers,N,M) ->
   Me = self(),
   Workers = lists:map(fun(X) ->
                     spawn_link(fun() -> worker(Me) end)
                  end,
                  lists:seq(1,NumWorkers)),
   NW = N div length(Workers),
   lists:map(fun(Worker) ->
               Worker ! {do, fun make_procs/1, {NW, M}}
             end, Workers),
   Procs = reduce(length(Workers), []),
   % io:format("Got procs~n", []),
   lists:foldl(fun(Pid,PrevPid) -> Pid ! {prev, PrevPid}, Pid end,
               self(),
               Procs).

reduce(0, L) ->
   L;
reduce(N, L) ->
   receive
      X ->
         reduce(N-1, L ++ X)
   end.

make_procs({N, M}) ->
   % io:format("make_procs ~p,~p~n", [N, M]),
   lists:map(fun(X) ->
               spawn_link(fun() -> loop(0, M) end)
             end,
             lists:seq(1,N)).

worker(Srv) ->
   receive
      {do, F, Args} ->
         Result = F(Args),
         % io:format("Got result~n",[]),
         Srv ! Result,
         % io:format("Sent result~n",[]),
         worker(Srv)
   end.
