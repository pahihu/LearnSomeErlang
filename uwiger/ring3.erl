-module(ring3).
-export([send/2]).

%% @doc Send M messages through a ring of N processes.
send(M, N) ->
  statistics(runtime),
  H = lists:foldl(
    fun(Id, Pid) -> spawn_link(fun() -> loop(Pid, M) end) end,
    self(),
    lists:seq(N, 2, -1)),
  {_, Time} = statistics(runtime),
  io:format("~p processes spawned in ~p ms~n", [N, Time]),
  statistics(runtime),
  H ! M,
  loop(H, M).

loop(Pid, M) ->
  receive
    1 ->
      {_, Time} = statistics(runtime),
      io:format("~p messages sent in ~p ms~n", [M, Time]),
      exit(self(), ok);
    Index ->
      Pid ! Index - 1,
      loop(Pid, M)
  end.
