-module(spawntime).
-export([start/0, start/1, measure/1, histogram/1]).

start() ->
   MaxProc = erlang:system_info(process_limit),
   Low = trunc(math:log2(MaxProc)) - 10,
   start(trunc(math:pow(2,Low))).

start(N) ->
   Data = measure(N),
   histogram(Data).

measure(N) ->
   measure(N,10,[]).

measure(_N, 0, L) ->
   L;
measure(N, I, L) ->
   io:format("Starting ~w processes...~n",[N]),
   [T1, _] = processes:imax(N),
   measure(2*N, I-1, [{N,T1} | L]).

histogram(Data) ->
   {_, Vals} = lists:unzip(Data),
   Min = lists:min(Vals),
   Max = lists:max(Vals),
   Step = (Max - Min) / 50,
   L = [{N, trunc((Val - Min)/Step), Val} || {N, Val} <- Data],
   draw(L).

draw([]) ->
   ok;
draw([{N, Bar, Val} | T]) ->
   io:format("~10.. w ~*..#s ~f~n",[N, Bar, "#", Val]),
   draw(T).

