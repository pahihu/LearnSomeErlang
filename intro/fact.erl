-module(fact).
-export([factorial/1, ifactorial/1, afactorial/1, dfactorial/1, bench/2]).


%% non tail-recursive
factorial(N) when N > 1 ->
   N * factorial(N-1);

factorial(N) when N =< 1 ->
   1.


%% instrumented
ifactorial(N) when N > 1->
   io:format("Calling from ~w.~n", [N]),
   Result = N * ifactorial(N-1),
   io:format("~w yields ~w.~n", [N,Result]),
   Result;

ifactorial(N) when N =< 1 ->
   io:format("Calling from 1.~n"),
   io:format("1 yields 1.~n"),
   1.


%% instrumented, using countup + accumulator => tail-recursive
afactorial(N) ->
   afactorial(1, N, 1).

afactorial(Current, N, Result) when Current =< N ->
   NewResult = Result*Current,
   io:format("~w yields ~w!~n", [Current, NewResult]),
   afactorial(Current+1, N, NewResult);

afactorial(Current, N, Result) ->
   io:format("Finished.~n"),
   Result.


%% using countdown + accumulator => tail-recursive
dfactorial(N) -> dfactorial(N,1).

dfactorial(N, Result) when N > 1 ->
   dfactorial(N-1, N*Result);
dfactorial(N, Result) when N =< 1 ->
   Result.


%% NB. contrary to popular belief, 
%%    factorial/1 is faster than dfactorial/1
bench(F,N) ->
   loop(F,N).

loop(F,N) when N > 0 ->
   F(200),
   loop(F,N-1);

loop(F,N) ->
   ok.
