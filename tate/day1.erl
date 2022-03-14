-module(day1).
-export([numwords/1, ten/0, select/1]).

-spec numwords(string()) -> non_neg_integer().
numwords([]) ->
   0;
numwords([$ |T]) ->
   numwords(T, out, 0);
numwords([H|T]) ->
   numwords(T, in, 1).

-spec numwords(string(), 'in' | 'out', non_neg_integer()) -> non_neg_integer().
numwords([], _, N) ->
   N;
numwords([$ |T], out, N) ->
   numwords(T, out, N);
numwords([H|T], out, N) ->
   numwords(T, in, N+1);
numwords([$ |T], in, N) ->
   numwords(T, out, N);
numwords([H|T], in, N) ->
   numwords(T, in, N).


ten() ->
   ten(10, 1).

ten(0, _) ->
   ok;
ten(N, M) ->
   io:format("~p ", [M]),
   ten(N-1, M+1).

select(Arg = success) ->
   io:format("~p~n", [Arg]);
select({error, Mesg}) ->
   io:format("error: ~p~n", [Mesg]).

