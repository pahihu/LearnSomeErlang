-module(concat).
-export([str/0, str/1, ccbin/1, cclst/1, bench/2]).

str() ->
   str(1000000).

str(N) ->
   str(0,N,[]).

str(N,N,Bs) ->
   list_to_binary(Bs);
str(I,N,Bs) ->
   str(I+1,N,[Bs,integer_to_list(I)]).

%% 342ms
ccbin(N) ->
   list_to_binary(lists:map(fun erlang:integer_to_list/1,lists:seq(1,N))).

%% 366ms
cclst(N) ->
   lists:flatmap(fun erlang:integer_to_list/1,lists:seq(1,N)).

elapsed_bin(F,N,X) ->
   {T, _} = timer:tc(F, [N]),
   T.

bench(F,N) ->
   L = [ elapsed_bin(F,N,X) || X <- lists:seq(1,10)],
   lists:sum(L) / length(L).
