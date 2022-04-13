-module(lib_misc).
-export([for/3, qsort/1, pythag/1, mypythag/1, perms/1]).
-export([odds_and_evens1/1, odds_and_evens2/1]).
-export([my_tuple_to_list/1]).
-export([my_time_func/1]).
-export([my_date_string/0]).
-export([sqrt/1]).
-export([read/1]).


read(File) ->
   case file:read_file(File) of
      {ok, Bin} ->
         Bin;
      {error, Why} ->
         throw({read, Why})
   end.


sqrt(X) when X < 0 ->
   error({squareRootNegativeArgument, X});
sqrt(X) ->
   math:sqrt(X).


for(Max, Max, F) ->
   [F(Max)];
for(I, Max, F) ->
   [F(I) | for(I+1, Max, F)].


qsort([]) ->
   [];
qsort([Pivot | T]) ->
   qsort([X || X <- T, X < Pivot])
   ++ [Pivot] ++
   qsort([X || X <- T, X >= Pivot]).


pythag(N) ->
   [{A,B,C} ||
      A <- lists:seq(1,N),
      B <- lists:seq(1,N),
      C <- lists:seq(1,N),
      A+B+C =< N,
      A*A+B*B =:= C*C
   ].

mypythag(N) ->
   [{A,B,C} ||
      A <- lists:seq(1,N),
      B <- lists:seq(1,max(0,N-A)),
      C <- lists:seq(1,max(0,N-A-B)),
      A+B+C =< N,
      A*A+B*B =:= C*C
   ].


perms([]) ->
   [[]];
perms(L) ->
   [[H|T] || H <- L, T <- perms(L -- [H])].


odds_and_evens1(L) ->
   Odds  = [X || X <- L, (X rem 2) =:= 1],
   Evens = [X || X <- L, (X rem 2) =:= 0],
   {Odds, Evens}.

odds_and_evens2(L) ->
   odds_and_evens_acc(L, [], []).

odds_and_evens_acc([H|T], Odds, Evens) ->
   case (H rem 2) of
      1 -> odds_and_evens_acc(T, [H | Odds], Evens);
      0 -> odds_and_evens_acc(T, Odds, [H | Evens])
   end;
odds_and_evens_acc([], Odds, Evens) ->
   {Odds, Evens}.


my_tuple_to_list(T) when is_tuple(T) ->
   my_tuple_to_list(T, tuple_size(T), []).

my_tuple_to_list(_T, 0, L) ->
   L;
my_tuple_to_list(T, N, L) ->
   my_tuple_to_list(T, N-1, [element(N, T) | L]).


elapsed_time({M1, S1, U1}, {M0, S0, U0}) ->
   (((M1-M0) * 1000000) + (S1 - S0)) * 1000000 + (U1 - U0).

my_time_func(F) ->
   T0 = erlang:timestamp(),
   F(),
   T1 = erlang:timestamp(),
   elapsed_time(T1,T0).


my_date_string() ->
   { Y,  M,  D} = date(),
   {HH, MM, SS} = time(),
   io:format("Today is ~w-~w-~w ~w:~w:~w~n", [Y, M, D, HH, MM, SS]).
