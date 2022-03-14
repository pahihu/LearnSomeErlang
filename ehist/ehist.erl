%% @author Joe Armstrong
%% @doc Example functions from the paper Erlang History
%% @copyright 2007 Joe Armstrong
%% @version 0.1

-module(ehist).
-export([fac/1, lookup/2, sort/1, adder/1, areacln/0, 
         catchit/1, trapit/1, g/1]).

%% @doc Calculates N!

-spec(fac(number()) -> number()).
fac(N) when N > 0 -> N * fac(N-1);
fac(0)            -> 1.


%% @doc Lookup key in binary trees. S subtree less than, and B subtree
%% contains greater than keys. Looks up key in the binary tree, returns 
%% value or not_found.

-spec(lookup(any(), tuple()) -> any()).
lookup(Key, {Key, Val, _, _}) ->
   {ok, Val};
lookup(Key, {Key1, Val, S, G}) when Key < Key1 ->
   lookup(Key, S);
lookup(Key, {Key1, Val, S, G}) ->
   lookup(Key, G);
lookup(Key, nil) ->
   not_found.


%% @doc Append lists. Append second list to first.

-spec(append(list(), list()) -> list()).
append([H|T], L) -> [H|append(T, L)];
append([],    L) -> L.


%% @doc Sort list. Sort the list, whose elements define the less than
%% operator.

-spec(sort(list()) -> list()).
sort([Pivot|T]) ->
   sort([X || X <- T, X < Pivot]) ++
   [Pivot] ++
   sort([X || X <- T, X >= Pivot]);
sort([]) ->
   [].


%% @doc Generates an adder function. Returns a function which adds
%% N to its argument.

-spec(adder(number()) -> fun()).
adder(N) -> (fun (X) -> X + N end).


%% @doc Area server. Accumulates the areas of squares and rectangles.

-spec(areasrv(number()) -> none()).
areasrv(Tot) ->
   receive
      {Pid, {square, X}} ->
         Pid ! X*X,
         areasrv(Tot + X*X);
      {Pid, {rectangle,[X,Y]}} ->
         Pid ! X*Y,
         areasrv(Tot + X*Y);
      {Pid, areas} ->
         Pid ! Tot,
         areasrv(Tot)
   end.


%% @doc Area client. Spawns an area server, submits data and
%% gets the accumulated area.

-spec(areacln() -> none()).
areacln() ->
   Pid = spawn(fun() -> ehist:areasrv(0) end),
   Pid ! {self(), {square, 10}},
   receive
      Area ->
         io:format("Area = ~p~n", [Area]);
      Mesg ->
         io:format("Unknown message ~p~n", [Mesg])
   end.


%% @doc Catches the exception which is thrown in f().

-spec(catchit(number()) -> none()).
catchit(X) ->
   case (catch f(X)) of
      {exception1, Why} ->
         io:format("Got exception ~p~n", [Why]);
      X ->
         io:format("Return ~p~n", [X])
   end.

f(X) when X > 0 ->
   X;
f(X) ->
   throw({exception1, non_positive}).


%% @doc Tests trapping exits. Call with any value.

-spec(trapit(any()) -> atom()).
trapit(X) ->
   process_flag(trap_exit, true),
   P = spawn_link(ehist, g, [X]),
   receive
      {'EXIT', P, Why} ->
         io:format("G exited with ~p~n", [Why]);
      X ->
         ok
   end.


%% @doc Exit function.

-spec(g(any()) -> none()).
g(X) ->
   exit(normal).


%% hot code replacement

iloop(Data, F) ->
   receive
      {request, Pid, Q} ->
         {Reply, Data1} = F(Q, Data),
         Pid ! Reply,
         iloop(Data1, F);
      {change_code, F1} ->
         iloop(Data, F1)
   end.

change_code(Server, F) ->
   Server ! {change_code, F}.
