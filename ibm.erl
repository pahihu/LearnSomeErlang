%%% IBM Developerworks
-module(ibm).
-export([fibo/1, printfibo/1, mathexp/1, hello/1, hi/0]).

%% print fibo arg. and result, with function as parameter

printfibo(N) ->
   {Micros,Res} = timer:tc(ibm,fibo,[N]),
   Millis = (Micros + 500) div 1000,
   io:format("Fib(~w) = ~w (~wms)~n", [N, Res, Millis]).

fibo(0) -> 0;
fibo(1) -> 1;
fibo(N) when N > 1 -> fibo(N-1) + fibo(N-2).

mathexp({sum, N, M}) -> N+M;
mathexp({sub, N, M}) -> N-M;
mathexp({square, N}) -> N*N.

hello(N) -> "hello " ++ N.             % lists:append()

hi() -> ("hello" -- "ello") ++ "i".    % lists:subtract().

%% NB. the actual spawning process itself will NEVER fail, even if
%% the function that you are calling does not exist. This reduces
%% the requirement to test the spawning process in your code.

%% Errors in processes beyond those in the Erlang shell are handled
%% and recorded by the error logger, a built-in process that handles
%% the error reporting.

dbrequest(L) ->
   receive
      {store, Key, Value} ->
         NewL = lists:keystore(Key, 1, L, {Key, Value});
      {get, Key, Pid} ->
         Pid ! lists:keyfind(Key, 1, L),
         NewL = L;
      {update, Key, Value} ->
         NewL = lists:keyreplace(Key, 1, L, {Key, Value});
      {delete, Key} ->
         NewL = lists:keydelete(Key, 1, L)
   end,
   dbrequest(NewL).

