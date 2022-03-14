-module(loan).
-export([payIt/4, init/0, calculate/3]).

sqrt(N, M) ->
   math:exp(math:log(N) / M).

monInterest(Interest) ->
   sqrt(1.0 + Interest / 100.0, 12).

months(Years) ->
   12 * Years.

pay({Mortgage, MonInterest, Payment, Months}, Print) ->
   FM = Mortgage + 0.0,
   FP = Payment + 0.0,
   Valid = check(FM, MonInterest, FP),
   if
      Valid == ok ->
         pay({FM, MonInterest, FP, Months}, 1, Print);
      true ->
         error
   end.

check(M, I, P) when M * I - M =< P ->
   ok;
check(M, I, _) ->
   io:format("Min. payment ~10.3. f~n", [M * I - M]),
   error.

pay({M, _, _, _}, N, _) when M =< 0.0 ->
   {ok, N-1};
pay({M, I, P, Months}, N, Print) when N =< Months ->
   if
      print == Print ->
         io:format("~10.3. f ~9.5. f ~w n=~w~n",[M,I,P,N]);
      true ->
         ok
   end,
   pay({M * I - P, I, P, Months}, N+1, Print);
pay(_, _, _) ->
   {error, months}.

payIt(M, I, P, Y) ->
   pay({M, monInterest(I), P, months(Y)}, print).

% Test for $10000, 6.15% interest rate, $200/month, max. 10 years.
init() ->
   payIt(10000, 6.15, 200, 10).


minmax(Mortgage, Interest, Months, Low, High) when Low + 0.0005 < High ->
   Mid = (Low + High) / 2.0,
   {Status, _} = pay({Mortgage, Interest, Mid, Months}, false),
   if
      Status == ok ->
         % io:format("~w ~w~n",[Low,Mid]),
         minmax(Mortgage, Interest, Months, Low, Mid);
      true ->
         % io:format("~w ~w~n",[Mid,High]),
         minmax(Mortgage, Interest, Months, Mid, High)
   end;
minmax(_, _, _, Low, _) ->
   % io:format("~w ~w~n",[Low,High]),
   Low.

% Calculate payment for mortgage, interest rate, years.
calculate(M, I, Y) ->
   FM = M + 0.0,
   FI = monInterest(I),
   minmax(FM, FI, months(Y), FM * FI - FM - 1.0, FM).
