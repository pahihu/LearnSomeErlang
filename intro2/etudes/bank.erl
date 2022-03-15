-module(bank).
-export([account/1]).

account(Balance) ->
   Tx = get_tx(),
   case Tx of
      deposit ->
         N = get_number("amount to deposit?"),
         NewBalance = if
            N < 0 ->
               err("Amount of deposit cannot be negative.~n"),
               Balance;
            N >= 10000 ->
               warning("Deposit of ~p might be subject to hold.~n",[N]),
               report(Balance + N);
            true ->
               info("Deposit of ~p.~n", [N]),
               report(Balance + N)
         end,
         account(NewBalance);
      withdraw ->
         N = get_number("amount to withdraw?"),
         NewBalance = if
            N < 0 ->
               err("Amount of withdraw cannot be negative.~n"),
               Balance;
            N > Balance ->
               err("Overdraw ~p from balance ~p.~n", [N,Balance]),
               Balance;
            true ->
               info("Withdraw of ~p.~n", [N]),
               report(Balance - N)
         end,
         account(NewBalance);
      balance ->
         info("Current balance is ~p.~n", [Balance]),
         account(Balance);
      quit ->
         ok
   end.

get_tx() ->
   Input = io:get_line("D)eposit W)withdraw B)alance Q)uit > "),
   Value = string:strip(Input, right, $\n),
   if
      length(Value) =:= 0 ->
         get_tx();
      true ->
         Cmd = hd(Value),
         Tx = char_to_tx(Cmd),
         if
            Tx == unknown ->
               io:format("Unknown command ~s.~n", [[Cmd]]),
               get_tx();
            true ->
               Tx
         end
   end.


char_to_tx($d) -> deposit;
char_to_tx($w) -> withdraw;
char_to_tx($b) -> balance;
char_to_tx($q) -> quit;

char_to_tx($D) -> deposit;
char_to_tx($W) -> withdraw;
char_to_tx($B) -> balance;
char_to_tx($Q) -> quit;

char_to_tx(X)  -> unknown.


report(Amount) ->
   io:format("Your new balance is ~p.~n", [Amount]),
   Amount.

info(Msg,Args) ->
   io:format(Msg, Args),
   error_logger:info_msg(Msg, Args).

warning(Msg,Args) ->
   io:format(Msg, Args),
   error_logger:warning_msg(Msg, Args).

err(Msg) ->
   err(Msg, []).

err(Msg,Args) ->
   io:format(Msg, Args),
   error_logger:error_msg(Msg, Args).


get_number(Prompt) ->
   io:format("Enter ~s > ", [Prompt]),
   Input = io:get_line(""),
   Value = string:strip(Input, right, $\n),
   case string:to_float(Value) of
      {error, no_float} ->
         {I, _} = string:to_integer(Value),
         I;
      {F, _} ->
         F
   end.

