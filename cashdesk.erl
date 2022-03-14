%%% Elixir example.
-module(cashdesk).
-export([start/0, buy/2, done/0, loop/1]).

prices() ->
   [{flour, 100}, {egg, 45}, {toilet_paper, 500}].

start() ->
   Pid = spawn(fun() -> init() end),
   register(cashdesk, Pid).

init() ->
   loop({0, prices()}).

buy(Product, Amount) ->
   cashdesk ! {buy, self(), Product, Amount},
   receive
      Response ->
         Response
   after 5000 ->
      cashdesk_closed
   end.

done() ->
   cashdesk ! {done, self()},
   receive
      Total ->
         Total
   end.

loop(State = {Total, Prices}) ->
   receive
      {buy, Customer, Product, Amount} ->
         case lists:keyfind(Product, 1, Prices) of
            false ->
               Customer ! not_available,
               loop(State);
            {Product, Price} ->
               Pay = Amount * Price,
               Customer ! ok,
               loop({Total + Pay, Prices})
         end;
      {done, Customer} ->
         Customer ! Total,
         loop({0, Prices})
   end.

