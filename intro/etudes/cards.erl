-module(cards).
-export([make_deck/0, show_deck/1, shuffle/1]).

make_deck() ->
   [{Rank, Suit} || Rank<-["A",2,3,4,5,6,7,8,9,10,"J","D","K"],
                    Suit<-["Clubs", "Diamonds", "Hearts", "Spades"]].

show_deck(Deck) ->
   lists:foreach(fun(Item) -> io:format("~p~n", [Item]) end, Deck).

shuffle(List) ->
   rand:seed(default),
   shuffle(List, []).

shuffle([], Acc) ->
   Acc;
shuffle(List, Acc) ->
   {Leading, [H | T]} = lists:split(rand:uniform(length(List)) - 1, List),
   shuffle(Leading ++ T, [H | Acc]).
