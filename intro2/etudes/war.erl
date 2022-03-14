-module(war).
-export([start/0, player/2]).
-define(TIMEOUT, 5000).


start() ->
   Deck = cards:shuffle(cards:make_deck()),
   {Cards1, Cards2} = lists:split(length(Deck) div 2, Deck),
   Player1 = spawn_link(war, player, ["Player1", Cards1]),
   Player2 = spawn_link(war, player, ["Player2", Cards2]),
   play({[Player1, Player2], false, {0, 0}}, []).


play(State, []) ->
   battle(State, []);
play(State = {Players, InWar, Stats}, Pile) ->
   lists:foreach(fun(P) -> P ! {getcards, self()} end, Players),
   Cards = get_cards(Stats),
   io:format("  adding cards to pile ~p~n",[Cards]),
   battle(State, Pile ++ Cards).
   

battle({Players, InWar, Stats = {NumBattles, NumWars}}, Pile) ->
   lists:foreach(fun(P) -> P ! {getcard, self()} end, Players),
   [{Player1, [Card1]}, {Player2, [Card2]}] = check_empty(Stats, receive_cards()),
   Hand = [Card1, Card2],
   CurrentPile = Pile ++ Hand,
   io:format("Battle ~p, on hand ~p~n", [NumBattles, Hand]),
   Rank1 = rank(Card1), Rank2 = rank(Card2),
   if
      Rank1 =:= Rank2 ->
         Winner = draw;
      Rank1 > Rank2 ->
         Winner = Player1;
      Rank1 < Rank2 ->
         Winner = Player2
   end,
   case Winner of
      draw ->
         if
            InWar ->
               NewInWar = true,
               NewNumWars = NumWars,
               io:format("  Draw, continuing war ~p...~n",[NumWars]);
            true ->
               NewInWar = true,
               NewNumWars = NumWars+1,
               io:format("  Draw, entering war ~p!~n",[NewNumWars])
         end,
         NewPile = CurrentPile;
      X ->
         io:format("  ~s won the battle...~n",[get_name(X)]),
         io:format("  adding ~p.~n", [CurrentPile]),
         add_cards(X, CurrentPile),
         NewInWar = false,
         NewNumWars = NumWars,
         NewPile = []
   end,
   play({Players, NewInWar, {NumBattles+1,NewNumWars}}, NewPile).


rank({Rank, Suit}) ->
   case Rank of
      "A" ->
         14;
      "K" ->
         13;
      "Q" ->
         12;
      "J" ->
         11;
      X when is_number(X) ->
         X
   end.


take(N, L) when length(L) < N ->
   {L, []};
take(N, L) ->
   lists:split(N, L).


get_name(Pid) ->
   Pid ! {name, self()},
   receive
      X ->
         X
   after ?TIMEOUT ->
      exit({timeout, get_name})
   end.

add_cards(Pid, Cards) ->
   Pid ! {addcards, self(), Cards},
   receive
      ack ->
         ok
   after ?TIMEOUT ->
      exit({timeout, add_cards})
   end.


player(Name, Cards) ->
   receive
      {name, Pid} ->
         Pid ! Name,
         NewCards = Cards;
      {getcard, Pid} ->
         {Ans, NewCards} = take(1, Cards),
         Pid ! {self(), Ans};
      {getcards, Pid} ->
         {Ans, NewCards} = take(2, Cards),
         Pid ! {self(), Ans};
      {addcards, Pid, Pile} ->
         NewCards = Cards ++ Pile,
         Pid ! ack
   end,
   player(Name, NewCards).


get_cards(Stats) ->
   [{Player1, Cards1}, {Player2, Cards2}] = check_empty(Stats, receive_cards()),
   Cards1 ++ Cards2.


check_empty({NumBattles, NumWars},
            Hand = [{Player1, Cards1}, {Player2, Cards2}]) ->
   if
      length(Cards1) =:= 0 ->
         Winner = Player2;
      length(Cards2) =:=0 ->
         Winner = Player1;
      true ->
         Winner = none
   end,
   case Winner of
      none ->
         Hand;
      X ->
         io:format("~nVictory! ~s won in ~p battles and ~p wars!~n",[get_name(X), NumBattles-1, NumWars]),
         exit(normal)
   end.


receive_cards() ->
   receive_cards([]).

receive_cards(Cards) when length(Cards) < 2 ->
   receive
      X ->
         receive_cards([X | Cards])
   after ?TIMEOUT ->
      exit({timeout, receive_cards})
   end;
receive_cards(Cards) ->
   Cards.
