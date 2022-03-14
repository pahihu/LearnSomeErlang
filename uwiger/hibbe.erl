%%% Hibernate trick. Ulf Wiger 15nov2005

-module(hibbe).
-export([spawn/0, cont/1, test/0]).

spawn() ->
   Me = self(),
   spawn_opt(fun() ->
               init(Me)
             end,[{priority,high},{fullsweep_after,17}]).

init(Parent) ->
   put(dict_item, dict_value),
   Parent ! {self(), process_info(self())},
   erlang:hibernate(?MODULE, cont, [Parent]).

cont(Parent) ->
   Parent ! {self(), process_info(self())}.

test() ->
   P = spawn(),
   io:format("~p~n", [process_info(P)]),
   P ! hi,
   receive
      X ->
         io:format("~p~n",[X])
   end.
