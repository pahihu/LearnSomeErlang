-module(dropsrv).
-export([drop/0]).
-include("records.hrl").

drop() ->
   setup(),
   handle_drops().

setup() ->
   planemo_storage:setup().

handle_drops() ->
   receive
   {From, Planemo, Distance} ->
      From ! {Planemo, Distance, fall_velocity(Planemo, Distance)},
      handle_drops()
   end.

fall_velocity(Planemo, Distance) when Distance >= 0 ->
   Gravity = case ets:lookup(planemos, Planemo) of
      [] ->
         9.8;
      [P] ->
         P#planemo.gravity
   end,
   math:sqrt(2 * Gravity * Distance).
