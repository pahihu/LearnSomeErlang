-module(dropsrv).
-export([drop/0, drop/1]).
-include("records.hrl").

drop() ->
   drop(ets).

drop(Store) ->
   Lookup = setup(Store),
   handle_drops(Lookup).

setup(ets) ->
   planemo_storage:setup(),
   (fun(Key) -> ets:lookup(planemos, Key) end);

setup(dets) ->
   {ok, T} = dets:open_file("planets"),
   (fun(Key) -> dets:lookup(T, Key) end).

handle_drops(Lookup) ->
   receive
   {From, Planemo, Distance} ->
      From ! {Planemo, Distance, fall_velocity(Planemo, Distance, Lookup)},
      handle_drops(Lookup)
   end.

fall_velocity(Planemo, Distance, Lookup) when Distance >= 0 ->
   Gravity = case Lookup(Planemo) of
      [] ->
         9.8;
      [P] ->
         P#planemo.gravity
   end,
   math:sqrt(2 * Gravity * Distance).
