-module(drop4).
-export([fall_velocity/2, fall_velo/2]).

fall_velocity1(Planemo, Distance) when Distance >= 0 ->
   case Planemo of
      earth -> math:sqrt(2 * 9.8 * Distance);
      moon  -> math:sqrt(2 * 1.6 * Distance);
      mars  -> math:sqrt(2 * 3.71 * Distance)   % no closing period!
   end.

fall_velocity2(Planemo, Distance) ->
   Gravity = case Planemo of
      earth when Distance >= 0 -> 9.8;
      moon  when Distance >= 0 -> 1.6;
      mars  when Distance >= 0 -> 3.71  % no closing period!
   end,
   math:sqrt(2 * Gravity * Distance).

fall_velocity(Planemo, Distance) when Distance >= 0 ->
   Gravity = case Planemo of
      earth -> 9.8;
      moon  -> 1.6;
      mars  -> 3.71  % no closing period!
   end,
   math:sqrt(2 * Gravity * Distance).

wordy(Velocity) ->
   if
      Velocity == 0 -> 'stable';
      Velocity  < 5 -> 'slow';
      Velocity >= 5, Velocity < 10 -> 'moving';
      Velocity >= 10, Velocity < 20 -> 'fast';
      Velocity >= 20 -> 'speedy'
   end.

warn(Planemo, Distance, Velocity) ->
   Description = if
      Velocity == 0 -> 'stable';
      Velocity  < 5 -> 'slow';
      (Velocity >= 5) and (Velocity < 10) -> 'moving';
      (Velocity >= 10) and (Velocity < 20) -> 'fast';
      Velocity >= 20 -> 'speedy'
   end,
   if
      (Velocity > 40) -> io:format("Look out below! ~w is too high on ~w.~n", 
                                    [Distance, Planemo]);
      true -> true
   end,
   Description.

fall_velo(Planemo, Distance) ->
   Velocity = fall_velocity(Planemo, Distance),
   warn(Planemo, Distance, Velocity).
