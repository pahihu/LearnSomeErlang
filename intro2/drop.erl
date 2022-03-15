-module(drop).
-export([drop/0, fall_velocity/2]).

drop() ->
   receive
   {From, Planemo, Distance} ->
      From ! {Planemo, Distance, fall_velocity(Planemo, Distance)},
      drop()
   end.

fall_velocity(Planemo, Distance) ->
   try
      Gravity = case Planemo of
         earth -> 9.8;
         moon  -> 1.6;
         mars  -> 3.71;
            X  -> throw({unknown, X})
      end,
      math:sqrt(2 * Gravity * Distance)
   of
      Result -> Result
   catch
      error:Error     -> { found, Error};
      throw:Exception -> {caught, Exception}
   after
      error_logger:info_msg("fall_velocity done~n")
   end.
