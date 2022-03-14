-module(convert).
-export([mps_to_mph/1, mps_to_kph/1]).


%% @doc Converts meters per second to miles per hour.

-spec(mps_to_mph(number()) -> number()).
mps_to_mph(Mps) ->
   2.23693629 * Mps.


%% @doc Converts meters per second to kilometers per hour.

-spec(mps_to_kph(number()) -> number()).
mps_to_kph(Mps) ->
   3.6 * Mps.
