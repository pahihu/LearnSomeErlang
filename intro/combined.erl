-module(combined).
-export([height_to_mph/1]).   % there will be more soon!
-import(drop, [fall_velocity/1]).
-import(convert, [mps_to_mph/1]).


%% @doc Convenience function to convert a fall velocity from meters
%% to miles per hour.

-spec(height_to_mph(number()) -> number()).

height_to_mph(Meters) ->
   mps_to_mph(fall_velocity(Meters)).
