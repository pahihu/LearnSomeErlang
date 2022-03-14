%% @author Simon St.Laurent <simonstl@simonstl.com> [http://simonstl.com]
%% @doc Functions calculating velocities achieved by objects
%% dropped in a vacuum.
%% @reference from <a href= "http://shop.oreilly.com/product/0636920025818.do" >
%% Introducting Erlang</a>,
%% O'Reilly Media, Inc., 2017.
%% @copyright 2017 by Simon St.Laurent
%% @version 0.1

-module(drop).
-export([fall_velocity/1]).


%% @doc Calculates the velocity of an object falling on Earth
%% as if it were in a vacuum (no air resistance). The distance is
%% the height from which the objects falls, specified in meters,
%% and the function returns a velocity in meters per second.

-spec(fall_velocity(number()) -> number()).

fall_velocity(Distance) ->
   math:sqrt(2 * 9.8 * Distance).

