%% @author Simon St.Laurent <simonstl@simonstl.com> [http://simonstl.com]
%% @doc Functions calculating velocities achieved by objects
%% dropped in a vacuum.
%% @reference from <a href= "http://shop.oreilly.com/product/0636920025818.do" >
%% Introducting Erlang</a>,
%% O'Reilly Media, Inc., 2017.
%% @copyright 2017 by Simon St.Laurent
%% @version 0.1

-module(drop3).
-export([fall_velocity/2, fall_velocity/1, bool/2, shortbool/2]).


%% @doc Calculates the velocity of an object falling on Earth
%% as if it were in a vacuum (no air resistance). The distance is
%% the height from which the objects falls, specified in meters,
%% and the function returns a velocity in meters per second.

-spec(fall_velocity(atom(), number()) -> number()).

fall_velocity(earth, Distance) when Distance >= 0 ->
   math:sqrt(2 * 9.8 * Distance);

fall_velocity(moon, Distance) when Distance >= 0 ->
   math:sqrt(2 * 1.6 * Distance);

fall_velocity(mars, Distance) when Distance >= 0 ->
   math:sqrt(2 * 3.71 * Distance);

% NB. _Var gets bound, but compiler does NOT warns if unused
fall_velocity(_Planemo, Distance) -> 
   io:format("Planemo is ~p~n", [_Planemo]),
   math:sqrt(2 * 9.8 * Distance).


fall_velocity(_Where = {Planemo, Distance}) ->
   fall_velocity(Planemo, Distance);

% NB. can be called with at least size(2) tuples!
fall_velocity(Where) when is_tuple(Where), size(Where) > 1 ->
   fall_velocity(element(1, Where), element(2, Where)).



%% boolean expressions, shortcut boolean expressions
bool(A, B) ->
   arg(A) and arg(B).

arg(A) ->
   io:format("arg(~p)~n",[A]),
   A.

shortbool(A, B) ->
   arg(A) andalso arg(B).

