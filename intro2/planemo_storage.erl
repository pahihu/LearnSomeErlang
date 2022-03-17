-module(planemo_storage).
-export([setup/0]).
-include("records.hrl").

setup() ->
   setup(ets:info(planemos)).

setup(undefined) ->
   PlanemoTable = ets:new(planemos, [named_table, {keypos, #planemo.name}]),
   read_data(),
   ets:info(PlanemoTable);
setup(Any) ->
   ok.

read_data() ->
   {ok, File} = file:open("planemos.txt",read),
   {ok, Header} = file:read_line(File),
   read_lines(file:read_line(File), File),
   ok = file:close(File).

read_lines(eof, File) ->
   ok;
read_lines({ok, Line},File) ->
   [Name, Gravity, Diameter, DistanceFromSun] = re:split(Line,"\\s+",[{return,list},trim]),
   Planemo = #planemo{name = list_to_atom(Name),
                      gravity = list_to_float(Gravity),
                      diameter = list_to_integer(Diameter),
                      distance_from_sun = list_to_float(DistanceFromSun)},
   % io:format("~p~n",[Planemo]),
   ets:insert(planemos, Planemo),
   read_lines(file:read_line(File), File).
