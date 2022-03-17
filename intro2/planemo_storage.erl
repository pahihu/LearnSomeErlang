-module(planemo_storage).
-export([setup/0, make_dets/0, make_mnesia/0]).
-export([query/0, query/1]).
-include("records.hrl").
-include_lib("stdlib/include/qlc.hrl").

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

make_dets() ->
   {ok, planets} = dets:open_file(planets, [{keypos, #planemo.name},
                                            {auto_save, 10000}]),
   ok = dets:insert(planets, ets:tab2list(planemos)),
   ok = dets:close(planets).

make_mnesia() ->
   mnesia:create_schema([node()]),
   mnesia:start(),
   mnesia:create_table(planemo, [{attributes, record_info(fields, planemo)}]),
   mnesia:transaction(fun() ->
                         lists:foreach(fun(T) -> ok = mnesia:write(T) end,
                                       ets:tab2list(planemos))
                      end).

query() ->
   query(9.8).

query(Gravity) ->
   mnesia:transaction(
      fun() ->
         qlc:e(
            qlc:q( [{X#planemo.name, X#planemo.gravity} || X <- mnesia:table(planemo),
                         X#planemo.gravity < Gravity] )
         )
      end
   ).
