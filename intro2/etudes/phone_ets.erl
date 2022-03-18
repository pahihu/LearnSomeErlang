-module(phone_ets).
-export([setup/0, setup/1]).
-export([summary/1, summary/0]).
-import(phone_utils, [sum_minutes/1, load_calls/1]).
-include("phone_records.hrl").

setup() ->
   setup("call_data.csv").

setup(FileName) ->
   create_table(ets:info(calls)),
   insert(calls, load_calls(FileName)),
   ets:info(calls).


insert(Tab, Recs) ->
   lists:foreach(fun(R) -> true = ets:insert(calls, R) end, Recs).


create_table(undefined) ->
   Tab = ets:new(calls, [bag, named_table, {keypos, #call.number}]);
create_table(Any) ->
   ok.


calls_for(Number) ->
   Recs = ets:lookup(calls, Number).


summary(Number) ->
   [Number, sum_minutes(calls_for(Number))].


summary() ->
   traverse(ets:first(calls), []).

traverse('$end_of_table', Result) ->
   Result;
traverse(Number, Result) ->
   traverse(ets:next(calls, Number),
            [{Number, sum_minutes(calls_for(Number))} | Result]).

