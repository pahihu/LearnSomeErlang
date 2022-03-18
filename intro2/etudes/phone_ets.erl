-module(phone_ets).
-export([setup/1]).
-export([summary/1, summary/0]).
-include("phone_records.hrl").

setup(FileName) ->
   create_table(ets:info(calls)),
   load_data(FileName),
   ets:info(calls).

create_table(undefined) ->
   Tab = ets:new(calls, [bag, named_table, {keypos, #call.number}]);
create_table(Any) ->
   ok.

load_data(FileName) ->
   {ok, File} = file:open(FileName, [read]),
   read_lines(file:read_line(File), File),
   ok = file:close(File).

split(Str, Sep) ->
   [A, B, C] = re:split(Str, Sep, [{return, list}]),
   {list_to_integer(A), list_to_integer(B), list_to_integer(C)}.

date_to_tuple(Date) ->
   split(Date, "-").

time_to_tuple(Time) ->
   split(Time, ":").

read_lines(eof, File) ->
   ok;
read_lines({ok, Line}, File) ->
   [Number, StartDate, StartTime, EndDate, EndTime] = re:split(Line, "[,\n]", [{return,list}, trim]),
   SD = date_to_tuple(StartDate),
   ST = time_to_tuple(StartTime),
   ED = date_to_tuple(EndDate),
   ET = time_to_tuple(EndTime),
   Call = #call{number = Number,
                start_date = SD,
                start_time = ST,
                end_date = ED,
                end_time = ET},
   % io:format("~p~n",[Call]),
   ets:insert(calls, Call),
   read_lines(file:read_line(File), File).


elapsed_minutes(StartDT, EndDT) ->
   T0=calendar:datetime_to_gregorian_seconds(StartDT),
   T1=calendar:datetime_to_gregorian_seconds(EndDT),
   (T1-T0 + 59) div 60.


summarize(Number ) ->
   Recs = ets:lookup(calls, Number),
   Minutes = lists:foldl(fun(#call{number=Number,
                                   start_date=SD, start_time=ST,
                                   end_date=ED, end_time=ET}
                              ,A) ->
                           A + elapsed_minutes({SD,ST}, {ED,ET})
                         end,
                         0,
                         Recs),
   {Number, Minutes}.


summary(Number) ->
   [summarize(Number)].


summary() ->
   traverse(ets:first(calls), []).

traverse('$end_of_table', Result) ->
   Result;
traverse(Number, Result) ->
   traverse(ets:next(calls, Number), [summarize(Number) | Result]).

