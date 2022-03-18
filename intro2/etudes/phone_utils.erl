-module(phone_utils).
-export([load_calls/1, load_custs/1, sum_minutes/1]).
-include("phone_records.hrl").


load_calls(File) ->
   load_csv(File, fun to_call/1).


load_custs(File) ->
   load_csv(File, fun to_cust/1).


split(Str, Sep) ->
   [A, B, C] = re:split(Str, Sep, [{return, list}]),
   {list_to_integer(A), list_to_integer(B), list_to_integer(C)}.

date_to_tuple(Date) ->
   split(Date, "-").

time_to_tuple(Time) ->
   split(Time, ":").


to_call([Number, StartDate, StartTime, EndDate, EndTime]) ->
   SD = date_to_tuple(StartDate),
   ST = time_to_tuple(StartTime),
   ED = date_to_tuple(EndDate),
   ET = time_to_tuple(EndTime),
   #call{number = Number, start_date = SD, start_time = ST,
                          end_date = ED, end_time = ET}.


to_cust([Number, Last, First, Middle, Rate]) ->
   #cust{number = Number,
         last = Last, first = First, middle = Middle,
         rate = list_to_float(Rate)}.


load_csv(FileName, ToRec) ->
   {ok, File} = file:open(FileName, [read]),
   Recs = read_lines(file:read_line(File), File, ToRec, []),
   ok = file:close(File),
   Recs.

read_lines(eof, File, ToRec, Result) ->
   Result;
read_lines({ok, Line}, File, ToRec, Result) ->
   Fields = re:split(Line, "[,\n]", [{return,list}, trim]),
   Rec = ToRec(Fields),
   % io:format("~p~n", [Rec]),
   read_lines(file:read_line(File), File, ToRec, [Rec | Result]).


elapsed_minutes(StartDT, EndDT) ->
   T0=calendar:datetime_to_gregorian_seconds(StartDT),
   T1=calendar:datetime_to_gregorian_seconds(EndDT),
   (T1-T0 + 59) div 60.


sum_minutes(Recs) ->
   lists:foldl(fun(#call{number=Number,
                         start_date=SD, start_time=ST,
                         end_date=ED, end_time=ET}
                   ,A) ->
                  A + elapsed_minutes({SD,ST}, {ED,ET})
               end,
               0,
               Recs).
