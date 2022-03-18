-module(phone_mnesia).
-export([setup/2]).
-export([summary/3]).
-include("phone_records.hrl").
-include_lib("stdlib/include/qlc.hrl").

setup(CallFile,CustFile) ->
   mnesia:start(),
   mnesia:create_table(calls, [{attributes, record_info(fields, call)},
                               {record_name, call},
                               {type, bag}]),
   mnesia:create_table(custs, [{attributes, record_info(fields, cust)},
                               {record_name, cust},
                               {type, set}]),
   insert(calls, load_calls(CallFile)),
   insert(custs, load_custs(CustFile)).

insert(Tab, Recs) ->
   mnesia:transaction(
      fun() ->
         lists:foreach(fun(R) -> ok = mnesia:write(Tab, R, write) end, Recs)
      end
   ).

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


summary(Last, First, Middle) ->
   CustHandle = qlc:q(
      [ C ||
        C <- mnesia:table(custs),
        C#cust.last == Last,
        C#cust.first == First,
        C#cust.middle == Middle ]
   ),

   {atomic, [Cust | _]} = mnesia:transaction(fun() -> qlc:e( CustHandle ) end),
   {atomic, Calls} = mnesia:transaction(
      fun() ->
         qlc:e(
            qlc:q( [ Call ||
               Customer <- CustHandle,
               Call <- mnesia:table(calls),
               Call#call.number == Customer#cust.number]
            )
         )
      end
   ),
   Minutes = sum_minutes(Calls),
   [{Cust#cust.number, Minutes, Minutes * Cust#cust.rate}].
