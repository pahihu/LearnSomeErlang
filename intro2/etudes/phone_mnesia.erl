-module(phone_mnesia).
-export([setup/0, setup/2, summary/3]).
-import(phone_utils, [load_calls/1, load_custs/1, sum_minutes/1]).
-include("phone_records.hrl").
-include_lib("stdlib/include/qlc.hrl").

setup() ->
   setup("call_data.csv", "customer_data.csv").

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
