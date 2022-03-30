-module(datetime).
%% --- timedelta ---
-export([timedelta/1,
         timedelta/2,
         timedelta/3,
         timedelta/4,
         timedelta/5,
         timedelta/6,
         timedelta/7]).
-export([timedelta_min/0, timedelta_max/0, timedelta_resolution/0]).
-export([days/1, seconds/1, microseconds/1]).
-export([add/2, sub/2, scale/2, divide/2, negate/1, magnitude/1]).
-export([str/1]).
-export([total_seconds/1]).


%% --- date ---
-export([date/3, today/0, fromtimestamp/1, fromordinal/1]).
-export([date_min/0, date_max/0, date_resolution/0]).
-export([year/1, month/1, day/1]).
-export([replace/2, timetuple/1, toordinal/1]).
-export([weekday/1, yday/1, isoweekday/1, isocalendar/1]).
-export([isoformat/1]).
-export([ctime/1]).

-export([test_timedelta/0]).
-export([test_date/0]).
-export([test/0]).



%% --- timedelta ---
idiv(A, B) ->
   floor(A / B).

mod(A, B) ->
   A - idiv(A, B) * B.

params() ->
   [{        days, 1,    1},
    {     seconds, 2,    1},
    {microseconds, 3,    1},
    {milliseconds, 3, 1000},
    {     minutes, 2,   60},
    {       hours, 2, 3600},
    {       weeks, 1,   7}].

normalize({Days, Seconds, Microseconds}) ->
   U = trunc(Microseconds),
   S = trunc(Seconds + idiv(U, 1000000)),
   D = trunc(Days + idiv(S, 86400)),
   {timedelta, D, 
               mod(S, 86400), 
               mod(U, 1000000)}.

timedelta(Days) when is_number(Days) ->
   normalize({Days, 0, 0});

timedelta(H) when is_map(H) ->
   L = lists:map(fun({What, Pos, Mult}) ->
               Val = maps:get(What, H, 0) * Mult,
               case Pos of
                  1 -> {Val,   0,   0};
                  2 -> {  0, Val,   0};
                  3 -> {  0, Val, Val}
               end
             end, params()),
   T = lists:foldl(fun({D, S, U}, {AccD, AccS, AccU}) ->
                  {D + AccD, S + AccS, U + AccU}
               end,
               {0, 0, 0}, L),
   normalize(T).


timedelta(Days, Seconds) ->
   normalize({Days, Seconds, 0}).

timedelta(Days, Seconds, Microseconds) ->
   normalize({Days, Seconds, Microseconds}).

timedelta(Days, Seconds, Microseconds, Milliseconds) ->
   timedelta(Days, Seconds, Microseconds + 1000 * Milliseconds).

timedelta(Days, Seconds, Microseconds, Milliseconds, Minutes) ->
   timedelta(Days, Seconds + 60 * Minutes, Microseconds, Milliseconds).

timedelta(Days, Seconds, Microseconds, Milliseconds, Minutes, Hours) ->
   timedelta(Days, Seconds + 3600 * Hours, Microseconds, Milliseconds, Minutes).

timedelta(Days, Seconds, Microseconds, Milliseconds, Minutes, Hours, Weeks) ->
   timedelta(Days + 7 * Weeks, Seconds, Microseconds, Milliseconds, Minutes, Hours).

timedelta_min() ->
   timedelta(-999999999).

timedelta_max() ->
   timedelta(#{days => 999999999,
               hours => 23,
               minutes => 59,
               seconds => 59,
               microseconds => 999999}).

timedelta_resolution() ->
   timedelta(0, 0, 1).


days({timedelta, Days, _Seconds, _Micros}) ->
   Days.

seconds({timedelta, _Days, Seconds, _Micros}) ->
   Seconds.

microseconds({timedelta, _Days, _Seconds, Micros}) ->
   Micros.


add({timedelta, D1, S1, U1}, {timedelta, D2, S2, U2}) ->
   normalize({D1 + D2, S1 + S2, U1 + U2});
add(Date, {timedelta, D, _S, _U}) ->
   fromordinal(toordinal(Date) + D).


sub({timedelta, D1, S1, U1}, {timedelta, D2, S2, U2}) ->
   normalize({D1 - D2, S1 - S2, U1 - U2});
sub(Date, {timedelta, D, _S, _U}) ->
   fromordinal(toordinal(Date) - D);
sub(Date1, Date2) ->
   timedelta(toordinal(Date1) - toordinal(Date2)).

scale({timedelta, D, S, U}, N) ->
   normalize({N * D, N * S, N * U}).

divide({timedelta, D, S, U}, N) ->
   normalize({D / N, S / N, U / N}).

negate({timedelta, D, S, U}) ->
   normalize({-D, -S, -U}).

magnitude({timedelta, D, S, U}) ->
   normalize({abs(D), abs(S), abs(U)}).


two_digit(N) ->
   if
      N < 10 ->
         [$0, $0 + N];
      true ->
         [$0 + (N div 10), $0 + (N rem 10)]
   end.

str({timedelta, D, S, U}) ->
   SS = S rem 60,  M = S div 60,
   MM = M rem 60, HH = M div 60,
   Ret1 = integer_to_list(D),
   Ret2 = if
      abs(D) > 1 ->
         Ret1 ++ " days, ";
      true ->
         Ret1 ++ " day, "
   end,
   Ret3 = Ret2 ++ integer_to_list(HH)
               ++ ":" ++ two_digit(MM)
               ++ ":" ++ two_digit(SS),
   if
      U > 0 ->
         Ret3 ++ "." ++ integer_to_list(U);
      true ->
         Ret3
   end.


total_seconds({timedelta, D, S, U}) ->
   D * 86400 + S + U / 1.0e6.



%% --- date ---
date(Y, M, D) ->
   {date, {Y, M, D}}.

today() ->
   {date, erlang:date()}.

fromtimestamp({Megasecs, Secs, _Micros}) ->
   % 719528 = toordinal(date(1970,1,1))
   fromordinal(719528 + (Megasecs * 1000000 + Secs) div 86400).

fromordinal(Days) ->
   {date, ymd(Days)}.
   

date_min() ->
   date(0, 1, 1).

date_max() ->
   date(9999, 12, 31).

date_resolution() ->
   timedelta(1).


ymd(Days) ->
   calendar:gregorian_days_to_date(Days).

year({date, {Y, _M, _D}}) -> Y.

month({date, {_Y, M, _D}}) -> M.

day({date, {_Y, _M, D}}) -> D.


replace({date, {Y, M, D}}, H) when is_map(H) ->
   NewY = maps:get(year, H, Y),
   NewM = maps:get(month, H, M),
   NewD = maps:get(day, H, D),
   date(NewY, NewM, NewD).

timetuple(Date = {date, {Y, M, D}}) ->
   {Y, M, D, 0, 0, 0, weekday(Date), yday(Date), -1}.

toordinal({date, YMD}) ->
   calendar:date_to_gregorian_days(YMD).

weekday(Date) ->
   isoweekday(Date) - 1.

yday(Date = {date, {Y, _M, _D}}) ->
   toordinal(Date) - toordinal(date(Y, 1, 1)) + 1.

isoweekday({date, YMD}) ->
   calendar:day_of_the_week(YMD).

isocalendar(Date = {date, YMD}) ->
   {IsoY, IsoWeek} = calendar:iso_week_number(YMD),
   {IsoY, IsoWeek, isoweekday(Date)}.

isoformat({date, {Y, M, D}}) ->
   integer_to_list(Y) ++ "-" ++ two_digit(M) ++ "-" ++ two_digit(D).

days() ->
   ["Mon", "Tue", "Wed", "Thu", "Fri", "Sat", "Sun"].

months() ->
   ["Jan", "Feb", "Mar", "Apr",
    "May", "Jun", "Jul", "Aug",
    "Sep", "Oct", "Nov", "Dec"].

ctime(Date = {date, {Y, M, D}}) ->
   lists:nth(isoweekday(Date), days())
      ++ " " ++ lists:nth(M, months())
      ++ " " ++ integer_to_list(D)
      ++ " 00:00:00 "
      ++ integer_to_list(Y).


%% --- tests ---

test_timedelta() ->
   Minus5Hours = timedelta(#{hours => -5}),
   {timedelta, -1, 68400, 0} = Minus5Hours,
   true = -18000 == total_seconds(Minus5Hours),
   Year = timedelta(365),
   AnotherYear = timedelta(#{weeks => 40, days => 84, hours => 23, minutes => 50, seconds => 600}),
   true = 31536000.0 == total_seconds(Year),
   true = Year == AnotherYear,
   TenYears = scale(Year, 10),
   3650 = days(TenYears),
   NineYears = sub(TenYears, Year),
   3285 = days(NineYears),
   ThreeYears = divide(NineYears, 3),
   1095 = days(ThreeYears),
   true = magnitude(sub(ThreeYears, TenYears)) == add(scale(ThreeYears, 2), Year).

test_date() ->
   1 = yday(date(2002,1,1)),
   2 = yday(date(2002,1,2)),
   Date1 = date(2002, 1, 1),
   Date1 = fromordinal(toordinal(Date1)),
   2 = weekday(date(2002, 12, 4)),
   3 = isoweekday(date(2002, 12, 4)),
   {2004, 1, 1} = isocalendar(date(2003, 12, 29)),
   {2004, 1, 7} = isocalendar(date(2004, 1, 4)),
   "2002-12-04" = isoformat(date(2002, 12, 4)),
   "Wed Dec 4 00:00:00 2002" = ctime(date(2002, 12, 4)),
   Today = today(),
   Today = fromtimestamp(now()),
   OldToday = date(2007, 12, 5),
   MyBirthday = date(year(OldToday), 6, 24),
   MyBirthday2 = if
      MyBirthday < OldToday ->
         replace(MyBirthday, #{year => year(OldToday) + 1});
      true ->
         MyBirthday
   end,
   TimeToBirthday = magnitude(sub(MyBirthday2, OldToday)),
   202 = days(TimeToBirthday).


test() ->
   test_timedelta(),
   test_date().
   
