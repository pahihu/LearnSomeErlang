-module(datetime).
-define(MAX_YEAR, 9999).
-record(timedelta, {days=0, seconds=0, microseconds=0}).
-record(date, {year, month, day}).
-record(time, {hour, minute, second, microsecond, tzinfo}).
-record(datetime, {date, time}).

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
-export([totimedelta/1]).
-export([timedelta_to_date/1, timedelta_to_time/1, timedelta_to_datetime/1]).
-export([less_than/2]).


%% --- date ---
-export([date/1, date/3, today/0, fromtimestamp/1, fromordinal/1]).
-export([date_min/0, date_max/0, date_resolution/0]).
-export([year/1, month/1, day/1]).
-export([replace/2, timetuple/1, toordinal/1]).
-export([weekday/1, yday/1, isoweekday/1, isocalendar/1]).
-export([isoformat/1]).
-export([ctime/1]).


%% --- time ---
-export([time/0, time/1, time/2, time/3, time/4, time/5]).
-export([hour/1, minute/1, second/1, microsecond/1, tzinfo/1]).
-export([utcoffset/1,dst/1]).


%% --- datetime ---
-export([datetime/0, datetime/1, datetime/2, datetime/3]).
-export([datetime/4, datetime/5, datetime/6, datetime/7, datetime/8]).
-export([utcnow/0, utcfromtimestamp/1]).
-export([combine/2]).
-export([datetime_min/0, datetime_max/0, datetime_resolution/0]).
-export([timetz/1, astimezone/2]).
-export([utctimetuple/1]).

-export([test_timedelta/0]).
-export([test_date/0]).
-export([test/0]).



%% --- timedelta ---
idiv(A, B) ->
   floor(A / B).

mod(A, B) ->
   A - idiv(A, B) * B.

td_params() ->
   [{        days, 1,    1},
    {     seconds, 2,    1},
    {microseconds, 3,    1},
    {milliseconds, 3, 1000},
    {     minutes, 2,   60},
    {       hours, 2, 3600},
    {       weeks, 1,   7}].

td_normalize({Days, Seconds, Microseconds}) ->
   U = trunc(Microseconds),
   S = trunc(Seconds + idiv(U, 1000000)),
   D = trunc(Days + idiv(S, 86400)),
   #timedelta{days = D,
              seconds = mod(S, 86400),
              microseconds = mod(U, 1000000)}.

timedelta(Days) when is_number(Days) ->
   td_normalize({Days, 0, 0});

timedelta(H) when is_map(H) ->
   L = lists:map(fun({What, Pos, Mult}) ->
               Val = maps:get(What, H, 0) * Mult,
               case Pos of
                  1 -> {Val,   0,   0};
                  2 -> {  0, Val,   0};
                  3 -> {  0,   0, Val}
               end
             end, td_params()),
   T = lists:foldl(fun({D, S, U}, {AccD, AccS, AccU}) ->
                  {D + AccD, S + AccS, U + AccU}
               end,
               {0, 0, 0}, L),
   td_normalize(T).


timedelta(Days, Seconds) ->
   td_normalize({Days, Seconds, 0}).

timedelta(Days, Seconds, Microseconds) ->
   td_normalize({Days, Seconds, Microseconds}).

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


days(TD) when is_record(TD, timedelta) ->
   TD#timedelta.days.

seconds(TD) when is_record(TD, timedelta) ->
   TD#timedelta.seconds.

microseconds(TD) when is_record(TD, timedelta) ->
   TD#timedelta.microseconds.


add(TD1, TD2) when is_record(TD1, timedelta), is_record(TD2, timedelta) ->
   td_normalize({TD1#timedelta.days + TD2#timedelta.days,
                 TD1#timedelta.seconds + TD2#timedelta.seconds,
                 TD1#timedelta.microseconds + TD2#timedelta.microseconds});
add(Date, TD) when is_record(Date,date),
                   is_record(TD, timedelta) ->
   fromordinal(toordinal(Date) + days(TD));
add(DateTime, TimeDelta) when is_record(DateTime, datetime),
                              is_record(TimeDelta, timedelta) ->
   timedelta_to_datetime(add(totimedelta(DateTime), TimeDelta)).


sub(TD1, TD2) when is_record(TD1, timedelta), is_record(TD2, timedelta) ->
   td_normalize({TD1#timedelta.days - TD2#timedelta.days,
                 TD1#timedelta.seconds - TD2#timedelta.seconds,
                 TD1#timedelta.microseconds - TD2#timedelta.microseconds});
sub(DateTime1, DateTime2) when is_record(DateTime1, datetime),
                               is_record(DateTime2, datetime) ->
   sub(totimedelta(DateTime1), totimedelta(DateTime2));
sub(DateTime, TimeDelta) when is_record(DateTime, datetime),
                              is_record(TimeDelta, timedelta) ->
   timedelta_to_datetime(sub(totimedelta(DateTime), TimeDelta));
sub(Date, TD) when is_record(TD, timedelta) ->
   fromordinal(toordinal(Date) - days(TD));
sub(Date1, Date2) ->
   timedelta(toordinal(Date1) - toordinal(Date2)).

scale(TD, N) when is_record(TD, timedelta) ->
   td_normalize({N * TD#timedelta.days, N * TD#timedelta.seconds, N * TD#timedelta.microseconds}).

divide(TD, N) when is_record(TD, timedelta) ->
   td_normalize({TD#timedelta.days / N,
                 TD#timedelta.seconds / N,
                 TD#timedelta.microseconds / N}).

negate(TD) when is_record(TD, timedelta) ->
   td_normalize({-TD#timedelta.days,
                 -TD#timedelta.seconds,
                 -TD#timedelta.microseconds}).

magnitude(TD) when is_record(TD, timedelta) ->
   td_normalize({abs(TD#timedelta.days),
                 abs(TD#timedelta.seconds),
                 abs(TD#timedelta.microseconds)}).


two_digit(N) ->
   if
      N < 10 ->
         [$0, $0 + N];
      true ->
         [$0 + (N div 10), $0 + (N rem 10)]
   end.

hhmm(HH, MM) ->
   two_digit(HH) ++ ":" ++ two_digit(MM).

hhmmss(HH, MM, SS) ->
   hhmm(HH, MM) ++ ":" ++ two_digit(SS).

str(TD) when is_record(TD, timedelta) ->
   Ret1 = integer_to_list(TD#timedelta.days),
   Ret2 = if
      abs(TD#timedelta.days) > 1 ->
         Ret1 ++ " days, ";
      true ->
         Ret1 ++ " day, "
   end,
   SS = TD#timedelta.seconds rem 60,  M = TD#timedelta.seconds div 60,
   MM = M rem 60, HH = M div 60,
   Ret3 = Ret2 ++ hhmmss(HH, MM, SS),
   if
      TD#timedelta.microseconds > 0 ->
         Ret3 ++ "." ++ integer_to_list(TD#timedelta.microseconds);
      true ->
         Ret3
   end.


total_seconds(TD) when is_record(TD, timedelta) ->
   TD#timedelta.days * 86400
   + TD#timedelta.seconds
   + TD#timedelta.microseconds / 1.0e6.



%% --- date ---
date(Y, M, D) ->
   #date{year = Y, month = M, day = D}.

date({Y, M, D}) ->
   date(Y, M, D);
date(Opts) when is_map(Opts) ->
   Y = maps:get(year,Opts,1),
   M = maps:get(month,Opts,1),
   D = maps:get(day,Opts,1),
   date(Y,M,D);
date(DateTime) when is_record(DateTime, datetime) ->
   DateTime#datetime.date.

today() ->
   date(erlang:date()).

fromtimestamp({Megasecs, Secs, _Micros}) ->
   % 719163 = toordinal(date(1970,1,1))
   fromordinal(719163 + (Megasecs * 1000000 + Secs) div 86400).

fromordinal(Days) ->
   date(ymd(365 + Days)).
   

date_min() ->
   date(0, 1, 1).

date_max() ->
   date(?MAX_YEAR, 12, 31).

date_resolution() ->
   timedelta(1).


ymd(Days) ->
   calendar:gregorian_days_to_date(Days).

year(Date) when is_record(Date, date) -> Date#date.year;
year(DateTime) when is_record(DateTime, datetime) ->
   year(DateTime#datetime.date).

month(Date) when is_record(Date, date) -> Date#date.month;
month(DateTime) when is_record(DateTime, datetime) ->
   month(DateTime#datetime.date).

day(Date) when is_record(Date, date) -> Date#date.day;
day(DateTime) when is_record(DateTime, datetime) ->
   day(DateTime#datetime.date).


replace(Date, H) when is_record(Date, date), is_map(H) ->
   NewY = maps:get(year,  H, Date#date.year),
   NewM = maps:get(month, H, Date#date.month),
   NewD = maps:get(day,   H, Date#date.day),
   date(NewY, NewM, NewD);
replace(Time, H) when is_record(Time, time), is_map(H) ->
   NewH = maps:get(hour,   H, Time#time.hour),
   NewM = maps:get(minute, H, Time#time.minute),
   NewS = maps:get(second, H, Time#time.second),
   NewU = maps:get(microsecond, H, Time#time.microsecond),
   NewTZ = maps:get(tzinfo, H, Time#time.tzinfo),
   time(NewH,NewM,NewS,NewU,NewTZ);
replace(DateTime, H) when is_record(DateTime, datetime), is_map(H) ->
   combine(replace(DateTime#datetime.date, H),
           replace(DateTime#datetime.time, H)).

timetuple(Date) when is_record(Date, date) ->
   {Date#date.year, Date#date.month, Date#date.day,
    0, 0, 0,
    weekday(Date), yday(Date), -1};
timetuple(DateTime) when is_record(DateTime, datetime) ->
   T  = DateTime#datetime.time,
   T1 = timetuple(DateTime#datetime.date),
   T2 = setelement(4, T1, hour(T)),
   T3 = setelement(5, T2, minute(T)),
   T4 = setelement(6, T3, second(T)),
   Dst = case tzinfo(T) of
      undefined ->
         -1;
      _X ->
         DstSec = seconds(dst(T)),
         if
            DstSec /= 0 -> 1;
            true        -> 0
         end
   end,
   setelement(9, T4, Dst).

date_to_tuple(Date) when is_record(Date, date) ->
   {Date#date.year, Date#date.month, Date#date.day}.

toordinal(Date) when is_record(Date, date) ->
   calendar:date_to_gregorian_days(date_to_tuple(Date)) - 365;
toordinal(DateTime) when is_record(DateTime, datetime) ->
   toordinal(DateTime#datetime.date).


totimedelta(TD) when is_record(TD, timedelta) ->
   TD;
totimedelta(Date) when is_record(Date, date) ->
   timedelta(toordinal(Date));
totimedelta(Time) when is_record(Time, time) ->
   timedelta(0,Time#time.hour*3600 + Time#time.minute*60 + Time#time.second, Time#time.microsecond);
totimedelta(DateTime) when is_record(DateTime, datetime) ->
   add(totimedelta(DateTime#datetime.date),
       totimedelta(DateTime#datetime.time)).

timedelta_to_date(TD) when is_record(TD, timedelta) ->
   fromordinal(TD#timedelta.days).

timedelta_to_time(TD) when is_record(TD, timedelta) ->
   SS = TD#timedelta.seconds rem 60, M = TD#timedelta.seconds div 60,
   time(M div 60, M rem 60, SS, TD#timedelta.microseconds).

timedelta_to_datetime(TD) when is_record(TD, timedelta) ->
   combine(timedelta_to_date(TD),
           timedelta_to_time(TD)).

less_than(A, B) ->
   totimedelta(A) < totimedelta(B).


weekday(Date) ->
   isoweekday(Date) - 1.

yday(Date) when is_record(Date, date) ->
   toordinal(Date) - toordinal(date(Date#date.year, 1, 1)) + 1.

isoweekday(Date) when is_record(Date, date) ->
   calendar:day_of_the_week(date_to_tuple(Date));
isoweekday(DateTime) when is_record(DateTime, datetime) ->
   isoweekday(DateTime#datetime.date).

isocalendar(Date) when is_record(Date, date) ->
   {IsoY, IsoWeek} = calendar:iso_week_number(date_to_tuple(Date)),
   {IsoY, IsoWeek, isoweekday(Date)};
isocalendar(DateTime) when is_record(DateTime, datetime) ->
   isocalendar(DateTime#datetime.date).

isoformat(Date) when is_record(Date, date) ->
   integer_to_list(Date#date.year)
   ++ "-" ++ two_digit(Date#date.month)
   ++ "-" ++ two_digit(Date#date.day);
isoformat(T) when is_record(T, time) ->
   Ret1 = hhmmss(T#time.hour, T#time.minute, T#time.second),
   Ret2 = if
      0 /= T#time.microsecond ->
         Ret1 ++ "." ++ T#time.microsecond;
      true ->
         Ret1
   end,
   if
      T#time.tzinfo /= undefined ->
         TZMinutes = seconds(utcoffset(T)) div 60,
         TZ_MM = TZMinutes rem 60, TZ_HH = TZMinutes div 60,
         Ret3 = if
            TZMinutes < 0 ->
               Ret2 ++ "-";
            true ->
               Ret2 ++ "+"
         end,
         Ret3 ++ hhmm(abs(TZ_HH), TZ_MM);
      true ->
         Ret2
   end;
isoformat(DateTime) when is_record(DateTime, datetime) ->
   T = DateTime#datetime.time,
   isoformat(DateTime#datetime.date) ++ "T" ++ isoformat(T).
      

days() ->
   ["Mon", "Tue", "Wed", "Thu", "Fri", "Sat", "Sun"].

months() ->
   ["Jan", "Feb", "Mar", "Apr",
    "May", "Jun", "Jul", "Aug",
    "Sep", "Oct", "Nov", "Dec"].

ctime(Date) when is_record(Date, date) ->
   ctime(Date, {0, 0, 0});
ctime(DateTime) when is_record(DateTime, datetime) ->
   T = DateTime#datetime.time,
   ctime(DateTime#datetime.date, {hour(T), minute(T), second(T)}).
ctime(Date, {HH, MM, SS}) when is_record(Date, date) ->
   lists:nth(isoweekday(Date), days())
      ++ " " ++ lists:nth(Date#date.month, months())
      ++ " " ++ integer_to_list(Date#date.day)
      ++ " " ++ hhmmss(HH, MM, SS)
      ++ " " ++ integer_to_list(Date#date.year).


%% --- time ---

t_normalize({Hours,Minutes,Seconds,Microseconds,TZInfo}) ->
   U = trunc(Microseconds),
   S = trunc(Seconds + idiv(U, 1000000)),
   M = trunc(Minutes + idiv(S, 60)),
   H = trunc(Hours + idiv(M, 60)),
   #time{hour = mod(H, 24)
         minute = mod(M, 60),
         second = mod(S, 60),
         microsecond = mod(U, 1000000),
         tzinfo = TZInfo}.

time() ->
   {_YMD1,{ H,  M,   S}} = calendar:local_time(),
   {_YMD2,{HU, MU, _MS}} = calendar:universal_time(),
   TZOffset = (H - HU) * 60 + M - MU,
   t_normalize({H, M, S, 0, TZOffset}).
time(Hour) when is_number(Hour) ->
   time(Hour,0);
time(H) when is_map(H) ->
   HH = maps:get(hour,H,0),
   M = maps:get(minute,H,0),
   S = maps:get(second,H,0),
   U = maps:get(microsecond,H,0),
   TZ = maps:get(tzinfo,H,undefined),
   t_normalize({HH,M,S,U,TZ});
time({HH, MM, SS}) when is_number(HH), is_number(MM), is_number(SS) ->
   time(HH,MM,SS);
time(DateTime) when is_record(DateTime, datetime) ->
   replace(DateTime#datetime.time, #{tzinfo => undefined}).
time(Hour,Minute) ->
   time(Hour,Minute,0).
time(Hour,Minute,Second) ->
   time(Hour,Minute,Second,0).
time(Hour,Minute,Second,Microsecond) ->
   time(Hour,Minute,Second,Microsecond,undefined).
time(Hour,Minute,Second,Microsecond,TZInfo) ->
   t_normalize({Hour,Minute,Second,Microsecond,TZInfo}).

   

hour(Time) when is_record(Time, time) -> Time#time.hour;
hour(DateTime) when is_record(DateTime, datetime) ->
   hour(DateTime#datetime.time).

minute(Time) when is_record(Time, time) -> Time#time.minute;
minute(DateTime) when is_record(DateTime, datetime) ->
   minute(DateTime#datetime.time).

second(Time) when is_record(Time, time) -> Time#time.second;
second(DateTime) when is_record(DateTime, datetime) ->
   second(DateTime#datetime.time).

microsecond(Time) when is_record(Time, time) -> Time#time.microsecond;
microsecond(DateTime) when is_record(DateTime, datetime) ->
   microsecond(DateTime#datetime.time).

time_to_tuple(Time) when is_record(Time, time) ->
   {Time#time.hour, Time#time.minute, Time#time.second}.

tzinfo(Time) when is_record(Time, time) -> Time#time.tzinfo;
tzinfo(DateTime) when is_record(DateTime, datetime) ->
   tzinfo(DateTime#datetime.time).

utcoffset(T) ->
   case tzinfo(T) of
      undefined ->
         undefined;
      X -> % expect in minutes
         timedelta(0, 60*X)
   end.

dst(Time) when is_record(Time, time) -> timedelta(0, 3600);
dst(DateTime) when is_record(DateTime, datetime) ->
   timedelta(0, 3600).


%% --- datetime ---

datetime() ->
   combine(today(), datetime:time()).
datetime(Year) when is_number(Year) ->
   datetime(Year, 1);
datetime(H) when is_map(H) ->
   combine(date(H), time(H)).
datetime(Year,Month) ->
   datetime(Year,Month,1).
datetime(Year,Month,Day) ->
   datetime(Year,Month,Day,0).
datetime(Year,Month,Day,Hour) ->
   datetime(Year,Month,Day,Hour,0).
datetime(Year,Month,Day,Hour,Minute) ->
   datetime(Year,Month,Day,Hour,Minute,0).
datetime(Year,Month,Day,Hour,Minute,Second) ->
   datetime(Year,Month,Day,Hour,Minute,Second,0).
datetime(Year,Month,Day,Hour,Minute,Second,Microsecond) ->
   datetime(Year,Month,Day,Hour,Minute,Second,Microsecond,undefined).
datetime(Year,Month,Day,Hour,Minute,Second,Microsecond,TZInfo) ->
   combine(date(Year,Month,Day),
           time(Hour,Minute,Second,Microsecond,TZInfo)).

from_ymd_hms({{Y,M,D},{HH,MM,SS}}) ->
   combine(date(Y,M,D), time(HH,MM,SS)).

utcnow() ->
   from_ymd_hms(calendar:universal_time()).

utcfromtimestamp(TimeStamp) ->
  from_ymd_hms(calendar:now_to_local_time(TimeStamp)).

combine(Date, Time) when is_record(Date, date), is_record(Time, time) ->
   #datetime{date = Date, time = Time}.

datetime_min() ->
   datetime(#{year => 1, month => 1, day => 1, tzinfo => undefined}).

datetime_max() ->
   datetime(?MAX_YEAR,12,31,23,59,59,999999,undefined).

datetime_resolution() ->
   timedelta(#{microseconds => 1}).

timetz(DateTime) when is_record(DateTime, datetime) ->
   DateTime#datetime.time.

astimezone(DateTime, TZInfo) ->
   Delta = timedelta(0, 60 * (tzinfo(DateTime) - TZInfo)),
   replace(sub(DateTime, Delta), #{tzinfo => TZInfo}).

utctimetuple(DateTime) when is_record(DateTime, datetime) ->
   [{UD, UT} | _ ] = calendar:local_time_to_universal_time_dst({date_to_tuple(DateTime#datetime.date), time_to_tuple(DateTime#datetime.time)}),
   Tuple = timetuple(combine(date(UD), time(UT))),
   setelement(9, Tuple, 0).


%% --- tests ---

test_timedelta() ->
   Minus5Hours = timedelta(#{hours => -5}),
   #timedelta{days=-1, seconds=68400, microseconds=0} = Minus5Hours,
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
   true = magnitude(sub(ThreeYears, TenYears)) == add(scale(ThreeYears, 2), Year),
   ok.

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
   Today = fromtimestamp(erlang:timestamp()),
   OldToday = date(2007, 12, 5),
   MyBirthday = date(year(OldToday), 6, 24),
   MyBirthday2 = if
      MyBirthday < OldToday ->
         replace(MyBirthday, #{year => year(OldToday) + 1});
      true ->
         MyBirthday
   end,
   TimeToBirthday = magnitude(sub(MyBirthday2, OldToday)),
   202 = days(TimeToBirthday),
   Date2 = fromordinal(730920),
   Date2 = date(2002,3,11),
   {2002, 3, 11, 0, 0, 0, 0, 70, -1} = timetuple(Date2),
   {2002, 11, 1} = isocalendar(Date2),
   "2002-03-11" = isoformat(Date2),
   ok.

test_time() ->
   T = time(#{hour => 12, minute => 10, second => 30, tzinfo => 60}),
   "12:10:30+01:00" = isoformat(T),
   12 = hour(T),
   10 = minute(T),
   30 = second(T),
   60 = tzinfo(T),
   TD = timedelta(0, 3600),
   TD = dst(T),
   ok.

test_datetime() ->
   Date = date(2005, 7, 14),
   Time = time(12, 30),
   DateTime1 = combine(Date, Time),
   "2005-07-14T12:30:00" = isoformat(DateTime1),
   DateTime2 = datetime(2006, 11, 21, 16, 30),
   {2006, 11, 21, 16, 30, 0, 1, 325, -1} = timetuple(DateTime2),
   {2006, 47, 2} = isocalendar(DateTime2),
   DateTime3 = datetime(#{year => 2005, month => 11, day => 21,
                          hour => 16, minute => 30,
                          tzinfo => 60}),
   GMT1 = timedelta(0,3600),
   GMT1 = utcoffset(DateTime3),
   DateTime4 = add(DateTime2, timedelta(0, 3600)),
   {2006, 11, 21, 17, 30, 0, 1, 325, -1} = timetuple(DateTime4),
   DateTime5 = add(DateTime2, timedelta(1)),
   {2006, 11, 22, 16, 30, 0, 2, 326, -1} = timetuple(DateTime5),
   DateTime6 = replace(DateTime5, #{tzinfo => 120}),
   {2006, 11, 22, 16, 30, 0, 2, 326, 1} = timetuple(DateTime6),
   DateTime7 = astimezone(DateTime6, 60),
   {2006, 11, 22, 15, 30, 0, 2, 326, 1} = timetuple(DateTime7),
   GMT1 = sub(DateTime6, DateTime7),
   DateTime8 = datetime(2006, 11, 22, 16, 30, 0, 0, undefined),
   DateTime8 = add(DateTime7, GMT1),
   ok.


test() ->
   test_timedelta(),
   test_date(),
   test_time(),
   test_datetime().
   
