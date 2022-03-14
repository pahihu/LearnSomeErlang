-module(dates).
-export([date_parts/1, is_leap/1, julian/1]).

date_parts(Date) ->
   [YearStr, MonthStr, DayStr] = re:split(Date, "-", [{return,list},trim]),
   {Year,  _} = string:to_integer(YearStr),
   {Month, _} = string:to_integer(MonthStr),
   {Day,   _} = string:to_integer(DayStr),
   [Year, Month, Day].

days() ->
   [31, 28, 31, 30,
    31, 30, 31, 31,
    30, 31, 30, 31].

%is_leap(Y) ->
%   if
%      0 =:= Y rem 400 ->
%         true;
%      0 =:= Y rem 100 ->
%         false;
%      true ->
%         0 =:= Y rem 4
%   end.

is_leap(Y) ->
   (Y rem 4 =:= 0 andalso Y rem 100 /= 0)
   orelse (Y rem 400 =:= 0).

julian(Date) ->
   [Year, Month, Day] = date_parts(Date),
   {Months, _} = lists:split(Month-1, days()),
   DOY = Day + lists:sum(Months),
   Leap = is_leap(Year),
   if
      (Month > 2) and Leap ->
         DOY + 1;
      true ->
         DOY
   end.
