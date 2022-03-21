-module(compre).
-export([males_over40/0, male_or_over40/0]).

people() ->
   [{"Federico", $M, 22},
    {"Kim",   $F, 45},
    {"Hansa", $F, 30},
    {"Tran",  $M, 47},
    {"Cathy", $F, 32},
    {"Elias", $M, 50}].

males_over40() ->
   [Name || {Name, Gender, Age} <- people(), Gender == $M, Age > 40].

male_or_over40() ->
   [Name || {Name, Gender, Age} <- people(), Gender == $M orelse Age > 40].
