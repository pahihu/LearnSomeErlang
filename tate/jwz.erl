-module(jwz).
-export([bench/0, pow/2]).

pow(_X, 0) ->
   1;
pow(X, Y) when Y rem 2 == 0 ->
   R = pow(X, Y div 2),
   R*R;
pow(X, Y) when Y rem 2 == 1 ->
   X * pow(X, Y-1).

bench() ->
   length(integer_to_list(pow(10, 1223146))).
