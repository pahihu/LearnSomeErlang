-module(bin).
-export([bin/1, bong/0, boing/0, unbound/1]).

bin(0) -> 0;
bin(1) -> 1.

bong() -> throw({bong,42}).

boing() ->
   receive
      X -> X
   after 3.4 ->
      timeout
   end.

unbound(X) ->
   X.
