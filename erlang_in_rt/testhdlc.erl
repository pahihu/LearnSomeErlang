-module(testhdlc).
-export([run/0, run/1]).

run() ->
   run(100).

run(N) ->
   B = mygen:bitstr(128),
   test(B,N),
   test2(B,N).

test(B,N) ->
   io:format("Testing module hdlc ~w iterations~n",[N]),
   io:format("Average enc() time ~w~n",
               [counter:timer(fun () -> hdlc:enc(B) end, N)]),
   E = hdlc:enc(B),
   D = hdlc:dec(E),
   element(2,D) == B.

test2(B,N) ->
   io:format("Testing module hdlc2 ~w iterations~n",[N]),
   io:format("Average enc() time ~w~n",
               [counter:timer(fun () -> hdlc2:enc(B) end, N)]),
   E = hdlc2:enc(B),
   D = hdlc2:dec(E),
   element(2,D) == B.
