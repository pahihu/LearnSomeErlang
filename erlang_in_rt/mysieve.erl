-module(sieve).
-export([till/1, reverse/1, remove/2, each/2, sieve/1]).

till(2,R) ->
   [2|R];
till(N,R) ->
   till(N-1,[N|R]).

till(N) ->
   till(N,[]).

reverse([],R) ->
   R;
reverse([H|T],R) ->
   reverse(T,[H|R]).

reverse(L) ->
   reverse(L,[]).

remove([],R,_) ->
   reverse(R);
remove([H|T],R,N) when 0 == H rem N ->
   remove(T,R,N);
remove([H|T],R,N) ->
   remove(T,[H|R],N).

remove(L,N) ->
   remove(L,[],N).

format([]) ->
   ok;
format([H|T]) ->
   io:format(" ~w",[H]),
   format(T).

each(_,[]) ->
   ok;
each(Fn, [H|T]) ->
   Fn(H),
   each(Fn, T).
   
sieve([H|T], N, R) when H =< N ->
   sieve(remove(T, H), N, [H|R]);
sieve(L, _, R) ->
   reverse(R) ++ L.

sieve(N) ->
   sieve(till(N), math:pow(N,0.5), []).

