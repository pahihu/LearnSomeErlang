-module(map).
-export([map/2, do/1, test/0]).

% The result is reversed.
%
% map(L, Fn) ->
%    map(L, [], Fn).

% map([], N, Fn) ->
%    N;
% map([H|T], N, Fn) when is_list(H) ->
%    map(T, [map(H, Fn) | N], Fn);
% map([H|T], N, Fn) ->
%    map(T, [apply(Fn, [H]) | N], Fn).

map(L, Fn) ->
   map(L, [], Fn).

% Shape preserving map, the result will have the same shape
% as the input.
map([], N, Fn) ->
   N;
map([H|T], N, Fn) when is_list(H) ->
   [map(H, Fn) | map(T, N, Fn)];
map([H|T], N, Fn) ->
   [apply(Fn, [H]) | map(T, N, Fn)].

% List of lists, where the head is a function, the tail is the args.
do([], N) ->
   N;
do([[Fn|Args]|T],N) ->
   [apply(Fn,Args) | do(T, N)].

do(L) ->
   do(L, []).


% Tests.
testdo() ->
   do([[fun erlang:length/1,[1,2,3]],
       [fun (A,B) -> A+B end,2,3]]).

testmap1() ->
   map([1,2,3], fun(X) -> X+1 end).

testmap2() ->
   map([1,[2,3],[3],[[4,5],6]], fun(X) -> X+1 end).

test() ->
   io:format("~w~n", [testdo()]),
   io:format("~w~n", [testmap1()]),
   io:format("~w~n", [testmap2()]).
