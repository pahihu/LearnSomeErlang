-module(dct).
-export([start/0, start/1, get/1, get/2, put/3, erase/2, dct/1]).

start() ->
   spawn(dct, dct, [[]]).

start(Node) ->
   spawn(Node, dct, dct, [[]]).

get(Pid) ->
   Pid ! {getall, self()},
   receive
      X -> X
   end.

get(Pid, Key) ->
   Pid ! {get, self(), Key},
   receive
      X -> X
   end.

put(Pid, Key, Value) ->
   Pid ! {put, self(), Key, Value},
   receive
      X -> X
   end.

erase(Pid, Key) ->
   Pid ! {erase, self(), Key},
   receive
      X -> X
   end.

dct(L) ->
   NewL = receive
            {put, Pid, Key, Value} ->
               {Code, List} = insert(L, [], Key, Value),
               Pid ! Code,
               List;
            {get, Pid, Key} ->
               Code = find(L, [], Key),
               Pid ! Code,
               L;
            {getall, Pid} ->
               Pid ! L,
               L;
            {erase, Pid, Key} ->
               {Code, List} = delete(L, [], Key),
               Pid ! Code,
               List;
            X ->
               L
   end,
   dct(NewL).

insert([], N, Key, Value) ->
   {undefined, [{Key, Value}|N]};
insert([{HKey,HVal} | T], N, Key, Value) when Key == HKey ->
   {HVal, lists:append(N, [{Key, Value} | T])};
insert([H|T], N, Key, Value) ->
   insert(T, [H|N], Key, Value).

find([], N, Key) ->
   undefined;
find([{HKey, HVal}|T], N, Key) when Key == HKey ->
   HVal;
find([H|T], N, Key) ->
   find(T, [H|N], Key).

delete([], N, _) ->
   {undefined, N};
delete([{HKey, HVal}|T], N, Key) when HKey == Key ->
   {HVal, lists:append(N, T)};
delete([H|T], N, Key) ->
   delete(T, [H|N], Key).
   
