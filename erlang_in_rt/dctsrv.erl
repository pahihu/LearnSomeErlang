%%% dct_server
-module(dctsrv).
-export([start/0, stop/0, status/0, dct/1]).

%% Start dct_server if not registered.
start() ->
   Pid = whereis(dct_server),
   if
      Pid == undefined ->
         register(dct_server, spawn(dctsrv, dct, [[]]));
      true ->
         false
   end.

%% Stop dct_server if running.
stop() ->
   Pid = whereis(dct_server),
   if
      Pid /= undefined ->
         Pid ! stop,
         unregister(dct_server);
      true ->
         false
   end.

%% Report status of dct_server: true - registered, false - unregistered.
status() ->
   Pid = whereis(dct_server),
   if
      Pid == undefined ->
         false;
      true ->
         true
   end.

%% Main server loop.
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
            stop ->
               exit(normal);  % normal process exit, no error report generated!
            X ->
               L
   end,
   dct(NewL).

%% Utility functions.

%% Insert/Update given Key.
insert([], N, Key, Value) ->
   {undefined, [{Key, Value}|N]};
insert([{HKey,HVal} | T], N, Key, Value) when Key == HKey ->
   {HVal, lists:append(N, [{Key, Value} | T])};
insert([H|T], N, Key, Value) ->
   insert(T, [H|N], Key, Value).

%% Find given Key.
find([], N, Key) ->
   undefined;
find([{HKey, HVal}|T], N, Key) when Key == HKey ->
   HVal;
find([H|T], N, Key) ->
   find(T, [H|N], Key).

%% Delete given Key.
delete([], N, _) ->
   {undefined, N};
delete([{HKey, HVal}|T], N, Key) when HKey == Key ->
   {HVal, lists:append(N, T)};
delete([H|T], N, Key) ->
   delete(T, [H|N], Key).
   
