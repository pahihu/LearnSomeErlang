%%% Client interface for dct_server.
-module(dctcln).
-export([connect/0, connect/1, get/1, get/2, put/3, erase/2]).

%% Connect to local dct_server.
connect() ->
   whereis(dct_server).

%% Connect to dct_server on Node.
connect(Node) ->
   {dct_server, Node}.

%% Get dictionary contents.
get(Pid) ->
   Pid ! {getall, self()},
   receive
      X -> X
   after
      10000 -> timeout
   end.

%% Get data for given Key.
get(Pid, Key) ->
   Pid ! {get, self(), Key},
   receive
      X -> X
   after
      10000 -> timeout
   end.

%% Insert/Update Key,Value.
put(Pid, Key, Value) ->
   Pid ! {put, self(), Key, Value},
   receive
      X -> X
   after
      10000 -> timeout
   end.

%% Erase Key, return old Value.
erase(Pid, Key) ->
   Pid ! {erase, self(), Key},
   receive
      X -> X
   after
      10000 -> timeout
   end.
