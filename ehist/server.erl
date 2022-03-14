% A generic client-server model with hot-code replacement.
-module(server).
-export([start/2, call/2, change_code/2]).

start(Fun, Data) ->
   spawn(fun() -> server(Fun, Data) end).

call(Server, Args) ->
   rpc(Server, {query, Args}).

change_code(Server, NewFunction) ->
   rpc(Server, {new_code, NewFunction}).

rpc(Server, Query) ->
   Server ! {self(), Query},
   receive
      {Server, Reply} -> Reply
   end.

server(Fun, Data) ->
   receive
      {From, {query, Query}} ->
         {Reply, NewData} = Fun(Query, Data),
         From ! {self(), Reply},
         server(Fun, NewData);
      {From,  {new_code, NewFunction}} ->
         From ! {self(), ack},
         server(Data, NewFunction)
   end.
