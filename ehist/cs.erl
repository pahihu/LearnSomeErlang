%%% Generic client-server
-module(cs).
-export([start/3, rpc/2]).

start(Name, Data, Fun) ->
   register(Name,
            spawn(fun() ->
                     loop(Data, Fun)
                  end)).

rpc(Name, Q) ->
   Tag = make_ref(),
   Name ! {request, self(), Tag, Q},
   receive
      {Tag, Reply} -> Reply
   end.

loop(Data, F) ->
   receive
      {request, Pid, Tag, Q} ->
         {Reply, Data1} = F(Q, Data),
         Pid ! {Tag, Reply},
         loop(Data1, F)
   end.
