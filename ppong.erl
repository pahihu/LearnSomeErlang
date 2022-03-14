-module(ppong).
-export([ping/1, serve/0, pong/0]).

serve() ->
   Self = whereis(pong),
   case Self of
      undefined ->
         io:format("Registering pong~n"),
         register(pong, spawn(ppong,pong,[]));
      true ->
         io:format("Unregistering pong~n"),
         unregister(pong)
   end.

pong() ->
   receive
      {ping, Pid, N} ->
         % io:format("PONG: <- ping ~p~n", [N]),
         % io:format("PONG: -> pong ~p~n", [N+1]),
         Pid ! {pong, N+1},
         pong();
      done ->
         exit(normal)
   end.

ping(N) ->
   Pong = spawn('two@Andrass-Air',ibm, pong, []),
   ping(N, 0, Pong).

ping(0, Mesg, Pid) ->
   Pid ! done,
   {ok, Mesg};
ping(N, Mesg, Pid) ->
   % io:format("PING: -> ping ~p~n",[Mesg]),
   Pid ! {ping, self(), Mesg},
   receive
      {pong, Ans} ->
         % io:format("PING: <- pong ~p~n", [Ans]),
         ping(N-1, Ans, Pid);
      X ->
         % io:format("PING: <- error ~p~n", [X]),
         {error, X}
   end.
