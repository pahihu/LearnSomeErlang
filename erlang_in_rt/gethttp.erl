-module(gethttp).
-export([gethttp/2, parseurl/1]).

gethttp(Url, Filename) ->
   {Host, SPort, Path} = parseurl(Url), 
   Port = list_to_integer(SPort),
   Request = list_to_binary("GET /" ++ Path ++ " HTTP/1.0\r\n\r\n"),
   Data = gethttp(Host, Port, Request),
   % Remove header from Data
   [Head, Body] = re:split(Data,"\r\n\r\n",[{return,list},{parts,2}]),
   file:write_file(Filename, Body).

%% NB. default socket behavior is to transform recv() to messages
%%     Pass {active, false} to enable recv() on socket.
gethttp(Host, Port, Request) ->
   {ok, Sock} = gen_tcp:connect(Host, Port, [inet]),
   ok = gen_tcp:send(Sock, Request),
   {ok, Bin} = do_recvmsg(Sock, []),
   gen_tcp:close(Sock),
   Bin.

%% {active, false}
do_recv(Sock, Bs) ->
   case gen_tcp:recv(Sock, 0) of
      {ok, B} ->
         do_recv(Sock, [Bs, B]);
      {error, closed} ->
         {ok, list_to_binary(Bs)}
   end.

%% default behavior
do_recvmsg(S, Bs) ->
   receive
      {tcp, S, B} ->
         do_recvmsg(S, [Bs, B]);
      {tcp_closed, S} ->
         {ok, list_to_binary(Bs)}
   end.

parseurl(Url) ->
   {match, Parts} = re:run(Url,
                           "(?:\\w+)://([a-z0-9\-.]+):*([0-9]*)/*(.*)",
                           [{capture,all,list}]),
   [Url, SHost, SPort, SPath] = Parts,
   hpp(SHost, SPort, SPath).

hpp(Host, [], []) ->
   {Host, "80", ""};
hpp(Host, [], Path) ->
   {Host, "80", Path};
hpp(Host, Port, []) ->
   {Host, Port, ""};
hpp(Host, Port, Path) ->
   {Host, Port, Path}.
