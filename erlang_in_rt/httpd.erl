-module(httpd).
-export([start/1, server/1, reqhandler/1, serverloop/1, getvalue/2]).

start(Port) ->
   spawn(httpd, server, [Port]).

server(Port) ->
   {ok, Lsock} = gen_tcp:listen(Port, [binary, {packet, 0},
      {active, false}]),
   serverloop(Lsock).

serverloop(Lsock) ->
   {ok, Sock} = gen_tcp:accept(Lsock),
   spawn(httpd, reqhandler, [Sock]),
   httpd:serverloop(Lsock).

%% -------------------------------
%% Handle request coming from Sock
%% -------------------------------
reqhandler(Sock) ->
   {ReqStr, Props, OptBody} = getfullreq(Sock),
   [ReqType, Query | Tail] = string:tokens(ReqStr, " \n\t"),
   io:format("---~p begin---~n", [ReqType]),
   io:format("     Query [~p]~n", [Query]),
   io:format("Properties~n~p~n", [Props]),
   io:format("---~p end---~n", [ReqType]),
   handlereq(ReqType, Sock, Query, Props, OptBody),
   gen_tcp:close(Sock).

handlereq("GET", Sock, Query, Props, OptBody) ->
   BaseName = re:replace(Query, "/$|^$", "/index.html", 
                           [{return, list}, global]),
   File = re:replace(BaseName, "^/+", "", [{return, list}]),
   sendfile(Sock, File);

handlereq("POST", Sock, Query, Props, OptBody) ->
   io:format("Body~n~p~n",[OptBody]),
   sendok(Sock),
   ok;

handlereq(_, Sock, Query, Props, OptBody) ->
   senderror(Sock, "HTTP/1.0 400 Bad Request\r\n").


%% --------------------------------------------------------------
%% Split list of lines, containing key-value pairs, into KV list.
%% --------------------------------------------------------------
getprops([], Props) ->
   Props;
getprops([H | T], Props) ->
   {match, [_, Key, Value]} = re:run(H,"(\\S+):\\s+(\.+)",[{capture,all,list}]),
   getprops(T, [{Key, Value} | Props]).

% getreq(Sock) ->
%    getreq(Sock, []).

% getreq(Sock, OrigStr) ->
%    {ok, Pack} = gen_tcp:recv(Sock, 0),
%    RecStr = binary_to_list(Pack),
%    NewStr = lists:append(OrigStr, RecStr),
%    Pos = string:str(NewStr, "\r\n"),
%    if
%       Pos =/= 0 ->
%          string:substr(NewStr, 1, Pos-1);
%       true ->
%          getreq(Sock, NewStr)
%    end.

%% --------------------------------------
%% Read full request, with optional body.
%% --------------------------------------
getfullreq(Sock) ->
   getfullreq(Sock,[]).

getfullreq(Sock, Str) ->
   {ok, Pack} = gen_tcp:recv(Sock, 0),
   RecStr = binary_to_list(Pack),
   NewStr = lists:append(Str, RecStr),
   Pos = string:str(NewStr, "\r\n\r\n"),
   if
      Pos =/= 0 ->
         HeadStr = string:substr(NewStr, 1, Pos-1),
         BodyChunk = list_to_binary(string:substr(NewStr, Pos + 4)),
         process_header(Sock, HeadStr, BodyChunk);
      true ->
         getfullreq(Sock, NewStr)
   end.

%% -----------------------------------
%% Get value for Key in property list.
%% -----------------------------------
getvalue([], Key) ->
   [];
getvalue([{Key,HVal} | T], Key) ->
   HVal;
getvalue([H | T], Key) ->
   getvalue(T, Key).


%% ------------------------------------------------
%% Read upto Len bytes from Sock, return as binary.
%% ------------------------------------------------
do_recv_n(Sock, Bs, Len) when Len =< 0 ->
   list_to_binary(Bs);
do_recv_n(Sock, Bs, Len) ->
   case gen_tcp:recv(Sock, 0) of
      {ok, B} ->
         do_recv_n(Sock, [Bs, B], Len - size(B));
      {error, closed} ->
         list_to_binary(Bs)
   end.


%% ------------------------------------------------------
%% Read body when content-length is present and non-zero.
%% ------------------------------------------------------
getbody(Sock, Body, []) ->
   Body;
getbody(Sock, Body, L) ->
io:format("***Len*** = ~p~n",[L]),
   Len = list_to_integer(L) - size(Body),
   do_recv_n(Sock, [Body], Len).


%% ------------------------------------------------------
%% 
%% ------------------------------------------------------
process_header(Sock, HeadStr, Body) ->
   % get Request, properties and Content-Length
   [ReqStr | PropStr] = re:split(HeadStr,"\r\n",[{return,list},trim]),
   Props = getprops(PropStr, []),
   ContentLen = getvalue(Props, "Content-Length"),
   % read optional body, with chunk in Body
   OptBody = getbody(Sock, Body, ContentLen),
   {ReqStr, Props, OptBody}.


sendfile(Sock, Filename) ->
   case file:read_file(Filename) of
      {ok, Binary} ->
         sendhtml(Sock, Binary);
      _ ->
         senderror(Sock, "HTTP/1.0 404 Not Found\r\n")
   end.


senderror(Sock, Msg) ->
   gen_tcp:send(Sock, list_to_binary(Msg)).


sendok(Sock) ->
   gen_tcp:send(Sock, list_to_binary("HTTP/1.0 200 OK\r\n\r\n")).


sendhtml(Sock, Binary) ->
   Body = binary_to_list(Binary),
   Header = "HTTP/1.0 200 OK\r\nContent-Length: " ++ integer_to_list(length(Body)) ++ "\r\n\r\n",
   Response = list_to_binary(Header ++ Body),
   Status = gen_tcp:send(Sock, Response),
   if
      Status =/= ok ->
         {error, Reason} = Status,
         io:format("Error reason: ~p~n", [Reason]);
      true ->
         ok
   end.
