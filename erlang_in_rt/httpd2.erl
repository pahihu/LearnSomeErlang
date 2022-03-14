-module(httpd2).
-export([start/1, server/1, reqhandler/1, serverloop/1,
         is_cgi/1, cgi_handler/1]).

start(Port) ->
   spawn(httpd2, server, [Port]).

server(Port) ->
   {ok, Lsock} = gen_tcp:listen(Port, [binary, {packet, 0},
      {active, false}]),
   serverloop(Lsock).

serverloop(Lsock) ->
   {ok, Sock} = gen_tcp:accept(Lsock),
   spawn(httpd2, reqhandler, [Sock]),
   httpd2:serverloop(Lsock).


%% -------------------------------
%% Return always a 2-element list.
%% -------------------------------

list2([]) ->
   [[], []];
list2([E]) ->
   [E, []];
list2(L) ->
   L.


%% ------------------------------------------
%% Split string into 2 parts using separator.
%% ------------------------------------------

split2(Str, Sep) ->
   list2(re:split(Str, Sep, [{return, list}, {parts, 2}])).


%% -----------------------------------------------------------
%% Split string into list of key-value tuples using separator.
%% -----------------------------------------------------------

splitkv([], Sep) ->
   [];
splitkv(Str, Sep) ->
   L = re:split(Str, Sep, [{return, list}, trim]),
   lists:foldl(
            fun(E,A) ->
               [K,V] = re:split(E,"=",[{return,list}]),
               [{K, V} | A]
            end, [], L).


%% ----------------------------------------------------------
%% Split the request URI into path, parameters, query-params.
%% /path/to/resource[;param1=value1...][?field1=value1...]
%% ----------------------------------------------------------

splitheader(Str) ->
   case string:str(Str, ";") of
      0 ->
         P = [],
         [Path, Q] = split2(Str, "\\?");
      _ ->
         [Path, PQ] = split2(Str, ";"),
         [P, Q] = split2(PQ, "\\?")
   end,
   {Path, splitkv(P, ";") , splitkv(Q, "&")}.


%% -------------------------------
%% Handle request coming from Sock
%% -------------------------------
reqhandler(Sock) ->
   {ReqStr, Headers, OptBody} = getfullreq(Sock),
   [ReqType, ReqPath | Tail] = string:tokens(ReqStr, " \n\t"),
   {Path, Params, QParams} = splitheader(ReqPath),
   io:format("---~p begin---~n", [ReqType]),
   io:format("      Path [~p]~n", [ReqPath]),
   io:format("Header~n~p~n", [Headers]),
   io:format("Params~n~p~n", [Params]),
   io:format("QueryParams~n~p~n", [QParams]),
   BaseName = re:replace(Path, "/$|^$", "/index.html", 
                           [{return, list}, global]),
   File = re:replace(BaseName, "^/+", "", [{return, list}]),
   io:format("      File [~p]~n", [File]),
   io:format("---~p end---~n", [ReqType]),
   handlereq(ReqType, Sock, {File, Params, QParams}, Headers, OptBody),
   gen_tcp:close(Sock).

handlereq("GET", Sock, FPQ, Headers, OptBody) ->
   sendresponse("GET", Sock, FPQ, Headers, OptBody);

handlereq("POST", Sock, FPQ, Headers, OptBody) ->
   io:format("Body~n~p~n",[OptBody]),
   sendresponse("POST", Sock, FPQ, Headers, OptBody);

handlereq(_, Sock, FPQ, Headers, OptBody) ->
   senderror(Sock, "HTTP/1.0 400 Bad Request\r\n").


%% -------------------------------
%% True when string ends in ".cgi"
%% -------------------------------

is_cgi(Str) ->
   string:right(Str, 4) =:= ".cgi".

cgi_handler(Str) ->
   list_to_atom(lists:flatten(string:replace(Str,".",""))).

cgi_recv(Pid, Bs) ->
   receive
      {data, Data} ->
         cgi_recv(Pid, [Bs, Data]);
      {error, Reason} ->
         {error, Reason};
      {closed} ->
         {ok, list_to_binary(Bs)}
   after
      10000 ->
         {error, timeout}
   end.

sendresponse(Method, Sock, {Path,Params,QParams}, Headers, OptBody) ->
   case is_cgi(Path) of
      true ->
         Args = [{method,   Method},
                  {headers, Headers},
                  {body,    OptBody},
                  {path,    Path},
                  {params,  Params},
                  {query,   QParams}],
         Cgi = spawn(cgi_handler(Path), main, [self(), Args]),
         Status = cgi_recv(Cgi, []),
% io:format("Status = ~p~n",[Status]),
         case Status of
            {ok, Data} ->
               sendhtml(Sock, Data);
            {error, Reason} ->
               senderror(Sock, "HTTP/1.0 400 Bad Request\r\n")
         end,
         ok;
      false ->
         sendfile(Sock,Path)
   end.


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
getbody(Sock, Body, false) ->
   Body;
getbody(Sock, Body, {_Key, Value}) ->
   % io:format("***Len*** = ~p~n",[L]),
   Len = list_to_integer(Value) - size(Body),
   do_recv_n(Sock, [Body], Len).


%% ------------------------------------------------------
%% 
%% ------------------------------------------------------
process_header(Sock, HeadStr, Body) ->
   % get Request, properties and Content-Length
   [ReqStr | PropStr] = re:split(HeadStr,"\r\n",[{return,list},trim]),
   Props = lists:foldl(
      fun (E,A) ->
         {match, [_, Key, Value]} = re:run(E, "(\\S+):\\s+(\.+)", [{capture,all,list}]),
         [{Key,Value} | A]
      end,
      [], PropStr),
   % read optional body, with chunk in Body
   OptBody = getbody(Sock, Body, lists:keyfind("Content-Length", 1, Props)),
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
