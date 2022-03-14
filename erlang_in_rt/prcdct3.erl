% Phone rolodex, using dictionary client (connects to local/remote server).
-module(prcdct3).
-export([teldir/0, teldir/1]).

teldir() ->
   run(dctcln:connect()).

teldir(Node) ->
   run(dctcln:connect(Node)).

run(Srv) ->
   getdata(Srv),
   menu(),
   querylp(Srv).

getdata(Srv) ->
   io:format("Enter name and phone number ", []),
   io:format("(blank line to end)~n", []),
   getlp(Srv).

getlp(Srv) ->
   Line = io:get_line('name phone> '),
   Lst = string:tokens(Line, " \n"),
   getlp(Srv,Lst).

getlp(Srv,[Name, Phone]) ->
   dctcln:put(Srv, Name, Phone),
   getlp(Srv);

getlp(Srv,Lst) when length(Lst) == 0 ->
   true;
getlp(Srv,_) ->
   io:format("Error~n"),
   getlp(Srv).

menu() ->
   io:format("Operation~n 1) Search 2) Add/Change "),
   io:format("3) List Names 4) Delete    0) Quit~n", []).

querylp(Srv) -> querylp(Srv, io:fread('op > ', "~d")).

querylp(Srv, {ok, [0]}) -> true;
querylp(Srv, {ok, [1]}) -> search(Srv),  querylp(Srv);
querylp(Srv, {ok, [2]}) -> getdata(Srv), querylp(Srv);
querylp(Srv, {ok, [3]}) -> lstname(Srv), querylp(Srv);
querylp(Srv, {ok, [4]}) -> delete(Srv),  querylp(Srv).

getnam() ->
   Line = io:get_line('name > '),
   getnam(string:tokens(Line, " \n")).

getnam([L]) -> L;
getnam(_) -> io:format("Error~n"), getnam().

search(Srv) -> io:format("~s~n", [dctcln:get(Srv, getnam())]).

lstname(Srv) -> lstnames(dctcln:get(Srv)).

lstnames([]) -> true;
lstnames([{Key, Value}|T]) -> io:format("~s~n", [Key]), lstnames(T).


delete(Srv) -> io:format("~s~n", [dctcln:erase(Srv, getnam())]).


