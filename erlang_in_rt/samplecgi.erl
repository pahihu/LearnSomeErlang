-module(samplecgi).
-export([main/2]).

main(Srv, Args) ->
   cgicln:send(Srv, <<"<html><head><title>Sample CGI</title></head><body>">>),
   cgicln:send(Srv, <<"<pre>\n">>),
   lists:foreach(
      fun({Key, Value}) ->
         Data = list_to_binary(term_to_string(Key,Value)),
         cgicln:send(Srv, Data)
      end, Args),
   cgicln:send(Srv, <<"</pre></body></html>">>),
   cgicln:close(Srv).

term_to_string(Key, Value) ->
   lists:flatten(io_lib:format("~p: ~p~n", [Key,Value])).
