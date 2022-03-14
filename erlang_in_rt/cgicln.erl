-module(cgicln).
-export([send/2, error/2, close/1]).

send(Srv, Data) ->
   Srv ! {data, Data}.

error(Srv, Reason) ->
   Srv ! {error, Reason}.

close(Srv) ->
   Srv ! {closed}.
