% hello_world.erl
-module(hello_world).
-export([hello/0]).

hello() ->
   io:format("~s~n",["Hello world!"]).
