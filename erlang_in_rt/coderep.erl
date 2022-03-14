-module(coderep).
-export([msglp/0]).

vers() ->
   2.

msglp() ->
   Msg = receive
            X -> X
   end,
   io:format("~w ~w~n", [Msg, vers()]),
   coderep:msglp().
%  ^^^^^^^^^^^^^

% NB. internal (statically linked) & imported refs
%     are resolved to the module where last fully 
%     qualified call made.

%     the fully qualified name triggers the module
%     replacement
