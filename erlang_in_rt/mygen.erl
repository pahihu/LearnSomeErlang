-module(mygen).
-export([bitstr/1]).

bitstr(N) ->
   bitstr([], N).

bitstr(L,0) ->
   L;
bitstr(L,N) ->
   bitstr(eightbit() ++ L, N-1).

% Returns random 8bit number in binary.
eightbit() ->
   L = "00000000" ++ integer_to_list(rand:uniform(256), 2),
   lists:map(fun (X) -> X - $0 end, lists:nthtail(length(L)-8,L)).
