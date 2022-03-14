-module(filecnt2).
-export([filecnt/1]).

% Accept a filename and attempt to open the file

filecnt(FileName) ->
   filecnt(FileName, file:open(FileName, [read])).

filecnt(FN, {error, Fd}) ->
   io:format("Unable to open file ~w because ~w~n",
               [FN, Fd]);
filecnt(FN, {Status, Fd}) ->
   fc({FN, Fd}, {0,0,0}).


% read next character
fc(F, CSL) ->
   {FN, Fd} = F,
   fc(F, file:read(Fd, 1), CSL).


% count characters and lines, return tuple

fc(F, eof, CSL) ->
   CSL;

fc({FN, Fd}, {error, Data}, CSL) ->
   io:format("Unable to read file ~w because ~w~n", [FN, Data]);

fc(F, {Result, "\n"}, {Chars,Stops,Lines}) ->
   fc(F, {Chars+1, Stops, Lines+1});

fc(F, {Result, "."}, {Chars,Stops,Lines}) ->
   fc(F, {Chars, Stops+1, Lines});

fc(F, _, {Chars,Stops,Lines}) ->
   fc(F, {Chars+1, Stops, Lines}).

