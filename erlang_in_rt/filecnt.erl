-module(filecnt).
-export([filecnt/1]).

% Accept a filename and attempt to open the file

filecnt(FileName) ->
   {Status, Data} = file:open(FileName, [read]),
   if
      Status == error ->
         io:format("Unable to open file ~w because ~w~n",
                     [FileName, Data]);
      true ->
         fc(FileName, Data, {0,0,0})
   end.

% count characters and lines, return tuple

fc(FN, Fd, {Chars, Stops, Lines}) ->
   C = file:read(Fd, 1),
   if
      C == eof ->
         {Chars, Stops, Lines};
      true ->
         {Result, Data} = C,
         if
            Result == error ->
               io:format("Unable to read file ~w because ~w~n",
                     [FN, Data]);
            true ->
               if
                  Data == "\n" ->
                     fc(FN, Fd, {Chars+1, Stops, Lines+1});
                  Data == "." ->
                     fc(FN, Fd, {Chars, Stops+1, Lines});
                  true ->
                     fc(FN, Fd, {Chars+1, Stops, Lines})
               end
         end
   end.
