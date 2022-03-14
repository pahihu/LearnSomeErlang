-module(app).
-export([app/2, maxx/1, minn/1]).

app([],R) ->
   R;
app([H|T], R) ->
   [H|app(T,R)].

maxx([],N) ->
   N;
maxx([H|T],N) ->
   if
      H > N ->
         maxx(T, H);
      true ->
         maxx(T, N)
   end.

maxx([]) ->
   nil;
maxx([H|T]) ->
   maxx(T, H).

minn([], N) ->
   N;
minn([H|T], N) ->
   if
      H < N ->
         minn(T, H);
      true ->
         minn(T, N)
   end.

minn([]) ->
   nil;
minn([H|T]) ->
   minn(T, H).
