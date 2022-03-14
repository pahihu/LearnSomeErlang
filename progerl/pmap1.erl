%%% Armstrong: Google mapreduce like.
-module(mapreduce).
-export([pmap1/2]).

pmap1(F, L) ->
   S = self(),
   Ref = erlang:make_ref(),
   foreach(fun(I) ->
               spawn(fun() -> do_f1(S, Ref, F, I) end)
           end, L),
   %% gather the results
   gather1(length(L), Ref, []).

do_f1(Parent, Ref, F, I) ->
   Parent ! {Ref, (catch F(I))}.

gather1(0, _, L) -> L;
gather1(N, Ref, L) ->
   receive
      {Ref, Ret} -> gather1(N-1, Ref, [Ret|L])
   end.
