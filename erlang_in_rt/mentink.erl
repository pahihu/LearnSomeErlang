-module(mentink).
-export([outer/1]).

inner(0) ->
   ok;
inner(N) ->
   inner(N-1).

outer(_,0) ->
   ok;
outer(N,M) ->
   inner(N),
   outer(N,M-1).

outer(N) ->
   outer(N,N).

%% 1030ms ~ 388.35 MIPS
