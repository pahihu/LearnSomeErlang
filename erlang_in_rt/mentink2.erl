-module(mentink2).
-export([outer/1]).

inner(N) when N > 0 ->
   inner(N-1);
inner(0) ->
   ok.

outer(N,M) when M > 0 ->
   inner(N),
   outer(N,M-1);
outer(_,0) ->
   ok.

outer(N) ->
   outer(N,N).

%%             ms    MIPS
%% OTP R24B02  887   450.95
%% SF3         844   475.93
%% iForth V6   838   477.32
