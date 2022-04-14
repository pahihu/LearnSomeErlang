-module(abc).
-export([f/1, a/2, b/1, fac/1]).
-import(lists, [map/2]).
%%% -compile(export_all).
-vsn(1234).
-author({joe,armstrong}).
-purpose("example of attributes").
-export([info/0, info/1]).

f(L) ->
   L1 = map(fun(X) -> 2*X end, L),
   lists:sum(L1).

a(X, Y) -> c(X) + a(Y).
a(X) -> 2*X.
b(X) -> X*X.
c(X) -> 3*X.

fac(1) -> 1;
fac(N) -> N * fac(N-1).

%% module_info(exports | imports | attributes | compile)

info() ->
   info(atom_to_list(?MODULE) ++ ".beam").

info(File) ->
   beam_lib:chunks(File, [attributes]).
