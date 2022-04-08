-module(rhash).
-export([hash/0, hash/1, new/1, try_convert/1]).
-export([to_hash/1]).
-export([compare/2]).
-export([element/2, store/3, any/1, any/2, assoc/2]).
-export([clear/1, compact/1]).
-export([default/1, default/2, default_proc/1]).
-export([set_default/2, set_default_proc/2]).
-export([delete/2, delete/3]).

-record(hash, {store, defval = nil, defproc = nil}).

hash() ->
   #hash{store = #{}}.
hash(L) ->
   #hash{store = maps:from_list(L)}.

new(DefProc) when is_function(DefProc,2) ->
   #hash{store = #{}, defproc = DefProc};
new(DefVal) ->
   #hash{store = #{}, defval = DefVal}.

try_convert(Any) ->
   try to_hash(Any) of
      Val ->
         if
            is_record(Val, hash) ->
               Val;
            true ->
               throw({not_hash, Val})
         end
   catch
      throw:Exception ->
         throw(Exception)
   end.

to_hash(Any) when is_map(Any) -> Any.

compare(H, OtherH) ->
   if
      H == OtherH->
         equal;
      true ->
         I = maps:intersect(H, OtherH),
         case I of
            H ->
               subset;
            _Any ->
               undefined
         end
   end.


element(K, H) ->
   case maps:find(K, H#hash.store) of
      {ok, V} ->
         V;
      error ->
         default(K, H)
   end.


store(K,V,H) ->
   S = H#hash.store,
   H#hash{store = maps:put(K, V, S)}.


any(H) ->
   case maps:size(H#hash.store) of
      0 -> false;
      _ -> true
   end.

any({K, V}, H) ->
   V == maps:get(K, H#hash.store, undefined);
any(Pred, H) when is_function(Pred, 2) ->
   none /= mymaps:search_pred(H#hash.store, Pred).


assoc(K, H) ->
   case maps:find(K, H#hash.store) of
      {ok, V} ->
         {K, V};
      error ->
         nil
   end.


clear(H) ->
   H#hash{store = #{}}.


compact(H) ->
   S = H#hash.store,
   H#hash{store = maps:filter(fun(_K, V) -> V /= nil end, S)}.


default(H) -> H#hash.defval.
default_proc(H) -> H#hash.defproc.

default(K, H) ->
   DefProc = H#hash.defproc,
   if
      DefProc /= nil ->
         DefProc(K, H);
      true ->
         H#hash.defval
   end.

set_default(DefVal, H) ->
   H#hash{defval = DefVal}.

set_default_proc(DefProc, H) when is_function(DefProc, 2) ->
   H#hash{defproc = DefProc}.


delete(K, H) ->
   S = H#hash.store,
   case maps:take(K, S) of
      {V, S2} ->
         {V, H#hash{store = S2}};
      error ->
         {nil, H}
   end.

delete(K, Fun, H) ->
   case delete(K, H) of
      {nil, H1} ->
         {Fun(K), H1};
      X ->
         X
   end.

