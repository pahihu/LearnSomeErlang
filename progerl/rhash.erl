-module(rhash).
-export([hash/0, hash/1, new/1, try_convert/1]).
-export([to_hash/1]).
-export([compare/2]).
-export([element/2, store/3, any/1, any/2, assoc/2]).
-export([clear/1, compact/1]).
-export([default/1, default/2, default_proc/1]).
-export([set_default/2, set_default_proc/2]).
-export([delete/2, delete/3, delete_if/2]).
-export([each/2, each_pair/2, each_key/2, each_value/2]).
-export([empty/1]).
-export([fetch/2, fetch/3]).

-export([update/2, update/3]).
-export([size/1]).
-export([to_a/1, to_h/1]).

-export([test/0]).

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


any(H) -> not empty(H).

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

delete_if(Pred, H) ->
   S1 = H#hash.store,
   S2 = maps:filter(fun(K,V) -> not Pred(K,V) end, S1),
   H#hash{store = S2}.


each(Fun, H) ->
   maps:foreach(Fun, H#hash.store).

each_pair(Fun, H) ->
   each(Fun, H).

each_key(Fun, H) ->
   each(fun(K,_V) -> Fun(K) end,H).

each_value(Fun, H) ->
   each(fun(_K, V) -> Fun(V) end,H).


empty(H) -> 0 =:= rhash:size(H).

fetch(K, H) -> maps:get(K, H#hash.store).

fetch(K, H, Fun) when is_function(Fun) ->
   case maps:find(K, H#hash.store) of
      {ok, V} ->
         V;
      error ->
         Fun(K)
   end;
fetch(K, H, Def) ->
   maps:get(K, H#hash.store, Def).


size(H) -> maps:size(H#hash.store).

to_a(H) -> maps:to_list(H#hash.store).

to_h(H) -> H.

update(OtherH, H) ->
   S1 = H#hash.store, S2 = OtherH#hash.store,
   H#hash{store = maps:merge(S1, S2)}.

update(OtherH, Fun, H) ->
   S1 = H#hash.store, S2 = OtherH#hash.store,
   H#hash{store = maps:merge_with(Fun, S1, S2)}.


%% --- tests ---
test_fetch() ->
   H = hash([{a,100},{b,200}]),
   100 = fetch(a,H),
   "go fish" = fetch(z,H,"go fish"),
   "go fish, z" = fetch(z,H,fun(K) -> "go fish, " ++ atom_to_list(K) end),
   ok.

test_update() ->
   H1 = hash([{a,100},{b,200}]),
   H2 = hash([{b,254},{c,300}]),
   H3 = update(H2, H1),
   [{a,100},{b,254},{c,300}] = to_a(H3),
   H4 = update(H2, fun(_K,V1,_V2) -> V1 end, H1),
   [{a,100},{b,200},{c,300}] = to_a(H4),
   ok.

test_clear() ->
   H1 = hash([{a,100}, {b,200}]),
   2 = rhash:size(H1),
   H2 = clear(H1),
   0 = rhash:size(H2),
   ok.

test_default() ->
   H1 = hash(),
   nil = default(H1),
   nil = default(2, H1),
   H2 = new("cat"),
   "cat" = default(H2),
   "cat" = default(2, H2),
   H3 = new(fun(K,_H) -> 10*K end),
   nil = default(H3),
   20 = default(2, H3),
   ok.

test_default_proc() ->
   DefProc = fun(K,_H) ->
               if is_list(K) -> K ++ K;
                  true -> K + K
               end
             end,
   H = new(DefProc),
   DefProc = default_proc(H),
   4 = rhash:element(2,H),
   "catcat" = rhash:element("cat",H),
   H2 = new(fun(K,_H) -> K*K end),
   P = default_proc(H2),
   4 = P(2,H2),
   ok.

test_delete() ->
   H1 = hash([{a,100},{b,200}]),
   {100, H2} = delete(a,H1),
   {nil, H3} = delete(z,H2),
   {"z not found", _H4} = delete(z,fun(K) -> atom_to_list(K) ++ " not found" end,H3),
   ok.

test_delete_if() ->
   H1 = hash([{a,100},{b,200},{c,300}]),
   H2 = delete_if(fun(K,_V) -> K >= b end,H1),
   #{ a := 100 } = H2#hash.store,
   ok.

test() ->
   test_fetch(),
   test_update(),
   test_clear(),
   test_default(),
   test_default_proc(),
   test_delete(),
   test_delete_if(),
   ok.
