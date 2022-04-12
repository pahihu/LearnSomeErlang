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
-export([flatten/1, flatten/2]).
-export([has_key/2, has_value/2]).
-export([hash_value/1]).
-export([include/2]).
-export([inspect/1, invert/1, keep_if/2, key/2, keys/1]).
-export([length/1, member/2, merge/2, merge/3]).
-export([reject/2, select/2]).
-export([shift/1]).
-export([values/1, values_at/2]).

-export([update/2, update/3]).
-export([size/1]).
-export([to_a/1, to_h/1, to_s/1]).

-export([xappend/1, xappend/2]).
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

reject(Pred,H) -> delete_if(Pred,H).


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

%% append is 1 level flatten, xappend can work as a multi-level flatten
xappend(L) ->
   xappend(L,[],1).

xappend(L, N) ->
   xappend(L,[],N).

xappend([],A,_N) ->
   A;
xappend([H|T],A,N) ->
   NewA = if
      is_list(H) andalso N > 0 ->
         A ++ xappend(H,[],N-1);
      true ->
         A ++ [H]
   end,
   xappend(T,NewA,N).

flatten(H) ->
   flatten(1,H).

flatten(Level,H) when Level >= 0 ->
   xappend(lists:map(fun tuple_to_list/1,to_a(H)),Level).

has_key(K,H) ->
   case maps:find(K,H#hash.store) of
      {ok, _V} ->
         true;
      error ->
         false
   end.

has_value(V,H) ->
   lists:member(V,values(H)).

hash_value(H) ->
   erlang:phash2(H#hash.store).

include(K,H) -> has_key(K,H).

to_s(H) ->
   lists:flatten(io_lib:format("~p",[H#hash.store])).

inspect(H) -> to_s(H).

invert(H) ->
   S = maps:from_list(lists:map(fun({K,V}) -> {V,K} end,to_a(H))),
   H#hash{store = S}.

keep_if(Pred, H) ->
   H#hash{store = maps:filter(Pred, H#hash.store)}.

select(Pred,H) -> keep_if(Pred,H).

key(V,H) ->
   case lists:keyfind(V,2,to_a(H)) of
      false ->
         nil;
      {K,V} ->
         K
   end.

keys(H) -> maps:keys(H#hash.store).

length(H) -> rhash:size(H).

member(K,H) -> has_key(K,H).

size(H) -> maps:size(H#hash.store).

to_a(H) -> maps:to_list(H#hash.store).

to_h(H) -> H.

update(OtherH, H) ->
   S1 = H#hash.store, S2 = OtherH#hash.store,
   H#hash{store = maps:merge(S1, S2)}.

update(OtherH, Fun, H) ->
   S1 = H#hash.store, S2 = OtherH#hash.store,
   H#hash{store = maps:merge_with(Fun, S1, S2)}.

merge(OtherH,H) -> update(OtherH,H).
merge(OtherH,Fun,H) -> update(OtherH,Fun,H).

shift(H) ->
   case empty(H) of
      true ->
         {default(H), H};
      false ->
         S = H#hash.store,
         {K1,V1,_I} = maps:next(maps:iterator(S)),
         {{K1,V1}, H#hash{store = maps:remove(K1,S)}}
   end.

values(H) ->
   maps:values(H#hash.store).

values_at(Ks,H) ->
   values_at(Ks, H, []).

values_at([], _H, A) ->
   lists:reverse(A);
values_at([Kh | Kt], H, A) ->
   values_at(Kt, H, [rhash:element(Kh,H) | A]).

%% --- tests ---
test_key() ->
   H = hash([{a,100},{b,200},{c,300},{d,300}]),
   b = key(200,H),
   c = key(300,H),
   nil = key(999,H),
   ok.

test_keep_if() ->
   H1 = hash([{n,1},{m,1},{y,3},{d,2},{a,0}]),
   H2 = keep_if(fun(_K,V) -> V rem 2 == 1 end,H1),
   [{m,1},{n,1},{y,3}] = to_a(H2),
   ok.

test_invert() ->
   H = hash([{n,100},{m,100},{y,300},{d,200},{a,0}]),
   H2 = invert(H),
   [{0,a},{100,n},{200,d},{300,y}] = to_a(H2),
   ok.

test_to_s() ->
   H = hash([{c,300},{a,100},{d,400},{c,300}]),
   "#{a => 100,c => 300,d => 400}" = to_s(H),
   ok.

test_include() ->
   H = hash([{a,100},{b,200}]),
   true = include(a,H),
   false = include(z,H),
   ok.

test_hash() ->
   H1 = hash([{a,1},{b,2}]),
   H2 = hash([{b,2},{a,1}]),
   H1 = H2,
   false = H1 /= H2,
   true = hash_value(H1) == hash_value(H2),
   ok.

test_has_value() ->
   H = hash([{a,100},{b,200}]),
   true = has_value(100,H),
   false = has_value(999,H),
   ok.

test_has_key() ->
   H = hash([{a,100},{b,200}]),
   true = has_key(a,H),
   false = has_key(z,H),
   ok.

test_flatten() ->
   H = hash([{1,one},{2,[2,two]},{3,three}]),
   [1,one,2,[2,two],3,three] = flatten(H),
   [1,one,2,2,two,3,three] = flatten(2,H),
   ok.

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

test_keys() ->
   H = hash([{a,100},{b,200},{c,300},{d,400}]),
   [a,b,c,d] = keys(H),
   ok.

test_length() ->
   H1 = hash([{d,100},{a,200},{v,300},{e,400}]),
   4 = rhash:length(H1),
   {_, H2} = delete(a,H1),
   3 = rhash:length(H2),
   ok.

test_member() ->
   H = hash([{a,100},{b,200}]),
   true = member(a,H),
   false = member(z,H),
   ok.

test_merge() ->
   H1 = hash([{a,100},{b,200}]),
   H2 = hash([{b,254},{c,300}]),
   H3 = merge(H2, H1),
   [{a,100},{b,254},{c,300}] = to_a(H3),
   H4 = merge(H2, fun(_K,OldV,NewV) -> NewV - OldV end, H1),
   [{a,100},{b,54},{c,300}] = to_a(H4),
   [{a,100},{b,200}] = to_a(H1),
   ok.

test_shift() ->
   H1 = hash([{1,a},{2,b},{3,c}]),
   {KV, H2} = shift(H1),
   {1,a} = KV,
   [{2,b},{3,c}] = to_a(H2),
   ok.

test_values() ->
   H = hash([{a,100},{b,200},{c,300}]),
   [100,200,300] = values(H),
   ok.

test_values_at() ->
   H = hash([{cat,feline},{dog,canine},{cow,bovine}]),
   [bovine,feline] = values_at([cow,cat],H),
   ok.

test() ->
   test_values_at(),
   test_values(),
   test_shift(),
   test_merge(),
   test_member(),
   test_length(),
   test_keys(),
   test_key(),
   test_keep_if(),
   test_invert(),
   test_to_s(),
   test_include(),
   test_hash(),
   test_has_value(),
   test_has_key(),
   test_flatten(),
   test_fetch(),
   test_update(),
   test_clear(),
   test_default(),
   test_default_proc(),
   test_delete(),
   test_delete_if(),
   ok.
