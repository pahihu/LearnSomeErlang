-module(bstone).
-export([run/0, nl/0]).


%% Fixed list of benchmarks.
benchmarks() ->
   [sort_bm, ref_bm, lists_bm, lc_bm, fun_bm, freq_bm, float_bm,
    call_tail_bm, call_bm, bs_sum_bm, bs_sum32_bm, bs_simple_bm,
    bs_float_bm, bs_bm, bin_to_term_bm].


%% Dynamic list of benchmarks, collected from local library.
modules() ->
   Files = filelib:wildcard("*.erl"),
   lists:map(fun(X) ->
               [A, _] = string:split(X, "."),
               list_to_atom(A)
             end,
             Files).


%% Load the code on connected nodes.
nl(_Nodes, []) ->
   ok;
nl(Nodes, [Mod | T]) ->
   {Mod, Bin, File} = code:get_object_code(Mod),
   {_Replies, _} = rpc:multicall(Nodes, code, load_binary, [Mod, File, Bin]),
   nl(Nodes, T).

nl() ->
   nl(erlang:nodes(), modules()).


%% Run all benchmarks.
run() ->
   run(benchmarks()).


%% Run each benchmark file.
run([]) ->
   ok;
run([M|T]) ->
   io:format("~n~p~n", [M]),
   Args = apply(M, benchmarks, []),
   run(M, Args),
   run(T).


%% Transform tuple to arg list.
run(M, {Iter,Fns}) ->
   run(M, Fns, Iter).


%% Run each function in module M iter times.
run(_M, [], Iter) ->
   ok;
run(M, [F | T], Iter) ->
   {Time, _} = timer:tc(M, F, [Iter]),
   io:format("~-18.. w ~7.. w~n", [F, Time]),
   run(M, T, Iter).
