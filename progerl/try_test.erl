-module(try_test).
-export([demo1/0, demo2/0, demo3/0]).

demo3() ->
   % erlang:get_stacktrace/0 removed!
   case (catch generate_exception(5)) of
      {'EXIT', Reason} ->
         Reason;
      Any ->
         Any
   end.

generate_exception(1) -> a;
generate_exception(2) -> throw(a);
generate_exception(3) -> exit(a);
generate_exception(4) -> {'EXIT', a};
generate_exception(5) -> error(a).

demo1() ->
   [catcher(I) || I <- [1,2,3,4,5]].

demo2() ->
   [{I, (catch generate_exception(I))} || I <- [1,2,3,4,5]].

report(Msg = {N, caught, _ExcType, _Reason}) ->
   io:format("Attention: an error occurred processing ~p!~n",[N]),
   Msg.

catcher(N) ->
   try generate_exception(N) of
      Val -> {N, normal, Val}
   catch
      throw:X -> report({N, caught, thrown, X});
      exit:X  -> report({N, caught, exited, X});
      error:X -> report({N, caught, error, X})
   end.

