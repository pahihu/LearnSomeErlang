% Francesco Cesarini OTP
call(Name, Msg) ->
   Ref = erlang:monitor(process, Name),
   Name ! {request, {Ref, self()}, Msg},
   receive
      {reply, Ref, Reply} ->
         erlang:demonitor(Ref, [flush]),           % removes 'DOWN' messages
         Reply;
      {'DOWN', Ref, process, _Name, _Reason} ->
         {error, no_proc}
   end.

%% Release statement of aims
%%
%% To scale the radical concurrency-oritented
%% programming paradigm to build reliable
%% general-purpose software, such as server-
%% based systems, on massively parallel machines
%% (10^5 cores).
