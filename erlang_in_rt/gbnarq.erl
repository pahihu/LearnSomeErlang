% Go-Back-N ARQ
-module(gbnarq).
-export([open/3, open/4, xmit/2, recv/1, procexists/1]).

open(Dest, Timeout, Retry) ->
   open(Dest, Timeout, Retry, 7).

open(Dest, Timeout, Retry, Ws) ->
   RefId = make_ref(),
   {Dest, RefId, 0, Timeout, Retry, Ws, []}.


%% ------------------------------------
%% Transmit oldest message from window.
%% ------------------------------------

xmit_old({_Dest, _RefId, _N, _Timeout, _Retry, _Ws, []}) ->
   ok;
xmit_old({Dest, RefId, _M, _Timeout, _Retry, _Ws, [{N, Mesg} | _T]}) ->
   Dest ! {self(), RefId, N, Mesg}.


%% ----------------------------------------
%% Transmit packet when window is not full.
%% ----------------------------------------

xmit({Dest, RefId, N, Timeout, Retry, Ws, Pkts}, Mesg) when length(Pkts) =< Ws ->
   Dest ! {self(), RefId, N, Mesg},
   % Window is in-order, oldest message first.
   {Dest, RefId, N+1, Timeout, Retry, Ws, Pkts ++ {N, Mesg}};


%% ------------------------------------
%% Transmit packet when window is full.
%% ------------------------------------

xmit({Dest, RefId, N, Timeout, Retry, Ws, Pkts}, Mesg) ->
   receive
      {RefId, ReqN, ack} ->
         % remove all packets sent, whose mesg. 
         % number is less than ReqN
         NewPkts = lists:dropwhile(
               fun({Seq, _Mesg}) -> Seq < ReqN end,
               Pkts),
         % transmit oldest packet in window
         S = {Dest, RefId, N, Timeout, Retry, Ws, NewPkts},
         xmit_old(S),
         S;
      _X ->
         % drop unknown mesg
         ok
   after Timeout ->
      case procexists(Dest) of
         true ->
            receive
            after Retry ->
               ok
            end,
            xmit({Dest, RefId, N, Timeout, Retry, Ws, Pkts}, Mesg);
         _ ->
            error
      end
   end.

recv(N) ->
   receive
      {Sender, RefId, N, Mesg} ->
         ReqN = N+1,
         Sender ! {RefId, ReqN, ack},
         {ReqN, Mesg};
      _X ->
         ok
   end.

procexists(Pid) when is_pid(Pid) ->
   Nd = node(Pid),
   ThisNd = node(),
   ListProc = if
      Nd == ThisNd ->
         processes();
      true ->
         rpc:call(Nd, erlang, processes, [])
   end,
   lists:member(Pid, ListProc);
procexists(Name) ->
   case whereis(Name) of
      undefined -> false;
      _ -> true
   end.
