-module(doc2).
-author('Maurice Castro').
-copyright('Copyright (c) 1998').
-vsn('$Id: doc.erl,v 1.4 1998/06/22 02:33:07 maurice Exp $').
-modified('Mon Jun 22 08:38:16 EST 1998').
-modified_by('maurice@serc').
-modified('Mon Jun 22 12:17:35 EST 1998').
-modified_by('maurice@serc').
-modified('Sat Feb 12 14:20:00 CET 2022').
-modified_by('pahihu@gmail.com').

-export([start/0, start/1, p/1, v/1, p/3, v/3, msglp/2]).

%%% --------------------------------------------------------------
%%% This module illustrates documentation conventions described in
%%% 'Erlang in Real Time' by Maurice Castro
%%% It implements a Counting Semaphore as defined by Hwang K,
%%% 'Advanced Computer Architecture' McGraw Hill, 1993, p638.
%%% Warning this code assumes no messages are lost.
%%% --------------------------------------------------------------

%% ---------------------------------------------------------------
%% This is the special case of a binary semaphore, use general
%% start routine to perform start.
%% ---------------------------------------------------------------

start() ->
   start(1).

%% ---------------------------------------------------------------
%% The start/1 function starts a server and returns its pid.
%% This is the general case
%% ---------------------------------------------------------------

start(N) ->
   % spawn the message loop with initial data of N
   spawn(doc, msglp, [N, []]).

%% ---------------------------------------------------------------
%% Number of retries.
%% ---------------------------------------------------------------

tries() ->
   5.

%% ---------------------------------------------------------------
%% Default timeout for P()/V().
%% ---------------------------------------------------------------

timeout() ->
   5000.

%% ---------------------------------------------------------------
%% Try to P(s), wait for acknowledge, exit if failed.
%% ---------------------------------------------------------------

try_p(_, 0, _) ->
   exit(p);
try_p(S, N, To) ->
   Self = self(),
   S ! {semaphore, p, Self},
   receive
      {ack, p, Self} ->
         ok
   after
      To ->
         try_p(S, N-1, To)
   end.

%% ---------------------------------------------------------------
%% P(s): if s > 0, then s = s - 1; else put in wait queue
%% Retry N times, with To timeout.
%% ---------------------------------------------------------------

p(S, N, To) ->
   try_p(S, N, To),
   % wait for a continue message to indicate exiting queue
   receive
      {semaphore, cont} ->
         true
   end.

%% ---------------------------------------------------------------
%% P(s): if s > 0, then s = s - 1; else put in wait queue
%% ---------------------------------------------------------------

p(S) ->
   p(S, tries(), timeout()).

%% ---------------------------------------------------------------
%% Try to V(s), wait for acknowledge, exit if failed.
%% ---------------------------------------------------------------

v(_, 0, _) ->
   exit(v);
v(S, N, To) ->
   Self = self(),
   S ! {semaphore, v, Self},     % server handles v
   receive
      {ack, v, Self} ->
         ok
   after
      To ->
         v(S, N-1, To)
   end.

%% ---------------------------------------------------------------
%% V(s): if wait queue not empty, wake up one; else s = s + 1
%% ---------------------------------------------------------------

v(S) ->
   v(S, tries(), timeout()).

%% ---------------------------------------------------------------
%% The msglp function handles cases:
%% ---------------------------------------------------------------

msglp(S, L) ->
   {NewS, NewL} = receive
      {semaphore, p, Pid} ->
         Pid ! {ack, p, Pid}, % send back ack
         if
            % P(s): if s > 0, then s = s -1;
            S > 0 ->
               Pid ! {semaphore, cont},
               {S - 1, L};
            true ->
               % else put in wait queue
               {S, [Pid|L]}
            end;
      % V(s): if wait queue not empty,
      {semaphore, v, Pid} when length(L) =/= 0 ->
         Pid ! {ack, v, Pid}, % send back ack
         [H|T] = L,
         % wake up one;
         H ! {semaphore, cont},
         {S, T};
      % if the list is empty on a v
      {semaphore, v, Pid} ->
         Pid ! {ack, v, Pid}, % send back ack
         % else s = s + 1
         {S+1, L}
   end,
   msglp(NewS, NewL).
