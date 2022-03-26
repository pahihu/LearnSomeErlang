-module(chatroom).
-behaviour(gen_server).
-export([start_link/0]).         % convenience call for startup
-export([init/1,                 % gen_server callbacks
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).
-define(SERVER, ?MODULE).        % macro that just defines this module as server


%%% convenience method for startup

start_link() ->
   gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).


%%% gen_server callbacks

init([]) ->
   application:start(inets),
   {ok, []}.

handle_call(Request, FromAlias, State) ->
   {From, _Any} = FromAlias,
   case Request of
      {login, UserName, ServerName} ->
         UserServer = {UserName, ServerName},
         case lists:keyfind(UserServer, 1, State) of
            false ->
               Reply = {ok, "Logged in."},
               NewState = lists:keystore(UserServer, 1, State, {UserServer,From});
            Tuple when is_tuple(Tuple) ->
               Reply = {duplicate, UserServer},
               NewState = State
         end;
      logout ->
         Reply = ok,
         NewState = lists:keydelete(From, 2, State);
      users ->
         Reply = [UserServer || {UserServer, _Pid} <- State],
         NewState = State;
      {who, _Person, ServerName} ->
         Reply = gen_server:call({person, ServerName}, get_profile),
         NewState = State;
      {say, Text} ->
         {FromUserServer, _FromPid} = lists:keyfind(From, 2, State),
         lists:foreach(fun({UserServer, _Pid}) ->
                        case UserServer of
                           FromUserServer ->
                              skip;
                           {_UserName, ServerName} ->
                              gen_server:cast({person, ServerName},
                                              {message, FromUserServer, Text})
                         end
                       end, State),
         Reply = ok,
         NewState = State;
      X ->
         Reply = {unknown, X},
         NewState = State
   end,
   {reply, Reply, NewState}.

handle_cast(_Msg, State) ->
   {noreply, State}.

handle_info(_Info, State) ->
   {noreply, State}.

terminate(_Reason, _State) ->
   ok.

code_change(_OldVsn, State, _Extra) ->
   {ok, State}.


%%% Internal functions
