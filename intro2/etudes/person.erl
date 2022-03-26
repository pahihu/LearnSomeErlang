-module(person).
-behaviour(gen_server).
-export([start_link/1]).         % convenience call for startup
-export([init/1,                 % gen_server callbacks
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).
-export([get_chat_node/0,        % public API
         login/1,
         logout/0,
         say/1,
         users/0,
         who/2,
         profile/2]).
-define(SERVER, ?MODULE).        % macro that just defines this module as server
-record(state, {server, profile}).   % internal state


%%% convenience method for startup

start_link(ChatroomServer) ->
   gen_server:start_link({local, ?SERVER}, ?MODULE, ChatroomServer, []).

get_chat_node() ->
   gen_server:call(person, get_chat_node).

login(UserName) ->
   gen_server:call(person, {server, {login, UserName, node()}}).

logout() ->
   gen_server:call(person, {server, logout}).

say(Text) ->
   gen_server:call(person, {server, {say, Text}}).

users() ->
   gen_server:call(person, {server, users}).

who(UserName, UserNode) ->
   gen_server:call(person, {server, {who, UserName, UserNode}}).

profile(Key, Value) ->
   gen_server:call(person, {profile, Key, Value}).


%%% gen_server callbacks

init(ChatroomServer) ->
   io:format("Chat node is: ~w~n", [ChatroomServer]),
   {ok, #state{server=ChatroomServer, profile=[]}}.

handle_call(Request, _From, State) ->
   Server = State#state.server,
   case Request of
      %% --- local messages ---
      get_chat_node ->
         Reply = Server,
         NewState = State;
      get_profile ->
         Reply = State#state.profile,
         NewState = State;
      {profile, Key, Value} ->
         NewState = State#state{profile = lists:keystore(Key, 1, State#state.profile, {Key, Value})},
         Reply = {ok, NewState#state.profile};
      %% --- server relay ---
      {server, Msg} ->
         Reply = gen_server:call({chatroom, Server}, Msg),
         NewState = State;
      X ->
         Reply = {unknown, X},
         NewState = State
   end,
   {reply, Reply, NewState}.

handle_cast(Msg, State) ->
   case Msg of
      {message, {FromUser, FromServer}, Text} ->
         io:format("~s (~w) says: ~p.~n", [FromUser, FromServer, Text]);
      _X ->
         unknown
   end,
   {noreply, State}.

handle_info(_Info, State) ->
   {noreply, State}.

terminate(_Reason, _State) ->
   ok.

code_change(_OldVsn, State, _Extra) ->
   {ok, State}.


%%% Internal functions
