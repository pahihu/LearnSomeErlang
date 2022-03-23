-module(weather).
-behaviour(gen_server).
-export([start_link/0]).   % convenience call for startup
-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).  % gen_server callbacks
-define(SERVER, ?MODULE).  % macro that just defines this module as server
-record(state, {recent}).  % recent calls

-include_lib("xmerl/include/xmerl.hrl").
-include("wuapi.hrl").


%%% convenience method for startup

start_link() ->
   gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).


%%% gen_server callbacks

init([]) ->
   application:start(inets),
   {ok, #state{recent=[]}}.

handle_call(_Request, _From, State) ->
   Zip = _Request,
   Reply = {ok, weather(Zip)},
   NewState = #state{ recent=most_recent([Zip|State#state.recent]) },
   {reply, Reply, NewState}.

handle_cast(_Msg, State) ->
   io:format("Most recent requests: ~w~n", [State#state.recent]),
   {noreply, State}.

handle_info(_Info, State) ->
   {noreply, State}.

terminate(_Reason, _State) ->
   ok.

code_change(_OldVsn, State, _Extra) ->
   {ok, State}.


%%% Internal functions

most_recent(L) when length(L) < 3 ->
   L;
most_recent(L) ->
   {Recent, _} = lists:split(2, L),
   Recent.

weather(Zip) ->
   Url = "http://api.weatherunlocked.com/api/current/us." ++
         integer_to_list(Zip) ++
         "?app_id=" ++ ?APP_ID ++
         "&app_key=" ++ ?APP_KEY,
   Ans = httpc:request(get, {Url, [{"Accept", "text/xml"}]}, [], []),
   case Ans of
      {ok, {Code, Headers, Contents}} ->
         analyze_info(Contents);
      {error, Info} ->
         {error, Info}
   end.


%% Take raw XML data and return a set of {key, value} tuples

analyze_info(WebData) ->
   ToFind = [latitude, longitude, altitude_m,
             weather_description,
             temperature_c, temperature_f],
   Parsed = element(1, xmerl_scan:string(WebData)),
   Children = Parsed#xmlElement.content,
   ElementList = [{E#xmlElement.name, extract_text(E#xmlElement.content)}
      || E <- Children, element(1, E) == xmlElement],
   lists:map(fun(Item) -> lists:keyfind(Item, 1, ElementList) end, ToFind).

extract_text(Content) ->
   Item = hd(Content),
   case element(1, Item) of
      xmlText ->
         Item#xmlText.value;
      _ ->
         ""
   end.
