-module(weather).
-behaviour(gen_server).
-export([start_link/0]).         % convenience call for startup
-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).        % gen_server callbacks
-export([report/1, recent/0, connect/1]).
-define(SERVER, ?MODULE).        % macro that just defines this module as server
-record(state, {recent, table}). % recent calls, table

-include_lib("xmerl/include/xmerl.hrl").
-include("weather_records.hrl").
-include("wuapi.hrl").


%%% convenience method for startup

start_link() ->
   gen_server:start_link({global, ?SERVER}, ?MODULE, [], []).

report(Zip) ->
   gen_server:call({global, weather}, Zip).

recent() ->
   {ok, Recent} = gen_server:call({global, weather}, ""),
   Recent.

connect(Node) ->
   net_adm:ping(Node).

%%% gen_server callbacks

init([]) ->
   application:start(inets),
   {ok, Tab} = dets:open_file("stations"),
   {ok, #state{recent=[], table=Tab}}.

handle_call(_Request, _From, State) ->
   Code = _Request,
   case Code of
      [] ->
         Reply = {ok, State#state.recent},
         NewState = State;
      _ ->
         Reply = {ok, weather(Code, State)},
         NewState = State#state{ recent=most_recent([Code|State#state.recent]) }
   end,
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

weather(Code, State) ->
   case dets:lookup(State#state.table, Code) of
      {error, Reason} ->
         {error, Reason};
      [#station{id = Code, lat = Lat, lon = Lon} | _] ->
         Url = "http://api.weatherunlocked.com/api/current/" ++
               Lat ++ "," ++ Lon ++
               "?app_id=" ++ ?APP_ID ++
               "&app_key=" ++ ?APP_KEY,
         query(Url)
   end.

weather(Zip) ->
   Url = "http://api.weatherunlocked.com/api/current/us." ++
         integer_to_list(Zip) ++
         "?app_id=" ++ ?APP_ID ++
         "&app_key=" ++ ?APP_KEY,
   query(Url).

query(Url) ->
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
