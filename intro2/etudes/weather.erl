-module(weather).
-export([init/0, test/1]).
-include_lib("xmerl/include/xmerl.hrl").
-include("weatherunlocked.hrl").

init() ->
   application:start(inets).


test(Zip) ->
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
   ToFind = [latitude,longitude,altitude_m,weather_description, temperature_c, temperature_f],
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
