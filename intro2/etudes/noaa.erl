-module(noaa).
-export([init/1, data/1, save/1]).
-include_lib("xmerl/include/xmerl.hrl").
-include("weather_records.hrl").

init(File) ->
   save(data(File)).

data(File) ->
   {ok, Bin} = file:read_file(File),
   Str = binary_to_list(Bin),
   Parsed = element(1, xmerl_scan:string(Str)),
   Children = Parsed#xmlElement.content,
   Stations = [E || E <- Children,
                    element(1, E) == xmlElement, 
                    element(2, E) == station],
   StationData = lists:map(fun stationData/1, Stations),
   lists:map(fun([{station_id, Id}, {latitude, Lat}, {longitude, Lon}] = S) ->
               #station{id = Id, 
                        lat = Lat,
                        lon = Lon}
             end, StationData).

save(Data) ->
   {ok, stations} = dets:open_file(stations, [{keypos, #station.id},
                                              {auto_save, 10000}]),
   ok = dets:insert(stations, Data),
   ok = dets:close(stations).

list_to_number(Value) ->
   case string:to_float(Value) of
      {error, no_float} ->
         {Int, _} = string:to_integer(Value),
         Int;
      {Float, _} ->
         Float
   end.

stationData(S) ->
   ToFind = [station_id, latitude, longitude],
   EltList = [{E#xmlElement.name, extract_text(E#xmlElement.content)}
      || E <- S#xmlElement.content, element(1, E) == xmlElement],
   lists:map(fun(Item) -> lists:keyfind(Item, 1, EltList) end, ToFind).

extract_text(Content) ->
   Item = hd(Content),
   case element(1, Item) of
      xmlText ->
         Item#xmlText.value;
      _ ->
         ""
   end.
   
