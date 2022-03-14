-module(translate_service).
-export([loop/0, translate/2]).

loop() ->
   receive
      {From, "casa"} ->
         From ! "house",
         loop();

      {From, "blanca"} ->
         From ! "white",
         loop();

      {From, "loco"} ->
         From ! "crazy",
         loop();

      {From, "hombre"} ->
         From ! "man",
         loop();

      {From, "poco"} ->
         From ! "little",
         loop();

      {From, _} ->
         From ! "I don't understand.",
         translate:loop()
   end.

translate(To, Word) ->
   To ! {self(), Word},
   receive
      Translation -> Translation
   after 5000 ->
      no_response
   end.
