-module(translate).
-export([loop/0]).

loop() ->
   receive
      "casa" ->
         io:format("house~n"),
         loop();

      "blanca" ->
         io:format("white~n"),
         loop();

      "loco" ->
         io:format("crazy~n"),
         loop();

      "hombre" ->
         io:format("man~n"),
         loop();

      "poco" ->
         io:format("little~n"),
         loop();

      _ ->
         io:format("I don't understand.~n"),
         translate:loop()
   end.
