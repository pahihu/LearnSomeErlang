-module(dropgen_app).
-behaviour(application).
-export([start/2, stop/1]).

%% code:add_path("ebin/")
%% application:load(dropgen)
%% application:loaded_applications()
%% application:start(dropgen)


start(_Type, _StartArgs) ->
   dropgen_sup:start_link().

stop(_State) ->
   ok.
