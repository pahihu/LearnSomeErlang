-module(weather_sup).
-behaviour(supervisor).
-export([start_link/0]).   % convenience call for startup
-export([init/1]).         % supervisor calls
-define(SERVER, ?MODULE).

%%% convenience method for startup
start_link() ->
   supervisor:start_link({local,?SERVER}, ?MODULE, []).


%%% supervisor callback
init([]) ->
   SupFlags = #{strategy   => one_for_one,   % one_for_all, rest_for_one, 
                                             % simple_one_for_one
                intensity  => 1,             % worker crash to terminate sup
                                             %  1 worker restart in every 5secs
                period     => 5},

   Drop = #{id       => 'weather',
            start    => {'weather', start_link, []},
            restart  => permanent,
            shutdown => 5000,
            type     => worker,
            modules  => ['weather']},        % dependencies

   {ok, {SupFlags, [Drop]}}.
