-module(bael_server_sup).
-behaviour(supervisor).
-include("bael.hrl").

-export([init/1]).
-export([start_link/0, upgrade/0]).

start_link()->
	supervisor:start_link({local, ?MODULE}, ?MODULE, []).

upgrade()->
	ok.

init([])->
	Strategy={one_for_one, 10, 10},
	SpecsList=[{lists:concat(["server_", ID]), {bael_server, start_link, []},
	 permanent, 5000, worker, dynamic}||
 	 ID<-lists:seq(0, ?MAX_SERVERS_POOL_SIZE-1)],
 	{ok, {Strategy, lists:flatten(SpecsList)}}.
