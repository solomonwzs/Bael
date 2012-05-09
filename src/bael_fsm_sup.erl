-module(bael_fsm_sup).
-behaviour(supervisor).
-include("bael.hrl").

-export([init/1]).
-export([start_link/0, upgrade/0]).

start_link()->
	supervisor:start_link({local, ?MODULE}, ?MODULE, []).

upgrade()->
	ok.

init([])->
	%ets:new(ets_fsm_state, [set, public, named_table]),
	Strategy={one_for_one, 10, 10},
	SpecsList=[{
			lists:concat(["fsm_", ID]), 
			{bael_fsm, start_link, []},
			permanent, 5000, worker, dynamic
		}||ID<-lists:seq(0, ?MAX_FSMS_POOL_SIZE-1)],
 	{ok, {Strategy, lists:flatten(SpecsList)}}.
