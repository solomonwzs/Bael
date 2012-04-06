-module(bael_fsm).
-behaviour(gen_fsm).
-export([start_link/0]).
-export([init/1, handle_event/3, handle_sync_event/4, handle_info/3,
	terminate/3, code_change/4]).
-export([sleep/2]).

start_link()->
	gen_fsm:start_link(?MODULE, [], []).

init([])->
	{ok, sleep, {}}.

handle_event(_Event, _StateName, StateData)->
	{next_state, sleep, StateData}.

handle_sync_event(_Event, _From, _StateName, StateData)->
	%{next_state, sleep, StateData}.
	{reply, null, sleep, StateData}.

handle_info(_Info, _StateName, StateData)->
	{next_state, sleep, StateData}.

terminate(_Reason, _StateName, _StateData)->
	ok.

code_change(_OldVsn, _StateName, StateData, _Extra)->
	{ok, sleep, StateData}.

sleep(_Event, StateData)->
	{next_state, sleep, StateData}.
