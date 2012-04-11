-module(bael_fsm).
-behaviour(gen_fsm).
-export([start_link/0]).
-export([init/1, handle_event/3, handle_sync_event/4, handle_info/3,
	terminate/3, code_change/4]).
-export([idle/2, idle/3, work/2, work/3]).

start_link()->
	gen_fsm:start_link(?MODULE, [], []).

init([])->
	{ok, idle, {}}.

handle_event(test_timer, StateName, StateData)->
	io:format("handle send_all_state_event: ~p~n", [test_timer]),
	io:format("fsm(~p) state: ~p~n", [self(), StateName]),
	gen_fsm:start_timer(1000, hello),
	{next_state, idle, StateData};
handle_event(just_test, StateName, StateData)->
	io:format("handle send_all_state_event: ~p~n", [just_test]),
	io:format("fsm(~p) state: ~p~n", [self(), StateName]),
	{next_state, idle, StateData};
handle_event(_Event, _StateName, StateData)->
	{next_state, idle, StateData}.

handle_sync_event(just_test, From, StateName, StateData)->
	io:format("handle sync_send_all_state_event(from: ~p): ~p~n", 
	 [From, just_test]),
	io:format("fsm(~p) state: ~p~n", [self(), StateName]),
	{reply, {self(), reply}, idle, StateData, hibernate};
handle_sync_event(_Event, _From, _StateName, StateData)->
	%{next_state, idle, StateData}.
	{reply, null, idle, StateData}.

handle_info(_Info, _StateName, StateData)->
	{next_state, idle, StateData}.

terminate(_Reason, _StateName, _StateData)->
	ok.

code_change(_OldVsn, _StateName, StateData, _Extra)->
	{ok, idle, StateData}.

idle({timeout, _Ref, Msg}, StateData)->
	io:format("handle start_timer: ~p~n", [Msg]),
	io:format("fsm(~p) state: idle~n", [self()]),
	{next_state, idle, StateData};
idle(Event, StateData)->
	io:format("handle send_event: ~p~n", [Event]),
	io:format("fsm(~p) state: idle~n", [self()]),
	{next_state, idle, StateData}.

idle(Event, From, StateData)->
	io:format("handle sync_send_event(from ~p): ~p~n", [From, Event]),
	io:format("fsm(~p) state: idle~n", [self()]),
	{reply, {self(), reply}, idle, StateData}.

work(Event, StateData)->
	io:format("handle send_event: ~p~n", [Event]),
	io:format("fsm(~p) state: work~n", [self()]),
	{next_state, idle, StateData}.

work(Event, From, StateData)->
	io:format("handle sync_send_event(from ~p): ~p~n", [From, Event]),
	io:format("fsm(~p) state: work~n", [self()]),
	{reply, {self(), reply}, idle, StateData}.
