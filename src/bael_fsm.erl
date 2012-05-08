-module(bael_fsm).
-behaviour(gen_fsm).
-include("bael.hrl").
-export([start_link/0, get_msg/1]).
-export([init/1, handle_event/3, handle_sync_event/4, handle_info/3,
		terminate/3, code_change/4]).
-export([idle/2, idle/3, working/2, working/3]).

start_link()->
	gen_fsm:start_link(?MODULE, [], []).

init([])->
	process_flag(trap_exit, true),
	write_ets_state_table(idle),
	{ok, idle, {}}.

handle_event(test_timer, StateName, StateData)->
	io:format("handle send_all_state_event: ~p~n", [test_timer]),
	io:format("fsm(~p) state: ~p~n", [self(), StateName]),
	gen_fsm:start_timer(3000, hello),
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
	{reply, {self(), reply}, StateName, StateData, hibernate};
handle_sync_event(get_state, _From, StateName, StateData)->
	{reply, StateName, StateName, StateData};
handle_sync_event(test_timeout, _From, StateName, StateData)->
	timer:sleep(2000),
	{reply, {self(), StateName}, StateName, StateData};
handle_sync_event(_Event, _From, StateName, StateData)->
	%{next_state, idle, StateData}.
	{reply, {self(), StateName}, StateName, StateData}.

handle_info({get_state, From}, StateName, StateData)->
	From!{self(), StateName, now()},
	io:format("handle info(from: ~p): ~p~n", [From, {get_state, From}]),
	{next_state, StateName, StateData}.
%handle_info(_Info, _StateName, StateData)->
%	{next_state, idle, StateData}.

terminate(_Reason, _StateName, _StateData)->
	io:format("fsm(~p) terminate: ~p~n", [self(), _Reason]),
	ok.

code_change(_OldVsn, _StateName, StateData, _Extra)->
	{ok, idle, StateData}.

idle({timeout, _Ref, Msg}, StateData)->
	io:format("handle start_timer: ~p~n", [Msg]),
	io:format("fsm(~p) state: idle~n", [self()]),
	{next_state, working, StateData, 5000};
idle({test_msg, Msg}, StateData)->
	timer:sleep(5000),
	emysql:execute(
		db_test,
		lists:concat([
			"insert into db (name) values ('", 
			Msg, 
			pid_to_list(self()),
			calendar:time_to_seconds(now()),
			"')"])),
	io:format("handle send_event: ~p~n", [{test_msg, Msg}]),
	io:format("fsm(~p) state: idle~n", [self()]),
	write_ets_state_table(idle),
	{next_state, idle, StateData};
idle(Event, StateData)->
	io:format("handle send_event: ~p~n", [Event]),
	io:format("fsm(~p) state: idle~n", [self()]),
	{next_state, idle, StateData}.

idle(Event, From, StateData)->
	io:format("handle sync_send_event(from ~p): ~p~n", [From, Event]),
	io:format("fsm(~p) state: idle~n", [self()]),
	{reply, {self(), reply}, idle, StateData}.

working(timeout, StateDate)->
	io:format("fsm(~p) finish work~n", [self()]),
	{next_state, idle, StateDate};
working(Event, StateData)->
	io:format("handle send_event: ~p~n", [Event]),
	io:format("fsm(~p) state: working~n", [self()]),
	{next_state, working, StateData}.

working(Event, From, StateData)->
	io:format("handle sync_send_event(from ~p): ~p~n", [From, Event]),
	io:format("fsm(~p) state: working~n", [self()]),
	{reply, {self(), reply}, working, StateData}.

get_msg({Page, Num})->
	{result_packet, _, _Fields, List, _}=emysql:execute(
		db_test,
		lists:concat([
			"select name from emysql_test limit ", 
			Page*Num, 
			", ",
			Num])),
	{[{test_msg, X}||X<-List], {Page+1, Num}}.

write_ets_state_table(StateName)->
	Pid=self(),
	ets:insert(ets_fsm_state, {Pid, 
			#fsm_state_info{
				pid=Pid,
				state=StateName,
				info=process_info(Pid)
			}}).
