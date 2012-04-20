-module(bael_msg_manager).
-behaviour(gen_fsm).
-include("bael.hrl").
-export([start_link/3]).
-export([init/1, handle_event/3, handle_sync_event/4, handle_info/3,
	terminate/3, code_change/4]).
-export([idle/2]).

start_link(FsmSupRef, GetMsgs, InitArgs)->
	gen_server:start_link({local, ?MODULE}, ?MODULE, 
	 [FsmSupRef, GetMsgs, InitArgs], []).

init([FsmSupRef, {M, F}, InitArgs])->
	{Msgs, NextArgs}=M:F(InitArgs),
	{ok, idle, #msg_manager_state{
		fsm_sup_ref=FsmSupRef,
		get_msgs={M, F},
		get_msgs_args=NextArgs,
		msg_list=Msgs
	}}.

handle_event(_Event, _StateName, StateData)->
	{next_state, idle, StateData}.

handle_sync_event(_Event, _From, _StateName, StateData)->
	%{next_state, idle, StateData}.
	{reply, null, idle, StateData}.

handle_info(_Info, _StateName, StateData)->
	{next_state, idle, StateData}.

terminate(_Reason, _StateName, _StateData)->
	ok.

code_change(_OldVsn, _StateName, StateData, _Extra)->
	{ok, idle, StateData}.

idle(start, StateData)->
	{next_state, idle, StateData}.

find_idle_fsm(FsmSupRef)->
	Func=fun(X)->
		try
			State=gen_fsm:sync_send_all_event(X, get_state, ?FSM_BUSY_TIMEOUT),
			{State, X}
		catch
			_:_->{busy, X}
		end
	end,
	hd(lists:foreach(Func, supervisor:which_children(FsmSupRef))).
