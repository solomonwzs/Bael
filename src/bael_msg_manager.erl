-module(bael_msg_manager).
-behaviour(gen_fsm).
-include("bael.hrl").
-export([start_link/2]).
-export([init/1, handle_event/3, handle_sync_event/4, handle_info/3,
	terminate/3, code_change/4]).
-export([idle/2, work/2]).

start_link(FsmSupRef, GetMsgs)->
	gen_server:start_link({local, ?MODULE}, ?MODULE, 
		[FsmSupRef, GetMsgs], []).

init([FsmSupRef, {M, F}])->
	{ok, idle, #msg_manager_state{
		fsm_sup_ref=FsmSupRef,
		get_msgs={M, F}
	}}.

handle_event(_Event, StateName, StateData)->
	{next_state, StateName, StateData}.

handle_sync_event(_Event, _From, StateName, StateData)->
	%{next_state, idle, StateData}.
	{reply, null, StateName, StateData}.

handle_info(_Info, StateName, StateData)->
	{next_state, StateName, StateData}.

terminate(_Reason, _StateName, _StateData)->
	ok.

code_change(_OldVsn, _StateName, StateData, _Extra)->
	{ok, idle, StateData}.

idle({start, Args}, StateData)->
	{next_state, work, StateData#msg_manager_state{
		get_msgs_args=Args
	}, 1}.

work(timeout, StateData)->
	{MsgList, NextArgs}=check_msg_list(StateData),
	case MsgList of
		[]->
			{next_state, idle, StateData#msg_manager_state{
				msg_list=[],
				get_msgs_args=null
			}};
		[Msg|Tail]->
			Pid=find_idle_fsm(StateData#msg_manager_state.fsm_sup_ref),
			gen_fsm:send_all_state_event(Pid, Msg),
			{next_state, work, StateData#msg_manager_state{
				msg_list=Tail,
				get_msgs_args=NextArgs
			}, 1}
	end.

check_msg_list(#msg_manager_state{
	msg_list=List,
	get_msgs={M, F},
	get_msgs_args=Args
})->
	case List of
		[]->
			M:F(Args);
		_->
			{List, Args}
	end.

find_idle_fsm(FsmSupRef)->
	Func=fun(X)->
		{_, Pid, _, _}=X,
		try
			State=gen_fsm:sync_send_all_event(Pid, get_state, 
				?FSM_BUSY_TIMEOUT),
			{State, Pid}
		catch
			_:_->{busy, Pid}
		end
	end,
	List=lists:foreach(Func, supervisor:which_children(FsmSupRef)),
	case proplists:get_all_values(idle, List) of
		[]->
			timer:sleep(?MSG_MANAGER_RETRY_TIME),
			find_idle_fsm(FsmSupRef);
		Res->
			hd(Res)
	end.
