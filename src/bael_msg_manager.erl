-module(bael_msg_manager).
-behaviour(gen_fsm).
-include("bael.hrl").
-export([start_link/3, start_link/4, find_idle_fsm/1]).
-export([init/1, handle_event/3, handle_sync_event/4, handle_info/3,
	terminate/3, code_change/4]).
-export([idle/2, working/2]).

start_link(FsmSupRef, GetMsgs, EtsRef)->
	gen_fsm:start_link(?MODULE, [FsmSupRef, GetMsgs, EtsRef], []).

start_link(FsmName, FsmSupRef, GetMsgs, EtsRef)->
	gen_fsm:start_link({local, FsmName}, ?MODULE, 
		[FsmSupRef, GetMsgs, EtsRef], []).

init([FsmSupRef, {M, F}, EtsRef])->
	{ok, idle, #msg_manager_state{
		fsm_sup_ref=FsmSupRef,
		get_msgs={M, F},
		ets_fsm=EtsRef
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
	io:format("start(~p)~n", [now()]),
	gen_fsm:send_event(self(), send_msg),
	{next_state, working, StateData#msg_manager_state{
		get_msgs_args=Args
	}};
idle(_Event, StateData)->
	{next_state, idle, StateData}.

working(send_msg, StateData)->
	{MsgList, NextArgs}=check_msg_list(StateData),
	if
		MsgList=/=[]->
			case find_idle_fsm(StateData#msg_manager_state.ets_fsm) of
				[]->
					Timer=timer:apply_after(?MSG_WAIT_IDLE_FSM_TIME, 
						gen_fsm, send_event, [self(), send_msg]),
					{next_state, working, 
						StateData#msg_manager_state{timer_ref=Timer}};
				FsmList->
					send_msg(FsmList, MsgList, 
						StateData#msg_manager_state.ets_fsm),
					gen_fsm:send_event(self(), send_msg),
					{next_state, working, 
						StateData#msg_manager_state{
							get_msgs_args=NextArgs
						}}
			end;
		true->
			io:format("finish(~p)~n", [now()]),
			{next_state, idle, StateData#msg_manager_state{
					msg_list=[],
					get_msgs_args=null
				}}
	end;
working(_Event, StateData)->
	{next_state, working, StateData}.

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

find_idle_fsm(EtsRef)->
	try
		List=ets:match(EtsRef, {'$1', #fsm_state_info{state=idle, _='_'}}),
		[Pid||[Pid]<-List]
	catch
		_:_->[]
	end.

suspend_fsm(EtsRef, FsmPid)->
	ets:insert(EtsRef, {FsmPid, 
			#fsm_state_info{
				pid=FsmPid,
				state=busy,
				info=process_info(FsmPid)
			}}).

send_msg([], _MsgList, _EtsRef)->
	ok;
send_msg(_FsmList, [], _EtsRef)->
	ok;
send_msg(FsmList, MsgList, EtsRef)->
	[Fsm|Tail0]=FsmList,
	[Msg|Tail1]=MsgList,
	suspend_fsm(EtsRef, Fsm),
	gen_fsm:send_event(Fsm, Msg),
	send_msg(Tail0, Tail1, EtsRef).
