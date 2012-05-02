-module(bael_msg_manager).
-behaviour(gen_fsm).
-include("bael.hrl").
-export([start_link/2, start_link/3, find_idle_fsm/1]).
-export([init/1, handle_event/3, handle_sync_event/4, handle_info/3,
	terminate/3, code_change/4]).
-export([idle/2, work/2]).

start_link(FsmSupRef, GetMsgs)->
	gen_fsm:start_link(?MODULE, [FsmSupRef, GetMsgs], []).

start_link(FsmName, FsmSupRef, GetMsgs)->
	gen_fsm:start_link({local, FsmName}, ?MODULE, 
		[FsmSupRef, GetMsgs], []).

init([FsmSupRef, {M, F}])->
	{ok, idle, #msg_manager_state{
		fsm_sup_ref=FsmSupRef,
		get_msgs={M, F}
	}}.

handle_event(_Event, StateName, StateData)->
	{next_state, StateName, StateData}.

handle_sync_event(get_state, _From, StateName, StateData)->
	{reply, StateName, StateName, StateData};
handle_sync_event(get_msg_list, _From, StateName, StateData)->
	{reply, StateData#msg_manager_state.msg_list, StateName, StateData};
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
	timer:apply_after(100, gen_fsm, send_event, [?MODULE, send_msg]),
	{next_state, work, StateData#msg_manager_state{
		get_msgs_args=Args
	}}.

%work(timeout, StateData)->
%	{MsgList, NextArgs}=check_msg_list(StateData),
%	case MsgList of
%		[]->
%			io:format("finish~n"),
%			{next_state, idle, StateData#msg_manager_state{
%				msg_list=[],
%				get_msgs_args=null
%			}};
%		[Msg|Tail]->
%			Pid=find_idle_fsm(StateData#msg_manager_state.fsm_sup_ref),
%			gen_fsm:send_event(Pid, Msg),
%			{next_state, work, StateData#msg_manager_state{
%				msg_list=Tail,
%				get_msgs_args=NextArgs
%			}, 500}
%	end.
work(send_msg, StateData)->
	{MsgList, NextArgs}=check_msg_list(StateData),
	bael_logger:logger_to_file("./log/log", "~p~n", [MsgList]),
	if 
		MsgList=/=[]->
			Func=fun(Msg)->
				Pid=find_idle_fsm(StateData#msg_manager_state.fsm_sup_ref),
				gen_fsm:send_event(Pid, Msg)
			end,
			lists:foreach(Func, MsgList),
			timer:apply_after(500, gen_fsm, send_event, [?MODULE, send_msg]),
			{next_state, work, StateData#msg_manager_state{
					msg_list=[],
					get_msgs_args=NextArgs
				}};
		true->
			io:format("finish(~p)~n", [now()]),
			{next_state, idle, StateData#msg_manager_state{
					msg_list=[],
					get_msgs_args=null
				}}
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

%find_idle_fsm(FsmSupRef)->
%	Func=fun(X)->
%		{_, Pid, _, _}=X,
%		try
%			State=gen_fsm:sync_send_all_state_event(Pid, get_state, 
%				?FSM_BUSY_TIMEOUT),
%			{State, Pid}
%		catch
%			_:_->{busy, Pid}
%		end
%	end,
%	List=lists:map(Func, supervisor:which_children(FsmSupRef)),
%	case proplists:get_all_values(idle, List) of
%		[]->
%			io:format("wait~n"),
%			timer:sleep(?MSG_MANAGER_RETRY_TIME),
%			find_idle_fsm(FsmSupRef);
%		Res->
%			hd(Res)
%	end.
find_idle_fsm([])->
	all_busy;
find_idle_fsm([Head|Tail])->
	try
		{_, Pid, _, _}=Head,
		idle=gen_fsm:sync_send_all_state_event(Pid, get_state, 
			?FSM_BUSY_TIMEOUT),
		Pid
	catch
		_:_->find_idle_fsm(Tail)
	end;
find_idle_fsm(FsmSupRef)->
	case find_idle_fsm(supervisor:which_children(FsmSupRef)) of
		all_busy->
			io:format("all busy, waiting...~n"),
			timer:sleep(?MSG_MANAGER_RETRY_TIME),
			find_idle_fsm(FsmSupRef);
		Pid->
			Pid
	end.
