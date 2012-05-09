-module(bael_msg_server).
-behaviour(gen_server).
-include("bael.hrl").
-export([start_link/2, start_link/3]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	code_change/3, terminate/2]).

start_link(FsmSupRef, GetMsgs)->
	gen_server:start_link(?MODULE, [FsmSupRef, GetMsgs], []).

start_link(Name, FsmSupRef, GetMsgs)->
	gen_server:start_link({local, Name}, ?MODULE, [FsmSupRef, GetMsgs], []).

init([FsmSupRef, {M, F}])->
	{ok, #msg_server_state{
			fsm_sup_ref=FsmSupRef,
			get_msgs={M, F}
		}}.

handle_call(get_msg, _From, State)->
	{MsgList, NextArgs}=check_msg_list(State),
	if
		MsgList=/=[]->
			[Msg|Tail]=MsgList,
			{reply, Msg, State#msg_server_state{
					get_msgs_args=NextArgs,
					msg_list=Tail
				}};
		true->
			{reply, no_msg, State#msg_server_state{
					get_msgs_args=undefined,
					msg_list=[]
				}}
	end;
handle_call({start_fsm, Args}, _From, State)->
	FsmSupRef=State#msg_server_state.fsm_sup_ref,
	FsmList=[Pid||{_, Pid, _, _}<-supervisor:which_children(FsmSupRef)],
	Func=fun(Pid)->
		Pid!{start, self()}
		%gen_fsm:send_event(Pid, {start, self()})
	end,
	Ret=lists:foreach(Func, FsmList),
	{reply, Ret, State#msg_server_state{get_msgs_args=Args}};
handle_call(msg_list, _From, State)->
	{reply, State#msg_server_state.msg_list, State};
handle_call(_Msg, _From, State)->
	{reply, reply, State}.

handle_cast(_Msg, State)->
	{noreply, State}.

handle_info(_Msg, State)->
	{noreply, State}.

code_change(_Vsn, State, _Extra)->
	{ok, State}.

terminate(_Reason, _State)->
	ok.

check_msg_list(#msg_server_state{
	msg_list=List,
	get_msgs={M, F},
	get_msgs_args=Args
})->
	case {List, Args} of
		{[], undefined}->
			{[], undefined};
		{[], _}->
			M:F(Args);
		_->
			{List, Args}
	end.
