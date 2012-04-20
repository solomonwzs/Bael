-define(HTTP_PORT, 8083).
-define(MAX_SERVERS_POOL_SIZE, 10).
-define(MAX_FSMS_POOL_SIZE, 10).
-define(MAX_MSG_POOL_SIZE, 100).
-define(FSM_BUSY_TIMEOUT, 1000).
-define(URLS, [
	{"^hello/?$", hello},
	{"^hello/(.+?)/?$", hello},
	{"^test/?$", test},
	{"^resoure/(.+?)/?$", resoure},
	{"^ajax_test/?$", ajax_test}
]).

-record(xml_element, {
	tag_name=undefined,
	parents=[],
	attr=[],
	value= <<>>
}).

-record(worker_state, {
	pid=self()
}).

-record(fsm_state, {
	pid,
	state
}).

-record(msg_manager_state,{
	fsm_sup_ref,
	msg_num=0,
	get_msgs,%{M, F}
	get_msgs_args,
	msg_list=[]
}).

-record(error_message, {
	type=undefined,
	what=undefined,
	trace=[]
}).
