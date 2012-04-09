-define(HTTP_PORT, 8083).
-define(MAX_SERVERS_POOL_SIZE, 10).
-define(MAX_FSMS_POOL_SIZE, 10).
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

-record(error_message, {
	type=undefined,
	what=undefined,
	trace=[]
}).
