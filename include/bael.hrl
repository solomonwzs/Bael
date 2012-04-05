-define(HTTP_PORT, 8083).
-define(MAX_WORKERS_POOL_SIZE, 10).
-define(URLS, [
	{"^hello/?$", hello},
	{"^hello/(.+?)/?$", hello},
	{"^test/?$", test},
	{"^resoure/(.+?)/?$", resoure},
	{"^ajax_test/?$", ajax_test}
]).

-record(bael_worker_state, {
		pid
	}).
