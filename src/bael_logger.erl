-module(bael_logger).
-export([logger_to_file/3]).

logger_to_file(Path, FormatStr, ValueList)->
	Trace=try
		throw('debug')
	catch
		_:_->erlang:get_stacktrace()
	end,
	{ok, F}=file:open(Path, write),
	io:format(F, lists:concat([FormatStr, "~n~p"]),
		lists:append(ValueList, [Trace])),
	file:close(F).
