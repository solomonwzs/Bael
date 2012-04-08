-module(bael_common_func).
-export([thing_to_binary/1]).

thing_to_binary(Thing)->
	if
		is_number(Thing) orelse is_binary(Thing)->Thing;
		is_list(Thing)->list_to_binary(Thing);
		is_tuple(Thing)->list_to_binary(tuple_to_list(Thing));
		true-> <<>>
	end.
