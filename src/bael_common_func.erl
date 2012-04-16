-module(bael_common_func).
-export([thing_to_binary/1, to_lower_case/1]).

thing_to_binary(Thing)->
	if
		is_number(Thing) orelse is_binary(Thing)->Thing;
		is_atom(Thing)->atom_to_binary(Thing, utf8);
		is_list(Thing)->list_to_binary(Thing);
		is_tuple(Thing)->list_to_binary(tuple_to_list(Thing));
		true-> <<>>
	end.

to_lower_case(String)->
	F=fun(X)->
		if 
			65=<X andalso X=<90-> X+32;
			true->X
		end
	end,
	lists:map(F, String).
