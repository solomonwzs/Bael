-module(bael_common_func).
-export([thing_to_binary/1, to_lower_case/1, time_string/0, time_string/1,
		string_split/2, thing_to_list/1, dict_html_encode/1, timestamp/0]).

thing_to_binary(Thing)->
	if
		is_number(Thing) orelse is_binary(Thing)->Thing;
		is_atom(Thing)->atom_to_binary(Thing, utf8);
		is_list(Thing)->list_to_binary(Thing);
		is_tuple(Thing)->list_to_binary(tuple_to_list(Thing));
		true-> <<>>
	end.

thing_to_list(Thing)->
	if
		is_list(Thing)->Thing;
		is_integer(Thing)->integer_to_list(Thing);
		is_float(Thing)->float_to_list(Thing);
		is_binary(Thing)->binary_to_list(Thing);
		is_tuple(Thing)->tuple_to_list(Thing);
		true->[]
	end.

to_lower_case(String)->
	F=fun(X)->
		if 
			65=<X andalso X=<90-> X+32;
			true->X
		end
	end,
	lists:map(F, String).

time_string()->
	time_string(now()).
time_string({MegaSecs, Secs, MicroSecs})->
	time_string(calendar:now_to_local_time({MegaSecs, Secs, MicroSecs}));
time_string(Time)->
	{{Year, Month, Day}, {Hour, Minute, Second}}=Time,
	lists:concat([Year, "-", Month, "-", Day, " ", Hour, ":", Minute, ":",
		Second]).

string_split([SplitChar], String)->
	Func=fun(T)->
		T=/=SplitChar
	end,
	{List1, List2}=lists:splitwith(Func, String),
	case List2 of
		[]->
			[List1];
		[_|Tail]->
			[List1|string_split([SplitChar], Tail)]
	end.

dict_html_encode(Dict)->
	Func0=fun(X)->
		String=thing_to_list(X),
		[mochiweb_html:escape(String)]
	end,
	Func1=fun(Key, D)->
		dict:update(Key, Func0, D)
	end,
	lists:foldl(Func1, Dict, dict:fetch_keys(Dict)).

timestamp()->
	{MegaSecs, Secs, MicroSecs}=now(),
	MegaSecs*1000000000000+Secs*1000000+MicroSecs.
