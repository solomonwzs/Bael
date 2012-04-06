-module(bael_struct).
-export([get_value/2, set_value/3]).

get_value(_, undefined)->
	undefined;
get_value([], Value)->
	Value;
get_value([Head|Tail], Struct)->
	try
		{struct, ValueList}=Struct,
		Key=thing_to_binary(Head),
		Value=proplists:get_value(Key, ValueList),
		get_value(Tail, Value)
	catch
		Type:What->
			Report=["get struct value failed",
			 {key, Head},
		 	 {type, Type},
		 	 {what, What},
		 	 {trace, erlang:get_stacktrace()}],
		 	error_logger:error_report(Report),
			Type(get_struct_value_failed)
	end.

set_value([Head|Tail], NewValue, Struct)->
	try
		{struct, ValueList}=Struct,
		Key=thing_to_binary(Head),
		Value=proplists:get_value(Key, ValueList),
		if
			Value=:=undefined->throw("invalid_key");
			Tail=:=[]->
				{struct, lists:append(proplists:delete(Key, ValueList),
				 [{Key, thing_to_binary(NewValue)}])};
			length(Tail)>0->
				{struct, lists:append(proplists:delete(Key, ValueList),
				 [{Key, set_value(Tail, thing_to_binary(NewValue), Value)}])}
	 	end
	catch
		Type:What->
			Report=["set struct value failed",
			 {key, Head},
			 {value, NewValue},
		 	 {type, Type},
		 	 {what, What},
		 	 {trace, erlang:get_stacktrace()}],
		 	error_logger:error_report(Report),
			Type(set_struct_value_failed)
	end.

thing_to_binary(P)->
	if
		is_binary(P) orelse is_number(P)->P;
		is_atom(P)->atom_to_binary(P, utf8);
		is_list(P)->list_to_binary(P)
	end.
