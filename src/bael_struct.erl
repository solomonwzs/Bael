-module(bael_struct).
-export([get_value/2]).

get_value(_, undefined)->
	undefined;
get_value([], Value)->
	Value;
get_value([Head|Tail], {struct, ValueList})->
	try
		Key=if
			is_binary(Head)->Head;
			is_atom(Head)->atom_to_binary(Head, utf8);
			is_list(Head)->list_to_binary(Head)
		end,
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

set_value([Head|Tail], {struct, ValueList})->
	ok.
