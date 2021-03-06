-module(bael_views).
-include("bael_mysql.hrl").
-export([table/2, hello/2, hello/3, ajax_test/2, base/2]).

base('GET', Req)->
	bael_shortcuts:render_ok(Req, bael_extends_dtl, []).

ajax_test('POST', Req)->
	PostData=Req:parse_post(),
	Json=proplists:get_value("json", PostData),
	%Struct=mochijson2:decode(Json),
	%NewStruct=bael_struct:set_value([person, name], jack, Struct),
	%io:format("~p~n~p~n", [Struct, Json]),
	Req:ok({"application/json", [], [Json]}). 

table('GET', Req)->
	QueryStringData=Req:parse_qs(),
	TableName=proplists:get_value("tablename", QueryStringData, 
	 ?DB_DEFAULT_TABLE),
	Page=list_to_integer(proplists:get_value("page", QueryStringData, "1")),
	Num=list_to_integer(proplists:get_value("num", QueryStringData, "50")),
	DataTable=emysql:execute(db_test, 
		list_to_binary(lists:concat([
	  		"select * from ", TableName, " limit ", (Page-1)*Num, ",", Num]))),
	handle_data_table(Req, DataTable).

hello('GET', Req)->
	QueryStringData=Req:parse_qs(),
	Username=proplists:get_value("username", QueryStringData, "Anonymous"),
	bael_shortcuts:render_ok(Req, bael_test_dtl, [{username, Username}]);
hello('POST', Req)->
	PostData=Req:parse_post(),
	Username=proplists:get_value("username", PostData, "Anonymous"),
	bael_shortcuts:render_ok(Req, bael_test_dtl, [{username, Username}]).

hello('GET', Req, SubPath)->
	bael_shortcuts:render_ok(Req, bael_test_dtl, [{username, SubPath}]).

handle_data_table(Req, DataTable)->
	{_ResultPacket, _, FieldList, TableData, _}=DataTable,
	FieldNames=get_fields(FieldList),
	bael_shortcuts:render_ok(
		Req, 
		bael_table_dtl, 
		[{field_names, FieldNames}, {table_data, TableData}]
	).
get_fields([])->
	[];
get_fields([Head|Tail])->
	List=tuple_to_list(Head),
	FieldName=lists:nth(7, List),
	[FieldName|get_fields(Tail)].
