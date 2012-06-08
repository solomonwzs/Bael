-module(bael_db_test).
-export([mysql_test/0, mnesia_test/0, cassandra_test/0, cassandra_test/2,
		mongodb_test/0, redis_test/0]).
-export([odbc_write_test/2, odbc_read_test/2, odbc_update_test/2]).
-export([emysql_write_test/1, emysql_read_test/1, emysql_update_test/1]).
-export([mnesia_write_test/1, mnesia_read_test/1]).
-export([cassandra_write_test/2, cassandra_read_test/2]).
-export([mongodb_write_test/2, mongodb_read_test/2]).
-export([redis_write_test/2, redis_read_test/2]).
-include("bael_mysql.hrl").
-include("./deps/cassandra_thrift/include/cassandra_types.hrl").
-define(MAX_TABLE_ROWS, 1000).
-define(CREATE_TEST_TABLE, "
	CREATE TABLE `bael_test` (
		`id` INT(10) NOT NULL AUTO_INCREMENT,
		`name` VARCHAR(128) NOT NULL DEFAULT '0',
		`pid` VARCHAR(128) NOT NULL DEFAULT '0',
		PRIMARY KEY (`id`)
	)
	COLLATE='utf8_general_ci'
	ENGINE=InnoDB;").
-record(test_record,{
	id, name, pid
}).

redis_test()->
	{ok, Client}=eredis:start_link(),
	io:format("redis test start...~n"),
	{T0, _}=timer:tc(?MODULE, redis_write_test, [Client, ?MAX_TABLE_ROWS]),
	io:format("write times:~p, case time:~p~n", [?MAX_TABLE_ROWS, T0]),
	{T1, _}=timer:tc(?MODULE, redis_read_test, [Client, ?MAX_TABLE_ROWS]),
	io:format("read times:~p, case time:~p~n", [?MAX_TABLE_ROWS, T1]),
	io:format("redis test finish.~n"),
	eredis:stop(Client).

redis_write_test(_, 0)->
	ok;
redis_write_test(Client, Num)->
	eredis:q(Client, ["SET", lists:concat(["key_", Num]), Num]),
	redis_write_test(Client, Num-1).

redis_read_test(_, 0)->
	ok;
redis_read_test(Client, Num)->
	eredis:q(Client, ["GET", lists:concat(["key_", Num])]),
	redis_read_test(Client, Num-1).

mongodb_test()->
	{ok, Conn}=mongo:connect({localhost, 27017}),
	mongo:do(safe, master, Conn, db_test, 
		fun()->
			mongo:delete(thing, {})
		end),
	io:format("mongodb test start...~n"),
	{T0, _}=timer:tc(?MODULE, mongodb_write_test, [Conn, ?MAX_TABLE_ROWS]),
	io:format("write times:~p, case time:~p~n", [?MAX_TABLE_ROWS, T0]),
	{T1, _}=timer:tc(?MODULE, mongodb_read_test, [Conn, ?MAX_TABLE_ROWS]),
	io:format("read times:~p, case time:~p~n", [?MAX_TABLE_ROWS, T1]),
	io:format("mongodb test finish.~n"),
	mongo:disconnect(Conn).

mongodb_write_test(_, 0)->
	ok;
mongodb_write_test(Conn, Num)->
	mongo:do(safe, master, Conn, db_test,
		fun()->
			mongo:save(thing, 
				{id, Num, name, lists:concat(["mongo_", Num]), 
					ip_start, Num*10+1, ip_end, (Num+1)*10})
		end),
	mongodb_write_test(Conn, Num-1).

mongodb_read_test(_, 0)->
	ok;
mongodb_read_test(Conn, Num)->
	mongo:do(safe, master, Conn, db_test,
		fun()->
			mongo:find_one(thing,
				%{id, Num})
				{ip_start, {'$lt', Num*10+2}, 
				 ip_end, {'$gt', Num*10+2}})
		end),
	mongodb_read_test(Conn, Num-1).

cassandra_write_test(_, 0)->
	ok;
cassandra_write_test(TC, Num)->
	case Num rem 10000 of
		0->io:format("~p...~n", [Num]);
		_->ok
	end,
	P=integer_to_list(Num),
	cassandra_write_test(TC, P, P),
	cassandra_write_test(TC, Num-1).

cassandra_write_test(TC, Key, Value)->
	%thrift_client:call(TC, set_keyspace, ["demo"]),
	thrift_client:call(TC, insert, [
			Key,
			#columnParent{column_family="Users"},
			#column{
				name="name",
				value=Value,
				timestamp=bael_common_func:timestamp()}, 
			?cassandra_ConsistencyLevel_ONE]).

cassandra_read_test(_, 0)->
	ok;
cassandra_read_test(TC, Num) when is_number(Num)->
	case Num rem 10000 of
		0->io:format("~p...~n", [Num]);
		_->ok
	end,
	P=integer_to_list(Num),
	cassandra_read_test(TC, P),
	cassandra_read_test(TC, Num-1);
cassandra_read_test(TC, Key) when is_list(Key)->
	%thrift_client:call(TC, set_keyspace, ["demo"]),
	thrift_client:call(TC, get, [Key, #columnPath{
				column_family="Users",
				column="name"}, ?cassandra_ConsistencyLevel_ONE]).

cassandra_test()->
	cassandra_test("192.168.1.50", 9160).

cassandra_test(Host, Port)->
	{ok, TC}=thrift_client_util:new(Host, Port, cassandra_thrift,[
			{strict_read, true},
			{strict_write, true},
			{framed, true}
		]),
	thrift_client:call(TC, set_keyspace, ["demo"]),
	io:format("cassandra test start...~n"),
	{T0, _}=timer:tc(?MODULE, cassandra_write_test, [TC, ?MAX_TABLE_ROWS]),
	io:format("write times:~p, case time:~p~n", [?MAX_TABLE_ROWS, T0]),
	{T1, _}=timer:tc(?MODULE, cassandra_read_test, [TC, ?MAX_TABLE_ROWS]),
	io:format("read times:~p, case time:~p~n", [?MAX_TABLE_ROWS, T1]),
	io:format("cassandra test finish.~n").

mysql_test()->
	{ok, Conn}=odbc:connect("DSN=bael_local", []),
	{selected, _, [{Count}]}=odbc:sql_query(Conn, lists:concat(["
		select count(*) from 
			`INFORMATION_SCHEMA`.`TABLES` 
			where `TABLE_SCHEMA`='", ?DB_DEFAULT_DB ,"' 
				and `TABLE_NAME`='bael_test'"])),
	if
		Count=/="0"->
			odbc:sql_query(Conn, "drop table bael_test");
		true->do_nothing
	end,
	odbc:sql_query(Conn, ?CREATE_TEST_TABLE),
	io:format("odbc test start...~n"),
	{T0, _}=timer:tc(?MODULE, odbc_write_test, [Conn, ?MAX_TABLE_ROWS]),
	io:format("write times:~p, case time:~p~n", [?MAX_TABLE_ROWS, T0]),
	{T1, _}=timer:tc(?MODULE, odbc_read_test, [Conn, ?MAX_TABLE_ROWS]),
	io:format("read times:~p, case time:~p~n", [?MAX_TABLE_ROWS, T1]),
	{T2, _}=timer:tc(?MODULE, odbc_update_test, [Conn, ?MAX_TABLE_ROWS]),
	io:format("update times:~p, case time:~p~n", [?MAX_TABLE_ROWS, T2]),
	io:format("odbc test finish.~n"),
	odbc:sql_query(Conn, "drop table bael_test"),
	odbc:sql_query(Conn, ?CREATE_TEST_TABLE),
	io:format("emysql test start...~n"),
	{T3, _}=timer:tc(?MODULE, emysql_write_test, [?MAX_TABLE_ROWS]),
	io:format("write times:~p, case time:~p~n", [?MAX_TABLE_ROWS, T3]),
	{T4, _}=timer:tc(?MODULE, emysql_read_test, [?MAX_TABLE_ROWS]),
	io:format("read times:~p, case time:~p~n", [?MAX_TABLE_ROWS, T4]),
	{T5, _}=timer:tc(?MODULE, emysql_update_test, [?MAX_TABLE_ROWS]),
	io:format("update times:~p, case time:~p~n", [?MAX_TABLE_ROWS, T5]),
	io:format("emysql test finish.~n"),
	odbc:disconnect(Conn).

odbc_write_test(Conn, Num)->
	F=fun(N)->
		Sql=lists:concat([
			"insert into bael_test(name, pid) value('",
				"odbc_", N, "', '", pid_to_list(Conn), "'
			)"
		]),
		odbc:sql_query(Conn, Sql)
	end,
	lists:foreach(F, lists:seq(1, Num)).

emysql_write_test(Num)->
	F=fun(N)->
		Sql=lists:concat([
			"insert into bael_test(name, pid) value('",
				"emysql_", N, "', '", pid_to_list(self()), "'
			)"
		]),
		emysql:execute(db_test, Sql)
	end,
	lists:foreach(F, lists:seq(1, Num)).

odbc_read_test(Conn, Num)->
	F=fun(N)->
		Sql=lists:concat([
			"select * from bael_test 
				where name='", "odbc_", N, "'"
		]),
		odbc:sql_query(Conn, Sql)
	end,
	lists:foreach(F, lists:seq(1, Num)).

emysql_read_test(Num)->
	F=fun(N)->
		Sql=lists:concat([
			"select * from bael_test 
				where name='", "emysql_", N, "'"
		]),
		emysql:execute(db_test, Sql)
	end,
	lists:foreach(F, lists:seq(1, Num)).

odbc_update_test(Conn, Num)->
	F=fun(N)->
		Sql=lists:concat([
			"update bael_test set
				pid=concat(pid, '-update')
			where name='odbc_", N, "'"
		]),
		odbc:sql_query(Conn, Sql)
	end,
	lists:foreach(F, lists:seq(1, Num)).

emysql_update_test(Num)->
	F=fun(N)->
		Sql=lists:concat([
			"update bael_test set
				pid=concat(pid, '-update')
			where name='emysql_", N, "'"
		]),
		emysql:execute(db_test, Sql)
	end,
	lists:foreach(F, lists:seq(1, Num)).

mnesia_test()->
	mnesia:delete_table(db_test_mnesia),
	Res=mnesia:create_table(db_test_mnesia, [
		{disc_only_copies, [node()]},
		{attributes, record_info(fields, test_record)},
		{record_name, test_record}
	]),
	io:format("mnesia test start...~p~n", [Res]),
	{T0, _}=timer:tc(?MODULE, mnesia_write_test, [?MAX_TABLE_ROWS]),
	io:format("write times:~p, case time:~p~n", [?MAX_TABLE_ROWS, T0]),
	{T1, _}=timer:tc(?MODULE, mnesia_read_test, [?MAX_TABLE_ROWS]),
	io:format("read times:~p, case time:~p~n", [?MAX_TABLE_ROWS, T1]),
	io:format("mnesia test finish.~n").

mnesia_write_test(Num)->
	F=fun(N)->
		Name=lists:concat(["mnesia_", N]),
		R=#test_record{
			id=Name,
			name=Name,
			pid=self()
		},
		mnesia:dirty_write(db_test_mnesia, R)
	end,
	lists:foreach(F, lists:seq(1, Num)).

mnesia_read_test(Num)->
	F=fun(N)->
%		mnesia:dirty_select(db_test_mnesia, [{
%			#test_record{id='$1', _='_'},
%			[{'==', '$1', lists:concat(["mnesia_", N])}],
%			['$_']
%		}])
		mnesia:dirty_read(db_test_mnesia, lists:concat(["mnesia_", N]))
	end,
	lists:foreach(F, lists:seq(1, Num)).
