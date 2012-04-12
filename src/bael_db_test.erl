-module(bael_db_test).
-export([db_test/0]).
-export([odbc_write_test/2, odbc_read_test/2, odbc_update_test/2]).
-export([emysql_write_test/1, emysql_read_test/1, emysql_update_test/1]).
-include("bael_mysql.hrl").
-define(MAX_TABLE_ROWS, 100).
-define(CREATE_TEST_TABLE, "
	CREATE TABLE `bael_test` (
		`id` INT(10) NOT NULL AUTO_INCREMENT,
		`name` VARCHAR(128) NOT NULL DEFAULT '0',
		`pid` VARCHAR(128) NOT NULL DEFAULT '0',
		PRIMARY KEY (`id`)
	)
	COLLATE='utf8_general_ci'
	ENGINE=InnoDB;").

db_test()->
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
	case Num of
		0->finish;
		_->
			Sql=lists:concat([
				"insert into bael_test(name, pid) value('",
					"odbc_", Num, "', '", pid_to_list(Conn), "'
				)"
		 	]),
			odbc:sql_query(Conn, Sql),
			odbc_write_test(Conn, Num-1)
	end.

emysql_write_test(Num)->
	case Num of
		0->finish;
		_->
			Sql=lists:concat([
				"insert into bael_test(name, pid) value('",
					"emysql_", Num, "', '", pid_to_list(self()), "'
				)"
		 	]),
			emysql:execute(db_test, list_to_binary(Sql)),
			emysql_write_test(Num-1)
	end.

odbc_read_test(Conn, Num)->
	case Num of
		0->finish;
		_->
			Sql=lists:concat([
				"select * from bael_test 
					where name='", "odbc_", Num, "'"
			]),
			odbc:sql_query(Conn, Sql),
			odbc_read_test(Conn, Num-1)
	end.

emysql_read_test(Num)->
	case Num of
		0->finish;
		_->
			Sql=lists:concat([
				"select * from bael_test 
					where name='", "emysql_", Num, "'"
			]),
			emysql:execute(db_test, list_to_binary(Sql)),
			emysql_read_test(Num-1)
	end.

odbc_update_test(Conn, Num)->
	case Num of
		0->finish;
		_->
			Sql=lists:concat([
				"update bael_test set
					pid=concat(pid, '-update')
				where name='odbc_", Num, "'"
			]),
			odbc:sql_query(Conn, Sql),
			odbc_update_test(Conn, Num-1)
	end.

emysql_update_test(Num)->
	case Num of
		0->finish;
		_->
			Sql=lists:concat([
				"update bael_test set
					pid=concat(pid, '-update')
				where name='emysql_", Num, "'"
			]),
			emysql:execute(db_test, list_to_binary(Sql)),
			emysql_update_test(Num-1)
	end.
