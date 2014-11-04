title: Ejabberd与Emysql集成
categories:
  - Communication
  - Ejabberd
tags:
  - ejabberd
  - emysql
toc: false
date: 2014-10-08 11:53:17
---
![][1]
开发模块需要使用到MySQL数据库,本文描述如何把`Emysql`集成到Ejabberd中.

## 修改Ejabberd

编辑`$EJABBERD_SRC/rebar.config.script`

配置`Deps`,增加`Emysql`依赖

```shell 配置rebar.config.script
Deps = [
    ...
    {emysql, ".*", {git, "git://github.com/Eonblast/Emysql.git"}}
],
```

如果已经编译过了ejabberd,请删除`deps/.built`, `deps/.got`两个文件, 并执行`make`

```shell 编译输出
root@fd4cc081e295:~/ejabberd# make
...
==> emysql (compile)
Generating "include/crypto_compat.hrl" ...
...supports cryto:hash/2
...writing "include/crypto_compat.hrl"
Compiled src/emysql.erl
Compiled src/emysql_worker.erl
Compiled src/emysql_conn_mgr.erl
Compiled src/emysql_sup.erl
Compiled src/emysql_util.erl
Compiled src/emysql_conn.erl
Compiled src/emysql_app.erl
Compiled src/emysql_statements.erl
Compiled src/emysql_auth.erl
Compiled src/emysql_conv.erl
Compiled src/emysql_tcp.erl
==> p1_mysql (compile)
==> p1_zlib (compile)
==> jiffy (compile)
==> goldrush (compile)
==> lager (compile)
==> p1_iconv (compile)
==> rel (compile)
==> ejabberd (compile)
Compiled src/mod_gbox_messager.erl
Compiled src/mod_online_users.erl
Compiled src/mod_cputime.erl
Compiled src/mod_system_information.erl
/usr/lib/erlang/bin/escript rebar skip_deps=true compile
==> rel (compile)
==> ejabberd (compile)
```

## 下载安装samples数据库

```shell 下载安装samples数据库
wget https://launchpad.net/test-db/employees-db-1/1.0.6/+download/employees_db-full-1.0.6.tar.bz2
tar jxf employees_db-full-1.0.6.tar.bz2
cd employees_db
mysql -t < employees.sql
```

## 修改模块

```erlang 获取CPU时间模块 https://gist.github.com/developerworks/77c3954e8622550f6c71 mod_cputime.erl
-module(mod_cputime).
-behaviour(gen_mod).
-export([
    start/2,
    stop/1,
    process_local_iq/3
]).
-include("ejabberd.hrl").
-include("jlib.hrl").
-include("logger.hrl").
-define(JUD_MATCHES, macro_body).
-define(NS_CPUTIME, <<"ejabberd:cputime">>).
start(Host, Opts) ->
    IQDisc = gen_mod:get_opt(
        iqdisc,
        Opts,
        fun gen_iq_handler:check_type/1,
        one_queue
    ),
    mod_disco:register_feature(Host, ?NS_CPUTIME),
    gen_iq_handler:add_iq_handler(ejabberd_local, Host, ?NS_CPUTIME, ?MODULE, process_local_iq, IQDisc),
    crypto:start(),
    application:start(emysql),
    Server = gen_mod:get_module_opt(Host,
        ?MODULE, server, fun(Server) -> binary_to_list(Server) end, "localhost"),
    Port = gen_mod:get_module_opt(Host,
        ?MODULE, port, fun(Port) -> Port end, 3306),
    Database = gen_mod:get_module_opt(Host,
        ?MODULE, database, fun(Database) -> binary_to_list(Database) end, "mysql"),
    User = gen_mod:get_module_opt(Host,
        ?MODULE, user, fun(User) -> User end, "root"),
    Password = gen_mod:get_module_opt(Host,
        ?MODULE, password, fun(Password) -> binary_to_list(Password) end, "root"),
    PoolSize = gen_mod:get_module_opt(Host,
        ?MODULE, poolsize, fun(PoolSize) -> PoolSize end, 1),
    Encoding = gen_mod:get_module_opt(Host,
        ?MODULE, encoding, fun(Encoding) -> list_to_atom(binary_to_list(Encoding)) end, utf8),
    ?DEBUG("MySQL Server: ~p~n", [Server]),
    ?DEBUG("MySQL Port: ~p~n", [Port]),
    ?DEBUG("MySQL DB: ~p~n", [Database]),
    ?DEBUG("MySQL User: ~p~n", [User]),
    ?DEBUG("MySQL Password: ~p~n", [Password]),
    ?DEBUG("MySQL PoolSize: ~p~n", [PoolSize]),
    ?DEBUG("MySQL Encoding: ~p~n", [Encoding]),
    emysql:add_pool(pool_employees, [
        {size, PoolSize},
        {user, User},
        {password, Password},
        {host, Server},
        {port, Port},
        {database, Database},
        {encoding, Encoding}
    ]),
    {_, _, _, Result, _} = emysql:execute(pool_employees, <<"SELECT * FROM employees LIMIT 10">>),
%%     [
%%         [10001,{date,{1953,9,2}},<<"Georgi">>,<<"Facello">>,<<"M">>,{date,{1986,6,26}}],
%%         [10002,{date,{1964,6,2}},<<"Bezalel">>,<<"Simmel">>,<<"F">>,{date,{1985,11,21}}],
%%         [10003,{date,{1959,12,3}},<<"Parto">>,<<"Bamford">>,<<"M">>,{date,{1986,8,28}}],
%%         [10004,{date,{1954,5,1}},<<"Chirstian">>,<<"Koblick">>,<<"M">>,{date,{1986,12,1}}],
%%         [10005,{date,{1955,1,21}},<<"Kyoichi">>,<<"Maliniak">>,<<"M">>,{date,{1989,9,12}}],
%%         [10006,{date,{1953,4,20}},<<"Anneke">>,<<"Preusig">>,<<"F">>,{date,{1989,6,2}}],
%%         [10007,{date,{1957,5,23}},<<"Tzvetan">>,<<"Zielinski">>,<<"F">>,{date,{1989,2,10}}],
%%         [10008,{date,{1958,2,19}},<<"Saniya">>,<<"Kalloufi">>,<<"M">>,{date,{1994,9,15}}],
%%         [10009,{date,{1952,4,19}},<<"Sumant">>,<<"Peac">>,<<"F">>,{date,{1985,2,18}}],
%%         [10010,{date,{1963,6,1}},<<"Duangkaew">>,<<"Piveteau">>,<<"F">>,{date,{1989,8,24}}]
%%     ].
    ?DEBUG("============================~n~p~n", [Result]),
    ok.
stop(Host) ->
    mod_disco:unregister_feature(Host, ?NS_CPUTIME),
    gen_iq_handler:remove_iq_handler(ejabberd_local, Host, ?NS_CPUTIME),
    ok.
process_local_iq(_From, _To, #iq{type = Type, sub_el = SubEl} = IQ) ->
    case Type of
        set ->
            IQ#iq{type = error, sub_el = [SubEl, ?ERR_NOT_ALLOWED]};
        get ->
            CPUTime = element(1, erlang:statistics(runtime)) / 1000,
            SCPUTime = lists:flatten(io_lib:format("~.3f", [CPUTime])),
            Packet = IQ#iq{type = result, sub_el = [
                #xmlel{name = <<"query">>, attrs = [{<<"xmlns">>, ?NS_CPUTIME}], children = [
                    #xmlel{name = <<"time">>, attrs = [], children = [{xmlcdata, list_to_binary(SCPUTime)}]}]}
            ]},
            Packet
    end.
```

查询结果日志输出

![查询结果日志输出][2]

## 参考资料

1. http://dev.mysql.com/doc/employee/en/employees-installation.html

  [1]: /assets/images/754522BD-38E0-40B9-9C1D-07E7988BDEC0.png
  [2]: /assets/images/0D78C5EA-8892-437F-B6FE-1F3E19404AE6.png