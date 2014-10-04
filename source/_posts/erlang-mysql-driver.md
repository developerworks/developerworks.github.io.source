title: Erlang MySQL驱动
categories:
  - Erlang
tags:
  - mysql
toc: false
date: 2014-10-02 03:23:39
---


## 编译

```
git clone git://github.com/dizzyd/erlang-mysql-driver.git erlang-mysql-driver.git
cd erlang-mysql-driver
wget -O ./build.sh http://svn.process-one.net/ejabberd-modules/mysql/trunk/build.sh
chmod +x ./build.sh
./build.sh
```

![编译erlang-mysql-driver][1]



## Examples

```
%% Author: Administrator
%% Created: 2011-5-15
%% Description: TODO: Add description to test
-module(test).
%%
%% Include files
%%
%%
%% Exported Functions
%%
-export([start/0]).
%%
%% API Functions
%%
%%
%% Local Functions
%%
start() ->
%% Build connection
%% 注意最后一个参数,utf8,支持中文必杀技.
    mysql:start_link(connection,"localhost", 3306,"root", "root", "erlang", undefined, utf8),
%% INSERT
    _result = mysql:fetch(connection, [<<"INSERT INTO test_table (id, first_name, last_name) VALUES(default, '你好', '世界')">>]),
    io:format("Result1: ~p~n", [_result]),
%% SELECT
    _result2 = mysql:fetch(connection, [<<"SELECT * FROM test_table;">>]),
    io:format("Result2: ~p~n", [_result2]),
%% UPDATE
    _result3 = mysql:fetch(connection, [<<"UPDATE test_table SET first_name = 'River' WHERE first_name = 'ZHIQIANG' AND last_name = 'HE';">>]),
    io:format("Result3: ~p~n", [_result3]),
%% SELECT
    _result4 = mysql:fetch(connection, [<<"SELECT * FROM test_table;">>]),
    io:format("Result4: ~p~n", [_result4]),
%% DELETE
    _result5 = mysql:fetch(connection, [<<"DELETE FROM test_table WHERE last_name = 'HE' AND first_name = 'River';">>]),
    io:format("Result5: ~p~n", [_result5]),
%% SELECT
    _result6 = mysql:fetch(connection, [<<"SELECT * FROM test_table;">>]),
    io:format("Result6: ~p~n", [_result6]),
    ok.
```



  [1]: /assets/images/build-erlang-mysql-driver.png