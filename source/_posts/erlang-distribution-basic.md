title: Erlang-分布式基础
categories:
  - Erlang
tags:
  - erlang
toc: false
date: 2011-05-31 03:32:00
---

2011-05-31 创建
2014-09-29 更新

## 知识点

- 具有相同cookie而且彼此互相连接的节点集称为Erlang 集群
- 设置Cookie的三种方式
    - `$HOME/.erlang.cookie`
    - `erl -setcookie AFRTY12ESS3412735ASDF12378`
    - `erlang:set_cookie(node(),C)`

## Example

```
-module(kvs).
%% Include files
%% Exported Functions
-export([start/0, store/2, lookup/1]).
%% API Functions
start() ->
    io:format("starting server...~n"),
    % 启动kvs服务,派生一个新进程,进入循环
    register(kvs, spawn(fun()-> loop() end)).
store(Key, Value) ->
    io:format("store~n"),
    rpc({store, Key, Value}).
lookup(Key) ->
    io:format("call lookup~n"),
    rpc({lookup, Key}).
% Local Functions
% 消息代理函数
rpc(Q) ->
    io:format("rpc~n"),
    % 向进程kvs发送消息 {self(), Q},其中self()表示当前进程Pid, Q为参数
    kvs ! {self(), Q},
    receive
        {kvs, Reply} ->
            Reply
    end.
loop() ->
    io:format("entering loop...~n"),
    receive
        {From, {store, Key, Value}} ->
            io:format("value ~p with ~p stored~n", [Value, Key]),
            put(Key, {ok,Value}),
            From ! {kvs, true},
            loop();
        {From, {lookup, Key}} ->
            io:format("get the value of key: ~p~n", [Key]),
            From ! {kvs, get(Key)},
            loop()
    end
```

## 操作步骤

- 启动两个终端, 分别输入

```
erl -sname server
erl -sname client
```

- 在真实的分布式环境中,请使用

```
erl -name server
erl -name client
```

`-name`需要使用到DNS服务,而`-sname`不需要使用DNS,可用于本机测试,局域网也可以使用`-sname`启动Erlang系统.

启动服务器, 派生一个新进程,立即进入循环

```
(server@localhost) > kvs:start().
starting server...
entering loop...
true
```

- 客户端通过RPC调用服务器函数 store 存储一个值

```
(client@localhost) > rpc:call('server@localhost',kvs,store,[google, "http://www.google.com"]).
```

- 服务器输出,再次进入loop等待下一个消息:

```
(server@localhost) > value "http://www.google.com" with google stored
(server@localhost) > loop
```

- 客户端查询

```
(client@localhost) > rpc:call('server@localhost',kvs,lookup,[google]).
call lookup
rpc
{ok,"http://www.google.com"}
```

- 服务器输出

```
(server@localhost) > get the value of key: google
(server@localhost) > loop
```

- 本地调用实际上也是通过rpc向kvs服务器发送消息

```
(server@localhost)2> kvs:lookup(google).
call lookup
rpc
get the value of key: google
loop
{ok,"http://www.google.com"}
```

## 消息传递图示

![消息传递图示][1]

## 结语

- 本文使用进程字典作为Key,Value存储仅作为示例,真实环境中是不可能的.切忌照搬.
- 可以把loop()函数中的put和get剥离到两个实现函数中
- 采用什么后端存储就是你的事情了.

  [1]: /assets/images/process-message-passing.png