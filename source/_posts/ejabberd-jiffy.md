title: 使用Jiffy输出JSON数据
categories:
  - Communication
tags:
  - ejabberd
  - jiffy
toc: true
date: 2014-09-28 13:05:43
---

## 编译

```
cd /tmp
git clone https://github.com/processone/ejabberd.git
cd ejabberd
chmod +x autogen.sh
./autogen.sh
./configure --enable-json --enable-mysql
make
make install
```

`jiffy.so`路径有个Bug, 请参考: https://github.com/processone/ejabberd/issues/309, 要解决此问题, 执行:

```
mv /lib/ejabberd/priv/lib/jiffy.so /lib/ejabberd/priv/jiffy.so
```

写作本文时,该BUG暂未解决.


## 一个例子

```
-module(mod_online_users).
-author('hezhiqiang').
-behaviour(gen_mod).
-export([
    start/2,
    stop/1,
    process/2
]).
-include("ejabberd.hrl").
-include("jlib.hrl").
-include("ejabberd_http.hrl").
-include("logger.hrl").
%% 处理函数,直接返回要输出到浏览器的内容
process(_LocalPath, _Request) ->
    Users = mnesia:table_info(session, size),
    OnlineUsers = jiffy:encode({[{<<"onlineusers">>, Users}]}),
    {200, [], OnlineUsers}.
start(_Host, _Opts) ->
    ?INFO_MSG("===Starting module mod_online_users===", []),
    ok.
stop(_Host) ->
    ?INFO_MSG("===Stopping module mod_online_users===", []),
    ok.
```

配置`/etc/ejabberd/ejabberd.yml`


```
  -
    port: 5280
    module: ejabberd_http
    request_handlers:
      "/pub/archive": mod_http_fileserver
      "/http-bind/": mod_http_bind
      "admin": ejabberd_web_admin
      "online": mod_online_users
    web_admin: true
    http_poll: true
    http_bind: true
    captcha: true
```

重启

```
ejabberdctl restart
```

访问

```
http://192.168.8.132:5280/online/
```

![在线用户数模块][1]


## 构造复杂JSON对象

### Erlang和JSON格式对照表

```
Erlang                          JSON            Erlang
==========================================================================
null                       -> null           -> null
true                       -> true           -> true
false                      -> false          -> false
"hi"                       -> [104, 105]     -> [104, 105]
<<"hi">>                   -> "hi"           -> <<"hi">>
hi                         -> "hi"           -> <<"hi">>
1                          -> 1              -> 1
1.25                       -> 1.25           -> 1.25
[]                         -> []             -> []
[true, 1.0]                -> [true, 1.0]    -> [true, 1.0]
{[]}                       -> {}             -> {[]}
{[{foo, bar}]}             -> {"foo": "bar"} -> {[{<<"foo">>, <<"bar">>}]}
{[{<<"foo">>, <<"bar">>}]} -> {"foo": "bar"} -> {[{<<"foo">>, <<"bar">>}]}
#{<<"foo">> => <<"bar">>}  -> {"foo": "bar"} -> #{<<"foo">> -> <<"bar">>}
```

### 示例代码

```
process(_LocalPath, _Request) ->
    ConnectedUsersNumber = ejabberd_sm:connected_users_number(),
    %% 获取在线用户列表
    AllSessionList = ejabberd_sm:dirty_get_sessions_list(),
    %% 构造JSON对象数组
    AllSessions = lists:map(fun({User, Server, Resource}) ->
        {[{user, User}, {server, Server}, {resource, Resource}]}
    end, AllSessionList),
    %% 编码JSON格式
    Json = jiffy:encode({[
        {connected_users_number, ConnectedUsersNumber},
        {sessions, AllSessions}
    ]}),
    {200, [], Json}.
```

### 输出

把上面的代码和下面的输出对照理解.

![JSON输出][2]


  [1]: /assets/images/9058AA0F-5ECA-4FCD-84BB-9CFAD161991F.png
  [2]: /assets/images/BB5D080B-5B6B-43EE-A8DF-0E7DEBE7BAF4.png
