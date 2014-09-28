title: 使用Jiffy输出JSON数据
categories:
  - Communication
tags:
  - ejabberd
  - jiffy
toc: false
date: 2014-09-28 13:05:43
---

编译

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

代码

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

  [1]: /assets/images/9058AA0F-5ECA-4FCD-84BB-9CFAD161991F.png