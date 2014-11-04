title: Ejabberd 调试
categories:
  - Communication
  - Ejabberd
tags:
  - XMPP
  - ejabberd
  - debugging
toc: false
date: 2014-09-23 12:07:18
---

本文通过分析Ejabberd服务器和客户端之间的通信,帮助你理解XMPP协议.

<!--more-->

## 配置

修改`/etc/ejabberd/ejabberd.yml`,把日志等级设置为`debug`

```
loglevel: 5
```

## 监视日志输出

```
tail -f /var/log/ejabberd/ejabberd.log
```

## 心跳包

心跳包用于保持客户端和服务器的持久连接, 下面是通过日志输出的一个示例

```
# 收到客户的一个ping包
2014-09-21 04:49:25.713 [debug] <0.2365.0>@ejabberd_receiver:process_data:343 Received XML on stream = <<"<iq type='get' id='purple3c1c1c76'><ping xmlns='urn:xmpp:ping'/></iq>">>
# 内部状态更新
2014-09-21 04:49:25.713 [debug] <0.2365.0>@shaper:update:117 State: {maxrate,1000,3.0968123357848385,1411274905955950}, Size=69 M=34.55350285694598, I=59757.352
# 客户端请求路由/处理
2014-09-21 04:49:25.713 [debug] <0.2366.0>@ejabberd_router:do_route:322 route
	from {jid,<<"hezhiqiang">>,<<"xmpp.myserver.info">>,<<"hezhiqiang-2">>,<<"hezhiqiang">>,<<"xmpp.myserver.info">>,<<"hezhiqiang-2">>}
	to {jid,<<"hezhiqiang">>,<<"xmpp.myserver.info">>,<<>>,<<"hezhiqiang">>,<<"xmpp.myserver.info">>,<<>>}
	packet {xmlel,<<"iq">>,[{<<"type">>,<<"get">>},{<<"id">>,<<"purple3c1c1c76">>}],[{xmlel,<<"ping">>,[{<<"xmlns">>,<<"urn:xmpp:ping">>}],[]}]}
2014-09-21 04:49:25.713 [debug] <0.2366.0>@ejabberd_local:do_route:296 local route
	from {jid,<<"hezhiqiang">>,<<"xmpp.myserver.info">>,<<"hezhiqiang-2">>,<<"hezhiqiang">>,<<"xmpp.myserver.info">>,<<"hezhiqiang-2">>}
	to {jid,<<"hezhiqiang">>,<<"xmpp.myserver.info">>,<<>>,<<"hezhiqiang">>,<<"xmpp.myserver.info">>,<<>>}
	packet {xmlel,<<"iq">>,[{<<"type">>,<<"get">>},{<<"id">>,<<...>>}],[{xmlel,<<...>>,...}]}
2014-09-21 04:49:25.714 [debug] <0.2366.0>@ejabberd_sm:do_route:514 session manager
	from {jid,<<"hezhiqiang">>,<<"xmpp.myserver.info">>,<<"hezhiqiang-2">>,<<"hezhiqiang">>,<<"xmpp.myserver.info">>,<<"hezhiqiang-2">>}
	to {jid,<<"hezhiqiang">>,<<"xmpp.myserver.info">>,<<>>,<<"hezhiqiang">>,<<"xmpp.myserver.info">>,<<>>}
	packet {xmlel,<<"iq">>,[{<<"type">>,<<"get">>},{<<"id">>,<<...>>}],[{xmlel,<<...>>,...}]}
# 服务器响应路由处理
2014-09-21 04:49:25.714 [debug] <0.2366.0>@ejabberd_router:do_route:322 route
	from {jid,<<"hezhiqiang">>,<<"xmpp.myserver.info">>,<<>>,<<"hezhiqiang">>,<<"xmpp.myserver.info">>,<<>>}
	to {jid,<<"hezhiqiang">>,<<"xmpp.myserver.info">>,<<"hezhiqiang-2">>,<<"hezhiqiang">>,<<"xmpp.myserver.info">>,<<"hezhiqiang-2">>}
	packet {xmlel,<<"iq">>,[{<<"id">>,<<"purple3c1c1c76">>},{<<"type">>,<<"result">>}],[]}
2014-09-21 04:49:25.714 [debug] <0.2366.0>@ejabberd_local:do_route:296 local route
	from {jid,<<"hezhiqiang">>,<<"xmpp.myserver.info">>,<<>>,<<"hezhiqiang">>,<<"xmpp.myserver.info">>,<<>>}
	to {jid,<<"hezhiqiang">>,<<"xmpp.myserver.info">>,<<"hezhiqiang-2">>,<<"hezhiqiang">>,<<"xmpp.myserver.info">>,<<"hezhiqiang-2">>}
	packet {xmlel,<<"iq">>,[{<<"id">>,<<"purp"...>>},{<<"type">>,<<...>>}],[]}
2014-09-21 04:49:25.714 [debug] <0.2366.0>@ejabberd_sm:do_route:514 session manager
	from {jid,<<"hezhiqiang">>,<<"xmpp.myserver.info">>,<<>>,<<"hezhiqiang">>,<<"xmpp.myserver.info">>,<<>>}
	to {jid,<<"hezhiqiang">>,<<"xmpp.myserver.info">>,<<"hezhiqiang-2">>,<<"hezhiqiang">>,<<"xmpp.myserver.info">>,<<"hezhiqiang-2">>}
	packet {xmlel,<<"iq">>,[{<<"id">>,<<"purp"...>>},{<<"type">>,<<...>>}],[]}
2014-09-21 04:49:25.714 [debug] <0.2366.0>@ejabberd_sm:do_route:612 sending to process <0.2366.0>
2014-09-21 04:49:25.714 [debug] <0.2366.0>@ejabberd_c2s:send_text:1865 Send XML on stream = <<"<iq from='hezhiqiang@xmpp.myserver.info' to='hezhiqiang@xmpp.myserver.info/hezhiqiang-2' id='purple3c1c1c76' type='result'/>">>
```

- 第2行:
    服务器接收到客户端的一个`<iq type='get' id='purple3c1c1c76'><ping xmlns='urn:xmpp:ping'/></iq>` ping `节`.
- 第4行:
    更新内部状态
- 第6~17行:
    客户的请求包路由处理
- 第8行:
    路由判断`xmpp.myserver.info`是本地域, 所以包被转交给`ejabberd_local`模块处理
...
...
...
TODO

分析:

路由过程分为两个阶段:

第一个阶段是收到的请求的路由,第二个阶段是响应的路由, 上述代码涉及到Ejabberd的三个内部模块

- `ejabberd_router` 路由处理程序
- `ejabberd_local`  本地路由处理程序,处理服务器相关的节
- `ejabberd_sm`     会话管理器,处理纯Jid相关的XML节

