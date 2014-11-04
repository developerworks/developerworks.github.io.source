title: Ejabberd  C2S模块状态分析
categories:
  - Communication
  - Ejabberd
tags:
  - ejabberd
  - ejabberd_c2s
toc: false
date: 2014-09-27 01:30:40
---

Ejabberd的`ejabberd_c2s`核心模块,是处理XMPP协议的核心处理模块,本文通过客户端和服务器的交互过程来分析其原理.


## 建立TCP连接

客户端建立TCP连接`ejabberd_listener`接受TCP连接,服务器日志输出

```
# ejabberd_listener 接收到客户端的TCP连接请求
2014-09-26 05:22:31.527 [info] <0.673.0>@ejabberd_listener:accept:313 (#Port<0.5846>) Accepted connection 172.17.42.1:53457 -> 172.17.0.27:5222
```

`ejabberd_c2s`进程初始状态为`wait_for_stream`,等待接收`{xmlstreamstart, _Name, Attrs}`消息

## 客户端打开流

客户端发送打开流Stanza

```
<stream:stream to='xmpp.myserver.info' xmlns='jabber:client' xmlns:stream='http://etherx.jabber.org/streams' version='1.0'>
```

服务器日志输出为

```
# ejabberd_receiver 接收到客户端的流打开请求
2014-09-26 05:22:31.527 [debug] <0.849.0>@ejabberd_receiver:process_data:343 Received XML on stream = <<"<stream:stream xmlns:stream="http://etherx.jabber.org/streams" xmlns="jabber:client" to="xmpp.myserver.info" version="1.0">">>
# 服务器确认客户的流打开请求,返回一个相应
2014-09-26 05:22:31.528 [debug] <0.850.0>@ejabberd_c2s:send_text:1869 Send XML on stream = <<"<?xml version='1.0'?><stream:stream xmlns='jabber:client' xmlns:stream='http://etherx.jabber.org/streams' id='3177127870' from='xmpp.myserver.info' version='1.0' xml:lang='en'>">>
# 服务器返回功能相应
2014-09-26 05:22:31.529 [debug] <0.850.0>@ejabberd_c2s:send_text:1869 Send XML on stream = <<"<stream:features><mechanisms xmlns='urn:ietf:params:xml:ns:xmpp-sasl'><mechanism>DIGEST-MD5</mechanism><mechanism>SCRAM-SHA-1</mechanism><mechanism>PLAIN</mechanism></mechanisms><c xmlns='http://jabber.org/protocol/caps' hash='sha-1' node='http://www.process-one.net/en/ejabberd/' ver='aIT+/ulfcbHXDKPkCA+iw9x5mU8='/><register xmlns='http://jabber.org/features/iq-register'/></stream:features>">>
```

`ejabberd_c2s`状态更新为`wait_for_feature_request`,等待接收`{xmlstreamstart, _Name, Attrs}`消息


## 客户端认证

客户端发送一个`<auth>`请求认证

```
# auth内的文本值无换行和空格,这里为了可读性而格式化
<auth xmlns='urn:ietf:params:xml:ns:xmpp-sasl' mechanism='SCRAM-SHA-1'>
    biwsbj1yb290LHI9ZDQxZDhjZDk4ZjAwYjIwNGU5ODAwOTk4ZWNmODQyN2U=
</auth>
```

服务器收到认证请求 并更新内部状态

```
# 客户端请求认证
2014-09-26 05:22:31.543 [debug] <0.849.0>@ejabberd_receiver:process_data:343 Received XML on stream = <<"<auth xmlns="urn:ietf:params:xml:ns:xmpp-sasl" mechanism="SCRAM-SHA-1">biwsbj1yb290LHI9ZDQxZDhjZDk4ZjAwYjIwNGU5ODAwOTk4ZWNmODQyN2U=</auth>">>
# 更新内部状态
2014-09-26 05:22:31.543 [debug] <0.849.0>@shaper:update:117 State: {maxrate,1000,0.0,1411708951528734}, Size=138
```

此时, `ejabberd_c2s`状态更新为`wait_for_sasl_response`, 同时返回`<challenge>`响应

```
# challenge内的文本值无换行和空格,这里为了可读性而格式化
<challenge
    xmlns="urn:ietf:params:xml:ns:xmpp-sasl"
    xmlns:stream="http://etherx.jabber.org/streams" version="1.0">
    cj1kNDFkOGNkOThmMDBiMjA0ZTk4MDA5...省略</challenge>
```

客户端响应`<challenge>`

```
<response
    xmlns='urn:ietf:params:xml:ns:xmpp-sasl'>
    Yz1iaXdzLHI9ZDQxZDhjZDk4ZjAw...省略</response>
```

服务器返回`<success>`

```
<success
    xmlns="urn:ietf:params:xml:ns:xmpp-sasl"
    xmlns:stream="http://etherx.jabber.org/streams"
    version="1.0">dj1saHU0SHBCZVVsc2Fla2dhUFN2cXlxZ3Jxdlk9</success>
```

`ejabberd_c2s`状态重新回到`wait_for_stream`,内部状态`StateData#state.authenticated`为非`false`

## 绑定资源

客户端重新发送`<stream>`

```
<stream:stream
    to='xmpp.myserver.info'
    xmlns='jabber:client'
    xmlns:stream='http://etherx.jabber.org/streams'
    version='1.0'>
```

服务器确认,并发送功能节

```
<stream:stream xmlns="jabber:client"
    xmlns:stream="http://etherx.jabber.org/streams"
    id="111324954"
    from="xmpp.myserver.info"
    version="1.0"
    xml:lang="en">
```

```
<stream:features xmlns="jabber:client"
    xmlns:stream="http://etherx.jabber.org/streams" version="1.0">
  <bind xmlns="urn:ietf:params:xml:ns:xmpp-bind"/>
  <session xmlns="urn:ietf:params:xml:ns:xmpp-session"/>
  <sm xmlns="urn:xmpp:sm:2"/>
  <sm xmlns="urn:xmpp:sm:3"/>
  <csi xmlns="urn:xmpp:csi:0"/>
  <c xmlns="http://jabber.org/protocol/caps"
    hash="sha-1"
    node="http://www.process-one.net/en/ejabberd/"
    ver="aIT+/ulfcbHXDKPkCA+iw9x5mU8="/>
  <register xmlns="http://jabber.org/features/iq-register"/>
</stream:features>
```

客户端选择绑定资源(资源由服务器生成)

```
<iq type='set' id='_bind_auth_2' xmlns='jabber:client'>
  <bind xmlns='urn:ietf:params:xml:ns:xmpp-bind'/>
</iq>
```

服务器应答

```
<iq id="_bind_auth_2"
    type="result"
    xmlns="jabber:client"
    xmlns:stream="http://etherx.jabber.org/streams"
    version="1.0">
  <bind xmlns="urn:ietf:params:xml:ns:xmpp-bind">
    <jid>root@xmpp.myserver.info/39189873701411708951913434</jid>
  </bind>
</iq>
```

客户端请求初始化会话

```
<iq type='set' id='_session_auth_2' xmlns='jabber:client'>
  <session xmlns='urn:ietf:params:xml:ns:xmpp-session'/>
</iq>
```

服务器应答

```
<iq type="result"
    xmlns="jabber:client"
    id="_session_auth_2"
    xmlns:stream="http://etherx.jabber.org/streams"
    version="1.0"/>
```

## 参考资料

1. 发现一篇类似的分析博客
http://my.oschina.net/hncscwc/blog/159826





