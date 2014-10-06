title: XMPP XEP-0198流管理 - 实例分析
categories:
  - Communication
tags:
  - XMPP
  - XEP
toc: false
date: 2014-10-03 04:07:43
---

客户端启用流管理
```
<enable xmlns='urn:xmpp:sm:3' resume='true'/>
```

服务端启用流管理

```
<enabled xmlns="urn:xmpp:sm:3"
    id="g2gCbQAAABkyNTY2MjI4NzA1MTQxMjIwMTU0MjEwMjkzaANiAAAFhGIAAE65YgAMkl8="
    resume="true" max="300"
    xmlns:stream="http://etherx.jabber.org/streams" version="1.0"/>
```

Strophe.js代码

```
var enable = function () {
  var stanza = $build('enable', {xmlns: 'urn:xmpp:sm:3', resume: true});
  connection.send(stanza.tree());
};
```

服务器日志

```
2014-09-29 22:12:39.416 [info] <0.14329.0>@ejabberd_c2s:handle_enable:2676
    Stream management with resumption enabled for root@xmpp.hezhiqiang.info/2619252428141228749171506
```

当启用流管理功能后,客户端在发送和接受每一个XML节的时候,会附带收到`<r/>`请求

```
<r xmlns="urn:xmpp:sm:3" xmlns:stream="http://etherx.jabber.org/streams" version="1.0"/>
```