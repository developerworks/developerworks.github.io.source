title: Ejabberd 发布订阅实验
categories:
  - Ejabberd
tags:
  - ejabberd
  - pubsub
  - strophe.js
toc: false
date: 2014-10-02 02:38:09
---

## 环境

| Software   | Version |
| ---------- | ------- |
| Ejabberd   | 14.07   |
| Strophe.js | 1.1.3   |


## 创建节点

Strophe.js代码

```
var createnode = function (node) {
  var iq = $iq({
    type: 'set',
    from: jid,
    to: session.pubsub,
    id: getId()
  }).c('pubsub', {xmlns: 'http://jabber.org/protocol/pubsub'})
    .c('create', {node: node});
  connection.send(iq.tree());
};
```

XML节

```
<!--客户端IQ-set请求-->
<iq type='set'
    from='root@xmpp.myserver.info'
    to='pubsub.xmpp.myserver.info'
    id='id-1412188942892'
    xmlns='jabber:client'>
  <pubsub xmlns='http://jabber.org/protocol/pubsub'>
    <create node='/home/test2'/>
  </pubsub>
</iq>
<!--服务端应答IQ-result-->
<iq from="pubsub.xmpp.myserver.info"
    to="root@xmpp.myserver.info/17735350171411959891145164"
    id="id-1412188942892"
    type="result"
    xmlns="jabber:client"
    xmlns:stream="http://etherx.jabber.org/streams"
    version="1.0">
  <pubsub xmlns="http://jabber.org/protocol/pubsub">
    <create node="/home/test2"/>
  </pubsub>
</iq>
```

## 订阅

Strophe.js代码

```
var subscribe = function (node) {
  var iq = $iq({
    type: 'set',
    from: jid,
    to: session.pubsub,
    id: getId()
  }).c('pubsub', {xmlns: 'http://jabber.org/protocol/pubsub'})
    .c('subscribe', {node: node, jid: jid});
  connection.send(iq.tree());
};
```

XML节


```
<!--客户端IQ-set请求-->
<iq type='set' from='root@xmpp.myserver.info'
    to='pubsub.xmpp.myserver.info' id='id-1412189193003'
    xmlns='jabber:client'>
  <pubsub xmlns='http://jabber.org/protocol/pubsub'>
    <subscribe node='/home/test' jid='root@xmpp.myserver.info'/>
  </pubsub>
</iq>
<!--服务端应答IQ-result-->
<iq from="pubsub.xmpp.myserver.info"
    to="root@xmpp.myserver.info/17735350171411959891145164"
    id="id-1412189193003" type="result"
    xmlns="jabber:client" xmlns:stream="http://etherx.jabber.org/streams"
    version="1.0">
  <pubsub xmlns="http://jabber.org/protocol/pubsub">
    <subscription
        jid="root@xmpp.myserver.info" subscription="subscribed" subid="583EAABFD09CF"
        node="/home/test"/>
  </pubsub>
</iq>
```


## 参考资料

1. http://wiki.jabbercn.org/XEP-0060#.E5.88.9B.E5.BB.BA.E8.8A.82.E7.82.B9
