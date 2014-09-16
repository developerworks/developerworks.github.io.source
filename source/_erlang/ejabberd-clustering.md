title: Ejabberd 集群
categories:
  - Server Technologies
tags:
  - ejabberd
  - xmpp
toc: false
date: 2014-09-13 01:36:13
---

## Ejabberd 集群

- `XMPP集群`由一个或多个节点构成. 这些节点通过端口`4369`网络相连,必须保证每个节点的`4369`端口处于开状态. 每个节点必须又相同的`Magic Cookie` (`ejabberd/.erlang.cookie`)
- 模块
  - `router`
  - `local router`
  - `session manager`
  - `s2s manager`


## 参考资料

1. http://www.quora.com/Servers/Can-ejabberd-chat-server-be-intergrated-with-scalable-and-fast-server-technologies-like-node-js-tornado-server-to-implement-an-in-browser-chat

2. Strophe.js

An XMPP library for JavaScript
http://strophe.im/strophejs/

3. http://www.rootop.org/pages/2863.html
4. https://github.com/jadahl/mod_restful