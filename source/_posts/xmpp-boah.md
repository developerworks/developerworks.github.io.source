title: XMPP 同步HTTP双向流
categories:
  - Communication
tags:
  - XMPP
  - bosh
  - javascript
toc: false
date: 2014-09-13 20:313:13
---


Javascript不支持TCP持久连接,Strophe使用同步HTTP双向流(BOSH)来模拟持久的有状态的连接[`XEP 0124`][1]


  [1]: xmpp.org/extensions/xep-0124.html