title: XMPP 名册版本(联系人信息)
categories:
  - xmpp
tags:
  - xmpp
toc: true
date: 2014-09-15 22:29:37
---

[XEP-0237: Roster Versioning][1] 名册版本的作用是为了避免每次登陆都下载整个联系人列表,登陆时首先获取服务器上的名册版本号,并同本地版本号进行对比,如果版本号相同,则名册信息位发生变化,不必下载.

`名册版本机制`可以显著的提升移动设备的的响应时间,并且减少网络带宽的消耗.


  [1]: http://xmpp.org/extensions/xep-0237.html
