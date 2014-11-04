title: Ejabberd中几个重要的Record结构
categories:
  - Communication
  - Ejabberd
tags:
  - ejabberd
  - record
toc: false
date: 2014-09-29 20:30:48
---

Ejabberd中几非常重要的记录结构, 贯穿整个Ejabberd的开发, 非常基础, 如果不能很好的理解,那么几乎不能很好的扩展Ejabberd的功能.



## `jid`

Jabber 标识, 最基本的一个record结构, 定义在文件`$EJABBERD_SRC/include/jlib.hrl`中,包含6个字段

```
-record(jid, {user = <<"">> :: binary(),
              server = <<"">> :: binary(),
              resource = <<"">> :: binary(),
              luser = <<"">> :: binary(),
              lserver = <<"">> :: binary(),
              lresource = <<"">> :: binary()}).
```

## `iq`

Info/Query 节, XMPP协议中三个基本的XML节之一, 并且是三个中最重要的一个, 大多数交互都是靠它来完成的. 定义在文件`$EJABBERD_SRC/include/jlib.hrl`中

```
-record(iq, {id = <<"">>       :: binary(),
             type = get        :: get | set | result | error,
             xmlns = <<"">>    :: binary(),
             lang  = <<"">>    :: binary(),
             sub_el = #xmlel{} :: xmlel() | [xmlel()]}).
```

## `xmlel`

一个XML元素记录, 用于构造自定义XML节
