title: Elixir 连接同一个局域网的Elixir节点
categories:
  - Elixir
tags:
  - Distributed Application
toc: true
date: 2014-11-20 01:28:22
---

## 本机两个终端连接两个节点

要点: 使用 `--sname` 启动选项

终端A:

    iex --sname A

终端2:

    iex --sname B

在终端A中运行

```
iex(A@localhost)1> Node.connect :'B@localhost'
true
iex(A@localhost)2> node
:"A@localhost"
iex(A@localhost)3> Node.list
[:"B@localhost"]
iex(A@localhost)4> [node | Node.list]
[:"A@localhost", :"B@localhost"]
iex(A@localhost)5>
```

## 在同一个局域网中连接两个节点

要点: 使用 `--name` 启动选项

启动`A`(192.168.8.100)节点

    iex --name A@192.168.8.100

启动`B`(192.168.8.200)节点

    iex --name B@192.168.8.200

从`192.168.8.100`连接到`192.168.8.200`, 在`192.168.8.100`(A)的终端执行:

    iex(A@192.168.8.100)1> Node.connect :'B@192.168.8.200'

验证连接的节点

    iex(A@192.168.8.100)2> Node.list


