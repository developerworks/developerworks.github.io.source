title: Elixir | 连接同一个局域网的Elixir节点
categories:
  - Elixir
tags:
  - Elixir
  - Distributed Application
toc: true
date: 2014-11-20 01:28:22
---

## 本机两个终端连接两个节点

要点: 使用 `--sname` 启动选项

终端A:

    iex --sname A

终端C:

    iex --sname C

在终端A中运行

```
iex(A@localhost)1> Node.connect :'C@localhost' %% 连接C节点
true
iex(A@localhost)2> node %% 显示当前节点名称
:"A@localhost"
iex(A@localhost)3> Node.list %% 列出连接到当前节点的其他节点
[:"C@localhost"]
iex(A@localhost)4> [node | Node.list] %% 列出当前节点和连接节点
[:"A@localhost", :"C@localhost"]
iex(A@localhost)5>
```

## 在同一个局域网中连接两个节点

要点: 使用 `--name` 启动选项

启动`A`(192.168.8.100)节点

    iex --name A@192.168.8.100

启动`C`(192.168.8.200)节点

    iex --name C@192.168.8.200

从`192.168.8.100`连接到`192.168.8.200`, 在`192.168.8.100`(A)的终端执行:

    iex(A@192.168.8.100)1> Node.connect :'C@192.168.8.200'

验证连接的节点

    iex(A@192.168.8.100)2> Node.list


连接到多个节点原理相同, 如果增加D(192.168.8.201),E(192.168.8.202),F(192.168.8.203)节点到集群中, 在A节点上(192.168.8.100)依次再执行

```
iex(A@192.168.8.100)1> Node.connect :'D@192.168.8.201'
iex(A@192.168.8.100)2> Node.connect :'E@192.168.8.202'
iex(A@192.168.8.100)3> Node.connect :'F@192.168.8.203'
```

## 参考资料

1. Connecting Elixir Nodes on the Same LAN
http://benjamintanweihao.github.io/blog/2014/05/25/connecting-elixir-nodes-on-the-same-lan/
