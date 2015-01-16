title: Erlang Ranch - 监听器
categories:
  - Erlang
tags:
  - Ranch
toc: false
date: 2015-01-14 10:23:11
---

Ranch 是一个Erlang Tcp服务器Acceptor池的实现.

监听器是一组进程, 其角色是在端口上监听新连接. 它管理一个Acceptor进程池,每个Acceptor阻塞,知道有新的连接请求到达