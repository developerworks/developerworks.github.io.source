title: Elixir | 分布式任务和配置
categories:
  - Elixir
tags:
  - Distributed
toc: false
date: 2014-11-20 17:46:57
---

启动节点A, 并定义一个模块

```
root@44adb2a6d305:~# iex --sname A
iex(foo@44adb2a6d305)1> defmodule Hell do
...(foo@44adb2a6d305)1>   def world, do: IO.puts "hello world"
...(foo@44adb2a6d305)1> end
```

启动节点C,并在节点C上Spawn一个在节点A上运行的进程, 并放回消息到C节点的终端

```
root@44adb2a6d305:~# iex --sname C
iex(bar@44adb2a6d305)1> Node.spawn_link :"foo@44adb2a6d305", fn -> Hello.world end
hello world
#PID<9010.79.0>
```

在节点C上定义要执行的代码, 并在A上运行, 然后把运行结果返回给C

```
iex> Node.spawn_link :"A@44adb2a6d305", fn -> Hello.world end
```

对于`Node.spawn_link2`, 有更好的替代

1. 可以使用`:rpc`模块执行远程节点上的函数.
2. 通过GenServer API请求运行在其他节点上的服务器.
3. 可以使用任务,任务可以在本地或远程节点上Spawn


