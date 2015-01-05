title: Elixir | 分布式任务和配置
categories:
  - Elixir
tags:
  - Elixir
  - Distributed
toc: false
date: 2014-11-20 17:46:57
---

启动节点A, 并定义一个模块

```
root@c87c9967219c:~# iex --sname A
iex(foo@c87c9967219c)1> defmodule Hell do
...(foo@c87c9967219c)1>   def world, do: IO.puts "hello world"
...(foo@c87c9967219c)1> end
```

启动节点C,并在节点C上Spawn一个在节点A上运行的进程, 并放回消息到C节点的终端

```
root@c87c9967219c:~# iex --sname C
iex(bar@c87c9967219c)1> Node.spawn_link :"foo@c87c9967219c", fn -> Hello.world end
hello world
#PID<9010.79.0>
```

在节点C上定义要执行的代码, 并在A上运行, 然后把运行结果返回给C

```
iex> Node.spawn_link :"A@c87c9967219c", fn -> Hello.world end
```

收发消息:

```
iex(B@c87c9967219c)1> pid = Node.spawn_link :"A@c87c9967219c", fn ->
...(B@c87c9967219c)1>   receive do
...(B@c87c9967219c)1>     {:ping, client} ->
...(B@c87c9967219c)1>       send client, :pong
...(B@c87c9967219c)1>   end
...(B@c87c9967219c)1> end
#PID<8005.65.0>
iex(B@c87c9967219c)2> send pid, {:ping, self}
{:ping, #PID<0.60.0>}
iex(B@c87c9967219c)3> flush
:pong
:ok
```

每次我们想要执行分布式计算的时候, 我们可以通过`Node.spawn_link/2`在其他节点上`spawn`一个进程, 但是我们应该避免在 supervision 树的外部去spawn子进程.

对于`Node.spawn_link2`, 有更好的替代

- 可以使用`:rpc`模块执行远程节点上的函数, 比如:
    ```
    # 在A@c87c9967219c节点上, 执行Hello模块的:world函数, 没有参数
    :rpc.call(:"A@c87c9967219c", Hello, :world, [])
    ```
- 通过[GenServer API](http://elixir-lang.org/docs/master/elixir/GenServer.html#call/3)请求运行在其他节点上的服务器, 比如:
    ```
    GenServer.call({name, node}, arg)
    ```
- 可以使用任务,任务可以在本地或远程节点上Spawn进程

## 参考资料

1. Distributed tasks and configuration
http://elixir-lang.org/getting_started/mix_otp/10.html

