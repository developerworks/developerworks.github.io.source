title: Elixir | 任务
categories:
  - Elixir
tags:
  - Elixir
  - Task
toc: false
date: 2014-11-21 11:41:09
---

## 什么是任务?

任务是进程, 是用于执行特定动作的进程, 通常较少或不与其他进程通信. 任务最常用的情况是异步地计算一个值.

```
task = Task.async(fn -> do_some_work() end)
res  = do_some_other_work()
res + Task.await(task)
```

任务通常不返回值, 但是有时候我们需要让任务计算一个值,随后读取其计算结果.

`async/await` 提供了一个非常简单的机制并发地计算.

```
iex(foo@44adb2a6d305)2> task = Task.async(fn -> :math.pow(7,2) + :math.pow(8,2) end)
%Task{pid: #PID<0.97.0>, ref: #Reference<0.0.0.238>}
iex(foo@44adb2a6d305)3> Task.await(task)
113.0
```

首先来说明异步, `Task.async/1`创建一个任务,并执行传递给它的函数, 如上述代码第2行所示, `Task.async/1`调用返回一个对任务的引用.

这行输出很好地阐述了文章开头这么一句话`任务是进程`.该输出实际上是一个`%Task` Map, 包含两个元素 进程的Pid, 以及一个引用对象.

随后可以通过调用`Task.await/1`获取其计算结果. 调用`Task.async/1`会创建一个新的进程,该进程连接到调用者进程. 任务结果以消息的形式返回给调用者进程.
`Task.await/2`用于读取由任务发送的消息.

创建任务`task1`

```
iex(foo@44adb2a6d305)4> task1 = Task.async(fn -> :math.pow(7,2) + :math.pow(8,2) end)
%Task{pid: #PID<0.100.0>, ref: #Reference<0.0.0.245>}
```

创建任务`task2`

```
iex(foo@44adb2a6d305)6> task2 = Task.async(fn -> 2+2 end)
%Task{pid: #PID<0.103.0>, ref: #Reference<0.0.0.252>}
```

分别获取两个任务的计算结果 `v1`, `v2`

```
v1 = Task.await(task1)
v2 = Task.await(task2)
```

再通过第三个任务`task3`合并计算两个任务的结果

```
iex(foo@44adb2a6d305)15> task3 = Task.async(fn -> v1 + v2 end)
%Task{pid: #PID<0.117.0>, ref: #Reference<0.0.0.288>}
iex(foo@44adb2a6d305)16> Task.await(task3)
117.0
```

还可以通过`start_link/1`和`start_link/3`创建任务并挂在到Supervition树中.

和`Task.async/1`不同,`Task.start_link/1`返回的是一个`{:ok,pid}`而不是一个任务引用,因此我无法通过`Task.await/2`获取其计算结果.

```
iex(foo@44adb2a6d305)21> Task.start_link(fn -> 1 + 1 end)
{:ok, #PID<0.128.0>}
```

这样的任务可以挂在到supervision树, 下面的代码片段显示了如何挂载一个任务到一个supervision树中:

```
import Supervisor.Spec
children = [
    worker(Task, [fn -> IO.puts "ok" end])
]
```

> 通过上述几个步骤,我们实现了并发处理. 下面阐述如何把任务分布到集群中的多个节点从而实现分布式.


## Supervision 树

`Task.Supervisor`模块允许开发者创建可动态第添加任务的监控进程(Supervisors):

```
{:ok, pid} = Task.Supervisor.start_link()
Task.Supervisor.async(pid, Module, :function, [arg1, arg2, arg3])
```

`Task.Supervisor`还可以在远程节点上创建任务, 只要该监控进程在本地或全局注册过:

```
# 在远程节点
Task.Supervisor.start_link(name: :tasks_sup)
# 在客户端
Task.Supervisor.async({:tasks_sup, :'bar@192.168.8.8'}, Module, :function, [arg1, arg2, arg3])
```

`Task.Supervisor`通常在监控树中以下列方式启动

```
import Supervisor.Spec
children = [
    supervisor(Task.Supervisor, [[name: :tasks_sup]
]
```

需要注意的是, 当处理分布式任务时, 应该使用`async/3`, 分别传递模块,函数,参数列表, 而不是以一个匿名函数作为参数的`async/1`.

## 分布 (Distrbuted)

TODO::





