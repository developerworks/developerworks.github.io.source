title: Erlang 资源池
categories:
  - Erlang
tags:
  - Resource Pool
toc: true
date: 2014-11-13 16:55:44
---

资源池是解决并发问题的一种常见模式

Versions:

- `2014-11-13` `Version 0.1`

## 简介

软件资源的创建需要消耗大量的时间和内存, 如果能重用, 将极大地改善应用程序的性能. 资源池是一个在不同的平台和语言中广泛的使用的方法.本文所述[Erlang资源池](http://sourceforge.net/projects/erlpool/)的设计灵感来源于Apache的Common Pool库. API和主要的功能是从其借用的, 但内部实现完全不同, 并使用了Erlang OTP设计原则, 以及Erlang并发模型.

## 设计

资源池由两个列表组成: `Active`(活动列表)和`Idle`(空闲列表). `Active`列表包含活跃的资源. `Idle`列表包含不活跃的资源

    图-1: 资源池为空的状态,活动列表和空闲列表都为空

    +-Pool-----------{0,0}-+
    |                      |
    | Active--+  Idle----+ |
    | |       |  |       | |
    | |       |  |       | |
    | |       |  |       | |
    | +-------+  +-------+ |
    +----------------------+

## 操作

### borrow

要从资源池中获得一个资源,需要调用函数`borrow`

```erlang
Resource = resource_pool:borrow(test_pool)
```

如果资源池的`Idle`列表为空(在没有资源可用的情况下),资源池会直接在`Active`列表中创建一个资源并授予调用进程.

    图-2: 创建一个新的资源,并把该资源放到 Active 资源列表

    +-Pool-----------{1,0}-+          +-Pool-----------{2,0}-+
    |                      |          |                      |
    | Active--+  Idle----+ |          | Active--+  Idle----+ |
    | |       |  |       | |          | |       |  |       | |
    | |       |  |       | |    =>    | | <R.2> |  |       | |
    | | <R.1> |  |       | |          | | <R.1> |  |       | |
    | +-------+  +-------+ |          | +-------+  +-------+ |
    +----------------------+          +----------------------+

如果在资源池的`Idle`列表中存在可用的资源. 将从`Idle`列表中取出一个资源,并转移到`Active`列表,然后授予调用者进程.

    图-3: 从空闲资源列表中得到一个资源,并把它转移到活动列表

    +-Pool-----------{1,2}-+          +-Pool-----------{2,1}-+
    |                      |          |                      |
    | Active--+  Idle----+ |          | Active--+  Idle----+ |
    | |       |  |       | |          | |       |  |       | |
    | |       |  | <R.2> | |    =>    | | <R.2> |  |       | |
    | | <R.1> |  | <R.3> | |          | | <R.1> |  | <R.3> | |
    | +-------+  +-------+ |          | +-------+  +-------+ |
    +----------------------+          +----------------------+

### return

一旦进程完成了对资源的使用,它必须把资源返回到资源池中

```erlang
resource_pool:return(test_pool,Resource)
```

换句话说,就是该资源从`Active`列表移动到`Idle`列表(图-4). 以让其他进程能够从资源池中获取可用的资源.

    图-4 进程把资源返还给资源池

    +-Pool-----------{2,1}-+          +-Pool-----------{1,2}-+
    |                      |          |                      |
    | Active--+  Idle----+ |          | Active--+  Idle----+ |
    | |       |  |       | |          | |       |  |       | |
    | | <R.2> |  |       | |    =>    | |       |  | <R.2> | |
    | | <R.1> |  | <R.3> | |          | | <R.1> |  | <R.3> | |
    | +-------+  +-------+ |          | +-------+  +-------+ |
    +----------------------+          +----------------------+

### add

有时候我们需要添加新的资源

```erlang
resource_pool:add(test_pool)
```

函数`add`创建一个新的资源,并添加到`Idle`列表中

    +-Pool-----------{2,1}-+          +-Pool-----------{2,2}-+
    |                      |          |                      |
    | Active--+  Idle----+ |          | Active--+  Idle----+ |
    | |       |  |       | |          | |       |  |       | |
    | | <R.2> |  |       | |    =>    | | <R.2> |  | <R.4> | |
    | | <R.1> |  | <R.3> | |          | | <R.1> |  | <R.3> | |
    | +-------+  +-------+ |          | +-------+  +-------+ |
    +----------------------+          +----------------------+


### invalidate

```erlang
resource_pool:invalidate(test_pool,Resource)
```

`invalidate` 函数把一个失败的资源标记为不可用,资源池然后会销毁这个资源.

    +-Pool-----------{2,1}-+          +-Pool-----------{1,1}-+
    |                      |          |                      |
    | Active--+  Idle----+ |          | Active--+  Idle----+ |
    | |       |  |       | |          | |       |  |       | |
    | | <R.2> |  |       | |    =>    | |       |  |       | |
    | | <R.1> |  | <R.3> | |          | | <R.1> |  | <R.3> | |
    | +-------+  +-------+ |          | +-------+  +-------+ |
    +----------------------+          +----------------------+

### 典型用例

```erlang
case resource_pool:borrow(test_pool) of
  {error, E} -> io:format("Error while borrow from pool, reason: ~p", [E]);
  Resource ->
    try
      resource:operation(Resource),
      resource_pool:return(test_pool, Resource)
    catch
      _:_ -> resource_pool:invalidate(test_pool, Resource)
    end,
end
```

## 大小限制

```erlang
{ok,Pid} = resource_pool:new(
    test_pool,resource_factory,resource_metadata,options
)
```

```
max_active,
max_idle,
min_idle
```

                +-Pool-----------{0,0}-+
                |                      |
                | Active--+  Idle----+ |
                | |       |  |_______|_|__ max_idle
    max_active__|_|_______|  |       | |
                | |       |  |       | |
                | |       |  |_______|_|__ min_idle
                | |       |  |       | |
                | +-------+  +-------+ |
                +----------------------+

### max_active

`Active`列表默认最大值为8. 如果达到限制`borrow`操作将会阻塞或失败.  值 -1 (或任何负值) 表示`Active`列表没有大小限制.

```erlang
{ok,Pid} =
    resource_pool:new(test_pool,resource_factory,[],[{max_active,20}])
```

### max_idle

`Idle`列表的最大大小,默认和`max_active`相同. 如果达到限制,后续`return`的资源会被销毁,值 -1 (或任何负值) 表示`Idle`列表没有大小限制.

```erlang
{ok,Pid} = resource_pool:new(
    test_pool,                          %% 资源池实例标识
    resource_factory,                   %% 资源工厂
    [],                                 %% 资源元数据
    [{max_active,20},{max_idle,10}]     %% 资源池选项
)
```

### min_idle

`Idle`列表默认的最小大小为0.

If it reaches the limit then following borrow operation will successfully supplies a resource to invoker and then pool will additionally create new resource in Idle container to provide min_idle condition.


```erlang
{ok,Pid} = resource_pool:new(
    test_pool,                                      %% 资源池标识
    resource_factory,                               %% 资源工厂
    [],                                             %% 资源元数据
    [{max_active,20},{max_idle,10},{min_idle,3}]    %% 资源池选项
)
```

关于空闲资源的最大和最小值应该根据实际需求控制在一个合理的区间. 太小会导致频繁的创建资源,太大又会消耗过多的内存.


## 行为选项

### 资源池耗尽时的borrow行为

- `{when_exhausted_action,fail}`
在耗尽的资源池上调用`borrow`将返回`{error,pool_exhausted}`

- `{when_exhausted_action,block}`
在耗尽的资源池上调用`borrow`将阻塞,知道有空闲的资源可用,等待的最大时间可以通过选项`max_wait`控制

- `{when_exhausted_action,grow}`
在耗尽的资源池上调用`borrow`将创建新资源,并增大`Active`列表的大小,此时`max_Idle`选项被忽略.


```erlang
{ok,Pid} = resource_pool:new(
    test_pool,                                     %% 资源池实例标识
    resource_factory,                              %% 资源工厂
    [],                                            %% 资源元数据
    [{max_active,20},{when_exhausted_action,fail}] %% 资源池选项
)
```

### 资源检查

Resource pool can check status of managed resources. Options `test_on_borrow` and `test_on_return` control how pool tests resources: before providing resource to invoker `{test_on_borrow, true}` and after a resource was returned to pool `{test_on_return, true}`. If pool finds that the resource is not alive during test then the resource will be destroyed.


### 资源在Idle列表中的顺序

Option `fifo` (first-input-first-output) controls order of extracting a resources from `Idle` list. Diagrams below illustrate this. Suppose we fill out `Idle` list in order: <R.1> was first, <R.2> is next, then <R.3>. Resource <R.4> is active in given moment. If `{fifo, true}` is set the `borrow` operation leads to situation below: resource <R.1> was came first and it becames active now (first input).

    +-Pool-----------{1,2}-+          +-Pool-----------{2,1}-+
    |                      |          |                      |
    | Active--+  Idle----+ |          | Active--+  Idle----+ |
    | |       |  | <R.3> | |          | |       |  |       | |
    | |       |  | <R.2> | |    =>    | | <R.1> |  | <R.3> | |
    | | <R.4> |  | <R.1> | |          | | <R.4> |  | <R.2> | |
    | +-------+  +-------+ |          | +-------+  +-------+ |
    +----------------------+          +----------------------+

If `{fifo, false}` is set it means that order will be last-input-first-output. `borrow` operation makes active resource <R.3> (last input).

    +-Pool-----------{1,2}-+          +-Pool-----------{2,1}-+
    |                      |          |                      |
    | Active--+  Idle----+ |          | Active--+  Idle----+ |
    | |       |  | <R.3> | |          | |       |  |       | |
    | |       |  | <R.2> | |    =>    | | <R.3> |  | <R.2> | |
    | | <R.4> |  | <R.1> | |          | | <R.4> |  | <R.1> | |
    | +-------+  +-------+ |          | +-------+  +-------+ |
    +----------------------+          +----------------------+

Default value for `fifo` is `false`.

### 计时

`max_wait`选项定义了一个时间, 当在资源池耗尽时调用`borrow`函数,并且 `when_exhausted_action` 设置为`block`时所等待的最大时间.

`max_idle_time` ,用来配置资源的最大空闲时间. 如果一个资源空闲的超过这个时间, 就会被销毁. 但至少要在资源池中保留`min_idle`个资源. 如果`max_idle_time`设置为`infinity`, 不会销毁任何空闲的资源.

## 资源池实例的维护

`pool_name`是一个原子, 多个进程可以使用该名字来访问资源池. `resource_factory`是一个负责创建和维护资源的模块名称. `resource_metadata`是一个包含资源初始化信息的对象. 该对象作为参数传递给`resource_factory`的每个函数来帮助维护一个资源.

### new

创建资源池

```erlang
{ok, Pid} = resource_pool:new(
    pool_name,          %% 资源池名称
    resource_factory,   %% 资源工厂
    resource_metadata   %% 资源元数据
)
```

### clear

清空资源池

```erlang
resource_pool:clear(pool_name)
```

### close

关闭资源池, 该函数终止资源池进程, 并销毁池中的所有资源

```erlang
ok = resource_pool:close(pool_name)
```

### 资源池统计

get_num_active,get_num_idle,get_number

## 资源工厂

## 示例

### MySQL Driver连接池

http://sourceforge.net/projects/erlmysql/

### Rabbit MQ 连接通道池

http://sourceforge.net/projects/erlpool/files/1.0.x/erl.resource.pool.example.zip/download

## 参考资料

1. https://erlangcentral.org/wiki/index.php?title=Resource_Pool