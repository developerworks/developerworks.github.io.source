title: Elixir | Mix 和 OTP - ETS
categories:
  - Elixir
tags:
  - Mix
  - OTP
  - ETS
toc: true
date: 2014-10-31 01:16:12
---

## ETS

每次我们需要查询一个`bucket`, 我们需要发送一个消息给`registry`. 在有些应用程序中这意味着`registry`也许变成瓶颈.

本章中,我们将学习学习ETS(Erlang Term Storage),以及如何把它用作一个缓存机制. 稍后我们将扩展其用途,持久化从监视进程到子进程的持久化数据,即使是在崩溃的时候.

> 警告! 别过早的使用ETS作为缓存! 记录并分析你的应用程序性能,识别哪部分是瓶颈. 这样你才知道是否应该使用缓存,以及应该缓存什么.一旦你决定了需求, 本章可作为一个如何使用ETS的例子.

### 作为缓存的ETS

ETS允许我们在内存表中存储任何Erlang/Elixir项式. 通过erlang的`:ets`模块处理ETS表:

```
iex> table = :ets.new(:buckets_registry, [:set, :protected])
8207
iex> :ets.insert(table, {"foo", self})
true
iex> :ets.lookup(table, "foo")
[{"foo", #PID<0.41.0>}]
```

当创建ETS表时, 要求两个必须的参数: 表的名称, 以及一组选项. 在可用的选项中,我们传递了表类型以及其访问规则. 我们选定了`:set`类型,表示其键在ETS表中是不能重复的.我们还设置了表的访问类型为`:protected`, 其含义为仅允许`创建该表的进程`可以对其进行写操作. 但允许所有其他进程对该ETS表进行读操作.

ETS表还可以有名称, 允许我们通过一个给定的名称来访问ETS表.

```
iex> :ets.new(:buckets_registry, [:named_table])
:buckets_registry
iex> :ets.insert(:buckets_registry, {"foo", self})
true
iex> :ets.lookup(:buckets_registry, "foo")
[{"foo", #PID<0.41.0>}]
```

让我们修改`KV.Registry`使用ETS表. 我们将使用与事件管理器, buckets supervisor相同的技术, 以及传递ETS表名称给`start_link`. 记住, 与服务器名称一样, 任何知道ETSB表名称的本地进程都可以方位该表.

打开`lib/kv/registry.ex`, 并修改其实现. 我们在被修改的部分添加了注释, 以标记我们所做的修改.

```
defmodule KV.Registry do
  use GenServer
  ## 客户端 API
  @doc """
  启动注册表.
  """
  def start_link(table, event_manager, buckets, opts \\ []) do
    # 1. 现在我们期望该表作为参数传递给服务器
    GenServer.start_link(__MODULE__, {table, event_manager, buckets}, opts)
  end
  @doc """
  查询存储在`table`中的`name`的bucket pid.
  Returns `{:ok, pid}` if a bucket exists, `:error` otherwise.
  """
  def lookup(table, name) do
    # 2. lookup now expects a table and looks directly into ETS.
    #    No request is sent to the server.
    case :ets.lookup(table, name) do
      [{^name, bucket}] -> {:ok, bucket}
      [] -> :error
    end
  end
  @doc """
  确保在`server`中有一个与给定的`name`相关的bucket.
  """
  def create(server, name) do
    GenServer.cast(server, {:create, name})
  end
  ## 服务器回调
  def init({table, events, buckets}) do
    # 3. We have replaced the names HashDict by the ETS table
    ets  = :ets.new(table, [:named_table, read_concurrency: true])
    refs = HashDict.new
    {:ok, %{names: ets, refs: refs, events: events, buckets: buckets}}
  end
  # 4. The previous handle_call callback for lookup was removed
  def handle_cast({:create, name}, state) do
    # 5. Read and write to the ETS table instead of the HashDict
    case lookup(state.names, name) do
      {:ok, _pid} ->
        {:noreply, state}
      :error ->
        {:ok, pid} = KV.Bucket.Supervisor.start_bucket(state.buckets)
        ref = Process.monitor(pid)
        refs = HashDict.put(state.refs, ref, name)
        :ets.insert(state.names, {name, pid})
        GenEvent.sync_notify(state.events, {:create, name, pid})
        {:noreply, %{state | refs: refs}}
    end
  end
  def handle_info({:DOWN, ref, :process, pid, _reason}, state) do
    # 6. Delete from the ETS table instead of the HashDict
    {name, refs} = HashDict.pop(state.refs, ref)
    :ets.delete(state.names, name)
    GenEvent.sync_notify(state.events, {:exit, name, pid})
    {:noreply, %{state | refs: refs}}
  end
  def handle_info(_msg, state) do
    {:noreply, state}
  end
end
```

注意:
在修改`KV.Registry.lookup/2`实现从服务器请求之前, 暂时直接从ETS表中读取, 多进程共享. 这是我们要实现的缓存机制的主要思路.

为了使缓存机制可以工作, 创建的ETS表需要有 `:protected`(默认), 这样所有的客户端才能够读取,并且仅有`KV.Registry`进程能够写. 当启动的时候,我们还设置了`read_concurrency: true`,以优化表在并发读操作场景下的性能.

The changes we have performed above have definitely broken our tests. For starters,
there is a new argument we need to pass to `KV.Registry.start_link/3`. Let's start amending our tests in `test/kv/registry_test.exs` by rewriting the `setup` callback: