title: Elixir Mix 和 OTP 任务以及gen_tcp
categories:
  - Elixir
tags:
  - Mix
  - OTP
  - gen_tcp
toc: false
date: 2014-10-31 14:13:57
---


这章将学习如何使用Erlang的`:gen_tcp`模块处理请求. 后续章节我们会扩展服务器使之能处理命令. 还提供了一个极好的机会探索Elixir的`Task`模块

## Echo服务器

首先通过实现一个echo服务器,开始学习TCP服务器. 它只是把接收到的文本返回给客户端. 我们慢慢的改进服务器直到它能够处理多个连接.

一个TCP服务器, 大致会执行如下步骤:

- 监听端口并获得套接字
- 等待客户端连接该端口,并Accept.
- 读取客户端请求并回写响应

下面来实现这些步骤, 转到`apps/kv_server`应用程序, 打开`lib/kv_server.ex`,添加下面的函数:

    def accept(port) do
      # The options below mean:
      #
      # 1. `:binary` - receives data as binaries (instead of lists)
      # 2. `packet: :line` - receives data line by line
      # 3. `active: false` - block on `:gen_tcp.recv/2` until data is available
      #
      {:ok, socket} = :gen_tcp.listen(port,
                        [:binary, packet: :line, active: false])
      IO.puts "Accepting connections on port #{port}"
      loop_acceptor(socket)
    end
    defp loop_acceptor(socket) do
      {:ok, client} = :gen_tcp.accept(socket)
      serve(client)
      loop_acceptor(socket)
    end
    defp serve(client) do
      client
      |> read_line()
      |> write_line(client)
      serve(client)
    end
    defp read_line(socket) do
      {:ok, data} = :gen_tcp.recv(socket, 0)
      data
    end
    defp write_line(line, socket) do
      :gen_tcp.send(socket, line)
    end

调用`KVServer.accept(4040)`启动服务器, `4040`为端口. 在`accept/1`中第一步是监听端口直到获得一个可用的套接字, 然后调用`loop_acceptor/1`. `loop_acceptor/1`仅仅是循环地接受客户端连接. 对于每个接受的连接, 调用`serve/1`.

`serve/1`是另一个循环调用, 其从套接字读取一行数据并把读取到的行写回套接字. 注意函数`serve/1`使用管道操作符 `|>` 来表达操作流.管道操作符对左边的表达式求值并把结果作为右侧函数的第一个参数传递. 上面的例子:

    socket |> read_line() |> write_line(socket)

等同于:

    write_line(read_line(socket), socket)

当使用 `|>` 操作符时, 由于操作符优先级的问题, 给函数调用添加必要的括号是非常重要的, 特别是, 这段代码:

    1..10 |> Enum.filter &(&1 <= 5) |> Enum.map &(&1 * 2)

实际上会转换为:

    1..10 |> Enum.filter(&(&1 <= 5) |> Enum.map(&(&1 * 2)))

这不是我们想要的结果, 因为传递给`Enum.filter/2`的函数作为给`Enum.map/2`的第一个参数传递, 解决办法是使用括号:

    # 译注: 虽然Elixir的函数调用通常情况下可以不使用括号,
    # 但是为了避免歧义或不必要的问题,建议所有函数调用其他语言中必须的括号风格
    1..10 |> Enum.filter(&(&1 <= 5)) |> Enum.map(&(&1 * 2))

`read_line/1`函数实现使用`:gen_tcp.recv/2`从套接字接收数据,  `write_line/2`使用`:gen_tcp.send/2`向套接字写入数据.

使用命令`iex -S mix`在`kv_server`应用程序中启动一个iex会话. 在IEx中运行:

    iex> KVServer.accept(4040)

服务器现在开始运行, 终端被阻塞. 我们使用 `telnet`客户端访问我们的服务器. 它在大多数操作系统中都有, 其命令行参数通常类似:

    $ telnet 127.0.0.1 4040
    Trying 127.0.0.1...
    Connected to localhost.
    Escape character is '^]'.
    hello
    hello
    is it me
    is it me
    you are looking for?
    you are looking for?

键入`hello`, 并敲击回车, 你会的到服务器的`hello`响应, 太棒了!

我的telnet客户端可以通过键入`ctrl + ]`, `quit`, 并敲击 `<Enter>`退出, 你的客户端可能有不同的步骤:

退出telnet客户端后, 你可能会在IEx(Elixir Shell)会话中看到如下错误:

    ** (MatchError) no match of right hand side value: {:error, :closed}
        (kv_server) lib/kv_server.ex:41: KVServer.read_line/1
        (kv_server) lib/kv_server.ex:33: KVServer.serve/1
        (kv_server) lib/kv_server.ex:27: KVServer.loop_acceptor/1


这是因为我们期望从`:gen_tcp.recv/2`接收数据,但是客户端关闭了连接. 服务器后续的版本修订需要更好的处理这种情况.

现在有一个更重要的Bug要解决: 如果TCP acceptor崩溃会发生什么? 因为没有监视进程, 服务器异常退出并且不能处理更多后续的请求, 因为它没有重启. 这就是为什么必须把服务器放在监控树当中.

## Tasks

我们已经学习过了代理(Agents), 通用服务器(Generic Servers), 以及事件管理器(Event Managers), 它们全部是适合处理多个消息或管理状态. 但是, 当我们只需要执行一些任务时,我们使用什么?

[Task模块](http://elixir-lang.org/docs/stable/elixir/Task.html "Task模块")恰好提供了这个功能. 例如, 其有一个`start_link/3`函数, 其接受一个模块, 函数和参数, 作为监控树(Supervision tree)的一部分允许我们运行一个给定的函数.

让我们试一下. 打开`lib/kv_server.ex`, 修改`start/2`中的监控进程为如下:

    def start(_type, _args) do
      import Supervisor.Spec
      children = [
        worker(Task, [KVServer, :accept, [4040]])
      ]
      opts = [strategy: :one_for_one, name: KVServer.Supervisor]
      Supervisor.start_link(children, opts)
    end

With this change, we are saying that we want to run KVServer.accept(4040) as a worker.
We are hardcoding the port for now, but we will discuss ways in which this could be changed later.

现在我们向把`KVServer.accept(4040)`作为一个worker运行. 现在我们硬编码了端口号, 但我们将会讨论能在以后修改的方法.

现在服务器作为监控数的一部分, 当运行应用程序的时候它应该自动地启动. 在终端中键入命令`mix run --no-halt`, 再次使用`telnet`客户端验证一切仍能工作:

    $ telnet 127.0.0.1 4040
    Trying 127.0.0.1...
    Connected to localhost.
    Escape character is '^]'.
    say you
    say you
    say me
    say me

Yes, 仍然可以工作. 如果你杀掉客户端, 将导致整个服务器崩溃, 你会看到另一个服务器进程立即启动.

同时连接两个客户端, 再次测试, 你发现第二个客户端并没有echo:

    $ telnet 127.0.0.1 4040
    Trying 127.0.0.1...
    Connected to localhost.
    Escape character is '^]'.
    hello
    hello?
    HELLOOOOOO?

这是因为我们在同一个进程中处理接受连接并处理请求. 一个客户端连接后, 同一时刻就不能在接受其他的客户端的连接了, 直到之前的请求处理完成.


## 任务监视器(Task supervisor)

为了使服务器能处理并发连接, 需要一个进程作为acceptor, 生成(spawns)一个额外的进程来处理请求. 解决办法是修改下面的代码:

    defp loop_acceptor(socket) do
      {:ok, client} = :gen_tcp.accept(socket)
      serve(client)
      loop_acceptor(socket)
    end

使用 `Task.start_link/1`, 类似于 `Task.start_link/3`, 它接受一个匿名函数作为参数, 而非模块,函数,参数:

    defp loop_acceptor(socket) do
      {:ok, client} = :gen_tcp.accept(socket)
      Task.start_link(fn -> serve(client) end)
      loop_acceptor(socket)
    end

我们已经犯了一次这样的错误. 记得么?

这个错误类似于当我们从registry调用`KV.Bucket.start_link/0`所犯的错误. 在任何bucket中的失败将带来整个registry当机.

上面的胆码有同样的瑕疵: 如果我们连接`serve(client)`任务到acceptor, 当处理一个请求的时候将导致acceptor崩溃, 结果所有其他的连接,断开(down)

> We fixed the issue for the registry by using a simple one for one supervisor. We are going to use the same tactic here,
> except that this pattern is so common with tasks that tasks already come with a solution: a simple one for one supervisor with temporary workers that we can just use in our supervision tree!

~~使用一个`simple_one_for_one`监视进程(supervisor)解决这个问题. 我们将使用相同的策略,except that this pattern is so common with tasks that tasks already come with a solution: 可以在监控树种使用一个`simple_one_for_one`监视器和临时的workers.~~

再次修改`start/2`, 添加一个监视进程到进程树:

    def start(_type, _args) do
      import Supervisor.Spec

      children = [
        supervisor(Task.Supervisor, [[name: KVServer.TaskSupervisor]]),
        worker(Task, [KVServer, :accept, [4040]])
      ]

      opts = [strategy: :one_for_one, name: KVServer.Supervisor]
      Supervisor.start_link(children, opts)
    end

使用名称`KVServer.TaskSupervisor`启动一个[Task.Supervisor](http://elixir-lang.org/docs/stable/elixir/Task.Supervisor.html "Task.Supervisor")进程. 记住, 因为acceptor任务依赖此监视进程, 该监视检查必须首先启动.

现在只需要修改`loop_acceptor/2`使用`Task.Supervisor`来处理每一个请求:


    defp loop_acceptor(socket) do
      {:ok, client} = :gen_tcp.accept(socket)
      Task.Supervisor.start_child(KVServer.TaskSupervisor, fn -> serve(client) end)
      loop_acceptor(socket)
    end

使用命令`mix run --no-halt`启动一个新的服务器, 然后可以打开多个并发的telnet客户端连接. 你还注意到退出一个客户端后并不会导致acceptor崩溃. 太棒了!

这里是完整的echo服务器在单个模块中的实现:

    defmodule KVServer do
      use Application

      @doc false
      def start(_type, _args) do
        import Supervisor.Spec

        children = [
          supervisor(Task.Supervisor, [[name: KVServer.TaskSupervisor]]),
          worker(Task, [KVServer, :accept, [4040]])
        ]

        opts = [strategy: :one_for_one, name: KVServer.Supervisor]
        Supervisor.start_link(children, opts)
      end

      @doc """
      Starts accepting connections on the given `port`.
      """
      def accept(port) do
        {:ok, socket} = :gen_tcp.listen(port,
                          [:binary, packet: :line, active: false])
        IO.puts "Accepting connections on port #{port}"
        loop_acceptor(socket)
      end

      defp loop_acceptor(socket) do
        {:ok, client} = :gen_tcp.accept(socket)
        Task.Supervisor.start_child(KVServer.TaskSupervisor, fn -> serve(client) end)
        loop_acceptor(socket)
      end

      defp serve(socket) do
        socket
        |> read_line()
        |> write_line(socket)

        serve(socket)
      end

      defp read_line(socket) do
        {:ok, data} = :gen_tcp.recv(socket, 0)
        data
      end

      defp write_line(line, socket) do
        :gen_tcp.send(socket, line)
      end
    end

因为我们已经修改了supervisor规范, 我们需要问: 我们的supervision策略仍然正确么?

在这种情形下, 答案是yes: 如果acceptor崩溃, 并不会导致现有的连接中断.从另一方面讲, 如果任务监视器(task supervisor)崩溃, 也不会导致acceptor崩溃. 对比registry, 最初每次registry崩溃的时候也会导致supervisor崩溃, 直到使用ETS来对状态持久化.





