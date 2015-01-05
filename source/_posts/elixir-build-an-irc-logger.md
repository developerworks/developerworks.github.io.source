title: Elixir | 构建一个IRC记录程序
categories:
  - Elixir
tags:
  - Elixir
toc: false
date: 2014-12-13 15:53:40
---

IRC日志记录程序主要解决时区的问题, 如果你的IRC客户端不是一直连接到IRC服务器, 那么可能错过很多精彩的讨论. 即使你一直开着你的IRC客户端,也可能由于网络的不问题导致IRC客户端断开.下面用Elixir开发的一个IRC日志程序用于记录IRC服务器各个频道的聊天记录, 可以通过日期查看所有订阅频道的聊天信息.

##

![IRC日志记录器用户界面](/assets/images/c429860617d7d0f4f5794903355570421decac1f_687474703a2f2f692e696d6775722e636f6d2f454471574562682e706e67.png)

## 先决条件

1. 需要安装Erlang OTP/17
2. 需要安装Elixir 1.0.1以上版本

## 创建项目

```elixir
$ mix new exile
* creating README.md
* creating .gitignore
* creating mix.exs
* creating config
* creating config/config.exs
* creating lib
* creating lib/exile.ex
* creating test
* creating test/test_helper.exs
* creating test/exile_test.exs
```

编译, (直接执行`make test`可以自动编译并测试)

```elixir
$ cd exile
mix test
```

## 添加第三方库

在`deps`函数添加第三方库`:socket`

```elixir
defmodule Exile.Mixfile do
  use Mix.Project
  def project do
    [app: :exile,
     version: "0.0.1",
     elixir: "~> 1.0",
     deps: deps]
  end
  def application do
    [applications: [:logger]]
  end
  defp deps do
    [
      {:socket, "~> 0.2.8"}
    ]
  end
end
```

下载第三方依赖库文件

```shell
$ mix deps.get
Running dependency resolution
Unlocked:   timex, socket
Dependency resolution completed successfully
  socket: v0.2.8
* Getting socket (Hex package)
Checking package (https://s3.amazonaws.com/s3.hex.pm/tarballs/socket-0.2.8.tar)
Using locally cached package
Unpacked package tarball (/Users/benjamintan/.hex/packages/socket-0.2.8.tar)
```

## 构建机器人程序

该机器人程序主要用于连接到IRC服务器,并箭筒IRC服务器的频道聊天信息, 并记录到数据库中.

### 创建bot.ex

`Exile.Bot`模块使用了`GenServer`行为, 用于构建通用服务器程序, 使用GenServer的优点是, 它可以被添加到Supervisor进程树中,对其状态进行监控.

```
defmodule Exile.Bot do
  use GenServer
end
```

### 实现需要的回调函数

```elixir
defmodule Exile.Bot do
  use GenServer
  @doc """
  调用start_link初始化,传递一个状态state, state是一个Map, 比如:
  %{
    host: "irc.freenode.net",
    port: 6667,
    chan: "#elixir-lang",
    nick: "elixir-bot",
    sock: socket
  }
  """
  def start_link(state) do
    GenServer.start_link(__MODULE__, state)
  end
  @doc """
  创建一个到IRC服务器的套接字连接
  """
  def init(state) do
    {:ok, sock} = Socket.TCP.connect(state.host, state.port, packet: :line)
    # 第三个参数0标识超时, 0为立即超时,并发生一个:timeout消息
    {:ok, %{state | sock: sock}, 0}
  end
  @doc """
  超时处理回调函数
  """
  def handle_info(:timeout, state) do
    IO.puts "TIMEOUT HANDLED"
    { :noreply, state }
  end
end
```

## 连接到IRC服务器

```elixir
def handle_info(:timeout, state) do
  state |> do_join_channel |> do_listen
  { :noreply, state }
end
```

## 进入一个频道

```elixir
defp do_join_channel(%{sock: sock} = state) do
  sock |> Socket.Stream.send!("NICK #{state.nick}\r\n")
  sock |> Socket.Stream.send!("USER #{state.nick} #{state.host} #{state.nick} #{state.nick}\r\n")
  sock |> Socket.Stream.send!("JOIN #{state.chan}\r\n")
  state
end
```

## 监听回复信息

进入频道后就可以通过下面的回调获取聊天信息

```elixir
defp do_listen(%{sock: sock} = state) do
  case state.sock |> Socket.Stream.recv! do
    data when is_binary(data)->
      case parse_message(data, sock) do
        message ->
          message
      end
      do_listen(state)
    nil ->
      :ok
  end
end
```

## 保持连接

## 处理其他消息



## 参考资料

1. Building an IRC Logger in Elixir
http://www.neo.com/2014/12/01/building-an-irc-logger-in-elixir