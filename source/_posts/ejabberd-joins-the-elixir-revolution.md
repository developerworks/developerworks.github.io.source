title: (译) Ejabberd加入了Elixir的变革
categories:
  - Ejabberd
tags:
  - Erlang
  - Elixir
toc: false
date: 2015-02-26 18:13:36
---

![Ejabberd加入了Elixir的变革](/assets/elixir/ejabberd_elixir.jpg)

原文:

https://blog.process-one.net/ejabberd-joins-the-elixir-revolution/

## 前提条件

安装Erlang R17

克隆Ejabberd源码添加Elixir支持.

```
git clone git@github.com:processone/ejabberd.git
cd ejabberd
chmod +x autogen.sh
./autogen.sh
./configure --prefix=$HOME/my-ejabberd --enable-elixir
make && make install
```

```
cd $HOME/my-ejabberd/
./sbin/ejabberdctl live

(ejabberd@localhost)1> m('Elixir.Enum').
Module 'Elixir.Enum' compiled: Date: January 24 2015, Time: 16.27
Compiler options:  [debug_info]
Object file: /Users/mremond/my-ejabberd/lib/ejabberd/ebin/Elixir.Enum.beam
...
```

## 用Elixir编写ejabberd插件

现在可以用Elixir来编写插件, 并注册钩子扩展ejabberd的功能.

把`*.ex`插件文件放到ejabberd的`lib`目录中, 让编译链知道如何编译它们.

在ejabberd的`lib/`目录中,添加如下文件`mod_presence_demo.ex`:

```elixir
defmodule ModPresenceDemo do
  @behaviour :gen_mod
  def start(_host, _opts) do
    :ok
  end
  def stop(_host) do
    :ok
  end
end
```

这是一个最小的ejabberd模块, 为了演示, 我们现在忽略`host`和`options`.

当你在Shell中敲入`make`, 模块应该能够正确的构建. 当执行`make install`时, 它会被安装到正确的位置.

现在可以在ejabberd配置文件`ejabberd.yml`中添加模块配置了, 向下面这样:

```
modules:
...
  ModPresenceDemo: {}
```

你可以直接使用模块的名称, Ejabberd会检测到这是一个Elixirmo模块,并能够正确的使用它.

下面来扩展一下这个模块的功能:

```elixir
defmodule ModPresenceDemo do
  import Ejabberd.Logger # this allow using info, error, etc for logging
  @behaviour :gen_mod
  def start(host, _opts) do
    info('Starting ejabberd module Presence Demo')
    Ejabberd.Hooks.add(:set_presence_hook, host, __ENV__.module, :on_presence, 50)
    :ok
  end
  def stop(host) do
    info('Stopping ejabberd module Presence Demo')
    Ejabberd.Hooks.delete(:set_presence_hook, host, __ENV__.module, :on_presence, 50)
    :ok
  end
  def on_presence(user, _server, _resource, _packet) do
    info('Receive presence for #{user}')
    :none
  end
end
```

当启动ejabberd的时候, 应该能够在日志中看到如下输出:

```
15:17:58.913 [info] Starting ejabberd module Presence Demo test
```

And anytime an XMPP client changes its presence, you should see the following in the log file:

任何时候, 如果客户端改变其在线状态(presence), 你可以看到如下输出:

```
15:30:01.266 [info] Receive presence for test
```
