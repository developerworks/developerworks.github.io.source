title: Ejabberd 套接字基础架构
categories:
  - Communication
  - Ejabberd
tags:
  - XMPP
  - ejabberd
  - socket
toc: false
date: 2014-09-24 23:20:04
---

Ejabberd 是一个网络服务器, 那么套接字必然是通信的基础. 通过本文我们要搞清楚下面几个问题:

1. Ejabberd 是如何工作的
2. 如何Hack它获取自定义的特性

本文分析的源码版本基于Ejabberd 2.1.10

<!--more-->

## 套接字基础结构

第一步的工作是阅读源代码, Ejabberd 是一个大项目,近10万行Erlang代码, 不可能一次性能够完全理解. 如阅读一本书一样, 首先要搞清楚核心思想.

XMPP服务器的核心思想是什么? 我们先来回顾一个典型的XMPP会话的核心使用方法:

1. Alice 在她的电脑上启动了一个XMPP客户端, 该客户端建立了一个到 `xmpp.example.org:5222` 的一个TCP连接.
2. 通过交换XMPP Stanza, 服务器对客户端进行认证
3. Alice 使用XMPP客户端与服务器交换 `message/presence/iq`, 完成诸如聊天, 上线通知等任务.

## 绑定监听端口

在Ejabberd中,整个服务器打包为一个OTP应用,与ejabberd套接字基础架构相关的模块是:

- `ejabberd_listener`
- `ejabberd_socket`
- `ejabberd_receiver`

`ejabberd_listener`监听在配置文件中指定的所有端口, 为每个端口生成一个套接字并监听在每个端口上. 端口绑定完成后, Ejabberd 启动两个进程: `ejabberd_receiver` 和 `业务逻辑模块` (`ejabberd_c2s`->`5222`). `ejabberd_receiver`负责接收进入的包,并转发给`业务逻辑模块`, 业务逻辑模块然后对包进行解析和处理, 然后调用`ejabberd_socket`去发送响应和请求.

我们从一个应用模块开始分析:

**ejabberd_app.erl**

```
start(normal, _Args) ->
    Sup = ejabberd_sup:start_link(),
    ejabberd_listener:start_listeners().
```

在 `ejabberd_sup:start_link/0`中, `ejabberd_listener:start_link/0`被调用:

**ejabberd_sup.erl**

```
init([]) ->
    Listener = {
        ejabberd_listener,{
            ejabberd_listener, start_link, []
        },
        permanent,
        infinity,
        supervisor,
        [ejabberd_listener]
    }.
```

在 `ejabberd_listener:init/0`中, 根据配置文件绑定tcp和ucp端口:

**ejabberd_listener.erl**

```
init(_) ->
    ets:new(listen_sockets, [named_table, public]),
    bind_tcp_ports(),
    {ok, { {one_for_one, 10, 1}, []}}.
bind_tcp_ports() ->
    case ejabberd_config:get_local_option(listen) of
        Ls ->
            lists:foreach(fun({Port, Module, Opts}) ->
                bind_tcp_port(Port, Module, Opts)
            end,Ls)
    end.
bind_tcp_port(PortIP, Module, RawOpts) ->
    %% portip has the following format: {5222, {0,0,0,0},tcp}
    {Port, IPT, IPS, IPV, Proto, OptsClean} = parse_listener_portip(PortIP, RawOpts),
    _Opts, SockOpts} = prepare_opts(IPT, IPV, OptsClean),
    %% save parsed listener options into ets table
    listen_tcp(PortIP, Module, SockOpts, Port, IPS).
listen_tcp(PortIP, Module, SockOpts, Port, IPS) ->
    gen_tcp:listen(Port, [binary,
        {packet, 0},
        {active, false},
        {reuseaddr, true},
        {nodelay, true},
        {send_timeout, ?TCP_SEND_TIMEOUT},
        {keepalive, true} |
        SockOpts]).
```

在 `ejabberd_listener:init/0` 初始化过程完成后, 所有在配置文件中指定的端口打开(处于监听状态), 但还未接收任何进入的链接.

## 接受进入的连接

接下来,在`ejabberd_listener:start_listeners/0`中套接字开始接受进入的连接:

**ejabberd_listener.erl**

```
start_listeners() ->
    %% load listeners config from ets table
    Ls2 = lists:map(
        fun({Port, Module, Opts}) ->
            start_listener(Port, Module, Opts)
        end
    end, Listeners).
start_listener(Port, Module, Opts) ->
    ChildSpec = {Port,
         {?MODULE, start, [Port, Module, Opts]},
         transient,
         brutal_kill,
         worker,
         [?MODULE]},
    supervisor:start_child(ejabberd_listeners, ChildSpec).
start(Port, Module, Opts) ->
    proc_lib:start_link(?MODULE, init, [Port, Module, Opts]).
init(PortIP, Module, RawOpts) ->
    {Port, IPT, IPS, IPV, Proto, OptsClean} = parse_listener_portip(PortIP, RawOpts),
    {Opts, SockOpts} = prepare_opts(IPT, IPV, OptsClean),
    init_tcp(PortIP, Module, Opts, SockOpts, Port, IPS)
init_tcp(PortIP, Module, Opts, SockOpts, Port, IPS) ->
    ListenSocket = listen_tcp(PortIP, Module, SockOpts, Port, IPS),
    proc_lib:init_ack({ok, self()}),
    accept(ListenSocket, Module, Opts).
accept(ListenSocket, Module, Opts) ->
    case gen_tcp:accept(ListenSocket) of
    {ok, Socket} ->
        ejabberd_socket:start(Module, gen_tcp, Socket, Opts),
        accept(ListenSocket, Module, Opts);
    {error, Reason} ->
        accept(ListenSocket, Module, Opts)
    end.
```

在上面的代码中, `start_listeners/0`为每个监听的端口派生了一个新的进程,并将每个派生的进程链接到`ejabberd_listeners`监视进程(`supervisor`:包工头,啥事儿不干,专门监视工人干活的,一但有工人罢工,就另外找个新的来接替)

如果你仔细阅读代码, 会发现套接字监听了两次(一次在`bind_tcp_ports/0`, 一次在`start_listeners/0`). 不知道为什么要这么做, 如果我删除了其中一个, 它也能工作的很好.

## 开始处理请求

**ejabberd_socket.erl**

```
start(Module, SockMod, Socket, Opts) ->
    ReceiverMod = ejabberd_receiver,  %% see explanation
    RecPid = ReceiverMod:start(Socket, SockMod, none, MaxStanzaSize),
    SocketData = #socket_state{sockmod = SockMod, socket = Socket, receiver = RecPid},
    case Module:start({?MODULE, SocketData}, Opts) of
        {ok, Pid} ->
            SockMod:controlling_process(Socket, Receiver) of
            ReceiverMod:become_controller(Receiver, Pid);
        {error, _Reason} ->
            ReceiverMod:close(Receiver);
    end;
```

当一个新套接字被`accepted`,调用`ejabberd_socket:start/4`处理套接字事件.`ejabberd`把这个任务划分成两个子任务: 一个处理所有的数据传输(发送和接收,封装底层的套接字实现),另一个解析和处理消息.

因此, 派生了一个接收进程(`ReceiverMod`)和一个逻辑处理进程(`Module`). 数据接收模块默认是`ejabberd_receiver`, 它接收XML Stanza, 并转发Stanza给业务逻辑模块

根据任务的不同业务逻辑模块可以是`ejabberd_c2s`或则`ejabberd_s2s`.

`SockMod:controlling_process(Socket, Receiver)` 所作的工作是把发送给套接字的所有数据转发给`receiver`模块处理. 业务逻辑模块启后, `ReceiverMod:become_controller(Receiver, Pid)`被调用让receiver知道把消息转发到哪里.

当调套接字accepted后, ejabberd 启动一个receiver和一个handler, receiver处理所有进入的数据, 做一些`预处理`并转发给handler, handler决定如何处理消息, 并且(也许)使用`ejabberd_socket`发送响应数据.

如果想要扩展ejabberd使用其他非XMPP协议: 这就是起点.
编写一个自定义的`receiver`模块解析协议, 一个自顶一个`handler`模块处理所有请求.

## 总结

We have taken a quick tour through the socket infrastructure in ejabberd. We learned that the ejabberd uses three modules: ejabberd_listener, ejabberd_socket and ejabberd_receiver to handle all the socket related stuff. ejabberd_listener binds and listens on ports, ejabberd_socket starts the receiver and the handler, and provides utils for outgoing data, the receiver handles and parses all incoming data, and forwards messages to the logic module. The logics, on the other handle, are handled by logic modules accroding to the config file. There are, however, many things that we left out, including:

customization of receiver/logic modules.
congestion control (shapers).
how the logic modules interacts with the socket infrastructure.

## 参考资料

1. The Ejabberd Socket Infrastructure
http://codescv.logdown.com/posts/189839-ejabberd-notes-the-listener-infrastructure

