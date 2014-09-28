title: Ejabberd 编写一个简单的Echo服务模块
categories:
  - Communication
tags:
  - xmpp
  - ejabberd
toc: true
date: 2014-09-25 00:51:59
---

本文继续讨论Ejabberd的套接字架构,为了简化理解过程,一个一个Echo服务模块作为示例,它从客户端接收任何包,并把该包原封不动地回传给客户端.

<!--more-->

## 注册监听器

首先我们必须在ejabberd的配置文件注册这个服务模块

**ejabberd.cfg**

```
{listen,[
    %% ...
    {5555, echo_service, []},
    %% …
    ]}
```

我们的服务模块监听在`5555/tcp`上,我们为该端口编写一个处理程序,这个处理程序使用`gen_fsm`骨架代码

**echo_service.erl**

```
%%%-------------------------------------------------------------------
%%% @author Chi Zhang <elecpaoao@gmail.com>
%%% @copyright (C) 2012, Chi Zhang
%%% @doc
%%%  echo service demo
%%% @end
%%% Created : 24 May 2012 by Chi Zhang <elecpaoao@gmail.com>
%%%-------------------------------------------------------------------
-module(echo_service).
-behaviour(gen_fsm).
%% API
-export([start_link/2]).
-export([start/2,
         socket_type/0]).
%% gen_fsm callbacks
-export([init/1, state_name/2, state_name/3, handle_event/3,
     handle_sync_event/4, handle_info/3, terminate/3, code_change/4]).
-define(SERVER, ?MODULE).
-include("ejabberd.hrl").
-record(state, {sockmod, csock, opts}).
%%%===================================================================
%%% API
%%%===================================================================
start(SockData, Opts) ->
    start_link(SockData, Opts).
socket_type() ->
    raw.
start_link(SockData, Opts) ->
    gen_fsm:start_link(?MODULE, [SockData, Opts], []).
%%%===================================================================
%%% gen_fsm
%%%===================================================================
init([{SockMod, CSock}, Opts]) ->
    ?ERROR_MSG("start with sockmod: ~p csock: ~p opts: ~p", [SockMod, CSock, Opts]),
    State = #state{sockmod=SockMod, csock=CSock, opts=Opts},
    activate_socket(State),
    {ok, state_name, State}.
state_name(_Event, State) ->
    {next_state, state_name, State}.
state_name(_Event, _From, State) ->
    Reply = ok,
    {reply, Reply, state_name, State}.
handle_event(_Event, StateName, State) ->
    {next_state, StateName, State}.
handle_sync_event(_Event, _From, StateName, State) ->
    Reply = ok,
    {reply, Reply, StateName, State}.
handle_info({_, CSock, Packet}, StateName, #state{sockmod=SockMod}=State) ->
    ?ERROR_MSG("received: ~p", [Packet]),
    SockMod:send(CSock, Packet),
    activate_socket(State),
    {next_state, StateName, State};
handle_info({tcp_closed, _CSock}, _StateName, State) ->
    ?ERROR_MSG("client closed: ~p", [State]),
    {stop, normal, State};
handle_info(_Info, StateName, State) ->
    ?ERROR_MSG("received: ~p", [_Info]),
    {next_state, StateName, State}.
terminate(_Reason, _StateName, _State) ->
    ?ERROR_MSG("terminated ~p", [_Reason]),
    ok.
code_change(_OldVsn, StateName, State, _Extra) ->
    {ok, StateName, State}.
%%%===================================================================
%%% Internal functions
%%%===================================================================
activate_socket(#state{csock=CSock}) ->
    inet:setopts(CSock, [{active, once}]).
```

非常简单,对吧? 仅仅是修改`gen_fsm`的几行代码,我们就又了一个完全能够工作的`echo`服务

第一件需要注意的是`start/2`和`socket_type/0`调用, 为什么需要呢? 回顾一下`ejabberd_socket`:

**ejabberd_socket.erl**

```
start(Module, SockMod, Socket, Opts) ->
    case Module:socket_type() of
    xml_stream ->
        MaxStanzaSize =
        case lists:keysearch(max_stanza_size, 1, Opts) of
            {value, {_, Size}} -> Size;
            _ -> infinity
        end,
        {ReceiverMod, Receiver, RecRef} =
        case catch SockMod:custom_receiver(Socket) of
            {receiver, RecMod, RecPid} ->
            {RecMod, RecPid, RecMod};
            _ ->
            RecPid = ejabberd_receiver:start(
                   Socket, SockMod, none, MaxStanzaSize),
            {ejabberd_receiver, RecPid, RecPid}
        end,
        SocketData = #socket_state{sockmod = SockMod,
                       socket = Socket,
                       receiver = RecRef},
        case Module:start({?MODULE, SocketData}, Opts) of
        {ok, Pid} ->
            case SockMod:controlling_process(Socket, Receiver) of
            ok ->
                ok;
            {error, _Reason} ->
                SockMod:close(Socket)
            end,
            ReceiverMod:become_controller(Receiver, Pid);
        {error, _Reason} ->
            SockMod:close(Socket),
            case ReceiverMod of
            ejabberd_receiver ->
                ReceiverMod:close(Receiver);
            _ ->
                ok
            end
        end;
    independent ->
        ok;
    raw ->
        case Module:start({SockMod, Socket}, Opts) of
        {ok, Pid} ->
            case SockMod:controlling_process(Socket, Pid) of
            ok ->
                ok;
            {error, _Reason} ->
                SockMod:close(Socket)
            end;
        {error, _Reason} ->
            SockMod:close(Socket)
        end
    end.
```

如上所示, 如果`Module:socket_type()` 返回原子`raw`,那么这个模块将不会使用`ejabberd_receiver`, 这就是我们想要的效果.因为我们需要完全控制一切. 在`ejabberd_socket`传递套接字模块(`gen_tcp`)和套接字调用`Module:start/2`之后(第42行),再调用`SockMod:controlling_process`转发该套接字收到的任何消息给`Module:start/2`返回的`Pid`, 该进程为我们刚刚写的`echo_service` gen_fsm进程.

当`echo_service`fsm启动时,套接字为`被动模式`,意思是,在调用`recv()`函数之前它不会获得任何数据.当把套接字模式设置为`active`后就可以收取数据了.每次我们接收到一个新的包,生成和发送响应,并再一次设置套接字为`active`(文件`echo_service.erl`中第一个`handle_info/3`).

最后当接收到`{tcp_closed, Sock}`进程消息是不要忘记关闭FSM,防止进程泄漏, 这看起来足够合理了, 但是等等, 如果还想要echo_service同时工作在UDP上呢?

## 添加UDP传输

如此简单,下面来看看在ejabberd中如何处理UDP套接字:

**ejabberd_listener.erl**

```
init_udp(PortIP, Module, Opts, SockOpts, Port, IPS) ->
    case gen_udp:open(Port, [binary,
                 {active, false},
                 {reuseaddr, true} |
                 SockOpts]) of
    {ok, Socket} ->
        %% Inform my parent that this port was opened succesfully
     proc_lib:init_ack({ok, self()}),
        udp_recv(Socket, Module, Opts);
    {error, Reason} ->
        socket_error(Reason, PortIP, Module, SockOpts, Port, IPS)
    end.
udp_recv(Socket, Module, Opts) ->
    case gen_udp:recv(Socket, 0) of
    {ok, {Addr, Port, Packet}} ->
        case catch Module:udp_recv(Socket, Addr, Port, Packet, Opts) of
        {'EXIT', Reason} ->
            ?ERROR_MSG("failed to process UDP packet:~n"
                   "** Source: {~p, ~p}~n"
                   "** Reason: ~p~n** Packet: ~p",
                   [Addr, Port, Reason, Packet]);
        _ ->
            ok
        end,
        udp_recv(Socket, Module, Opts);
    {error, Reason} ->
        ?ERROR_MSG("unexpected UDP error: ~s", [format_error(Reason)]),
        throw({error, Reason})
    end.
```

不想tcp那样每个新的连接都要派生(spawning)一个新进程, udp套接字每个端口只需要一个进程来处理. 要使用UDP传输, 只需要简单地添加`udp_recv`到我们的模块:

**echo_service.erl**

```
udp_recv(Socket, Addr, Port, Packet, Opts) ->
    ?ERROR_MSG("udp receive: socket ~p addr ~p port ~p packet ~p opts ~p", [Socket, Addr, Port, Packet, Opts]),
    gen_udp:send(Socket, Addr, Port, Packet).
```

这在多数情况下足够了, 但是必须小心的时: 如果`Module:udp_recv/5`调用被阻塞,它将阻塞任何其他的数据被处理, 因此,在真实的应用场景中,要准备多个进程处理UDP请求!

## 使用自定义套接字选项

`ejabberd_listener`的监听选项在多数情况下是满足我们的需要的. 如果想要自定义套接字选项,而非默认选项,比如,想要从套接字接收任何数据之前设置`{packet,4}`? 容易!

首先在配置文件中添加该选项:

**ejabberd.cfg**

```
{listen,
    [
    %% ...
    {5556, echo_service, [{packet, 4}]},
    %% …
    ]}
```

然后,在`echo_service`模块中添加一个`setopts`步骤:

**echo_service.erl**

````
nit([{SockMod, CSock}, Opts]) ->
    ?ERROR_MSG("start with sockmod: ~p csock: ~p opts: ~p", [SockMod, CSock, Opts]),
    State = #state{sockmod=SockMod, csock=CSock, opts=Opts},
    set_opts(State),
    activate_socket(State),
    {ok, state_name, State}.
set_opts(#state{csock=CSock, opts=Opts}) ->
    Opts1 = lists:filter(fun(inet) -> false;
                ({packet, _}) -> true;
                ( _ ) -> false
             end, Opts),
    inet:setopts(CSock, Opts1).
```

我们添加了一个过滤去来过滤提供的选项, 仅允许设置有效的选项. 现在监听在`5556/tcp`的`echo`服务要求一个`4-octet`指定整个包的长度

## 总结

我们编写了一个非常建档的echo服务来学习如何使用ejabberd的套接字基础架构. 要编写一个简单的TCP服务, 我们仅需要实现`socket_type()`让它返回`raw`, 并在`Mod:start/2`中派生一个进程处理整个套接字. 要编写一个简单的UDP服务,我们仅需要提供一个`udp_recv/5`回调.

本文么有包括的事:

1. TLS传输 ? (提示: 使用包含在ejabberd中的tls模块)
2. 如何分离套接字数据接收和处理逻辑? (提示: 在`Mod:start`中启动并返回你自己的receiver)
3. 如何使用内置的`ejabberd_receiver`和`ejabberd_socket`?(提示: 让`socket_type/0`返回`xml_stream`)

上面的问题留作实践

## 参考资料

1. Writing a Simple Echo Service Module
http://codescv.logdown.com/posts/189840-writing-a-simple-echo-service-module