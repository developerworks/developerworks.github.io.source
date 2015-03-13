title: Ejabberd模块开发, 简介
categories:
  - Ejabberd
tags:
  - ejabberd
  - XMPP
toc: false
date: 2014-09-26 11:30:18
---

Ejabberd的内部模块以`插件`的方式工作. 每个模块是模块名称以`mod_`开头的`erlang模块`.

<!--more-->

## 模块API

所有内部模块`必须`使用`gen_mod`行为, 并且必须提供下列API

```
%% Host = string()
%% Opts = [{Name, Value}]
%% Name = Value = string()
start(Host, Opts) -> ok
stop(Host) -> ok
```

## Ejabberd核心模块

### 路由处理 (ejabberd_router)

该模块是`XMPP包路由`的主要模块, 根据`目标域`进行路由,它有两个表: 本地和全局的路由表. 每个包的目标域首先在`本地表`搜索.如果发现, 数据包被路由到适当的进程. 否则,它搜索的全局表,路由到适当的ejabberd节点或进程,如果两个表都找不到,数据包会发送到S2S管理器进行处理.

### 会话管理 (ejabberd_sm)

该模块路由数据包到本地用户.

#### API

`get_user_resources/2`

```
%% User = Server = string()
get_user_resources(User, Server)
```

返回一个已连接用户的所有资源

`dirty_get_sessions_list/0`

```
%% JID = {Username, Host, Resource}
%% Username = Host = Resource = string()
dirty_get_sessions_list() -> [JID]
```

返回打开的所有会话的JID


`get_vh_session_list/0`

返回一个虚拟主机的所有会话JID



## 钩子

Ejabberd实现了一个事件系统, 当发生特定事件的时候, 模块可以获得事件通知并执行特定的功能.


### API

```
%% Hook = atom()
%% Host = string() | global
%% Module = atom()
%% Function = atom()
%% Priority = integer()
ejabberd_hooks:add(Hook, Host, Module, Function, Priority)
ejabberd_hooks:delete(Hook, Host, Module, Function, Priority)
```

The `Hook` parameter is the name of the event (see below).
The `Host` parameter is the name of the `virtual host related to the event`, or the atom `global`. Some events are designed to run only hooks where Host is the atom `global`.
The `Module` and `Function` parameters describe the hook to be called when the event occurs.
The `Priority` parameter is the hook rank, to determine in which order the hooks are run (when several hooks are defined for the same event), use it if you have dependencies between hooks.

To stop ejabberd from calling other functions in that hook, your function must return the `atom()` `stop`. Another way to stop the call chain is to return tuple `{stop, Val}` to override the return value.

- `offline_message_hook`

- `set_presence_hook(User, Server, Resource, Priority)`

This hook is processed whenever a connected user sends a presence stanza to the server
当已连接用户发送一个出席节到服务器是,该钩子被处理.

该钩子接受4个参数, 分别是

