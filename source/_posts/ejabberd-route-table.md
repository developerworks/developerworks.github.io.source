title: Ejabberd-路由表
categories:
  - Communication
  - Ejabberd
tags:
  - ejabberd
  - XMPP
toc: false
date: 2014-09-29 00:10:06
---

ejabberd内部模块可以通过一个XMPP名称(`-define(PROCNAME, ejabberd_mod_echo).`),添加自身到服务器的路由表中,这些模块被称为`服务`,服务模块必须同时实现`gen_server`和`gen_mod`行为


## 例子
[mod_echo.erl][1]是一个使用路由机制的例子.

## API

```
ejabberd_router:register_route(Host)
ejabberd_router:unregister_route(Host),
* Host = string()
```

### gen_server API

下面上个函数用作模块的API

```
start_link(Host, Opts) ->
    Proc = gen_mod:get_module_proc(Host, ?PROCNAME),
    gen_server:start_link({local, Proc}, ?MODULE, [Host, Opts], []).
```

```
start(Host, Opts) ->
    Proc = gen_mod:get_module_proc(Host, ?PROCNAME),
    ChildSpec = {Proc,{?MODULE, start_link, [Host, Opts]}, temporary, 1000, worker, [?MODULE]},
    supervisor:start_child(ejabberd_sup, ChildSpec).
```

```
stop(Host) ->
    Proc = gen_mod:get_module_proc(Host, ?PROCNAME),
    gen_server:call(Proc, stop),
    supervisor:terminate_child(ejabberd_sup, Proc),
    supervisor:delete_child(ejabberd_sup, Proc).
```

### gen_server 回调

下面的函数必须定义并导出.

```
init([Host, Opts]) -> {ok, State} |
                      {ok, State, Timeout} |
                      ignore |
                      {stop, Reason}
```

```
handle_info(Info, State) -> {noreply, State} |
                            {noreply, State, Timeout} |
                            {stop, Reason, State}
* Info = {route, From, To, Packet}
* To = From = #jid (see jlib core module)
* Packet = {xmlelement, Name, Attrs, SubEl}
```

```
terminate(Reason, State) -> void()
```

```
handle_call(stop, _From, State) -> {stop, normal, ok, State}.
```

```
handle_cast(_Msg, State) -> {noreply, State}.
```

```
code_change(_OldVsn, State, _Extra) -> {ok, State}.
```

`init`函数用于初始化模块.`Host`为模块所在虚拟主机名称,`Opts`为在配置文件中设置的模块选项,这些选项可以通过**[gen_mod:get_opt/3][2]**函数获取.**[ejabberd_router:register_route/1][3]**函数在`init`回调中执行.

`terminate/2`用于停止模块. `ejabberd_router:unregister_route`函数在此回调中被调用.

`handle_info/2`用于获取发送给该模块的XMPP包. **[ejabberd_router:route/1][3]**用于对包进行重新路由.

All other callbacks can be written as shown above.








  [1]: https://github.com/processone/ejabberd/blob/master/src/mod_echo.erl
  [2]: https://www.process-one.net/en/wiki/gen_mod
  [3]: https://www.process-one.net/en/wiki/ejabberd_router

