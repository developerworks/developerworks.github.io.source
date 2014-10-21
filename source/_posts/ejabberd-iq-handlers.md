title: Ejabberd-IQ节处理程序
categories:
  - Communication
tags:
  - ejabberd
  - iq
toc: false
date: 2014-09-28 23:28:55
---


Ejabberd内部模块可以注册自身,并使用指定名称空间处理IQ节, 其方法类似于[事件和钩子][1]这种机制.

模块[mod_last.erl][2]就使用了这种机制, 同时也使用了[事件和钩子][1]


## API

```
gen_iq_handler:add_iq_handler(Scope, Host, Namespace, Module, Function, IQDisc)
gen_iq_handler:remove_iq_handler(Scope, Host, Namespace, Module, Function, IQDisc)
* Scope = ejabberd_local | ejabberd_sm
* Namespace = string() (某些名称空间宏定义在jlib.hrl头文件中)
* Host = string()
* Module = atom()
* Fonction = atom()
* IQDisc = no_queue | one_queue | {queues, N} | parallel
* N = integer()
```

**ejabberd_local**作用域用于注册发到服务器本身的**IQ**.
**ejabberd_sm**作用域用于注册发送到一个账号的**纯JID**(Bare JID)的**IQ**,**Host**为与该**IQ**相关的虚拟主机名称.

**Module**和**Function**描述了一个处理器函数,当收到**指定名称空间的IQ节**时,该函数被调用.该处理器函数必须具有如下声明:

```
Module:Function(From, To, IQ) -> IQ
* From = To = #jid
* IQ = #iq
```


处理函数返回结果IQ (IQ-result), IQDisc描述当前IQ如何处理:

**no_queue**: 不创建新线程运行该处理程序
**one_queue**: 创建一个专用线程运行该处理程序
**{queues, N}**: 创建**N**个线程运行该处理程序
**parallel**: 对每一个接收到的IQ创建一个线程



## IQ处理模块模板

修改以适应Ejabberd **14.07**

```
-module(mod_iqtest).
-behaviour(gen_mod).
-export([start/2, stop/1,
  process_sm_iq/3 %% I assume this is needed to handle JID to JID communication of the IQ?
]).
-include("ejabberd.hrl").
-include("jlib.hrl").
-include("logger.hrl").
-define(NS_TEST, "http://jabber.org/protocol/test").
-define(NS_TEST2, "http://jabber.org/protocol/test2").
start(Host, Opts) ->
  IQDisc = gen_mod:get_opt(
    iqdisc,
    Opts,
    fun gen_iq_handler:check_type/1,
    one_queue
  ),
  gen_iq_handler:add_iq_handler(
    ejabberd_sm, Host, ?NS_TEST, ?MODULE, process_sm_iq, IQDisc
  ),
  gen_iq_handler:add_iq_handler(
    ejabberd_sm, Host, ?NS_TEST2, ?MODULE, process_sm_iq, IQDisc
  ),
  ?INFO_MSG("Loading module 'mod_iqtest' v.01", []).
stop(Host) ->
  gen_iq_handler:remove_iq_handler(
    ejabberd_sm, Host, ?NS_TEST
  ),
  gen_iq_handler:remove_iq_handler(
    ejabberd_sm, Host, ?NS_TEST2
  ),
  ?INFO_MSG("Stoping module 'mod_iqtest' ", []).
process_sm_iq(_From, _To, #iq{type = get, xmlns = ?NS_TEST} = IQ) ->
  ?INFO_MSG("Processing IQ Get query:~n ~p", [IQ]),
  IQ#iq{
    type = result, sub_el = [{xmlelement, "value", [], [{xmlcdata, "Hello World of Testing."}]}]
  };
process_sm_iq(_From, _To, #iq{type = get, xmlns = ?NS_TEST2} = IQ) ->
  ?INFO_MSG("Processing IQ Get query of namespace 2:~n ~p", [IQ]),
  IQ#iq{
    type = result, sub_el = [{xmlelement, "value", [], [{xmlcdata, "Hello World of Test 2."}]}]
  };
process_sm_iq(_From, _To, #iq{type = set} = IQ) ->
  ?INFO_MSG("Processing IQ Set: it does nothing", []),
  IQ#iq{
    type = result, sub_el = []
  };
process_sm_iq(_From, _To, #iq{sub_el = SubEl} = IQ) ->
  ?INFO_MSG("Processing IQ other type: it does nothing", []),
  IQ#iq{
    type = error, sub_el = [SubEl, ?ERR_FEATURE_NOT_IMPLEMENTED]
  }.
```
## 参考资料

1. IQ处理程序模板
https://www.ejabberd.im/node/5035?q=node/5035
2. Ejabberd事件和钩子
https://www.process-one.net/en/wiki/ejabberd_events_and_hooks
3. mod_last模块
https://github.com/processone/ejabberd/blob/master/src/mod_last.erl

  [1]: https://www.process-one.net/en/wiki/ejabberd_events_and_hooks
  [2]: https://github.com/processone/ejabberd/blob/master/src/mod_last.erl


