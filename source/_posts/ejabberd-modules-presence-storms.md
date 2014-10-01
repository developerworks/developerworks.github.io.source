title: 开发Ejabberd模块
categories:
  - Communication
tags:
  - ejabberd
  - xmpp
toc: true
date: 2014-09-18 17:43:00
---

原文: http://metajack.im/2008/08/28/writing-ejabberd-modules-presence-storms

使用 [ejabberd][1] 最大的好处是易于扩展其服务器的功能.

每个ejabberd模块需要实现`gen_mod`行为. erlang行为是一个函数集合, 实现了一个行为的模块必须支持行为定义的函数. `gen_mod`行为要求两个函数: `start/2`和`stop/`.当ejabberd服务器启动和停止模块的时候会调用对应的函数.

<!-- more -->

一个ejabberd模块的骨架如下:

```
-module(mod_sunshine).
-behavior(gen_mod).
-export([start/2, stop/1]).
start(_Host, _Opts) ->
    ok.
stop(_Host) ->
    ok.
```


## 钩子

- 钩子关联一组函数(函数链), 其中的函数可以是由模块注册的,也可以是又ejabberd的其他组件注册的, 比如: 会话管理器
- 和钩子相关的`函数链`中的函数有`优先级`
- 钩子相关事件触发的时候,ejabberd会按照`优先级顺序`执行函数链中的函数
- 函数链中的任意一个函数可以终止链中后续函数的执行

## 离线消息钩子

当一个用户向另一个离线用户发送消息是, `offline_message_hook` 钩子被触发, ejabberd 的会话管理器默认会向这个钩子的函数链的`末尾`添加一个函数, 这个函数会返回`service unavailable`错误.

ejabberd 提供的`mod_offline` 模块, 使用同样的钩子(`offline_message_hook`), 向数据库中存储离线消息, 当离线用户下次登录时会把之前存储的离线消息发送给这个登录的用户. 这是通过向 `offline_message_hook` 钩子添加一个`比会话管理器添加的优先级更高的函数`实现的.

当触发`offline_message_hook`钩子时, 该函数首先把离线消息存储到数据库中, 并告知`钩子处理器`终止在函数链中后续函数的执行, 这样, 会话管理器提供的错误处理函数就不会执行了.


## 出席钩子(出席 Presence),


出席(Presence)俗称用户上线(Online)

`set_presence_hook`

ejabberd 提供了许多钩子来让模块代码能在发生任何事件的时候能够处理这个事件. 该钩子在当用户向服务器发送了一个出席节(`<presence xmlns="jabber:client"/>`)时触发. 很常见的方法是在模块的`start/2`中添加钩子函数,并在`stop/1`中删除钩子函数,示例如下:

```
-module(mod_sunshine).
-behavior(gen_mod).
-export([start/2, stop/1, on_presence/4]).
start(Host, _Opts) ->
    ejabberd_hooks:add(set_presence_hook, Host, ?MODULE, on_presence, 50),
    ok.
stop(Host) ->
    ejabberd_hooks:delete(set_presence_hook, Host, ?MODULE, on_presence, 50),
    ok.
on_presence(_User, _Server, _Resource, _Packet) ->
    none.
```

钩子`set_presence_hook`中的函数接受4个参数,分别是: `用户`, `服务器`, `发送出席节的资源`(资源一般为某个账号的某个在线设备), 以及`实际的出席节`, 该钩子函数要求返回`none`.

## 实际的应用

这个模块要解决的实际问题是阻止`出席风暴(presence storm)`, 我们可以把出席风暴定义为: 一个用户在`interval`秒的间隔内重复发送同样的出席节`count`次. 现在并不知道`count`和`interval`的最佳值, 所以把它留着为ejabberd配置文件选项是最好的.

模块需要存储状态,以判断用户是否在发送出席风暴. 因为ejabberd没有传递状态给 `on_presence/4`, 所以我们必须自己追踪这个状态. 我们使用erlang内置的数据库`mnesia`来存储这个状态.

最后, 模块需要把那些判定为发送出席风暴的用户断开, 或者添加类似禁止登陆1小时的惩罚. 但是ejabberd并没有类似的API或消息, 因此必须把断开连接的代码添加到ejabberd内部的`c2s`模块.

有时候你完全搞不懂在erlang内部发生了了什么事情, 解决办法--打印运行信息到日志文件. ejabberd提供了几个不同日志等级的宏. 这些宏定义在`ejabberd.hrl`文件中. 如果要使用这些宏, 必须把`ejabberd.hrl`文件包含到要使用的模块中.

最常用的是 `?INFO_MSG`, 该函数接受一个`string()`类型的参数, 即一个一个参数列表, 如果无参数列表, 必须传递一个空列表`[]`.

```
-module(mod_sunshine).
-behavior(gen_mod).
-include("ejabberd.hrl").
-export([start/2, stop/1, on_presence/4]).
start(Host, _Opts) ->
    ?INFO_MSG("mod_sunshine starting", []), %% 打印日志
    ejabberd_hooks:add(set_presence_hook, Host, ?MODULE, on_presence, 50),
    ok.
stop(Host) ->
    ?INFO_MSG("mod_sunshine stopping", []),
    ejabberd_hooks:delete(set_presence_hook, Host, ?MODULE, on_presence, 50),
    ok.
on_presence(_User, _Server, _Resource, _Packet) ->
    none.
```

## 处理选项

要传递选项给`mod_sunshine`模块,我们需要修改ejabberd配置文件,并添加如下行到模块选项列表中:

```
{mod_sunshine, [{count, 10}, {interval, 60}]}
```

该选项告知`mod_sunshine`模块,如果任何人在`60`秒之内发送`10`次以上的相同的出席信节将被认为发生了出席风暴. 现在我们需要在模块代码中获取这些选项.

读者也许主要到了`start/2`在`Opts`变量中传入了选项. `on_presence/4`函数并没接受这些选项, 那么我们是怎么得到这些选项的呢?

`gen_mod`有一个API函数从配置文件中获取`模块选项` - `gen_mod:get_module_opt(Host, Module, Opt, Default)`. 该函数的第一个参数是一个`虚拟服务器域名`,它是在ejabberd配置文件`hosts`列表中定义的.

`gen_mod:get_module_opt/4` 还能让你定义默认选项值.

```
-module(mod_sunshine).
-behavior(gen_mod).
-include("ejabberd.hrl").
-export([start/2, stop/1, on_presence/4]).
%% @Host 虚拟服务器域名
%% @_Opts 定义在ejabberd配置文件中的模块选项
start(Host, _Opts) ->
    ?INFO_MSG("mod_sunshine starting", []),
    ejabberd_hooks:add(set_presence_hook, Host, ?MODULE, on_presence, 50),
    ok.
stop(Host) ->
    ?INFO_MSG("mod_sunshine stopping", []),
    ejabberd_hooks:delete(set_presence_hook, Host, ?MODULE, on_presence, 50),
    ok.
on_presence(_User, Server, _Resource, _Packet) ->
    %% get options
    StormCount = gen_mod:get_module_opt(Server, ?MODULE, count, 10),
    TimeInterval = gen_mod:get_module_opt(Server, ?MODULE, interval, 60),
    none.
```

## 使用 Mnesia 持久化状态

`mnesia`配合erlang的`record`,是最易使用的. Erlang 记录类似C语言的结构体. 它是具有名称字段的Erlang元组(tuple). 我们需要为`mod_sunshine`模块创建一个记录存储`用户(usr)`, `数据包(packet)`,`包发送的时间(start)`, 以及`包发送的次数(count)`. 该记录看似如下:

```
-record(sunshine, {usr, packet, start, count}).
```

该记录的名称为`sunshine`, 注意 `usr` 并不是 `user` 类型, 它实际上是一个包含`用户@服务器/资源`的`完全JID`的缩写.


## 创建 Mnesia 表

现在有了一个记录, 下面必须创建一个Mnesia表才能使用它.

创建和清空表:

```
start(Host, _Opts) ->
    ?INFO_MSG("mod_sunshine starting", []),
    mnesia:create_table(sunshine,
            [{attributes, record_info(fields, sunshine)}]),
    mnesia:clear_table(sunshine),
    ejabberd_hooks:add(set_presence_hook, Host, ?MODULE, on_presence, 50),
    ok.
```

`create_table/2` 不会删除存在的表, 因此需要使用`mnesia:clear_table/1`来把旧的数据清理掉.


## 读写数据库

Mnesia is a transactional database. To read and write to it, one normally uses `mnesia:transaction/1` which takes a function to execute inside the transaction.
This can be slow, so often `mnesia:dirty_read/2` is used to skip transactions for reads, and we will use that here.


Mnesia 是一个事务数据库. 要读和写,一般使用`mnesia:transaction/1`, 它接受一个函数在事务中运行的函数作为参数.

In order to keep the user, server, and resource key consistent, we must stringprep the username and server.
Among other things, this ensures that mixed case usernames get lower cased. This is easy in ejabberd since it provides a library of functions for JID manipulation called `jlib`.
To use it, we include `jlib.hrl` in our module.

Once we have a consistent key, we look it up in the `sunshine` table.
There are three possibilities: no record is found, a record is found for the current packet,
or a record is found for a different packet.

骨架如下.

```
on_presence(User, Server, Resource, Packet) ->
    %% get options
    StormCount = gen_mod:get_module_opt(Server, ?MODULE, count, 10),
    TimeInterval = gen_mod:get_module_opt(Server, ?MODULE, interval, 60),

    LUser = jlib:nodeprep(User),
    LServer = jlib:nodeprep(Server),

    case catch mnesia:dirty_read(sunshine, {LUser, LServer, Resource}) of
        [] ->
            %% no record for this key
            ok;
        [#sunshine{usr={LUser, LServer, Resource},
                   packet=Packet, start=_TimeStart, count=_Count}] ->
            %% record for this key and packet exists
            ok;
        [#sunshine{usr={LUser, LServer, Resource},
                   packet=_OtherPacket, count=_OtherCount}] ->
            %% a record for this key was found, but for another packet
            ok
    end,
    none.
```


Those new to Erlang are probably a little weirded out right now. Erlang uses pattern matching extensively.
First we read from the database, and attempt to match the pattern of the result against each section of the case statement.
If a variable in the pattern already contains a value, it must match the value in the result.
If a variable in the pattern does not have a value, it gets the value of the result in that spot.

If we get an empty list, there is no record matching the key. Note that we get a list as a result because some Mnesia table types support duplicate keys.
In our table, the result will always be an empty list or a list with one item.

The next two patterns match a row. `#sunshine{...}` is a record reference for the sunshine record. In the first of the two patterns,
all the variables have values except for `_TimeStart` and `_Count`. This means the result must be a record that matches the record for this user and this packet.
The second pattern matches a record for this user, but with any packet, as `_OtherPacket` is without a value.

Pattern matching is nice and powerful. Not only did we single out exactly the results we needed without any if statements,
we also already put the interesting fields into their own variables!

Now we just need to code the actions for each of these three cases.

In the first case, we create a new entry for that user and packet.

In the second case, we need to determine we're within `TimeInterval` seconds of `StartTime`. If not, we need to reset the count and start time for this user and packet.
Otherwise, if the count is above `StormCount`, the user is sending a presence storm and needs to be disconnected, and if the count is below `StormCount` we just increment the count.

其次,需要判断从`StartTime`开始到现在,是否在`TimeInterval`这个限定时间内, 如果超时, 需要重置计数器和开始时间, 如果在`TimeInterval`这个限定时间内用户发送出席节超过`StormCount`次,就断开客户端连接. 如果低于`StormCount`次, 那么仅仅增加这个计数器.

In the final case, we've received a different presence packet, so we need to overwrite the user's record in Mnesia with a new one for this packet.

Remember that Mnesia writes happen inside transactions as you look at the code below. The writes are put into anonymous functions which are then passed to `mnesia:transaction/1`
for execution inside a transaction.

Mnesia的写操作是在事务中进行的, 写操作被放到一个匿名函数中并传递给`mnesia:transaction/1`用于在事务中执行.

```
on_presence(User, Server, Resource, Packet) ->
    LUser = jlib:nodeprep(User),
    LServer = jlib:nodeprep(Server),
    {MegaSecs, Secs, _MicroSecs} = now(),
    TimeStamp = MegaSecs * 1000000 + Secs,
    %% get options
    StormCount = gen_mod:get_module_opt(Server, ?MODULE, count, 10),
    TimeInterval = gen_mod:get_module_opt(Server, ?MODULE, interval, 60),
    case catch mnesia:dirty_read(sunshine, {LUser, LServer, Resource}) of
        [] ->
            %% no record for this key, so make a new one
            F = fun() ->
                mnesia:write(#sunshine{usr = {LUser, LServer, Resource},
                packet = Packet,
                start = TimeStamp,
                count = 1})
            end,
            mnesia:transaction(F);
        [#sunshine{usr = {LUser, LServer, Resource},
            packet = Packet, start = TimeStart, count = Count}] ->
            %% record for this key and packet exists, check if we're
            %% within TimeInterval seconds, and whether the StormCount is
            %% high enough.  or else just increment the count.
            if TimeStamp - TimeStart > TimeInterval ->
                F = fun() ->
                    mnesia:write(#sunshine{usr = {LUser, LServer, Resource},
                    packet = Packet,
                    start = TimeStamp,
                    count = 1})
                end,
                mnesia:transaction(F);
                Count =:= StormCount ->
                    SID = ejabberd_sm:get_session_pid(LUser, LServer, Resource),
                    SID ! disconnect,
                    F = fun() ->
                        mnesia:delete({sunshine, {LUser, LServer, Resource}})
                    end,
                    mnesia:transaction(F);
                true ->
                    F = fun() ->
                        mnesia:write(#sunshine{usr = {LUser, LServer, Resource},
                        packet = Packet,
                        start = TimeStamp,
                        count = Count + 1})
                    end,
                    mnesia:transaction(F)
            end;
        [#sunshine{usr = {LUser, LServer, Resource},
            packet = _OtherPacket, count = _OtherCount}] ->
            %% a record for this key was found, but for another packet,
            %% so we replace it with a new record.
            F = fun() ->
                mnesia:write(#sunshine{usr = {LUser, LServer, Resource},
                packet = Packet,
                start = TimeStamp,
                count = 1})
            end,
            mnesia:transaction(F)
    end,
    none.
```

## 断开用户连接

The only thing left to do is disconnect the user when a presence storm is detected. I wish this was as easy as `ejabberd_sm:disconnect(User, Server, Resource)`,
 but it seems that the ejabberd developers have not yet added something along these lines. To solve this, we will use Erlang's message passing to notify
 the user's c2s process that it should disconnect the user.

After some exploring, I discovered you can get the c2s process identifier for a given user by calling `ejabberd_sm:get_session_pid/3` which takes the user,
server, and resource. Once we know the process identifer, we can send it a message with Erlang's `!` operator.

First let's finish out `on_presence/4` by replacing the placeholder comment with the `disconnect` message to the c2s process for the user, shown below.

最后一件事是当检测到出席风暴时断开用户连接. 我认为有`ejabberd_sm:disconnect(User, Server, Resource)`这样简单的方, 但是ejabberd的开发者并没有为我们提供这样的函数, 因此我们需要使用erlang的消息传递机制来通知用户的`c2s`进程断开该用户的连接.

可以通过`ejabberd_sm:get_session_pid/3`来获取用户的`c2s`进程断开用户连接. 该函数接受的三个参数分别是`用户`, `服务器` 和`资源`. 一旦我们获得进程标识符, 我们可以通过erlang的消息操作符`!`向其发送消息.

替换在`on_presence`/4中的注释:

```
%% TODO: disconnect user
```

为

```
%% disconnect the user
SID = ejabberd_sm:get_session_pid(LUser, LServer, Resource),
SID ! disconnect,
```

现在我们的模块编写完成. 但是`c2s`进程并不理发送的消息, 在文件`ejabberd_c2s.erl`中, 我们必须给`disconnect`消息提供一个新的消息处理器, 添加下面的语句到`ejabberd_c2s.erl`文件的`handle_info/3`中.

```
handle_info(disconnect, _StateName, StateData) ->
    send_text(StateData, ?STREAM_TRAILER),
    {stop, normal, StateData};
```

## 结语

We're all finished, and mod_sunshine can be deployed to stop the evil presence storms. To test that it works, just send the same presence stanza a bunch of times, really fast. You will find yourself quickly disconnected. You will probably need to write a quick test client for this as you might not be able to trigger duplicate presence with your normal XMPP client. I highly recommend Strophe.js for this task.

I hope that this tutorial has been helpful to you, and that you use this knowledge only for good. Go have fun implementing your wild and crazy ideas for server modules! If you have any suggestions and questions, please let me know in the comments.

The full source of `mod_sunshine.erl` is available `here`.



  [1]: http://www.ejabberd.im/
  [2]: http://metajack.im/2008/08/28/writing-ejabberd-modules-presence-storms/



