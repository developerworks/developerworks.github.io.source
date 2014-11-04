title: Ejabberd 14.x版本系列模块开发过程中遇到的坑
categories:
  - Communication
  - Ejabberd
tags:
  - ejabberd
  - XMPP
toc: false
date: 2014-09-29 21:18:49
---

网上的资料多数都是`2.x`版本的, 从Ejabberd2.x系列迁移到14.x,路上遇到很多坑坑洼洼, 这里以一个`mod_cputime`模块为实例记录一下坑爹的过程.

## 自定义新的名称空间问题

如果你想要直接使用`mod_disco:register_feature/2`来创建一个名称空间,*那是不行的*,为什么不行, 下面我来细讲:

你会碰到各种`badxml`的错误. 从结果倒推,ejabberd应该是使用了`xmpp_codec.erl`来验证交互过程中传递的XML节的有效性,无效的统统抛弃,增强安全性,这样模块就不能随意增加名称空间.

设`$EJABBERD_SRC`为源码根目录.

- 在`14.07`中, 首先需要在`$EJABBERD_SRC/tools/xmpp_codec.spec`增加你需要扩展的名称空间, 例如:

```
-xml(cpu_time,
     #elem{name = <<"time">>,
           xmlns = <<"ejabberd:cputime">>,
           result = '$cdata',
           cdata = #cdata{label = '$cdata', required = true }}).
-xml(cpu,
    #elem{name = <<"query">>,
          xmlns = <<"ejabberd:cputime">>,
          result = {cpu, '$time'},
          refs = [#ref{name = cpu_time,
                       label = '$time',
                       min = 0, max = 1}]}).
```

- 然后在ejabberd的源代码根目录下执行`make spec`生成新的`xmpp_codec.hrl`,`xmpp_codec.erl`两个文件.

- `make && make install`

- `ejabberdctl restart`

还有一点要注意的时, 所有`#xmlel`, `#jid`, `#iq`这些记录中对应的XML标签名称, 属性名称, 以及CDATA的数据类型必须是类似`<<"iq">>`这种二进制字符串. 否则也会出现各种意想不到的错误.

Ejabberd的文档不多,也比较碎片化,为了避免掉进各种坑里面,还是多研究源码,多试错.

## 添加新名称空间iq处理模块

```
-module(mod_cputime).
-behaviour(gen_mod).
-export([
    start/2,
    stop/1,
    process_local_iq/3
]).
-include("ejabberd.hrl").
-include("jlib.hrl").
-include("logger.hrl").
-define(NS_CPUTIME, <<"ejabberd:cputime">>).
start(Host, Opts) ->
    IQDisc = gen_mod:get_opt(
        iqdisc,
        Opts,
        fun gen_iq_handler:check_type/1,
        one_queue
    ),
    mod_disco:register_feature(Host, ?NS_CPUTIME),
    gen_iq_handler:add_iq_handler(ejabberd_local, Host, ?NS_CPUTIME, ?MODULE, process_local_iq, IQDisc),
    ok.
stop(Host) ->
    mod_disco:unregister_feature(Host, ?NS_CPUTIME),
    gen_iq_handler:remove_iq_handler(ejabberd_local, Host, ?NS_CPUTIME),
    ok.
process_local_iq(_From, _To, #iq{type = Type, sub_el = SubEl} = IQ) ->
    case Type of
        set ->
            IQ#iq{type = error, sub_el = [SubEl, ?ERR_NOT_ALLOWED]};
        get ->
            CPUTime = element(1, erlang:statistics(runtime)) / 1000,
            SCPUTime = lists:flatten(io_lib:format("~.3f", [CPUTime])),
            Packet = #iq{type = result, sub_el = [
                #xmlel{name = <<"query">>, attrs = [{<<"xmlns">>, ?NS_CPUTIME}], children = [
                    #xmlel{name = <<"time">>, attrs = [], children = [{xmlcdata, list_to_binary(SCPUTime)}]}]}
            ]},
            Packet
    end.
```

把上面的代码放到`$EJABBERD_SRC/src`下面,然后`make && make install`,
在配置文件`/etc/ejabberd/ejabberd.yml`增加一个模块配置,如下:

```
##
## Modules enabled in all ejabberd virtual hosts.
##
modules:
  mod_cputime: {}
```

执行`ejabberdctl restart`重启ejabberd.

## 验证

客户端发起一个`IQ-get`请求

```
<iq type='get' to='xmpp.myserver.info' xmlns='jabber:client'>
  <query xmlns='http://jabber.org/protocol/disco#info'/>
</iq>
```

服务端`IQ-result`应答

![自定义名称空间的Features列表][1]


## 验证模块是否能够工作

客户端发起`IQ-get`请求一个CPU时间

```
<iq type='get' to='xmpp.myserver.info' xmlns='jabber:client'>
  <query xmlns='ejabberd:cputime'/>
</iq>
```

服务端`IQ-result`应答

```
<iq from="xmpp.myserver.info"
    to="root@xmpp.myserver.info/1507629570141186050149354"
    type="result"
    xmlns="jabber:client"
    xmlns:stream="http://etherx.jabber.org/streams"
    version="1.0">
  <query xmlns="ejabberd:cputime">
    <time>3.840</time>
  </query>
</iq>
```

客户端代码在[这里][2],包含三个文件, [下一篇][3]把[获取房间的用户列表][4]模块移植到`ejabberd 14.07`

## 要用到二进制字符串的地方

这里强调一下

- 定义名称空间的位置
```
define(NS_CPUTIME, <<"ejabberd:cputime">>).
```
- XML元素的属性名称和属性值
- `jlib:jid_to_string()`接受一个`#jid`或一个三元组,`User`,`Server`,`Resource`为字符串类型(Erlang中的字符串就是一个字符列表). 从`jlib.erl`中的定义我们可以看到,内部使用了`iolist_to_binary`对列表进行转换. 这里类型不要传错了

```
jid_to_string(#jid{user = User, server = Server,
		   resource = Resource}) ->
    jid_to_string({User, Server, Resource});
jid_to_string({N, S, R}) ->
    Node = iolist_to_binary(N),
    Server = iolist_to_binary(S),
    Resource = iolist_to_binary(R),
    S1 = case Node of
	   <<"">> -> <<"">>;
	   _ -> <<Node/binary, "@">>
	 end,
    S2 = <<S1/binary, Server/binary>>,
    S3 = case Resource of
	   <<"">> -> S2;
	   _ -> <<S2/binary, "/", Resource/binary>>
	 end,
    S3.
```



## 参考资料

1. Ejabberd Developers Guide
http://www.girlsnn.net/ejabberd/doc/dev.html#sec15
2. 获取用户房间列表
http://blog.zlxstar.me/blog/2013/07/21/dicorvery-user-muclist
3. Strophe.js API文档
http://strophe.im/strophejs/doc/1.1.3/files/strophe-js.html
4. Ejabberd IQ 处理程序
https://www.process-one.net/en/wiki/ejabberd_IQ_handlers/

  [1]: /assets/images/2EC981C0-DFFB-415F-BBA5-F767C4EB5DA0.png
  [2]: https://gist.github.com/developerworks/842fbc5b6092b3c5823e
  [3]: /2014/09/30/ejabberd-modules-get-room-member-list
  [4]: http://blog.zlxstar.me/blog/2013/07/21/dicorvery-user-muclist

