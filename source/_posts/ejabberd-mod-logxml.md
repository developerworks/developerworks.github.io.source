title: Ejabberd XML格式的XMPP消息日志模块
categories:
  - Communication
tags:
  - ejabberd
toc: false
date: 2014-10-15 16:33:20
---


```yaml mod_logxml.erl模块配置
modules:
  mod_logxml:
    #stanza: [message, other]
    #direction: [internal, vhosts, external]
    #orientation: [send, recv]
    logdir: "/var/log/ejabberd/xmllogs"
    #timezone: universal
    #rotate_days: 10
    rotate_megs: 1000000
    #rotate_kpackets: no
    #check_rotate_kpackets: 1
```

模块文件, 完整文件,请点击右侧`mod_logxml.erl`连接

```erlang Ejabberd mod_logxml模块,记录stanza到XML日志文件中 https://gist.github.com/developerworks/34bf6015f1f4890a210e mod_logxml.erl
-module(mod_logxml).
-author('badlop@ono.com').
-behaviour(gen_mod).
-export([
  start/2,
  init/7,
  stop/1,
  send_packet/3,
  receive_packet/4
]).
-include("ejabberd.hrl").
-include("logger.hrl").
-include("jlib.hrl").
-define(PROCNAME, ejabberd_mod_logxml).
start(Host, Opts) ->
  %% 日志存储目录
  Logdir = gen_mod:get_opt(logdir, Opts, fun(A) -> A end, "/tmp/jabberlogs/"),
  ?DEBUG("EJABBERD_MOD_LOGXML Logdir: ~p", [Logdir]),
  %% 日志滚动天数
  Rd = gen_mod:get_opt(rotate_days, Opts, fun(A) -> A end, 1),
  ?DEBUG("EJABBERD_MOD_LOGXML Rd: ~p", [Rd]),
  %% 日志滚动兆字节数,默认日志文件满10MB后创建新文件记录当前日志输出
  Rf = case gen_mod:get_opt(rotate_megs, Opts, fun(A) -> A end, 10) of
         no -> no;
         Rf1 -> Rf1 * 1024 * 1024
       end,
  ?DEBUG("EJABBERD_MOD_LOGXML Rf: ~p", [Rf]),
  %% 按接收到的XMPP数据包的数量滚动
  Rp = case gen_mod:get_opt(rotate_kpackets, Opts, fun(A) -> A end, 10) of
         no -> no;
         Rp1 -> Rp1 * 1000
       end,
  ?DEBUG("EJABBERD_MOD_LOGXML Rp: ~p", [Rp]),
  %% 日志滚动选项
  RotateO = {Rd, Rf, Rp},
  CheckRKP = gen_mod:get_opt(check_rotate_kpackets, Opts, fun(A) -> A end, 1),
  ?DEBUG("EJABBERD_MOD_LOGXML RotateO: ~p", [RotateO]),
  %% 时区配置选项
  Timezone = gen_mod:get_opt(timezone, Opts, fun(A) -> A end, local),
  ?DEBUG("EJABBERD_MOD_LOGXML Timezone: ~p", [Timezone]),
  %% 数据流方向
  Orientation = gen_mod:get_opt(orientation, Opts, fun(A) -> A end, [send, recv]),
  ?DEBUG("EJABBERD_MOD_LOGXML Orientation: ~p", [Orientation]),
  %% XMPP节
  Stanza = gen_mod:get_opt(stanza, Opts, fun(A) -> A end, [iq, message, presence, other]),
  ?DEBUG("EJABBERD_MOD_LOGXML Stanza: ~p", [Stanza]),
  %% 连接方向
  Direction = gen_mod:get_opt(direction, Opts, fun(A) -> A end, [internal, vhosts, external]),
  ?DEBUG("EJABBERD_MOD_LOGXML Direction: ~p", [Direction]),
  %% 过滤器选项
  FilterO = {
    {orientation, Orientation},
    {stanza, Stanza},
    {direction, Direction}},
  %% 是否显示IP地址
  ShowIP = gen_mod:get_opt(show_ip, Opts, fun(A) -> A end, false),
  ?DEBUG("EJABBERD_MOD_LOGXML ShowIP: ~p", [ShowIP]),
  %% 用户收发XMPP数据包钩子
  ejabberd_hooks:add(user_send_packet, Host, ?MODULE, send_packet, 90),
  ejabberd_hooks:add(user_receive_packet, Host, ?MODULE, receive_packet, 90),
  %% 进程注册
  register(
    %% 获取模块
    gen_mod:get_module_proc(Host, ?PROCNAME),
    spawn(?MODULE, init, [Host, Logdir, RotateO, CheckRKP, Timezone, ShowIP, FilterO])
  ).
...
...
...
省略
```

## 格式化输出

```python 格式化XML
#!/usr/bin/python
import tail
import xml.dom.minidom

def pretty_print(xml_string):
    fragment = xml.dom.minidom.parseString(xml_string)
    string = fragment.toprettyxml(indent="  ", newl="\n", encoding="utf-8")
    print(string)
# Create a tail instance
t = tail.Tail('xmpp.hezhiqiang.info.xml')
# Register a callback function to be called when a new line is found in the followed file.
# If no callback function is registerd, new lines would be printed to standard out.
t.register_callback(pretty_print)
# Follow the file with 5 seconds as sleep time between iterations.
# If sleep time is not provided 1 second is used as the default time.
t.follow(s=1)
```

## 参考资料

1. http://stackoverflow.com/questions/24012466/xmpp-traffic-logging-ejabberd-13-12
2. http://stackoverflow.com/questions/749796/pretty-printing-xml-in-python
3. https://github.com/kasun/python-tail
4. https://docs.python.org/2/library/xml.dom.minidom.html