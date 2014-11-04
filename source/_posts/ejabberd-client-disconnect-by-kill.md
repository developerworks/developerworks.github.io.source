title: Ejabberd客户连接状态变化的服务器日志分析
categories:
  - Communication
  - Ejabberd
tags:
  - ejabberd
toc: false
date: 2014-10-24 16:47:07
---

客户端进程被杀掉的服务器连接处理日志

{% raw %}
```
(ejabberd@localhost)1> 08:45:52.549 [info] ({socket_state,gen_tcp,#Port<0.5458>,<0.494.0>}) Close session for zsb@xmpp.hezhiqiang.info/zhangshuibodeMacBook-Pro
08:45:52.549 [info] mod_gbox_messager: client disconnected.
08:45:52.549 [info] mod_gbox_messager: sid: {{1414,140336,249563},<0.495.0>}
08:45:52.549 [info] mod_gbox_messager: jid: <<"zsb@xmpp.hezhiqiang.info/zhangshuibodeMacBook-Pro">>
08:45:52.549 [info] mod_gbox_messager: session info: [{ip,{{192,168,8,103},52041}},{conn,c2s},{auth_module,ejabberd_auth_internal}]
08:45:52.549 [info] mod_gbox_messager: client ip: "192.168.8.103"
08:45:52.549 [info] mod_gbox_messager: client port: 52041
08:45:52.549 [debug] node_action <<"pubsub.xmpp.hezhiqiang.info">> <<"flat">> get_entity_affiliations [<<"pubsub.xmpp.hezhiqiang.info">>,{<<"zsb">>,<<"xmpp.hezhiqiang.info">>,<<"zhangshuibodeMacBook-Pro">>}]
08:45:52.549 [debug] node_call <<"flat">> get_entity_affiliations [<<"pubsub.xmpp.hezhiqiang.info">>,{<<"zsb">>,<<"xmpp.hezhiqiang.info">>,<<"zhangshuibodeMacBook-Pro">>}]
08:45:52.549 [debug] node_action <<"pubsub.xmpp.hezhiqiang.info">> <<"hometree">> get_entity_affiliations [<<"pubsub.xmpp.hezhiqiang.info">>,{<<"zsb">>,<<"xmpp.hezhiqiang.info">>,<<"zhangshuibodeMacBook-Pro">>}]
08:45:52.549 [debug] node_call <<"hometree">> get_entity_affiliations [<<"pubsub.xmpp.hezhiqiang.info">>,{<<"zsb">>,<<"xmpp.hezhiqiang.info">>,<<"zhangshuibodeMacBook-Pro">>}]
08:45:52.549 [debug] node_action <<"pubsub.xmpp.hezhiqiang.info">> <<"pep">> get_entity_affiliations [<<"pubsub.xmpp.hezhiqiang.info">>,{<<"zsb">>,<<"xmpp.hezhiqiang.info">>,<<"zhangshuibodeMacBook-Pro">>}]
08:45:52.549 [debug] node_call <<"pep">> get_entity_affiliations [<<"pubsub.xmpp.hezhiqiang.info">>,{<<"zsb">>,<<"xmpp.hezhiqiang.info">>,<<"zhangshuibodeMacBook-Pro">>}]
08:45:52.549 [debug] tree_call <<"pubsub.xmpp.hezhiqiang.info">> get_node [<<"pubsub.xmpp.hezhiqiang.info">>,<<"http://jabber.org/protocol/tune">>]
08:45:52.549 [debug] disabling for <<"zsb">>
08:45:52.549 [info] MOD_GBOX_MESSAGER: unset_presence_hook: <<"zsb">><<"xmpp.hezhiqiang.info">><<"zhangshuibodeMacBook-Pro">><<>>
08:45:52.550 [debug] unset_presence for <<"zsb">> @ <<"xmpp.hezhiqiang.info">> / <<"zhangshuibodeMacBook-Pro">> -> <<>> (0 resources)
08:45:52.550 [debug] route
        from {jid,<<"zsb">>,<<"xmpp.hezhiqiang.info">>,<<"zhangshuibodeMacBook-Pro">>,<<"zsb">>,<<"xmpp.hezhiqiang.info">>,<<"zhangshuibodeMacBook-Pro">>}
        to {jid,<<"zsb">>,<<"xmpp.hezhiqiang.info">>,<<>>,<<"zsb">>,<<"xmpp.hezhiqiang.info">>,<<>>}
        packet {xmlel,<<"presence">>,[{<<"type">>,<<"unavailable">>}],[]}
08:45:52.550 [debug] local route
        from {jid,<<"zsb">>,<<"xmpp.hezhiqiang.info">>,<<"zhangshuibodeMacBook-Pro">>,<<"zsb">>,<<"xmpp.hezhiqiang.info">>,<<"zhangshuibodeMacBook-Pro">>}
        to {jid,<<"zsb">>,<<"xmpp.hezhiqiang.info">>,<<>>,<<"zsb">>,<<"xmpp.hezhiqiang.info">>,<<>>}
        packet {xmlel,<<"presence">>,[{<<"type">>,<<"unav"...>>}],[]}
08:45:52.550 [debug] session manager
        from {jid,<<"zsb">>,<<"xmpp.hezhiqiang.info">>,<<"zhangshuibodeMacBook-Pro">>,<<"zsb">>,<<"xmpp.hezhiqiang.info">>,<<"zhangshuibodeMacBook-Pro">>}
        to {jid,<<"zsb">>,<<"xmpp.hezhiqiang.info">>,<<>>,<<"zsb">>,<<"xmpp.hezhiqiang.info">>,<<>>}
        packet {xmlel,<<"presence">>,[{<<"type">>,<<"unav"...>>}],[]}
```
{% endraw %}

客户端正常退出,服务器会收到一个流关闭标记`</stream:stream>`(下图右侧第一行日志), 而客户端被杀死收到不到流关闭标记(左侧).
还未分析客户端连接超时或断开的情况.

![客户端正常退出和网络突然断开,服务器日志对比][1]



  [1]: /assets/images/CA275FA1-0736-445C-AFA1-959C88A4EA33.png