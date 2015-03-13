title: XMPP XEP-0065 SOCKS5 字节流
categories:
  - XEP
tags:
  - XMPP
  - XEP
toc: false
date: 2014-09-22 22:49:24
---

带外发送二进制数据/文件的两种方式

1. 实体间的端到端(P2P)连接
2. 代理/中继服务器

<!--more-->

## 直接发送数据

直接发送数据适用于你的PC直接连接到外网,并且本机有外部IP地址的情况

## 通过代理/中继服务器发送数据

当你在一个局域网内,或者防火墙内时, 外部网络不能直接连接到你的电脑, 需要使用NAT穿透来获取网关的外部IP/端口

## 直接还是中继

```
<iq from="alice@realworld.lit/home-at-last"
    id="uy461vfw"
    to="bestfriend@school.lit/laptop"
    type="set">
    <query xmlns="http://jabber.org/protocol/bytestreams" sid="dv917fb4" mode="tcp">
        <streamhost jid="alice@realworld.lit/home-at-last" host="192.168.4.1" port="5086"/>
        <streamhost jid="streamhostproxy.realworld.lit" host="24.24.24.1" port="5999"/>
    </query>
</iq>
```

接受者收到上述`iq`节时,会先尝试连接第一个`streamhost`, 如果不能连接再尝试第二个`streamhost`. 先尝试直接连接,再尝试中继


## 通过代理发送数据

发送端请求

```
<iq from="alice@realworld.lit/home-at-last"
    id="uy461vfw"
    to="bestfriend@school.lit/laptop"
    type="set">
    # sid 非常重要, 这是发起段生成的流ID, 用于标识收发两端在同一个流中.
    <query xmlns="http://jabber.org/protocol/bytestreams" sid="dv917fb4" mode="tcp">
        # 接收端首先会尝试连接第一个streamhost, 如果不可达,再连接第二个
        <streamhost jid="alice@realworld.lit/home-at-last" host="192.168.4.1" port="5086"/>
        <streamhost jid="streamhostproxy.realworld.lit" host="24.24.24.1" port="5999"/>
    </query>
</iq>
```

接收端应答,告知发送端选择哪个主机进行连接

```
<iq from="bestfriend@school.lit/laptop"
    id="uy461vfw"
    to="alice@realworld.lit/home-at-last"
    type="result">
    <query xmlns="http://jabber.org/protocol/bytestreams">
        # 选择代理连接方式
        <streamhost-used jid="streamhostproxy.realworld.lit"/>
    </query>
</iq>
```

当发送者受到应答后, 请求代理服务器激活流


```
<iq from="alice@realworld.lit/home-at-last"
    id="dl4wr217"
    to="streamhostproxy.realworld.lit"
    type="set">
    <!--这个query元素的sid属性标识了要激活的流-->
    <query
        xmlns="http://jabber.org/protocol/bytestreams"
        sid="dv917fb4">
        <activate>bestfriend@school.lit/laptop</activate>
    </query>
</iq>
```

如果请求被代理服务器确认, 返回一个确认应答后, 发送者就可以发送数据了.

上述我们是假设代理服务器可用的情况, 如果不能确定代理服务器是否可用, 可以通过发送一个服务查询来指导是否一个服务器支持SOCK5 字节流


```
<iq from="alice@realworld.lit/home-at-last" id="o6y1g48s"
    to="example.com"
    type="get">
    <query xmlns="http://jabber.org/protocol/disco#items"/>
</iq>
```

如果应答包含 ``

```
<iq from="xmpp.hezhiqiang.info" to="root@xmpp.hezhiqiang.info/3918842679141588400159153" id="id-1415884446759" type="result" xmlns="jabber:client" xmlns:stream="http://etherx.jabber.org/streams" version="1.0">
  <query xmlns="http://jabber.org/protocol/disco#items">
    ...
    <item jid="proxy.xmpp.hezhiqiang.info"/>
    ...
  </query>
</iq>
```