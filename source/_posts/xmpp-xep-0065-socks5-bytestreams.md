title: XMPP XEP-0065 SOCKS5 字节流
categories:
  - Communication
tags:
  - xmpp
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
<iq from="alice@realworld.lit/home-at-last" id="uy461vfw" to="bestfriend@school.lit/laptop" type="set">
  <query xmlns="http://jabber.org/protocol/bytestreams" sid="dv917fb4" mode="tcp">
    <streamhost jid="alice@realworld.lit/home-at-last" host="192.168.4.1" port="5086"/>
    <streamhost jid="streamhostproxy.realworld.lit" host="24.24.24.1" port="5999"/>
  </query>
</iq>
```

接受者收到上述`iq`节时,会先尝试连接第一个`streamhost`, 如果不能连接再尝试第二个`streamhost`. 先尝试直接连接,再尝试中继


