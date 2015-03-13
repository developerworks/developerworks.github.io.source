title: XMPP XEP-0095 流初始化协商过程
categories:
  - XEP
tags:
  - XMPP
toc: true
date: 2014-09-24 01:28:37
---

本规范定义了一个XMPP扩展,用于在两个XMPP实体间初始化数据流, 其用途包括:

1. 文件传输
2. 音频聊天
3. 视频会议

<!--more-->

## 特性发现

查询接受者的Disco信息

```
<iq type='get'
    to='receiver@jabber.org/resource'
    from='sender@jabber.org/resource'
    id='info1'>
  <query xmlns='http://jabber.org/protocol/disco#info'/>
</iq>
```

接受者返回特性列表


```
<iq type='result'
    to='sender@jabber.org/resource'
    from='receiver@jabber.org/resource'
    id='info1'>
  <query xmlns='http://jabber.org/protocol/disco#info'>
    ...
    <feature var='http://jabber.org/protocol/si'/>
    <feature var='http://jabber.org/protocol/si/profile/file-transfer'/>
    ...
  </query>
</iq>
```

    TODO::

## 示例

本示例一个实际的文件传输过程来说明文件传输的交互过程.

首先 hezhiqiang 发送了一个 `iq`节给 test1 告知 hezhiqiang 支持的两种流方法(`socks5 bytestream`, `in-bind bytestreams`)

```
<iq from='hezhiqiang@xmpp.myserver.info/hezhiqiang-2'
    to='test1@xmpp.myserver.info/hezhiqiang-macbookpro' type='set' id='purple69ca5fc7'>
  <si xmlns='http://jabber.org/protocol/si' id='purple69ca5fc8'
      profile='http://jabber.org/protocol/si/profile/file-transfer'>
    <file xmlns='http://jabber.org/protocol/si/profile/file-transfer' name='sery-lvs-cluster.pdf' size='532473'/>
    <feature xmlns='http://jabber.org/protocol/feature-neg'>
      <x xmlns='jabber:x:data' type='form'>
        <field var='stream-method' type='list-single'>
          <option>
            <value>http://jabber.org/protocol/bytestreams</value>
          </option>
          <option>
            <value>http://jabber.org/protocol/ibb</value>
          </option>
        </field>
      </x>
    </feature>
  </si>
</iq>
```

test1 收到消息后返回要使用的流方法

```
<iq xmlns="jabber:client" type="result"
    to="hezhiqiang@xmpp.myserver.info/hezhiqiang-2" id="purple69ca5fc7">
  <si xmlns="http://jabber.org/protocol/si">
    <feature xmlns="http://jabber.org/protocol/feature-neg">
      <x xmlns="jabber:x:data" type="submit">
        <field var="stream-method">
          <value>http://jabber.org/protocol/bytestreams</value>
        </field>
      </x>
    </feature>
  </si>
</iq>
```

发送者收到后再次向接受者通知在什么位置(地址/端口)发送数据.

```
<!--发送者-->
<iq from='hezhiqiang@xmpp.myserver.info/hezhiqiang-2'
    to='test1@xmpp.myserver.info/hezhiqiang-macbookpro' type='set' id='purple69ca5fc9'>
  <query xmlns='http://jabber.org/protocol/bytestreams' sid='purple69ca5fc8'>
    <streamhost jid='hezhiqiang@xmpp.myserver.info/hezhiqiang-2' host='192.168.8.104' port='57526'/>
    <streamhost jid='hezhiqiang@xmpp.myserver.info/hezhiqiang-2' host='172.16.40.1' port='57526'/>
    <streamhost jid='hezhiqiang@xmpp.myserver.info/hezhiqiang-2' host='192.168.172.1' port='57526'/>
    <streamhost jid='hezhiqiang@xmpp.myserver.info/hezhiqiang-2' host='公网地址隐藏' port='57526'/>
    <streamhost jid='proxy.xmpp.myserver.info' host='172.17.0.7' port='7777'/>
  </query>
</iq>
```

接受者一旦选择了一个地址, 那么就可以开始传输数据了.

```
<!--接收者, 收到该消息即开始发送文件-->
<iq from='test1@xmpp.myserver.info/hezhiqiang-macbookpro'
    to='hezhiqiang@xmpp.myserver.info/hezhiqiang-2' xml:lang='en' type='result' id='purple69ca5fc9'>
  <query xmlns='http://jabber.org/protocol/bytestreams'>
    <streamhost-used jid='hezhiqiang@xmpp.myserver.info/hezhiqiang-2'/>
  </query>
</iq>
```

    TODO::考虑文件传输在Node.js中的实现方法.