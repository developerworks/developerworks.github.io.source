title: XMPP XEP-0085 聊天状态通知
categories:
  - Communication
tags:
  - xmpp
  - xep-0085
  - strophe.js
  - ejabberd
toc: false
date: 2014-09-20 13:53:27
---

该扩展协议比较容易理解, 在我们最常用的多数IM即时聊天工具都能看到对方是否正在输入. 可以让我们知道对方对此次会话的关注情况. 该扩展协议解决了即时聊天中记的几个问题:

1. 这个参与者是否已经停止了输入？
2. 这个参与者是否注意这次聊天？
3. 这个参与者是否暂时没有激活这个会话(换言之，此时没有注意这次聊天)？
4. 这个参与者是否已离开(也就是说不再参与这次聊天了)？

<!-- more -->

XEP-0085 支持5中状态,分别是: `active`,`composing`,`inactive`,`gone`,`paused`, 其定义的[XML 名称空间定义][1]为:

```
<?xml version='1.0' encoding='UTF-8'?>

<xs:schema
    xmlns:xs='http://www.w3.org/2001/XMLSchema'
    targetNamespace='http://jabber.org/protocol/chatstates'
    xmlns='http://jabber.org/protocol/chatstates'
    elementFormDefault='qualified'>
  <xs:annotation>
    <xs:documentation>
      The protocol documented by this schema is defined in
      XEP-0085: http://www.xmpp.org/extensions/xep-0085.html
    </xs:documentation>
  </xs:annotation>
  <xs:element name='active' type='empty'/>
  <xs:element name='composing' type='empty'/>
  <xs:element name='gone' type='empty'/>
  <xs:element name='inactive' type='empty'/>
  <xs:element name='paused' type='empty'/>
  <xs:simpleType name='empty'>
    <xs:restriction base='xs:string'>
      <xs:enumeration value=''/>
    </xs:restriction>
  </xs:simpleType>
</xs:schema>
```

聊天状态通知的message节格式为:

```
<message from="hezhiqiang@xmpp.hezhiqiang.info/hezhiqiang-2"
       to="root@xmpp.hezhiqiang.info"
       type="chat"
       id="purple3811e7aa"
       xmlns="jabber:client"
       xmlns:stream="http://etherx.jabber.org/streams"
       version="1.0">
    <active xmlns="http://jabber.org/protocol/chatstates"/>
</message>
```

## 参考资料

1. XEP-0085 中文
http://wiki.jabbercn.org/XEP-0085
2. XEP-0085 英文
http://xmpp.org/extensions/xep-0085.html



  [1]: http://www.xmpp.org/schemas/chatstates.xsd

