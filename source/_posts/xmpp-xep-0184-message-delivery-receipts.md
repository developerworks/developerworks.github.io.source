title: XMPP XEP-0184消息回执
categories:
  - Communication
tags:
  - xmpp
  - xep
toc: false
date: 2014-10-01 22:47:12
---

[XEP-0184][XEP-0184], 定义如下:

本规范定义了一个关于`消息送达收条`的XMPP协议扩展,消息的发送者可以要求消息的接收设备在接收到消息后发送一个确认消息(收条). 比如: 发邮件给对方可以要求一个邮件回执,以确定对方收到消息. 如果没有收到回执,可以重新发送.

<!--more-->

## 判断接收者是否支持消息回执协议

可以通过两种方式判断接收者是否支持消息回执协议

### 方法一


发送者发送一个IQ-get

```
<iq from='northumberland@shakespeare.lit/westminster'
    id='disco1'
    to='kingrichard@royalty.england.lit/throne'
    type='get'>
  <query xmlns='http://jabber.org/protocol/disco#info'/>
</iq>
```

接受者应答IQ-result

```
<iq from='kingrichard@royalty.england.lit/throne'
    id='disco1'
    to='northumberland@shakespeare.lit/westminster'
    type='result'>
  <query xmlns='http://jabber.org/protocol/disco#info'>
    <feature var='urn:xmpp:receipts'/>
  </query>
</iq>
```

如果包含`<feature var='urn:xmpp:receipts'/>`说明接收者支持消息回执协议.

### 方法二

通过 Entity Capabilities ([XEP-0115][XEP-0115])判断接收者是否支持消息回执


## 协议格式

消息的发送者为了让消息的接收者发送一个消息回执, 定义了一个专用的名称空间`urn:xmpp:receipts`, 在此名称空下又两个允许的元素:

- `<request/>` 包含在消息内,消息发送者期望知道消息是否已经被接收.
- `<received/>` 包含在确认消息中, 告知消息的发送者该消息已经接收.

发送者发送一个要求收条的消息

```
<message
    from='northumberland@shakespeare.lit/westminster'
    id='richard2-4.1.247'
    to='kingrichard@royalty.england.lit/throne'>
  <body>My lord, dispatch; read o'er these articles.</body>
  <request xmlns='urn:xmpp:receipts'/>
</message>
```

接收者应答,表示消息已收到

```
<message
    from='kingrichard@royalty.england.lit/throne'
    id='bi29sg183b4v'
    to='northumberland@shakespeare.lit/westminster'>
  <received xmlns='urn:xmpp:receipts' id='richard2-4.1.247'/>
</message>
```

如果发送者没有被授权查看接收者是否在线, 接收不应该发送回执确认. (在线对其隐身)

## XML模式定义

```
<?xml version='1.0' encoding='UTF-8'?>
<xs:schema
    xmlns:xs='http://www.w3.org/2001/XMLSchema'
    targetNamespace='urn:xmpp:receipts'
    xmlns='urn:xmpp:receipts'
    elementFormDefault='qualified'>
  <xs:annotation>
    <xs:documentation>
      The protocol documented by this schema is defined in
      XEP-0184: http://xmpp.org/extensions/xep-0184.html
    </xs:documentation>
  </xs:annotation>
  <xs:element name='received'>
    <xs:complexType>
      <xs:simpleContent>
        <xs:extension base='empty'>
          <xs:attribute name='id' type='xs:string' use='optional'/>
        </xs:extension>
      </xs:simpleContent>
    </xs:complexType>
  </xs:element>
  <xs:element name='request' type='empty'/>
  <xs:simpleType name='empty'>
    <xs:restriction base='xs:string'>
      <xs:enumeration value=''/>
    </xs:restriction>
  </xs:simpleType>
</xs:schema>
```
  [XEP-0184]: http://xmpp.org/extensions/xep-0184.html
  [XEP-0115]: http://xmpp.org/extensions/xep-0115.html