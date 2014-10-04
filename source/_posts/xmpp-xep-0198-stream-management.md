title: XMPP XEP-0198流管理 - 协议
categories:
  - Communication
tags:
  - xmpp
  - stream management
  - xep-0198
toc: true
date: 2014-10-03 04:07:43
---

本文包括两个部分

- 本规范引用自如下连接并对其**修订**,**校对**,**补充**
[http://blog.csdn.net/yuedong56/article/details/38120101][2].

## 介绍

[XMPP Core][3] 用XMPP定义了流的XML技术(也就是流的建立和终止,包括认证和加密).但是核心的XMPP协议并没有为管理一个灵活的XML流提供工具.

流管理背后的基本概念是,初始化的实体(一个服务端或者客户端)和接收的实体(一个服务端)可以为更灵活的管理stream交换`命令`.下面两条流管理的特性被广泛的关注,因为它们可以提高网络的可靠性和终端用户的体验:

- Stanza确认(Stanza Acknowledgements) -- 能够确认一段或者一系列Stanza是否已被某一方接收.
- 流恢复(Stream Resumption) -- 能够迅速的恢复(resume)一个已经被终止的流.

流管理用较的短XML元素实现了这些特性,这些XML元素是在流的标准上的.这些元素并不是XMPP意义上的`stanzas`(也就是说,不是`<iq/>`,`<message/>`,或`<presence/>`这样的`stanzas`,`stanzas`在**RFC 6120**中有定义),它们不会在流管理中被`counted`或者被`acked`,因为它们是为管理stanzas本身而存在的.

流管理是在XML流的标准上使用的.检查一个给定的流TCP的连通性的时候,特别推荐使用`whitespace keepalives`(见**RFC 6120**),[XMPP Ping (XEP-0199)][4]或者`TCP keepalives`.对比流管理,高级消息处理[Advanced Message Processing (XEP-0079)][5]和消息回执[Message Delivery Receipts (XEP-0184)][6],定义了`Ack`,它可以通过多个流实现端对端的传输;这些特性在一些特殊情况中是有用的,但是没必要去检查在两个xmpp实体之间直接传递的流.

注:流管理可以用于服务端到服务端,客户端到服务端的流.但是,为了方便,本规范只讨论客户端到服务端的流.同样的原则也适用于服务器到服务器的流.(在本文档中,以**C:**开头的都是由客户端发送的,由**S:**开头的都是由服务器发送的).

<!--more-->

## 流特性

服务端返回一个流的header给客户端,紧接着要发送流的features,这些features包括一个名称空间为`urn:xmpp:sm:3`的`<sm/>`元素(见Namespace Versioning 关于版本号增加的可能性).

注:除非客户端已经通过验证并且绑定了一个资源,否则客户端不能进行流管理协商;见下面的特定的限制条件.

**例1:服务器发送新的流的header和流的features.**

```
S: <stream:stream
       from='example.com'
       xmlns='jabber:client'
       xmlns:stream='http://etherx.jabber.org/streams'
       version='1.0'>
```

```
S: <stream:features>
     <bind xmlns='urn:ietf:params:xml:ns:xmpp-bind'/>
     <sm xmlns='urn:xmpp:sm:3'/>
   </stream:features>
```

## 启用流管理
    
要启用流管理,客户端发送一个`<enable/>`命令给服务端.

**例2:客户端启用流管理**

```
C: <enable xmlns='urn:xmpp:sm:3'/>
```

如果客户端想要恢复一个流,`<enable>`元素必须包括一个布尔类型的`resume`属性,其默认为`false`.关于恢复一个先前会话,请参考恢复一节.

`<enable/>`元素可以包含一个`max`属性,用来指定客户端最优的最大恢复时间,以秒为单位.

一旦受到启用请求,服务端必须回复一个`<enabled/>`元素,或者一个`<failed/>`,注明 `urn:xmpp:sm:3` 的命名空间namespace.`<failed/>`元素表示在建立流管理会话中存在一个问题.`<enabled/>`元素表示成功的建立了流管理会话.

**例3:服务端启用流管理**

```
S: <enabled xmlns='urn:xmpp:sm:3'/>
```

然后可以使用流管理的特性定义下面的内容.

如果服务端允许会话恢复,服务端必须包括一个resume属性,值为`ture`或者`1`.

**例4:服务端启用允许会话恢复的流管理**

```
S: <enabled xmlns='urn:xmpp:sm:3' id='some-long-sm-id' resume='true'/>
```

`<enabled/>`元素可以包含一个`max`属性,用来指定服务端特定的最大恢复时间.

`<enabled/>`属性可以包含一个`location`属性,用来指定服务端的IP地址或者域名(端口可选)用于重连,见**RFC6120**的4.9.3.19部分(即,`domainpart:port`, IPv6地址在方括号里`[…]`,见[RFC5952][7]).如果重连那个location失败了,标准的XMPP重连算法在**RFC6120**中有详细说明.

除非通过了身份验证,客户端禁止启用流管理;也就是说客户端不能发送一个`<enabled/>`元素直到完成了身份验证.(例如,SASL,[Non-SASL Authentication (XEP-0078)][8]或者 [Server Dialback (XEP-0220)][9]已经成功完成).

对于客户端到服务端的连接,在没有完成资源绑定前,客户端禁止启用流管理,除非正在恢复一个之前的会话(见Resumption).

服务端应该实施这个命令,如果这个规则被违反了,要返回一个`<failed/>`元素.

**例5:如果客户端试图在未绑定资源前启用流管理,服务端返回错误**

```
S: <failed xmlns='urn:xmpp:sm:3'>
     <unexpected-request xmlns='urn:ietf:params:xml:ns:xmpp-stanzas'/>
   </failed>
```

## Acks

启用流管理(流管理)之后,客户端或者服务端可以在任意时间通过流发送ack元素.ack元素可以是:

`<a/>`元素用于回答一个确认收到的请求或者发送一个未被请求的ack.
`<r/>`元素用于请求收到节(stanzas)的确认信息.

> (注解: 有点绕,简而言之就是一端发起`<r/>`请求,另一端必须以`<a/>`应答)

属性定义如下:

`h`属性标识最后处理的节(即,服务器确认收到最后一节).

一个`<a/>`元素必须具备一个`h`属性.

`<r/>`元素没有已定义属性.

定义:确认一条之前收到的ack元素,节是被服务器处理后再发送出去的.对于服务端的处理,我们是指服务端接管一条或多条节(如,直接处理节,将节传给本地的一个实体,比如相同服务器下的另一个客户端,或者将节发送给其他服务器的远程实体);一个节在确认被服务端处理之前,发送方都要对节负责(例如,如果没有被服务端确认处理,客户端要重发这个节或者生成一个错误).

收到一个`<r/>`元素的回执并不意味着新的节已经传送给对方,只有收到一个`<a/>`元素的回执,如果`h`属性的值已经递增过,才说明这个新的节已经被处理过.

`h`值在流管理中被启用或被请求启用时,是从零开始的.当第一个节被处理的时候,`h`值增加为`1`,并随着接下来新的节被处理,`h`值不断增加.在一些极端情况下,在流管理对话中被处理的节的数量,


超出了无符号整型数所表示的最大数(2的32次方)[XML Schema Part 2][10],`h`值应该被置为`0`,而不是2的32次方.


注意:任何给定的流,都有两个`h`值,一个被客户端用于跟踪节是否被服务端处理,另一个被服务端用于跟踪节是否被处理自客户端.当客户端向服务端发送`<enable/>`时,客户端初始化它的`h`值为零,同样服务端发送`<enable/>`给客户端时,也初始化它的`h`值为零(服务端会立即回复`<enable/>`,同时设置它的计数器为零).初始化后,客户端根据处理的节的数目不断增加`h`值,服务端也相应不断增加`h`值.

下面的带注释的例子,包括客户端发的一段消息,一个确认接收的请求,一个节的ack.

**例6:Simple stanza acking**

```
C: <enable xmlns='urn:xmpp:sm:3'/>
<!-- Client sets outbound count to zero.(客户端设置outbound为0) -->
```

```
C: <message from='laurence@example.net/churchyard'
            to='juliet@example.com'
            xml:lang='en'>
     <body>
       I'll send a friar with speed, to Mantua,
       with my letters to thy lord.
     </body>
   </message>
<!-- Note that client need not wait for a response.(注意客户端不需要等待回复) -->
```


```
S: <enabled xmlns='urn:xmpp:sm:3'/>
   <!--
        Server receives enable, and responds,
        setting both inbound and outbound counts
        to zero. (服务端接收enable,回复客户端,设置inbound和outboard数为零)
        In addition, client sets inbound count to zero. (另外,客户端设置inbound为零)
   -->
```

```
C: <r xmlns='urn:xmpp:sm:3'/>
```

```
S: <a xmlns='urn:xmpp:sm:3' h='1'/>
```

当一个`<r/>`元素(`request`)被收到时,接收方必须通过给发送方发送一个带h值的`<a/>`元素的回执确认,`h`值大小为`<r/>`元素接受方处理的节的数量.回复必须在接收到`<r/>`元素后立即发送,除了超时以外的任何情况,都不能保留.例如,有时候客户端的网速比较慢,它在确认收到前可能希望在一段时间内收集很多节,服务端可能希望减少传入的节.发送方无需等待继续发送节的ack.
任何一方都可以在任何时间发送一条`<a/>`元素(如,接收完确切数目的节后,或者一个确定的时间段过后),即便是未收到对方发送的`<r/>`元素.
当一方收到一个`<a/>`元素,必须返回一个h值作为当前流最后处理的那条出站节(outbound stanza)的序列号.
如果一个流结束了,也没有在`<enable/>`元素中指定的时间内恢复,序列号和任何相关的状态都可以被双方丢弃.在会话状态被丢弃前,应该对任何未处理的节(即在收到最近的h值后发送的节)采取替代措施.
- 服务端应该采用相同的方式处理未确认的节,对于发送到不可用资源的节,服务端应该给发送方返回一个错误,或者把该节提交给离线仓库存储.
- 面向用户的客户端应该悄悄地(不用通知用户)通过连接重新发送节,或者通过一个合适的交互方式通知用户发送失败.

因为未被确认收到的节可能已经被对方接收,重发有可能导致重复发送.尽管至少可以通过每个节的id协助接收方排除重复接收的节,但是该协议还无法预防这种情况的发生.

## 恢复

XML流有可能发生意外终止(如,因为网络中断).这种情况需要迅速的恢复之前的流,而不是完成流的新建,名册的检索和状态的广播.

另外,该协议会在先前的连接中交换最后收到的流的序列号,允许实体最终确定哪些节需要重发,哪些节不需要重发,通过重放消除重复.

请求中流都是可恢复的,当开启流管理时,客户端必须给`<enable/>`元素增加一个`resume`属性,它的值可以是`true`或者`1`.

**例7:客户端开启流管理**

```
C: <enable xmlns='urn:xmpp:sm:3' resume='true'/>
```

要允许流被恢复,服务端必须在`<enable/>`元素中包含一个`resume`属性,设为`ture`或者`1`;必须包含一个`id`属性,用于作为流的标识符.

**例8:服务端允许流恢复**

```
S: <enabled xmlns='urn:xmpp:sm:3' id='some-long-sm-id' resume='true'/>
```

**定义**

> `id`属性为流管理定义了一个唯一标识(`SM-ID`).SM-ID必须由服务端生成.客户端必须认定SM-ID是一个不透明的类型,因此不允许分配给SM-ID任何语义.服务端可以编码任何它认为有用的信息,如full JID`<localpart@domain.tld/resource>`,赋给SM-ID(如full JID拼上一个当前的属性).任何在XML中被允许的字符属性都是可用的.SM-ID不允许被当前和以后的会话重用(但是服务器不需要确保SM-ID一直是唯一的,只有在服务器继续运行的时候).SM-ID不应该超过4000个字节.

如上所述,`<enable/>`元素可以包含一个`location`属性,用于指明重连的首选位置(如,一个为已经联网的客户端保存状态的特定的链接管理器).

**例9:服务器会选择在一个特定的位置重新链接**

```
S: <enabled xmlns='urn:xmpp:sm:3'
            id='some-long-sm-id'
            location='[2001:41D0:1:A49b::1]:9222'
            resume='true'/>
```

如果流意外停止了,客户端就会向服务器开一个TCP链接.所有事件的顺序如下:

1.断开链接后,客户端向服务端新开一个TCP链接,选择location属性中指明的地址(如果有的话).
2.客户端发送初始化的流的Header.
3.服务端向客户端回复流的Header.
4.服务端发送流的Features.
5.客户端发送 STARTTLS 请求.
6.服务端通知客户端继续进行TLS协议.
7.双方完成一次TLS握手(注意:如果正在进行恢复会话,同时也在用TLS,建议利用TLS会话恢复[RFC5077][11],进一步优化XML流的恢复).
8.客户端发送新初始化的流的header.
9.服务端向客户端回复流的header.
10.服务端向客户端发送流的features,要求SASL协议,提供合适的SASL机制(注意:如果服务端认为在TLS会话恢复中提供的信息足够的真实可信,它可以提供SASL EXTERNAL 机制;具体的请参考[draft-cridland-sasl-tls-sessions][12]).
11.双方完成SASL协议.
12.客户端发送新初始化的流的header.
13.服务端向客户端回复流的header.
14.服务端发送流的features,提供SM的features.
15.客户端请求恢复之前的流.

注意:这些事件的顺序也可能以上的顺序,取决于服务端提供SM features的时间,客户端是否选择STARTTLS,等等.另外,事实上服务端到服务端的流经常没有完成SASL协议,甚至也不完成TLS协议.上面讲的并没有改变**RFC6120**中规定的有关流协议的任何规则.然后,因为流管理适用于xml节(不是任何其他xml元素)的交换,这使得当可能要开始向另一方发送节的时候(不是之前),服务端提供SM features变得有意义.见[Recommended Order of Stream Feature Negotiation (XEP-0170)][13].

请求恢复之前已经终止的流,客户端要发送一个`<resume/>`元素,附上`urn:xmpp:sm:3`的命名空间.`<resume/>`元素必须包含一个`previd`属性,该属性的值为要恢复的流的SM-ID,还有包含一个h值,该值标识通过要恢复的流,服务端发送给客户端的最后一个被处理的节的序列号(万一客户端没有收到任何的节,它将设置h值为0).

例10:流恢复请求

```
C: <resume xmlns='urn:xmpp:sm:3'
           h='some-sequence-number'
           previd='some-long-sm-id'/>
```

如果服务器能恢复之前的流,必须返回一个`<resumed/>元素,该元素必须包含一个`previd`属性,值为要恢复的流的SM-ID,还有必须包含一个h值,值为该值标识通过要恢复的流,客户端发送给服务端的最后一个被处理的节的序列号(万一服务端没有收到任何的节,它将设置h值为0).

例11:被恢复的流

```
S: <resumed xmlns='urn:xmpp:sm:3'
            h='another-sequence-number'
            previd='some-long-sm-id'/>
```

如果服务端不支持会话恢复,必须返回一个`<failed/>`元素,该元素要包含一个`<feature-not-implemented/>`的错误条件.如果服务端没有把`previd`识别为一个更早的会话(例如,可能因为之前的会话已经超时),也必须返回一个`<failed/>`元素,该元素必须包含一个`<item-not-found/>`的错误条件.在这两种失败的情况中,服务器都应该允许客户端在这时绑定一个资源,而不是强制客户端重新开始流协议处理过程和重新认证.

如果之前的流恢复了,此时服务端还依然保持着之前认证过(previously-identified)的会话开着,应该终止之前的流.

当一个会话被恢复时,双方处理过程如下:

- 序列值沿用了之前会话的,而且不会被新的流修改.
- 一旦收到`<resume/>`或者`<resumed/>`元素,客户端和服务端利用h值重新发送所有因为断开连接而丢失的节.事实上,应该处理这俩元素的`h值`,就像在`<a/>`元素中处理它一样(即,在发出队列中标记节为已经被处理),除此之外,必须将任何仍被标记为未处理的节重新发送给对方.
- 双方都应该重发在先前的对话中未被标记处理的节,重发时是根据对方记录的序列号.
- 正在重连的客户端不能请求名册,因为在客户端断开连接的过程中任何名册信息的改变都会在流管理会话恢复后被发送给客户端.
- 客户端不能在其试图恢复之前presence状态的时候发送presence节,因为这个状态可能已经被服务端保存.
- 双方都不能试图重建状态信息(如,Service Discovery (XEP-0030) [15] information).

## 错误处理

如果对于一个`<enable/>`或者`<resume/>`元素,如果出现了错误,服务器必须返回一个`<failed/>`元素.这个元素要包含一个错误条件,它必须是**RFC6120**里定义的所有节错误条件中的一个.

下面是个例子.

例12:服务端返回错误

```
S: <failed xmlns='urn:xmpp:sm:3'>
     <unexpected-request xmlns='urn:ietf:params:xml:ns:xmpp-stanzas'/>
   </failed>
```

流管理错误被认为是可恢复的.但是,错误的使用流管理可能会导致流的终止.

## 流的关闭

一个干净的关闭的流不同于一个未完成的流.如果客户端想干净的关闭它的流,并且结束它的会话,它必须发送一个`</stream:stream>`,以便服务端能够发送一个代表客户端的unavailable presence信息.

如果流没有被干净的关闭,那么服务端应认为流没有完成(即使客户端关掉了跟服务端的TCP连接),要在一个有限的时间内保持代表客户端的会话.在离开一个未完成状态的流之前,客户端可以发送任何希望的presence信息.

## 情景(Scenarios)

下面的几种情景列举了几种不同的流管理的使用.这些例子都是关于客户端和服务端之间的,但是流管理还可以用于服务端对对服务端的流.

### 基本的acking情景

流管理协议可以通过acks,在没有恢复会话的能力的条件下提高可靠性.一个基本的实现将执行以下操作:

- 作为一个客户端,发送不带任何属性的`<enable/>`,不用管在`<enable/>`回复中的属性.

### Efficient Acking Scenario

The basic acking scenario is wasteful because the client requested an ack for each stanza. A more efficient approach is to periodically request acks (e.g., every 5 stanzas). This is shown schematically in the following pseudo-XML.

**例21: An efficient session**

```
C: <enable/>
S: <enabled/>
C: <message/>
C: <message/>
C: <message/>
C: <message/>
C: <message/>
C: <r/>
S: <a h='5'/>
C: <message/>
C: <message/>
C: <message/>
C: <message/>
C: <message/>
C: <r/>
S: <a h='10'/>
```

In particular, on mobile networks, it is advisable to only request and/or send acknowledgements when an entity has other data to send, or in lieu of a whitespace keepalive or XMPP ping (XEP-0199).


## 安全

As noted, a server MUST NOT allow an client to resume a stream management session until after the client has authenticated (for some value of "authentication"); this helps to prevent session hijacking.

## IANA Considerations

This XEP requires no interaction with the Internet Assigned Numbers Authority (IANA) [16].

## XMPP Registrar Considerations

### Protocol Namespaces

This specification defines the following XML namespace:

- `urn:xmpp:sm:3`

The [XMPP Registrar][17] includes the foregoing namespace in its registry at
http://xmpp.org/registrar/namespaces.html, as described in Section 4 of [XMPP Registrar Function (XEP-0053)][18].

### Protocol Versioning

If the protocol defined in this specification undergoes a revision that is not fully backwards-compatible with an older version, the XMPP Registrar shall increment the protocol version number found at the end of the XML namespaces defined herein, as described in Section 4 of **XEP-0053**.

### Stream Features

The XMPP Registrar includes `urn:xmpp:sm:3` in its registry of stream features at
http://xmpp.org/registrar/stream-features.html

## XML 模式

```
<?xml version='1.0' encoding='UTF-8'?>
<xs:schema
  xmlns:xs='http://www.w3.org/2001/XMLSchema'
  targetNamespace='urn:xmpp:sm:3'
  xmlns='urn:xmpp:sm:3'
  elementFormDefault='qualified'>
  <xs:annotation>
    <xs:documentation>
      The protocol documented by this schema is defined in
      XEP-0198: http://www.xmpp.org/extensions/xep-0198.html
    </xs:documentation>
  </xs:annotation>
  <xs:import namespace='urn:ietf:params:xml:ns:xmpp-stanzas'
             schemaLocation='http://xmpp.org/schemas/stanzaerror.xsd'/>
  <xs:element name='a'>
    <xs:complexType>
      <xs:simpleContent>
        <xs:extension base='empty'>
          <xs:attribute name='h'
                        type='xs:integer'
                        use='required'/>
        </xs:extension>
      </xs:simpleContent>
    </xs:complexType>
  </xs:element>
  <xs:element name='enable'>
    <xs:complexType>
      <xs:simpleContent>
        <xs:extension base='empty'>
          <xs:attribute name='max'
                        type='xs:positiveInteger'
                        use='optional'/>
          <xs:attribute name='resume'
                        type='xs:boolean'
                        use='optional'
                        default='false'/>
        </xs:extension>
      </xs:simpleContent>
    </xs:complexType>
  </xs:element>
  <xs:element name='enabled'>
    <xs:complexType>
      <xs:simpleContent>
        <xs:extension base='empty'>
          <xs:attribute name='id'
                        type='xs:string'
                        use='optional'/>
          <xs:attribute name='location'
                        type='xs:string'
                        use='optional'/>
          <xs:attribute name='max'
                        type='xs:positiveInteger'
                        use='optional'/>
          <xs:attribute name='resume'
                        type='xs:boolean'
                        use='optional'
                        default='false'/>
        </xs:extension>
      </xs:simpleContent>
    </xs:complexType>
  </xs:element>
  <xs:element name='failed'>
    <xs:complexType>
      <xs:sequence xmlns:err='urn:ietf:params:xml:ns:xmpp-stanzas'
                   minOccurs='0'
                   maxOccurs='1'>
        <xs:group ref='err:stanzaErrorGroup'/>
      </xs:sequence>
    </xs:complexType>
  </xs:element>
  <xs:element name='r' type='empty'/>
  <xs:element name='resume' type='resumptionElementType'/>
  <xs:element name='resumed' type='resumptionElementType'/>
  <xs:element name='sm'>
    <xs:complexType>
      <xs:choice>
        <xs:element name='optional' type='empty'/>
        <xs:element name='required' type='empty'/>
      </xs:choice>
    </xs:complexType>
  </xs:element>
  <xs:complexType name='resumptionElementType'>
    <xs:simpleContent>
      <xs:extension base='empty'>
        <xs:attribute name='h'
                      type='xs:unsignedInt'
                      use='required'/>
        <xs:attribute name='previd'
                      type='xs:string'
                      use='required'/>
      </xs:extension>
    </xs:simpleContent>
  </xs:complexType>
  <xs:simpleType name='empty'>
    <xs:restriction base='xs:string'>
      <xs:enumeration value=''/>
    </xs:restriction>
  </xs:simpleType>
</xs:schema>
```



  [1]: http://xmpp.org/extensions/xep-0198.html
  [2]: http://blog.csdn.net/yuedong56/article/details/38120101
  [3]: http://wiki.jabbercn.org/RFC6120#.E5.85.A8.E5.B1.80.E5.9C.B0.E5.9D.80
  [4]: http://xmpp.org/extensions/xep-0199.html
  [5]: http://xmpp.org/extensions/xep-0079.html
  [6]: http://xmpp.org/extensions/xep-0184.html
  [7]: http://tools.ietf.org/html/rfc5952
  [8]: http://xmpp.org/extensions/xep-0078.html
  [9]: http://xmpp.org/extensions/xep-0220.html
  [10]: http://www.w3.org/TR/xmlschema-2
  [11]: http://tools.ietf.org/html/rfc5077
  [12]: http://tools.ietf.org/html/draft-cridland-sasl-tls-sessions
  [13]: http://xmpp.org/extensions/xep-0170.html
  [17]: http://xmpp.org/registrar/
  [18]: http://xmpp.org/extensions/xep-0053.html

