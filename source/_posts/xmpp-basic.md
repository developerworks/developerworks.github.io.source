title: XMPP Stanza (节)
categories:
  - Communication System
tags:
  - xmpp
  - stanza
toc: true
date: 2014-09-15 02:17:25
---



XMPP 通信的基本单元是本成为`stanza`的东西,类似下面的实例:

```
<stream:stream>
    <iq type="get"><query xmlns="jabber:iq:roster"/></iq>
    <presence/>
    <message to="william_duan@jabber.org" from="test_account@jabber.org" type="chat"><body>Hello</body></message>
    <presence type="unavailable"/>
</stream:stream>
```

<!-- more -->

在上面的例子中,我们可以看到一些XMPP节(stanza),包括`<iq>`,`<message>`以及`<presence>`.接下来就对这些节(stanza)做一个大致的了解

## 节的共通属性

### from

表示节(stanza)的发送方,在发送节(stanza)时,一般来说不推荐设定,服务器会自动设定正确的值,如果你设定了不正确的值,服务器将会拒收你的节(stanza)信息.如果在客户端到服务器端的通信中接收的节(stanza)中没有本属性,会被默认解释为信息是由服务器发出的.如果在服务器到服务器的通信中接收的节(stanza)中没有本属性,则会被解释为一个error.

### to

表示节(stanza)的接收方.
如果在客户端到服务器端的通信中没有设置本属性,服务器会默认解释为信息是发给自己的.

### type

指定节(stanza)的类型.每种节(stanza)都会有几种可能的设定值.所有的节(stanza)都会有一个error类型,,表明这个节(stanza)是一个error回应,对这样的节(stanza)信息不需要进行回应.

### id

表示一个特定的请求.在<iq>节中,这个属性是必须要指定的,但是在其他两个节(stanza)中是一个可选属性.

```
<presence>
```

`presence`节(stanza)用来控制和表示实体的在线状态,可以展示从离线到在线甚至于离开,不能打扰等复杂状态,另外,还能被用来建立和结束在线状态的订阅.


下面是一些节(stanza)的例子:
```
<presence/>
```

设定用户状态为在线


```
<presence type="unavailable"/>
```

设定用户状态为离线

```
<presence>
    <show>away</show>
    <status>at the ball</status>
</presence>
```

用于显示用户状态的详细信息.上面的例子表明用户因为at the ball在离开状态.

`<show>`标签在presence节点中最多出现一次,可以有以下取值:away,chat,dnd,xa.

away:离线
char:交谈中
dnd:希望不被打扰
xa:离开一段时间

`<status>`标签用于显示额外信息

```
<presence>
    <status>touring the countryside</status>
    <priority>10</priority>
</presence>
```

在这个节中,出现了一个<priority>标签,表示现在连接的优先级. 每个连接可以设置从-128到127的优先级,默认是设置为0.用户可以在这个标签里修改相应的优先级.如果有相应的设置的话,用户送往纯JID的将会送到优先级最高的那个连接,如果设置值为负数的话,则表示送往纯JID的消息将永远不会送达该连接.

扩展<presence>节点

1.在线状态预定(presence subscription)
首先我们来看一个例子:

```
<presence from="william_duan@jabber.org" to="test_account@jabber.org" type="subscribe"/>
<presence from="test_account@jabber.org" to="william_duan@jabber.org" type="subscribed"/>
```

通过上述交互,william_duan就能看到test_account的在线状态,并能接收到test_account的在线状态通知了.

`<message>` 正如名字一样,message节(stanza)用于用户之间传递消息.这消息可以是单纯的聊天信息,也可以某种格式化的信息. message节点信息是传递之后就被忘记的.当消息被送出之后,发送者是不管这个消息是 否已经送出或者什么时候被接收到.通过扩展协议,可以改变这样一种状况.
下面我们看一些例子:

```
<message from="william_duan@jabber.org" to="test_account@jabber.org" type="chat">
<body>Come on</body>
<thread>23sdfewtr234weasdf</thread>
</message>
```

私人聊天信息

```
<message from="test_account@jabber.org" to="william_duan@jabber.org" type="groupchat">
    <body>welcome</body>
</message>
```

多人聊天信息

上面的两个例子都包含了一个`<type>`标签,这个标签表明了消息的类型,可以取下面的一些值. `chat`:私人聊天信息,在IM中是最常见的.

`error`:错误信息
`normal`:不怎么使用的类型
`groupchat`:多人聊天信息
`headline`:通常用在自动服务中,不需要回应.

`<body>`标签里面是具体的消息内容.


`<iq>` iq节(stanza)主要是用于`Info/Query模式`的消息请求,他和Http协议比较相似.可以发出get以及set请求,就如同http中的GET以及POST.
`<iq>`节点需要有回应,有`get`,`set`两种请求以及`result`,`error`两种回应.
下面我们看看一些例子:

```
<iq from="william_duan@jabber.org/study" type="get" id="roster1">
    <query xmlns="jabber:iq:roster"/>
</iq>
```

william_duan请求自己的联系人列表.

```
<!--请求发生错误. -->
<iq to="william_duan@jabber.org/study" type="error" id="roster1">
    <query xmlns="jabber:iq:roster"/>
    <error type="cancel">
    <feature-not-implemented xmlns="urn:ietf:params:xml:ns:xmpp-stanzas"/>
</error>
</iq>
```



```
<!-- 请求成功,返回william_duan的联系人列表.每一个`<item>`标签代表了一个联系人信息. -->
<iq to="william_duan@jabber.org/study" type="error" id="roster1">
    <query xmlns="jabber:iq:roster"/>
    <item jid="account_one@jabber.org" name="one"/>
    <item jid="account_two@jabber.org" name="two"/>
</iq>
```


## 参考资料

1. 图书 `Professional XMPP Programming with JavaScript and jQuery`
2. XMPP基本概念--节
http://m.oschina.net/blog/179909
3. XMPP Over Websocket
https://tools.ietf.org/html/draft-moffitt-xmpp-over-websocket-04
4. RFC6120
http://wiki.jabbercn.org/RFC6120#.E6.89.93.E5.BC.80.E6.B5.81
5. XMPP服务器软件
http://wiki.jabbercn.org/XMPP%E6%9C%8D%E5%8A%A1%E5%99%A8%E8%BD%AF%E4%BB%B6