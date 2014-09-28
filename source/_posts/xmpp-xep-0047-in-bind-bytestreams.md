title: XMPP XEP-0047 带内字节流
categories:
  - Communication
tags:
  - xmpp
  - xep-0047
toc: false
date: 2014-09-22 22:03:27
---

该协议扩展有一个比较经典的应用场景. 通常XMPP服务器为了性能上的考虑,限制了`Stanza`(XML节)的大小不能超过指定的字节数.

当我在野外探险的时候,我发现了一个仙境,但是我通过XMPP客户端给我的兄弟伙说的时候,他既然不相信,怎么能让他相信我呢,我带了手机,我首先想到的时给他发送一张我拍的照片. 但是我的XMPP服务器限制了最大不能发送超过64KB大小的`节`. 所以我不得不把照片数据切分为多个小于64KB的`块`, 然后把这些块连续的发给我的兄弟,让他收完后在组装起来.

<!--more-->

要实现分块/组装的功能,下面又几个要求:

1. XMPP限制了传输的数据类型, 所以二进制数据必须编码为BASE64格式的字符串.
2. 我要把照片切分成等大的N个块(最后一个块可能小于这个大小)
3. 我的兄弟收到这些块之后在拼接起来最后显示这张图片.

下面来看看这个转换过程:


- 我首先发送一个`iq`初始化请求,告知我的兄弟我要开始发送照片`块`了.

```
<iq from="he.zhiqiang@xmpp.hezhiqiang.info"
    id="iy2s986q"
    to="zhiqiang.he@xmpp.hezhiqiang.info"
    type="set">
    <open sid="dv917fb4" block-size="4096" xmlns="http://jabber.org/protocol/ibb"/>
</iq>
```

要注意的时,`iq`节包含一个**直接子元素** `open`, 其属性`block-size`指定了每个块的大小, `xmlns`属性指明了`open`元素所在的名称空间, 最重要的时`sid`,其作为字节流的唯一标识.

- 我的兄弟接受了我的初始化请求

```
<iq from="zhiqiang.he@xmpp.hezhiqiang.info" id="iy2s986q" to="he.zhiqiang@xmpp.hezhiqiang.info" type="result"/>
```

- 现在我可以传输`块`了

```
<!--这里为了代码的可读性,data中的base64数据是截断了的.-->
<message from="he.zhiqiang@xmpp.hezhiqiang.info"
         to="zhiqiang.he@xmpp.hezhiqiang.info" id="ck39fg47">
    <data xmlns="http://jabber.org/protocol/ibb"
          sid="dv917fb4"
          seq="0">qANQR1DBwU4DX7jmYZnncmUQB/9KuKBddzQH+tZ1ZywKK0yHKnq57kWq+RFtQdCJ...</data>
</message>
```

- 对于每一个我发送的块, 为避免在传输过程中丢失某个`块`, 我添加了一个序列号属性`seq`

```
<message from="he.zhiqiang@xmpp.hezhiqiang.info"
         to="zhiqiang.he@xmpp.hezhiqiang.info" id="fh91f36s">
    <data xmlns="http://jabber.org/protocol/ibb"
          sid="dv917fb4"
          seq="1">dNADE1QOjH4QK7wzLMaapzHDO...</data>
</message>
```

- 当我发送完最后一个块的时候,我告知我的兄弟, 我发送完了,你可以把这些`块`组装起来了.

```
<iq from="he.zhiqiang@xmpp.hezhiqiang.info"
    id="fr61g835" to="zhiqiang.he@xmpp.hezhiqiang.info" type="set">
    <close xmlns="http://jabber.org/protocol/ibb" sid="dv917fb4"/>
</iq>
```

- 对于每个IQ-gets和IQ-gets节,都需要发送一个回执.

```
<iq from="sister@realworld.lit/home" id="fr61g835" to="alice@wonderland.lit/rabbithole" type="result"/>
```

- 至此, 照片发送成功, 我证明了我说的真话. 我是一个诚实的人.

<!--
## 协议

### 创建字节流

### 发送数据

### 关闭字节流

## 双向性

## 使用Message Stanzas
-->

## 参考资料

1. 图书
XMPP: The Definitive Guide Building Real-Time Applications with Jabber Technologies 英文版 `139`页
2. XEP-0047 带内字节流
http://xmpp.org/extensions/xep-0047.html

