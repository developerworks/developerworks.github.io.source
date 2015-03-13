title: 移动3G网络下的流管理
categories:
  - Ejabberd
tags:
  - Stream Management
  - XEP-198
toc: false
date: 2014-12-18 19:45:31
---

这篇文章的起因是因为朋友开发的移动社交APP使用Ejabberd的时候遇到了3G网络频繁断开连接的问题.
由于3G网络的特性,在3G网络内的客户端IP可能是频繁变化的,你手持移动设备在不停的移动,遇到建筑物,
进入电梯等都可能导致3G网络突然中断.等你再次连接上服务器的时候,之前的状态就已经丢失了.

为了解决这类问题, XMPP基金会发布了一个XMPP扩展协议[XEP-198 Stream Management](http://demo.netfoucs.com/yuedong56/article/details/38120101) 类支持XMPP会话的恢复.

Ejabberd在14.05之后内置了对流管理的支持, 所以要使服务器支持流管理, 必须升级到至少14.05, Ejabberd的默认配置是打开了流管理功能的.

下面是服务器需要支持的配置选项:

```
listen:
  -
    port: 5222
    module: ejabberd_c2s
    ##
    ## If TLS is compiled in and you installed a SSL
    ## certificate, specify the full path to the
    ## file and uncomment these lines:
    ##
    ## certfile: "/path/to/ssl.pem"
    ## starttls: true
    ##
    ## To enforce TLS encryption for client connections,
    ## use this instead of the "starttls" option:
    ##
    ## starttls_required: true
    ##
    ## Custom OpenSSL options
    ##
    ## protocol_options:
    ##   - "no_sslv3"
    ##   - "no_tlsv1"
    max_stanza_size: 65536
    shaper: c2s_shaper
    access: c2s
    zlib: true
    ##
    ## 打开(true)或关闭(false)Ejabberd的流管理(XEP 198)功能, 默认为true
    ##
    stream_management: true
    ##
    ## 消息确认队列, 用于存储客户端未确认的消息队列, 用于重传, 当超过此限制时,
    ## 客户端会话自动被Ejabberd终止, 有效值为正整数和`infinity`, 默认值为500
    ##
    max_ack_queue: 500
    ##
    ## 会话超时是重传
    ##
    ## 未被客户端确认的消息将在会话超时的时候重传(retransimission),
    ## 该行为通常是我们所期望的, 但是在某些环境下可能导致不期望的结果, 比如:
    ## 当一个消息同时发送给两个资源时, 如果一个资源超时, 那么另一个资源将收到服务器重传的消息,
    ## 这不是我们所期望的结果. 因此Ejabberd默认对此选项的设置为false, 含义为当会话超时后,
    ## 返回一个错误, 而不是重传消息
    ##
    ## 为了避免上述两个资源其中一个超时的副作用,可以:
    ## 1. 如果一个JID 只允许有一个 Resource同时在线, 那么我们可以把该选项设置为true
    ## 2. 如果一个JID 允许不止一个 Resource同时在线, 建议把该选项设置为false
    resend_on_timeout: false
```

要启用XMPP扩展协议XEP 198所描述的流恢复功能, 还需要客户端主动打开流恢复功能. 客户端要判断服务器是否支持流管理特性, 在流初始化阶段服务器会返回如下信息:


```
<stream:features xmlns="jabber:client"
    xmlns:stream="http://etherx.jabber.org/streams" version="1.0">
  <bind xmlns="urn:ietf:params:xml:ns:xmpp-bind"/>
  <session xmlns="urn:ietf:params:xml:ns:xmpp-session"/>
  <sm xmlns="urn:xmpp:sm:2"/>
  <sm xmlns="urn:xmpp:sm:3"/>
  <csi xmlns="urn:xmpp:csi:0"/>
  <c xmlns="http://jabber.org/protocol/caps"
    hash="sha-1"
    node="http://www.process-one.net/en/ejabberd/"
    ver="aIT+/ulfcbHXDKPkCA+iw9x5mU8="/>
  <register xmlns="http://jabber.org/features/iq-register"/>
</stream:features>
```

如果包含:

```
  ...
  <sm xmlns="urn:xmpp:sm:2"/>
  <sm xmlns="urn:xmpp:sm:3"/>
  ...
```

那么说明服务器是支持流管理特性的. 这个时候我们可以让客户端发送一个`<enable>`XML片段通知服务器端打开流管理.

```
<enable xmlns='urn:xmpp:sm:3' resume='true'/>
```

同时服务响应一个`<enabled>`标签,表示服务器的流管理已经打开.

```
<enabled xmlns="urn:xmpp:sm:3"
    id="g2gCbQAAABk5NDIyMTUxMzkxNDE4OTA0NzEwMjA1OTUxaANiAAAFimIADc4EYgAO508="
    resume="true"
    max="300"
    xmlns:stream="http://etherx.jabber.org/streams"
    version="1.0"/>
```

`<enabled>`标签上有几个重要的属性:

- `id` 流管理的会话ID
- `resume` 是否支持流恢复
- `max` 会话超时时间

上述的流管理会话的建立过程必须是在通过了身份认证之和资源绑定之后, 如果未`通过身份认证`和`资源绑定`尝试建立一个流管理会话将会得到一个`<fail/>`.



## 流的恢复

```
<resume
    xmlns='urn:xmpp:sm:3'
    h='some-sequence-number'
    previd='g2gCbQAAABk5NDIyMTUxMzkxNDE4OTA0NzEwMjA1OTUxaANiAAAFimIADc4EYgAO508='/>
```

- <resume>必须带上`xmlns='urn:xmpp:sm:3'`属性
- `h` 表示断开连接之前最后一次从服务器收到的XML节的序列号
- `previd` 上一次流会话的ID

客户端发送流恢复请求之后, 服务端会重新生成一个`id`

```
<enabled
    xmlns="urn:xmpp:sm:3"
    id="g2gCbQAAABozNDY3MjMxMTkyMTQxODkwNjE4MDM1NjIwM2gDYgAABYpiAA3Tw2IAAlr+"
    resume="true"
    max="300"
    xmlns:stream="http://etherx.jabber.org/streams" version="1.0"/>
```

我们看到服务器响应了一个`<enabled>`标签, 其属性`id`的值和之前是不同的.

客户端需要记录这个值, 以备下一次连接中断后的流恢复.


