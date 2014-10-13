title: XMPP权威指南第09章-Jingle多媒体会话
categories:
  - Communication
tags:
  - XMPP
  - XMPP-definitive-guide
toc: false
date: 2014-09-28 01:18:53
---

## To Instant Messaging and Beyond

Jingle的主要协议规范定义在[XEP-0166][1]中,同时还包含许多相关协议.

Jingle提供了一个可靠的机制设置在Internet上的的音频呼叫(通过[XEP-0167][2]), 基本的Jingle方法可以被用于协商和管理任何类型的媒体会话, 包括视频聊天,文件传输,屏幕共享.

Jingle为应用程序类型和传输方法定义了一个可插拔的模型. Jingle用于设置哪些不适于通过XMPP设置的会话. XMPP是为传输大量的XML片段优化的,而不是媒体数据. 互联网社区已经定义了传输音视频,文件和其他应用类型的非常好的技术,Jingle只是重用这些技术来实现富媒体会话.

基本的思想是, Jingle使用XMPP作为`信号通道`设置,管理和终止媒体会话, 媒体数据本身是通过P2P或媒体中继(Media Relay)传输.

> **通道和连接**
> 在Jingle中有两种类型的通道, SIP和其他多媒体技术.
> `信号通道`用于用于发送会话管理消息,`媒体通道`用于发送媒体数据本身. 媒体通道可以通过两种方式建立链接, P2P(两个客户端直接连接), `mediated`(数据通过一个中继服务器转发)

## Jingle 模型

## 呼叫

## NAT

NAT穿透的几种技术:

- STUN
- TRUN
- ICE


## ICE上的Jingle

## 其他的Jingle Action

## 慨括


  [1]: http://xmpp.org/extensions/xep-0166.html
  [2]: http://xmpp.org/extensions/xep-0167.html

