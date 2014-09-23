title: XMPP Addressing
categories:
  - Communication System
tags:
  - xmpp
toc: false
date: 2014-09-15 01:46:30
---


- 每个`实体`(Entity)有一个或多个地址, 这个地址被称为`JID`(Jabber Identifer)
- 每个`地址`由三部分组成, `本地部分`, `域名`, `资源`, 域名部分是必须的, 取决于上下文的不同,另外两个部分(`本地部分`,`资源`)是可选的.
<!--more-->
- `本地部分`一般标识一个用户, 但也可以标识其他对象, 比如在多用户聊天的情况下表示一个`房间`(Room)
- `资源`一般用于表示一个`客户端连接`, 比如同一账号的多个客户端连接到服务器即可用资源表示,
    ```
    # 家里的电脑
    dannis@talk.google.com/home
    # 办公司的电脑
    dannis@talk.google.com/office
    ```
你可以同时打开Gmail和Gtalk,两端能够同时收发消息

- JID分为两类 `Bare JID`,`Full JID`, Bare JID 省略了资源部分,
例如:`Full JID`为:
`dannis@talk.google.com/office`,
其对应的`Bare JID`为:
`dannis@talk.google.com`

- 在有些情况下`Bare JID`和`Full JID`相同,比如当访问一个服务器或者一个房间时.

- 发送到用户的`Bare JID`会转发到多个已连接的用户资源