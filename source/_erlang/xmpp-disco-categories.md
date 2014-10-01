title: XMPP服务发现标识
categories:
  - Communication
tags:
  - xmpp
toc: true
date: 2014-09-28 16:14:09
---

This is the official registry of values for the 'category' and 'type' attributes of the <identity/> element within the 'http://jabber.org/protocol/disco#info' namespace (see XEP-0030: Service Discovery), as registered with the XMPP Registrar.


## 账号(account)

1. account

The "account" category is to be used by a server when responding to a disco request sent to the bare JID (user@host addresss) of an account hosted by the server.

| Type | Description | XML |
| ---- | ----------- | --- |
| admin | The user@host is an administrative account | `<identity category='account' type='admin'/>` |
| anonymous | The user@host is a "guest" account that allows anonymous login by any user | `<identity category='account' type='anonymous'/>` |
| registered | The user@host is a registered or provisioned account associated with a particular non-administrative user | `<identity category='account' type='registered'/>` |

## 认证(auth)
## 自动化(automation)

`automation`分类由实体和节点组成, 这些实体和节点提供自动化的或程序化的交互.

| Type | Description | XML |
| ---- | ----------- | --- |
|command-list | The node for a list of commands; valid only for the node "http://jabber.org/protocol/commands" | `<identity category='automation' type='command-list'/>` |
|command-node | A node for a specific command; the "node" attribute uniquely identifies the command | `<identity category='automation' type='command-node'/>` |
|rpc | An entity that supports Jabber-RPC. | `<identity category='automation' type='rpc'/>` |
|soap | An entity that supports the SOAP XMPP Binding. | `<identity category='automation' type='soap'/>` |
|translation | An entity that provides automated translation services. | `<identity category='automation' type='translation'/>` |


## 客户端(client)
## 协作(collaboration)
## 组件(component)
## 会议(conference)
## 目录(directory)
## 网关(gateway)
## 头条(headline)
## 层次(hierarchy)
## 代理(proxy)
## 发布订阅(pubsub)
## 服务器(server)
## 存储(store)

## 参考资料:

1. http://xmpp.org/registrar/disco-categories.html


