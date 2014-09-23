title: XMPP 设置BOSH连接管理器
categories:
  - Communication System
tags:
  - xmpp
  - bosh
toc: false
date: 2014-09-16 16:39:13
---

Web 浏览器对XMPP没有原生的支持, 为了在Web应用中使用XMPP, 需要一个中间件来把无状态的HTTP请求转换为有状态的XMPP连接. 这个过程被称为`同步HTTP双向流`(Bi-directional stream Over Synchronous HTTP), 提供这个服务的服务器被称为连接管理器.

<!--more-->

## 连接管理器(独立)

| Connection Manager                | Official Site                                                               |
| --------------------------------- | --------------------------------------------------------------------------- |
| PUNJAB                            | https://github.com/twonds/punjab                                            |
| node-xmpp-bosh                    | https://github.com/dhruvbird/node-xmpp-bosh                                 |

## 连接管理器(XMPP服务器内置)

| XMPP Server                | Module                                                   |
| ---------------------------| ---------------------------------------------------------|
| Ejabberd                   | MOD_HTTP_BIND                                            |

## 参考资料

