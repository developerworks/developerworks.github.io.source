title: Jabber 服务器/客户端库
categories:
  - jabber
tags:
  - jabber
toc: true
date: 2014-09-14 22:29:37
---


## Clients

| Name                | Official Site                                                               |
| ------------------- | --------------------------------------------------------------------------- |
| XMPPFramework       | https://github.com/robbiehanson/XMPPFramework                               |
| Strophe.js          | http://strophe.im/strophejs/                                                |
| Candy.js            | https://github.com/candy-chat/candy                                         |
| Converse.js         | https://conversejs.org/                                                     |

## Servers

| Server       | Implementation Language | Official Website
| -----------  | ----------------------- | --------------------
| Prosody      | Lua                     | https://prosody.im/
| Ejabberd     | Erlang                  |
| Tigase       | Java                    |
| MongooseIM   | Erlang                  |

## Gateways

| Name                                   |
| -------------------------------------- |
| Kaazing WebSocket Gateway              |


## Articles


1. https://www.linode.com/docs/applications/messaging/
2. http://anders.conbere.org/2011/05/03/get_xmpp_-_bosh_working_with_ejabberd_firefox_and_strophe.html
3. http://kaazing.com/products/editions/kaazing-websocket-gateway-xmpp/



问题:     使用命令`mongooseim start`后大概30秒内,服务器自动停止,
        `mongooseim ping`出现 `Failed RPC connection to the node mongooseim@localhost: nodedown`

答案:     使用`sudo mongooseim start`

