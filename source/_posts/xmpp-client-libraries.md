title: Jabber 服务器/客户端库
categories:
  - Communication
tags:
  - jabber
  - library
toc: true
date: 2014-09-14 22:29:37
---


## Clients

| Name                | Language    | Official Site                                    |
| ------------------- | ----------- | ------------------------------------------------ |
| XMPPFramework       | Objective-C | https://github.com/robbiehanson/XMPPFramework    |
| Strophe.js          | Javascript  | http://strophe.im/strophejs/                     |
| Candy.js            | Javascript  | https://github.com/candy-chat/candy              |
| Converse.js         | Javascript  | https://conversejs.org/                          |
| stanza.io           | Javascript  | https://github.com/otalk/stanza.io               |
| gloox               | C++         | http://camaya.net/                               |
## Servers

| Server       | Implementation Language | Official Website                    |
| -----------  | ----------------------- | ------------------------------------|
| Prosody      | Lua                     | https://prosody.im/                 |
| Ejabberd     | Erlang                  | https://www.ejabberd.im/            |
| Tigase       | Java                    | http://www.tigase.org/              |
| MongooseIM   | Erlang                  | https://github.com/esl/MongooseIM   |

## Gateways/Connection Managers

| Name                                   |
| -------------------------------------- |
| Kaazing WebSocket Gateway              |
| node-xmpp-bosh                         |


## Articles

1. https://www.linode.com/docs/applications/messaging/
2. http://anders.conbere.org/2011/05/03/get_xmpp_-_bosh_working_with_ejabberd_firefox_and_strophe.html
3. http://kaazing.com/products/editions/kaazing-websocket-gateway-xmpp/



问题:     使用命令`mongooseim start`后大概30秒内,服务器自动停止,
        `mongooseim ping`出现 `Failed RPC connection to the node mongooseim@localhost: nodedown`

答案:     使用`sudo mongooseim start`

