title: XMPP连接管理器 node-xmpp-bosh
categories:
  - Communication System
tags:
  - ejabberd
  - nodejs
toc: false
date: 2014-09-18 17:43:15
---

Ejabberd 对Websocket的支持很不好,在Github上搜索到一个nodejs 开发的链接管理器 `node-xmpp-bosh`支持Websocket,这样我就能在浏览器中分析XMPP协议了,记录一下安装和配置方法,备忘.

项目地址: https://github.com/dhruvbird/node-xmpp-bosh

<!-- more -->

### 需要首先安装nodejs

```
wget http://nodejs.org/dist/v0.10.32/node-v0.10.32.tar.gz
tar zxf node-v0.10.32.tar.gz
cd node-v0.10.32
./configure
make
make install
```

### 安装 node-xmpp-bosh

```
git clone https://github.com/dhruvbird/node-xmpp-bosh.git
cd node-xmpp-bosh
npm install
```

### 配置

修改项目目录下的配置文件`bosh.conf.example.js`, 本机`5222`已经被ejabberd占用,改为`5288`,修改`firewall`部分,加入允许连接的目标XMPP服务器

```
exports.config = {
    port: 5288,
    host: '0.0.0.0',
    path: /^\/http-bind(\/+)?$/,
    logging: 'INFO',
    ...
    ...
    ...
    firewall: {
        // allow: [ /* 'jabber.org', /(.*\.)?jappix.com$/ */ ],
        allow: ['xmpp.hezhiqiang.info'],
        deny:  [ /* 'gmail.com' */ ]
    },
    ...
    ...
    ...
```

### 运行

```
root@4a3dadbef35e:~/node-xmpp-bosh# ./run-server.js --config=./bosh.conf.example.js
+---------------------------------------------------------------------------------------------------------------------------+
| Starting BOSH server 'v0.7.11' on 'http://0.0.0.0:5288/^\/http-bind(\/+)?$/' at 'Wed Sep 17 2014 05:37:20 GMT+0000 (UTC)' |
+---------------------------------------------------------------------------------------------------------------------------+
+--------------------------------------------------------------------------------------------------------+
| Starting WEBSOCKET server 'v0.7.11' on ws://0.0.0.0:5288' at 'Wed Sep 17 2014 05:37:20 GMT+0000 (UTC)' |
+--------------------------------------------------------------------------------------------------------+
```

BOSH 服务器监控界面

```
http://192.168.8.132:5288/http-bind/
```

![BOSH 服务器监控界面][1]

  [1]: /assets/images/90108E4F-DCDF-4D1F-8A7D-25757F1ED2C7.png