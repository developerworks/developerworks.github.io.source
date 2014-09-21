title: XMPP-0133 服务管理
categories:
  - Communication System
tags:
  - xmpp
  - xep-0133
  - ejabberd
  - strophe.js
toc: true
date: 2014-09-21 16:00:48
---

本文说明了如何使用Javascript Strophe.js 实现XEP-0133扩展协议定义的XMPP服务器管理功能,

01. 添加用户
02. 删除用户
03. 禁用一个用户
04. 重新启用一个用户
05. 结束用户会话
06. 获取用户密码
07. 修改用户密码
08. 获取用户名册(联系人列表)
09. 获取用户最后登录时间
10. 获取用户统计
11. 编辑黑名单
12. 编辑白名单
13. 获取已注册用户数
14. 获取禁止用户数
15. 获取在线用户数
16. 获取活动用户数
17. 获取空闲用户数
18. 获取已注册用户列表
19. 获取被禁止的用户列表
20. 获取在线用户类别
21. 获取活动用户列表
22. 获取空闲用户列表
23. 向在线用户发送通告
24. 设置MOTD(Message of the Day)
25. 编辑MOTD
26. 删除MOTD
27. 设置欢迎消息
28. 删除欢迎消息
29. 编辑管理员列表
30. 重启服务
31. 关闭服务

<!-- more -->

## 配置环境

|操作系统      |XMPP服务器       | 客户端库
|------------ | -------------- | -------
|Ubuntu 14.04 | Ejabberd 14.07 | Strophe.js

使用`node-xmpp-bosh`作为Websocket代理连接到Ejabberd, 需要配置管理员账号

```
...
...
...
###'   ACCESS CONTROL LISTS
acl:
  ##
  ## The 'admin' ACL grants administrative privileges to XMPP accounts.
  ## You can put here as many accounts as you want.
  ##
  admin:
     user:
       - "root": "xmpp.hezhiqiang.info"
...
...
...
```

## 获取用户注册数

请求:
```
<iq to='xmpp.hezhiqiang.info' type='set' xml:lang='en' xmlns='jabber:client'>
  <command xmlns='http://jabber.org/protocol/commands' node='http://jabber.org/protocol/admin#get-registered-users-num' action='execute'/>
</iq>
```

响应:

```
<iq from="xmpp.hezhiqiang.info" to="root@xmpp.hezhiqiang.info/40924564951411172979418239" type="result" xmlns="jabber:client" xmlns:stream="http://etherx.jabber.org/streams" version="1.0">
  <command xmlns="http://jabber.org/protocol/commands" sessionid="2014-09-20T00:29:51.291298Z" node="http://jabber.org/protocol/admin#get-registered-users-num" status="completed">
    <x xmlns="jabber:x:data">
      <field type="hidden" var="FORM_TYPE">
        <value>http://jabber.org/protocol/admin</value>
      </field>
      <field type="text-single" label="Number of registered users" var="registeredusersnum">
        <value>13</value>
      </field>
    </x>
  </command>
</iq>
```

Strophe.js代码:

```
var admin = {
  /**
   * 获取注册用户数
   * @return integer
   */
  get_register_users_number: function () {
    var iq = $iq({
      to: session.domain,
      type: 'set',
      'xml:lang': 'en'
    }).c('command', {
        xmlns: 'http://jabber.org/protocol/commands',
        node: 'http://jabber.org/protocol/admin#get-registered-users-num',
        action: 'execute'
      });
    connection.send(iq.tree());
  }
};
```

## 获取注册用户列表

Ejabberd实现和XEP-0133扩展协议有差异, Ejabberd使用非标准的方式获取已注册用户列表

### Ejabberd 实现

请求

```
<iq xmlns='jabber:client' type='get' to='xmpp.hezhiqiang.info' from='root@xmpp.hezhiqiang.info'>
  <query xmlns='http://jabber.org/protocol/disco#items' node='all users'/>
</iq>
```

响应

```
# 响应IQ节
<iq from="xmpp.hezhiqiang.info" to="root@xmpp.hezhiqiang.info/231005051114111783732339" type="result" xmlns="jabber:client" xmlns:stream="http://etherx.jabber.org/streams" version="1.0">
  <query xmlns="http://jabber.org/protocol/disco#items" node="all users">
    <item jid="hezhiqiang@xmpp.hezhiqiang.info" name="hezhiqiang@xmpp.hezhiqiang.info"/>
    <item jid="root@xmpp.hezhiqiang.info" name="root@xmpp.hezhiqiang.info"/>
    <item jid="test1@xmpp.hezhiqiang.info" name="test1@xmpp.hezhiqiang.info"/>
    <item jid="test10@xmpp.hezhiqiang.info" name="test10@xmpp.hezhiqiang.info"/>
    <item jid="test2@xmpp.hezhiqiang.info" name="test2@xmpp.hezhiqiang.info"/>
    <item jid="test3@xmpp.hezhiqiang.info" name="test3@xmpp.hezhiqiang.info"/>
    <item jid="test4@xmpp.hezhiqiang.info" name="test4@xmpp.hezhiqiang.info"/>
    <item jid="test5@xmpp.hezhiqiang.info" name="test5@xmpp.hezhiqiang.info"/>
    <item jid="test6@xmpp.hezhiqiang.info" name="test6@xmpp.hezhiqiang.info"/>
    <item jid="test7@xmpp.hezhiqiang.info" name="test7@xmpp.hezhiqiang.info"/>
    <item jid="test8@xmpp.hezhiqiang.info" name="test8@xmpp.hezhiqiang.info"/>
    <item jid="test9@xmpp.hezhiqiang.info" name="test9@xmpp.hezhiqiang.info"/>
    <item jid="user2@xmpp.hezhiqiang.info" name="user2@xmpp.hezhiqiang.info"/>
  </query>
</iq>
```

Strophe.js代码:

```
function ServiceAdministration(host) {
  this.node_prefix = 'http://jabber.org/protocol/admin#';
  this.nodes = {
    ADD_USER: 'add-user',
    DELETE_USER: 'delete-user',
    DISABLE_USER: 'disable-user',
    REENABLE_USER: 'reenable-user',
    END_USER_SESSION: 'end-user-session',
    GET_USER_PASSWORD: 'get-user-password',
    CHANGE_USER_PASSWORD: 'change-user-password',
    GET_USER_ROSTER: 'get-user-roster',
    GET_USER_LASTLOGIN: 'get-user-lastlogin',
    USER_STATS: 'user-stats',
    EDIT_BLACKLIST: 'edit-blacklist',
    EDIT_WHITELIST: 'edit-whitelist',
    GET_REGISTERED_USERS_NUM: 'get-registered-users-num',
    GET_DISABLED_USERS_NUM: 'get-disabled-users-num',
    GET_ONLINE_USERS_NUM: 'get-online-users-num',
    GET_ACTIVE_USERS_NUM: 'get-active-users-num',
    GET_IDLE_USERS_NUM: 'get-idle-users-num',
    // Ejabberd 实现方式和XEP-0133标准有却别
    GET_REGISTERED_USERS_LIST_EJABBERD: 'all users',
    GET_REGISTERED_USERS_LIST: 'get-registered-users-list',
    GET_DISABLED_USERS_LIST: 'get-disabled-users-list',
    GET_ONLINE_USERS_LIST: 'get-online-users-list',
    GET_ACTIVE_USERS: 'get-active-users',
    GET_IDLE_USERS: 'get-idle-users',
    ANNOUNCE: 'announce',
    SET_MOTD: 'set-motd',
    EDIT_MOTD: 'edit-motd',
    DELETE_MOTD: 'delete-motd',
    SET_WELCOME: 'set-welcome',
    DELETE_WELCOME: 'delete-welcome',
    EDIT_ADMIN: 'edit-admin',
    RESTART: 'restart',
    SHUTDOWN: 'shutdown'
  };
  this.base_iq = {
    xmlns: 'jabber:client',
    type: 'set',
    to: host
  };
  this.getCopyBaseIQ = function () {
    return JSON.parse(JSON.stringify(this.base_iq));
  };
  this.getCommand = function (node) {
    return Strophe.xmlElement('command', {xmlns: 'http://jabber.org/protocol/commands', action: 'execute', node: this.node_prefix + node});
  };
  this.send = function (iq) {
    connection.send(iq.tree());
  };
  this.addUser = function () {
    var iq_attributes = this.getCopyBaseIQ();
    iq_attributes.from = jid;
    iq_attributes.to = session.domain;
    var iq = $iq(iq_attributes).cnode(this.getCommand(this.nodes.ADD_USER));
    this.send(iq);
  };
  this.getRegisterUserNumber = function () {
    var iq_attributes = this.getCopyBaseIQ();
    iq_attributes.from = jid;
    iq_attributes.to = session.domain;
    var iq = $iq(iq_attributes).cnode(this.getCommand(this.nodes.GET_REGISTERED_USERS_NUM));
    this.send(iq);
  };
  this.getRegisterUserList = function () {
    var iq_attributes = this.getCopyBaseIQ();
    iq_attributes.from = jid;
    iq_attributes.to = session.domain;
    var iq = $iq(iq_attributes).cnode(this.getCommand(this.nodes.GET_REGISTERED_USERS_LIST));
    this.send(iq);
  };
  /**
   * Ejabberd实现方式不兼容XEP-0133, Ejabberd采用Disco服务发现机制获取注册用户列表
   */
  this.getRegisterUserList2 = function () {
    var iq_attributes = this.getCopyBaseIQ();
    iq_attributes.from = jid;
    iq_attributes.to = session.domain;
    iq_attributes.type = 'get';
    var iq = $iq(iq_attributes).c('query', {
      xmlns: 'http://jabber.org/protocol/disco#items',
      node: this.nodes.GET_REGISTERED_USERS_LIST_EJABBERD
    });
    this.send(iq);
  };
};
var client_session = {
  user: 'root',
  domain: 'xmpp.hezhiqiang.info'
};
var serviceadmin = new ServiceAdministration(client_session.domain);
serviceadmin.getRegisterUserList2();
```