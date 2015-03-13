title: Ejabberd 多人聊天协议分析-配置房间
categories:
  - Ejabberd
tags:
  - MUC
toc: true
date: 2014-11-07 11:36:45
---

## 概述

MUC(Multiple User Chat), 即多人聊天, 多个用户可以在一个房间(群)里面相互交流. 一个成员发送的信息能够被所有群中的成员看到. XMPP的MUC和QQ群在功能上很多都是相似的. 可以对比两者的区别来学习和理解XMPP的多人聊天扩展.

群里面又各种角色, 不同的角色拥有不同的权限, 例如:

- 群的创建者具有所有权限
- 群成员只能聊天
- 管理员可以踢人, 邀请等管理任务.

## 获取配置信息

```
<iq type='get' xml:lang='zh' to='xmpp@conference.xmpp.hezhiqiang.info' xmlns='jabber:client'>
  <query xmlns='http://jabber.org/protocol/muc#owner'/>
</iq>
```

## 配置聊天室表单IQ-result

```
<iq from="xmpp@conference.xmpp.hezhiqiang.info"
    to="root@xmpp.hezhiqiang.info/37262574821415332900124309"
    type="result"
    xmlns="jabber:client"
    xmlns:stream="http://etherx.jabber.org/streams"
    version="1.0">
  <query xmlns="http://jabber.org/protocol/muc#owner">
    <instructions>您需要一个兼容 x:data 的客户端来配置房间</instructions>
    <x xmlns="jabber:x:data" type="form">
      <title>房间 xmpp@conference.xmpp.hezhiqiang.info 的配置 </title>
      <field type="hidden" var="FORM_TYPE">
        <value>http://jabber.org/protocol/muc#roomconfig</value>
      </field>
      <field type="text-single" label="房间标题" var="muc#roomconfig_roomname">
        <value>群标题</value>
      </field>
      <field type="text-single" label="房间描述" var="muc#roomconfig_roomdesc">
        <value>群描述</value>
      </field>
      <field type="boolean" label="永久保存该房间" var="muc#roomconfig_persistentroom">
        <value>1</value>
      </field>
      <field type="boolean" label="使房间可被公开搜索" var="muc#roomconfig_publicroom">
        <value>1</value>
      </field>
      <field type="boolean" label="公开参与人列表" var="public_list">
        <value>1</value>
      </field>
      <field type="boolean" label="进入此房间需要密码" var="muc#roomconfig_passwordprotectedroom">
        <value>0</value>
      </field>
      <field type="text-private" label="密码" var="muc#roomconfig_roomsecret">
        <value/>
      </field>
      <field type="list-single" label="允许的与会人最大数" var="muc#roomconfig_maxusers">
        <value>200</value>
        <option label="5">
          <value>5</value>
        </option>
        <option label="10">
          <value>10</value>
        </option>
        <option label="20">
          <value>20</value>
        </option>
        <option label="30">
          <value>30</value>
        </option>
        <option label="50">
          <value>50</value>
        </option>
        <option label="100">
          <value>100</value>
        </option>
        <option label="200">
          <value>200</value>
        </option>
      </field>
      <field type="list-single" label="将真实 Jabber ID 显示给" var="muc#roomconfig_whois">
        <value>moderators</value>
        <option label="仅主持人">
          <value>moderators</value>
        </option>
        <option label="任何人">
          <value>anyone</value>
        </option>
      </field>
      <field type="boolean" label="设置房间只接收会员" var="muc#roomconfig_membersonly">
        <value>1</value>
      </field>
      <field type="boolean" label="设置房间只接收主持人" var="muc#roomconfig_moderatedroom">
        <value>1</value>
      </field>
      <field type="boolean" label="用户默认被视为参与人" var="members_by_default">
        <value>1</value>
      </field>
      <field type="boolean" label="允许用户更改主题" var="muc#roomconfig_changesubject">
        <value>1</value>
      </field>
      <field type="boolean" label="允许用户发送私聊消息" var="allow_private_messages">
        <value>1</value>
      </field>
      <field type="list-single" label="允许访客发送私聊消息至" var="allow_private_messages_from_visitors">
        <value>anyone</value>
        <option label="没有人">
          <value>nobody</value>
        </option>
        <option label="仅主持人">
          <value>moderators</value>
        </option>
        <option label="任何人">
          <value>anyone</value>
        </option>
      </field>
      <field type="boolean" label="允许用户查询其它用户" var="allow_query_users">
        <value>1</value>
      </field>
      <field type="boolean" label="允许用户发送邀请" var="muc#roomconfig_allowinvites">
        <value>0</value>
      </field>
      <field type="boolean"
        label="更新在线状态时允许用户发送状态文本" var="muc#roomconfig_allowvisitorstatus">
        <value>1</value>
      </field>
      <field type="boolean" label="允许用户更改昵称" var="muc#roomconfig_allowvisitornickchange">
        <value>1</value>
      </field>
      <field type="boolean" label="允许访客发送声音请求" var="muc#roomconfig_allowvoicerequests">
        <value>1</value>
      </field>
      <field type="text-single"
        label="声音请求的最小间隔(以秒为单位)" var="muc#roomconfig_voicerequestmininterval">
        <value>1800</value>
      </field>
      <!-- 验证码白名单: 该名单中的Jid可以绕过验证码验证-->
      <field type="jid-multi"
        label="从验证码挑战中排除 Jabber ID" var="muc#roomconfig_captcha_whitelist"/>
      <field type="boolean" label="启用服务器端聊天记录" var="muc#roomconfig_enablelogging">
        <value>0</value>
      </field>
    </x>
  </query>
</iq>
```

## 提交房间配置表单 IQ-set

```
<!-- 提交房间配置表单 -->
<iq type='set' id='purple6b0f6f1d' to='xmpp@conference.xmpp.hezhiqiang.info'>
    <query xmlns='http://jabber.org/protocol/muc#owner'>
        <x xmlns='jabber:x:data' type='submit'>
            <field var='FORM_TYPE'>
                <value>http://jabber.org/protocol/muc#roomconfig</value>
            </field>
            <field var='muc#roomconfig_roomname'>
                <value>群标题</value>
            </field>
            <field var='muc#roomconfig_roomdesc'>
                <value>群描述</value>
            </field>
            <field var='muc#roomconfig_persistentroom'>
                <value>1</value>
            </field>
            <field var='muc#roomconfig_publicroom'>
                <value>1</value>
            </field>
            <field var='public_list'>
                <value>1</value>
            </field>
            <field var='muc#roomconfig_passwordprotectedroom'>
                <value>0</value>
            </field>
            <field var='muc#roomconfig_roomsecret'>
                <value/>
            </field>
            <field var='muc#roomconfig_maxusers'>
                <value>200</value>
            </field>
            <field var='muc#roomconfig_whois'>
                <value>moderators</value>
            </field>
            <field var='muc#roomconfig_membersonly'>
                <value>1</value>
            </field>
            <field var='muc#roomconfig_moderatedroom'>
                <value>1</value>
            </field>
            <field var='members_by_default'>
                <value>1</value>
            </field>
            <field var='muc#roomconfig_changesubject'>
                <value>1</value>
            </field>
            <!-- 允许私有消息-->
            <field var='allow_private_messages'>
                <value>1</value>
            </field>
            <field var='allow_private_messages_from_visitors'>
                <value>anyone</value>
            </field>
            <!-- 允许查询用户 -->
            <field var='allow_query_users'>
                <value>1</value>
            </field>
            <!-- 是否允许邀请 -->
            <field var='muc#roomconfig_allowinvites'>
                <value>0</value>
            </field>
            <field var='muc#roomconfig_allowvisitorstatus'>
                <value>1</value>
            </field>
            <field var='muc#roomconfig_allowvisitornickchange'>
                <value>1</value>
            </field>
            <field var='muc#roomconfig_allowvoicerequests'>
                <value>1</value>
            </field>
            <field var='muc#roomconfig_voicerequestmininterval'>
                <value>1800</value>
            </field>
            <field var='muc#roomconfig_captcha_whitelist'/>
            <field var='muc#roomconfig_enablelogging'>
                <value>0</value>
            </field>
        </x>
    </query>
</iq>
```

## 获取服务器上的聊天室列表

IQ-get

```
<iq type='get' to='conference.xmpp.hezhiqiang.info' xmlns='jabber:client'>
  <query xmlns='http://jabber.org/protocol/disco#items'/>
</iq>
```

IQ-result

```
<iq from="conference.xmpp.hezhiqiang.info"
    to="root@xmpp.hezhiqiang.info/36782688381415333849585695"
    type="result"
    xmlns="jabber:client"
    xmlns:stream="http://etherx.jabber.org/streams" version="1.0">
  <query xmlns="http://jabber.org/protocol/disco#items">
    <item jid="test@conference.xmpp.hezhiqiang.info" name="聊天室标题 (0)"/>
    <item jid="xmpp@conference.xmpp.hezhiqiang.info" name="群标题 (0)"/>
  </query>
</iq>
```

TODO::需要扩展支持分页显示