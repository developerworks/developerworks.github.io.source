title: Ejabberd 配置
categories:
  - Communication System
tags:
  - ejabberd
  - xmpp
  - bosh
toc: false
date: 2014-09-16 17:07:26
---

从Ejabber 13.10开始Ejabberd把原来Erlang Term的配置文件格式改为YAML格式的配置文件. 下面列举了一些常用的配置项

<!--more-->

## 注册限制

```
## By default the frequency of account registrations from the same IP
## is limited to 1 account every 10 minutes. To disable, specify: infinity
registration_timeout: infinity
```

默认情况, 同一个IP的账号注册频率限制为每隔10分钟一次, 设置为`infinity`可禁用此限制



## 配置管理员账号

```
acl:
  ##
  ## The 'admin' ACL grants administrative privileges to XMPP accounts.
  ## You can put here as many accounts as you want.
  ##
  admin:
     user:
       - "root": "xmpp.hezhiqiang.info"
```

## 虚拟主机配置

```
hosts:
  - "host1.hezhiqiang.info"
  - "host2.hezhiqiang.info"
```

## 设置服务器消息默认语言

```
language: "en"
```

## 设置虚拟主机默认语言,覆盖服务器设置

```
host_config:
  "xmpp.hezhiqiang.info":
    language: "zh"
```

## 模块配置

```
modules:
  mod_adhoc: {}
  mod_announce: # recommends mod_adhoc
    access: announce
  mod_blocking: {} # requires mod_privacy
  mod_caps: {}
  mod_carboncopy: {}
  mod_client_state:
    drop_chat_states: true
    queue_presence: false
  mod_configure: {} # requires mod_adhoc
  mod_disco: {}
  mod_echo: {}
  mod_irc: {}
  mod_http_bind: {}
  mod_http_fileserver:
    docroot: "/var/www"
    accesslog: "/var/log/ejabberd/access.log"
  mod_last: {}
  mod_muc:
    ## host: "conference.@HOST@"
    access: muc
    access_create: muc_create
    access_persistent: muc_create
    access_admin: muc_admin
  mod_muc_log: {}
  mod_offline:
    access_max_user_messages: max_user_offline_messages
  mod_ping: {}
  mod_pres_counter:
    count: 5
    interval: 60
  mod_privacy: {}
  mod_private: {}
  mod_proxy65: {}   # XEP-0065 SOCKS5 Bytestreams
  mod_pubsub:
    access_createnode: pubsub_createnode
    ## reduces resource comsumption, but XEP incompliant
    ignore_pep_from_offline: true
    ## XEP compliant, but increases resource comsumption
    ## ignore_pep_from_offline: false
    last_item_cache: false
    plugins:
      - "flat"
      - "hometree"
      - "pep" # pep requires mod_caps
  ## mod_pubsub_odbc:
  ##
  mod_register:
  ##
    ## Protect In-Band account registrations with CAPTCHA.
    ##
    ## captcha_protected: true
    ##
    ## Set the minimum informational entropy for passwords.
    ##
    ## password_strength: 32
    ##
    ## After successful registration, the user receives
    ## a message with this subject and body.
    ##
    welcome_message:
    subject: "Welcome!"
    body: |-
      Hi.
      Welcome to this XMPP server.
    ##
    ## When a user registers, send a notification to
    ## these XMPP accounts.
    ##
    ## registration_watchers:
    ##   - "admin1@example.org"
    ##
    ## Only clients in the server machine can register accounts
    ##
    ip_access: trusted_network
    ##
    ## Local c2s or remote s2s users cannot register accounts
    ##
    access_from: register
    access: register
  mod_roster: {}
  mod_shared_roster: {}
  mod_stats: {}
  mod_time: {}
  mod_vcard: {}
  mod_version:
    show_os: true
    iqdisc: parallel
```


## 数据库配置

```
###   ==============
###   DATABASE SETUP

## ejabberd by default uses the internal Mnesia database,
## so you do not necessarily need this section.
## This section provides configuration examples in case
## you want to use other database backends.
## Please consult the ejabberd Guide for details on database creation.

##
## MySQL server:
##
## odbc_type: mysql
## odbc_server: "server"
## odbc_database: "database"
## odbc_username: "username"
## odbc_password: "password"
##
## If you want to specify the port:
## odbc_port: 1234

##
## PostgreSQL server:
##
## odbc_type: pgsql
## odbc_server: "server"
## odbc_database: "database"
## odbc_username: "username"
## odbc_password: "password"
##
## If you want to specify the port:
## odbc_port: 1234
##
## If you use PostgreSQL, have a large database, and need a
## faster but inexact replacement for "select count(*) from users"
##
## pgsql_users_number_estimate: true

##
## ODBC compatible or MSSQL server:
##
## odbc_type: odbc
## odbc_server: "DSN=ejabberd;UID=ejabberd;PWD=ejabberd"

##
## Number of connections to open to the database for each virtual host
##
## odbc_pool_size: 10

##
## Interval to make a dummy SQL request to keep the connections to the
## database alive. Specify in seconds: for example 28800 means 8 hours
##
## odbc_keepalive_interval: undefined
```