title: Ejabberd 集群
categories:
  - Communication
  - Ejabberd
tags:
  - ejabberd
  - clustering
toc: false
date: 2014-09-23 10:36:08
---

## 安装Erlang 17.3 (Ubuntu 14.04)



## 配置工作流概要

- 修改两个服务器的HOSTNAME
- 修改两个服务器的`/etc/hosts`(内网)或DNS(外网)
- 修改`/etc/ejabberd/ejabberd.yml`的`hosts`
    -  设置虚拟主机名称
- 修改`/etc/ejabberd/ejabberdctl.cfg`
    -  修改`ERLANG_NODE`(节点名)
    -  修改`INET_DIST_INTERFACE`(节点地址)
- 下载和编译`easy_cluster`
    ```
    wget https://github.com/chadillac/ejabberd-easy_cluster/raw/master/easy_cluster.erl
    mv easy_cluster.erl /lib/ejabberd/ebin
    cd /lib/ejabberd/ebin
    erlc easy_cluster.erl
    ```
- 使用`easy_cluster`

启动ejabberd到live模式

    ```
    ejabberdctl live
    ```

执行

```
easy_cluster:test_node('name@node')
```

- 如何验证集群成功了
    在`ejabberdctl live`模式下执行
    ```
    mnesia:info().
    ```
    查看`running db nodes`, 如果包含两个集群的节点列表. 那么集群就运行起来了.


## 配置示例

将设有两台服务器

```
master.chat-server.info
slave.chat-server.info
```

## 使用MySQL替代Mnesia

登陆MySQL

```
mysql> mysql -uroot -proot --host=192.168.8.33
```

创建数据库

```
create database ejabberd charset = utf8
```

导入表结构

```
source /root/ejabberd/sql/mysql.sql
```

Ejabberd的部分模块支持在MySQL存储数据, 比如`mod_roster`, 需要在模块配置中设置`db_type: odbc`并创建数据库表结构, 例如:

编辑`/etc/ejabberd/ejabber.yml`,设置`mod_roster`模块的`db_type: odbc`

```
mod_roster:
  db_type: odbc
```

- 修改`/etc/hosts`, 在两台服务器上分别增加如下记录:
```
192.168.8.100   ejabberd@master.chat-server.info
192.168.8.200   ejabberd@slave.chat-server.info
```
- 两台服务器上分别设置HOSTNAME为`master.chat-server.info`, 和`slave.chat-server.info`
```
vi /etc/hostname
```


两台服务器分别配置`/etc/ejabberd/ejabberdctl.cfg`:

- 修改节点名称`ERLANG_NODE`

        ```
        # master
        ERLANG_NODE=ejabberd@master.chat-server.info
        # slave
        ERLANG_NODE=ejabberd@slave.chat-server.info
        ```

- 分别设置`/etc/ejabberd/ejabberd.yml`中的hosts

        ```
        # master
        hosts:
          - "master.chat-server.info"
        # slave
        hosts:
          - "slave.chat-server.info"
        ```


## 参考资料

1. http://chad.ill.ac/post/35967173942/easy-ejabberd-clustering-guide-mnesia-mysql
2. https://raymii.org/s/tutorials/Set_up_a_federated_XMPP_Chat_Network_with_ejabberd.html
3. https://www.ejabberd.im/node/5669?q=node/5669
4. http://cqwjfck.blog.chinaunix.net/uid-22312037-id-3509097.html
5. http://chad.ill.ac/post/55193155663/easy-ejabberd-clustering-multi-master-fault-tolerant
6. http://tdewolf.blogspot.com/2009/07/clustering-ejabberd-nodes-using-mnesia.html