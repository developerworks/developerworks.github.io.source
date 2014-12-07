title: Ejabber 集群
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





配置`/etc/ejabberd/ejabberdctl.cfg`,修改节点名称`ERLANG_NODE`

```
#.
#' ERLANG_NODE: Erlang node name
#
# The next variable allows to explicitly specify erlang node for ejabberd
# It can be given in different formats:
# ERLANG_NODE=ejabberd
#   Lets erlang add hostname to the node (ejabberd uses short name in this case)
# ERLANG_NODE=ejabberd@hostname
#   Erlang uses node name as is (so make sure that hostname is a real
#   machine hostname or you'll not be able to control ejabberd)
# ERLANG_NODE=ejabberd@hostname.domainname
#   The same as previous, but erlang will use long hostname
#   (see erl (1) manual for details)
#
# Default: ejabberd@localhost
#
ERLANG_NODE=ejabberd@scm
```

## 参考资料

1. http://chad.ill.ac/post/35967173942/easy-ejabberd-clustering-guide-mnesia-mysql
2. https://raymii.org/s/tutorials/Set_up_a_federated_XMPP_Chat_Network_with_ejabberd.html
3. https://www.ejabberd.im/node/5669?q=node/5669
4. http://cqwjfck.blog.chinaunix.net/uid-22312037-id-3509097.html
5. http://chad.ill.ac/post/55193155663/easy-ejabberd-clustering-multi-master-fault-tolerant
6. http://tdewolf.blogspot.com/2009/07/clustering-ejabberd-nodes-using-mnesia.html