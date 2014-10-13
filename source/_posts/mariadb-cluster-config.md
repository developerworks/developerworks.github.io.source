title: Mariadb 集群配置备忘
categories:
  - MySQL
tags:
  - mariadb
date: 2014-08-16 11:36:40
toc: false
---

## 三个节点的`mariadb`集群配置

<!-- more -->

```
[mysqld]
#mysql settings
binlog_format=ROW
default-storage-engine=innodb
innodb_autoinc_lock_mode=2
query_cache_size=0
query_cache_type=0
bind-address=0.0.0.0
#galera settings
wsrep_provider=/usr/lib/galera/libgalera_smm.so
wsrep_cluster_name="my_wsrep_cluster"
wsrep_cluster_address="gcomm://172.168.172.136,172.168.172.137,172.168.172.138"
wsrep_sst_method=rsync



# wsrep provider configuration (cluster)
wsrep_provider=/usr/lib/galera/libgalera_smm.so
wsrep_provider_options="gcache.size=256M; gcache.page_size=128M"
wsrep_cluster_address=gcomm://
wsrep_cluster_name="mariadb_cluster"
wsrep_node_address="db1"
wsrep_node_name="db1"
wsrep_sst_method=xtrabackup
wsrep_sst_auth="root:MyR00tPasswd"
wsrep_node_incoming_address=172.168.172.136
wsrep_sst_receive_address=172.168.172.136
wsrep_slave_threads=16


# wsrep provider configuration
wsrep_provider=/usr/lib/galera/libgalera_smm.so
wsrep_provider_options="gcache.size=256M; gcache.page_size=128M"
wsrep_cluster_address=gcomm://db2
wsrep_cluster_name="mariadb_cluster"
wsrep_node_address="db2"
wsrep_node_name="db2"
wsrep_sst_method=xtrabackup
wsrep_sst_auth="root:MyR00tPasswd"
wsrep_node_incoming_address=172.168.172.137
wsrep_sst_receive_address=172.168.172.137
wsrep_slave_threads=16


# wsrep provider configuration
wsrep_provider=/usr/lib/galera/libgalera_smm.so
wsrep_provider_options="gcache.size=256M; gcache.page_size=128M"
wsrep_cluster_address=gcomm://db3
wsrep_cluster_name="mariadb_cluster"
wsrep_node_address="db3"
wsrep_node_name="db3"
wsrep_sst_method=xtrabackup
wsrep_sst_auth="root:MyR00tPasswd"
wsrep_node_incoming_address=172.168.172.138
wsrep_sst_receive_address=172.168.172.138
wsrep_slave_threads=16
```