title: Tsung 用户手册
categories:
  - QA
tags:
  - testing
  - erlang
  - tsung
toc: true
date: 2014-09-15 13:29:37
---

`Tsung`是用Erlang开发的一个大规模负载测试工具, 由于Erlang天生就是为大规模并发设计的, 因此`Tsung`可以模拟大规模的用户操作, 这是Erlang本身给Tsung带来的优势.

<!-- more -->

## 服务器和客户端

Scenarios start with clients (Tsung cluster) and server definitions:

### 基本设置

对于`非分布式`负载, 可以使用基本设置如下:

```
<clients>
  <client host="localhost" use_controller_vm="true"/>
</clients>
<servers>
  <server host="192.168.1.1" port="80" type="tcp"></server>
</servers>
```

这会在同一台机器的同一个Erlang虚拟机中启动负载测试

servers是进入集群的入口点. 可以点击多个服务器, 默认每个服务器的权重为`1`, 并且每个会话会依据权重`随机`选择一个服务器. 你可以像这样设置每个服务器的权重.(权重可以是`整数`,或`浮点数`):

```
<servers>
  <server host="server1" port="80" type="tcp" weight="4"></server>
  <server host="server2" port="80" type="tcp" weight="1"></server>
</servers>
```

(由于`weight`选项是在版本 `1.5.0` 之后实现的, 之前的版本采用轮训 `round robin` 的方式选择服务器).
`type`可以是 `tcp`, `ssl`, `udp` (对应的IPv6为 `tcp6`, `ssl6`, `udp6`, 在版本1.4.2及以上可用), 或者 `websocket` (仅在1.5.0及高版本可用)


### 高级设置

The next example is more complex, and use several features for advanced distributed testing:

```
<clients>
  <client host="louxor" weight="1" maxusers="800">
    <ip value="10.9.195.12"></ip>
    <ip value="10.9.195.13"></ip>
  </client>
  <client host="memphis" weight="3" maxusers="600" cpu="2"/>
</clients>
<servers>
  <server host="10.9.195.1" port="8080" type="tcp"></server>
</servers>
```

Several virtual IP can be used to simulate more machines. This is very useful when a load-balancer use the client’s IP to distribute the traffic among a cluster of servers. New in 1.1.1: IP is no longer mandatory. If not specified, the default IP will be used.

New in 1.4.0: You can use `<ip scan="yes" value="eth0"/>` to scan for all the IP aliases on a given interface (`eth0` in this example).

In this example, a second machine is used in the Tsung cluster, with a higher weight, and 2 cpus. Two Erlang virtual machines will be used to take advantage of the number of CPU.

> Note Even if an Erlang VM is now able to handle several CPUs (erlang SMP), benchmarks shows that it’s more efficient to use one VM per CPU (with SMP disabled) for tsung clients. Only the controller node is using SMP erlang. Therefore, `cpu` should be equal to the number of cores of your nodes. If you prefer to use erlang SMP, add the `-s` option when starting tsung (and don’t set cpu in the config file).

By default, the load is distributed uniformly on all CPU (one CPU per client by default). The weight parameter (integer) can be used to take into account the speed of the client machine. For instance, if one real client has a weight of 1 and the other client has a weight of 2, the second one will start twice the number of users as the first (the proportions will be 1/3 and 2/3). In the earlier example where for the second client has 2 CPU and weight=3, the weight is equal to 1.5 for each CPU.

#### maxusers


### Running Tsung with a job scheduler

Tsung is able to get its client node list from a batch/job scheduler. It currently handle PBS/torque, LSF and OAR. To do this, set the `type` attribute to `batch`, e.g.:

```
<client type="batch" batch="torque" maxusers="30000">
```

If you need to scan IP aliases on nodes given by the batch scheduler, use scan_intf like this:

```
<client type="batch" batch="torque" scan_intf='eth0' maxusers="30000">
```

## 6.3 监控

Tsung能够使用多个与远程代理通信的后端监控远程服务器. 这是配置在`<monitoring>`节中的. 可用的统计有: CPU活动, 平均负载和内存用量.

你可以从一个作业调度器获取要监控的节点, 比如:

    <monitor batch="true" host="torque" type="erlang"></monitor>

支持多种远程代理(默认为erlang):

### 6.3.1 Erlang

远程代理由Tsung启动. 其使用erlang通信机制来获取服务器上的统计信息. 例如, 下面是一个基于Erlang代理的用于监控包含6个计算机的集群:

    <monitoring>
      <monitor host="geronimo" type="erlang"></monitor>
      <monitor host="bigfoot-1" type="erlang"></monitor>
      <monitor host="bigfoot-2" type="erlang"></monitor>
      <monitor host="f14-1" type="erlang"></monitor>
      <monitor host="f14-2" type="erlang"></monitor>
      <monitor host="db" type="erlang"></monitor>
    </monitoring>

> 注意: 被监控的计算机需要能够通过网络访问, 并且必须允许erlang通信(没防火墙更好). 需要配置SSH(或 rsh)允许无密码访问. 必须在所有节点上使用相同版本的Erlang/OTP, 否则可能不能正常工作!

如果不能在远程服务器上安装Erlang, 可以使用其他可用的代理.

*1.5.1中新增*

erlang 监控包含一个选项, 用mysqladmin来监控mysql数据库. 像这样使用:

    <monitor host="db" type="erlang"></monitor>
     <mysqladmin port="3306" username="root" password="sesame" />
    </monitor>

可用的统计:

- mysql线程数
- 查询数


### SNMP

### Munin

*1.3.1新增.*
