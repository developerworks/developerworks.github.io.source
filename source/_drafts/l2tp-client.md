title: Ubuntu 14.04 配置 l2tp 客户端
categories:
  - ubuntu
tags:
  - ipsec
  - l2tp
toc: true
date: 2014-09-02 11:54:33
---

这玩意儿配置起来真的很恼火, 没有图形界面的服务器需要链接到国外的网站更新软件. 翻墙其实并不难,`GFW`总是干扰你,目前比较稳定的方式是是用`L2TP`,可以保持`N`个小时不断线.

## 安装需要的软件包

```
apt-get update
apt-get install -y openswan xl2tpd
```

## 版本信息

xl2tpd version:  **xl2tpd-1.3.6**



## 配置 IPSec

需要修改两个配置文件

```
/etc/ipsec.conf     # IPSec相关的服务配置信息
/etc/ipsec.secrets  # VPN服务器地址,共享秘钥
```

IPSec 服务配置文件 `/etc/ipsec.conf`, **注意此配置文件的缩进是8个空格**

```
# /etc/ipsec.conf - Openswan IPsec configuration file
version	2.0
config setup
        dumpdir=/var/run/pluto/
        nat_traversal=yes
        virtual_private=%v4:10.0.0.0/8,%v4:192.168.0.0/16,%v4:172.16.0.0/12,%v4:25.0.0.0/8,%v6:fd00::/8,%v6:fe80::/10
        oe=off
        protostack=netkey
        plutoopts="--interface=em1"
conn L2TP-PSK
        authby=secret
        pfs=no
        auto=add
        keyingtries=3
        dpddelay=30
        dpdtimeout=120
        dpdaction=clear
        rekey=yes
        ikelifetime=8h
        keylife=1h
        type=transport
        left=%defaultroute
        leftnexthop=%defaultroute
        leftprotoport=17/1701
        #下面是外网VPN服务器IP地址,例如
        right=121.121.121.121
        #rightprotoport=17/1701
```

VPN服务器地址,以及共享密钥配置文件

```
# /etc/ipsec.secrets
%any 119.81.162.106:   PSK     "www.jiachongs.com"
```

验证IPSec

```
root@localhost:~# ipsec verify
Checking your system to see if IPsec got installed and started correctly:
Version check and ipsec on-path                             	[OK]
Linux Openswan U2.6.38/K3.13.0-32-generic (netkey)
Checking for IPsec support in kernel                        	[OK]
 SAref kernel support                                       	[N/A]
 NETKEY:  Testing XFRM related proc values                  	[OK]
	[OK]
	[OK]
Checking that pluto is running                              	[OK]
 Pluto listening for IKE on udp 500                         	[OK]
 Pluto listening for NAT-T on udp 4500                      	[OK]
Checking for 'ip' command                                   	[OK]
Checking /bin/sh is not /bin/dash                           	[WARNING]
Checking for 'iptables' command                             	[OK]
Opportunistic Encryption Support                            	[DISABLED]
```

测试链接

```
ipsec auto --up L2TP-PSK
```


## 配置 L2TP

```
vi /etc/xl2tpd/xl2tpd.conf
```

```
[lac vpn-connection]
lns = <The IP address of your VPN server>
refuse chap = yes
refuse pap = yes
require authentication = yes
name = vpn-server
ppp debug = yes
pppoptfile = /etc/ppp/options.l2tpd.client
length bit = yes
```

## 链接到VPN

## 设置路由通过隧道传递数据

## 脚本

为了方便, 下面给出一些上述过程的批处理脚本

### 启动脚本

```
#!/bin/bash

/etc/rc.d/openswan start
sleep 2
/etc/rc.d/xl2tpd start
/usr/sbin/ipsec auto --up L2TP-PSK
echo "c vpn-connection" > /var/run/xl2tpd/l2tp-control
sleep 2
PPP_GW_ADD=`./getip.sh ppp0`

ip route add 68.68.32.79 via 192.168.1.1 dev eth0
ip route add default via $PPP_GW_ADD
ip route del default via 192.168.1.1
```

## 参考资料

1. https://wiki.archlinux.org/index.php/L2TP/IPsec_VPN_client_setup
2. http://www.elastichosts.com/support/tutorials/linux-l2tpipsec-vpn-client/
3. http://www.jacco2.dds.nl/networking/linux-l2tp.html



