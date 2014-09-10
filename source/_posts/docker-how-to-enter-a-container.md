title: Docker - 如何进入一个正在运行的容器
categories:
  - docker
tags:
  - container
  - docker
toc: false
date: 2014-09-07 18:56:34
---

遇到这个需求是因为由于长时间登陆到SSH服务器没有活动,超时自动断线. 重新登陆SSH后丢失了容器的SHELL,需要重新进入容器.

搜刮了各种引擎后,主要有`lxc-attach`,`nsenter`, `nsinit`几种方式, 其中`lxc-attach`官方已经不建议使用,`nsinit`是官官方基于`libcontainer`开发的一个容器管理工具,需要安装`Golang`运行环境,稍微麻烦, 为了方便选用`nsenter`

`Ubuntu 14.04`的`util-linux`包是2.20版本的, `nsenter` 在`2.23`中才有, 所以需要编译`util-linux`包的源码

<!-- more -->

## nsenter

```
cd /tmp
wget https://www.kernel.org/pub/linux/utils/util-linux/v2.24/util-linux-2.24.tar.gz
tar zxf util-linux-2.24.tar.gz
cd util-linux-2.24
./configure --without-ncurses
make nsenter
cp nsenter /usr/local/bin
```

为了连接到容器,需要找出容器的第一个进程的PID:

首先查看一下容器的ID
```
root@localhost:~# docker ps --no-trunc
CONTAINER ID                                                       IMAGE               COMMAND             CREATED             STATUS              PORTS                    NAMES
27db60e7b9537954d62892eee64aa320f23923f5bee833f54d39fb3b9379f0c3   d9d3a87018ae        "/bin/bash"         About an hour ago   Up About an hour    0.0.0.0:8080->8080/tcp   evil_pare
```

其次, 找出容器第一个进程的PID

{% raw %}
```
root@localhost:~# docker inspect --format "{{ .State.Pid }}" 27db60e7b9537954d62892eee64aa320f23923f5bee833f54d39fb3b9379f0c3
27188
```
{% endraw %}

我们看到容器第一个进程的PID为`27188`,接下来就可以通过`nsenter`进入容器了

```
root@localhost:~# nsenter --target 27188 --mount --uts --ipc --net --pid
```

设置一下别名,减少输入

```
root@localhost:~# alias nsenter='nsenter --mount --uts --ipc --net --pid --target'
```

现在我们就可以这样输入了:

```
root@localhost:~# nsenter 27188
```

## lxc-attach

Docker `0.9`之后的`lxc-attach`方式已经被弃用,如需使用`lxc-attach`,要修改一个Docker的启动参数,编辑`/etc/default/docker`配置文件,设置选项`DOCKER_OPTS="-e lxc"`并重启Docker服务

```
# /etc/default/docker
DOCKER_OPTS="-e lxc"
```

重启 docker 服务

```
service docker restart
```

## nsinit


## 参考资料

1. https://blog.codecentric.de/en/2014/07/enter-docker-container/