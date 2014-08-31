title: Docker - 管理镜像
categories:
  - docker
tags:
  - manage images
toc: true
date: 2014-08-31 11:29:07
---

本文将会说明通过Docker命令行如何显示本机当前的镜像, 如何拉取镜像, 如何搜索Docker hub上的镜像, 以及如何创建自定义镜像并提交到Docker hub

<!--more-->

## 列出本地已下载镜像

```
root@localhost:~$ sudo docker images
REPOSITORY          TAG                 IMAGE ID            CREATED             VIRTUAL SIZE
ubuntu              14.04               c4ff7513909d        2 weeks ago         213 MB
ubuntu              latest              c4ff7513909d        2 weeks ago         213 MB
training/webapp     latest              31fa814ba25a        3 months ago        278.6 MB
```

## 下载一个新的镜像

```
root@localhost:~$ sudo docker pull centos
Pulling repository centos
b157b77b1a65: Download complete
b1bd49907d55: Download complete
511136ea3c5a: Download complete
34e94e67e63a: Download complete
```

## 搜索镜像

```
root@localhost:~$ sudo docker search gerrit
NAME                      DESCRIPTION                                     STARS     OFFICIAL   AUTOMATED
edgester/gerrit           The Gerrit code review system. v2.7 using ...   3
nikolas/gerrit            A Docker container for Gerrit code review....   2
tylerwhall/gerrit-psql                                                    0                    [OK]
eclipsesource/es-gerrit   Internally used Gerrit container                0
jyonkov/gerrit                                                            0
bartk/gerrit                                                              0
erwyn/jenkins-gerrit                                                      0
larrycai/gerrit           The Gerrit code review system. v2.8 using ...   0                    [OK]
jgeewax/gerrit            Gerrit + Apache 2. See https://github.com/...   0
```

## 创建自己的镜像

1. 更新从一个镜像创建的容器, 并把更新提交回镜像
2. 可以使用`Dockerfile`创建一个镜像

### 更新和提交镜像

我以官方的`ubuntu`镜像为基础,创建自己的一个镜像, 其中通过`apt-get`安装了`git`版本管理工具

- 启动一个容器,并安装`git`工具

```
root@localhost:~$ sudo docker run -i -t ubuntu /bin/bash
root@89eb687c9446:/#
root@89eb687c9446:/# apt-get install -y git
```

- 保存一个容器为镜像

记录下需要保存的容器ID `89eb687c9446`, 并退出容器

```
root@89eb687c9446:/# exit
```

执行

```
root@localhost:~$ sudo docker commit -m="Install git" -a="Developerworks Dev" 89eb687c9446 developerworks/ubuntu:dev
1c34d940014238623d5b4bcc92684db4af8cf0d59d67e28597ec858984ce8ce4
```

终端输出一个64位长度的字符串, 表示成功提交, 我们执行`docker images`显示一下当前本机的镜像列表

```
root@mecil:~# docker images
REPOSITORY              TAG                 IMAGE ID            CREATED             VIRTUAL SIZE
developerworks/ubuntu   dev                 1c34d9400142        12 seconds ago      321.9 MB
ubuntu                  14.04               c4ff7513909d        2 weeks ago         213 MB
ubuntu                  latest              c4ff7513909d        2 weeks ago         213 MB
centos                  centos6             b1bd49907d55        4 weeks ago         212.5 MB
centos                  centos7             b157b77b1a65        4 weeks ago         243.7 MB
centos                  latest              b157b77b1a65        4 weeks ago         243.7 MB
training/webapp         latest              31fa814ba25a        3 months ago        278.6 MB
```

在上面的`docker images`命令输出中, 我们看到刚才创建的`developerworks/ubuntu`仓库, 其镜像ID为`1c34d9400142`, 到此, 通过修改容器创建一个镜像的过程就是这样, 接下来说明如何通过`Dockerfile`创建一个全新的镜像

输入`docker commit`可查看命令行帮助:

```
Usage: docker commit [OPTIONS] CONTAINER [REPOSITORY[:TAG]]
Create a new image from a container's changes
-a, --author=""     Author (e.g., "John Hannibal Smith <hannibal@a-team.com>")
-m, --message=""    Commit message
-p, --pause=true    Pause container during commit
```

### 从Dockerfile构建镜像

`docker commit`命令是一个非常简单的扩展一个镜像的命令. 但是它有些笨重, 并且在团队中分享镜像的开发过程不是一件容易的事情. 为此, 我们用`docker build`来从头构造一个全新的镜像.

首先, 需要创建一个`Dockerfile`来告知如何构造一个镜像. 下面的命令是编译`Node.js`源码并安装

```
# 创建工作目录
$ mkdir image && cd image
# 创建Dockerfile
$ echo "FROM ubuntu:14.04
RUN echo 'debconf debconf/frontend select Noninteractive' | debconf-set-selections
MAINTAINER developerworks <developerworks@163.com>
RUN apt-get update
RUN apt-get install -y build-essential
RUN apt-get install -y wget
RUN apt-get install -y python
RUN wget http://nodejs.org/dist/v0.10.31/node-v0.10.31.tar.gz
RUN tar zxf node-v0.10.31.tar.gz && cd node-v0.10.31 && ./configure && make && make install" > Dockerfile
# 构建镜像
$ docker build -t="developerworks/ubuntu-nodejs-runtime" .
# 或者
$ docker build -t="developerworks/ubuntu-nodejs-runtime" http://developerworks.github.io/code/docker/Dockerfile
```

建议一个`RUN`只运行单条命令, 成功创建镜像后,`RUN`命令的结果实际上是缓存过的, 再次执行同一个`RUN`命令的时候`Docker`会直接使用缓存过的运行结果.

然后,如果你需要更新这个Image, 增加一些npm包, 你只需要把 `FROM ubuntu:14.04` 改为 `FROM developerworks/ubuntu-nodejs-runtime`, 删除所有`RUN`指令, 并添加 `RUN npm install -g forever`, 新的`Dockerfile`如下:

```
FROM developerworks/ubuntu-nodejs-runtime
MAINTAINER developerworks <developerworks@163.com>
RUN npm install -g forever
```

执行:

```
root@localhost:~/images# docker build -t="developerworks/ubuntu-nodejs-runtime:add-forever" .
Sending build context to Docker daemon  2.56 kB
Sending build context to Docker daemon
Step 0 : FROM developerworks/ubuntu-nodejs-runtime
 ---> 2c01d3956f80
Step 1 : MAINTAINER developerworks <developerworks@163.com>
 ---> Running in 24f1d1115c7d
 ---> 22218cb058df
Removing intermediate container 24f1d1115c7d
Step 2 : RUN npm install -g forever
 ---> Running in 4278228003b9
npm WARN engine hawk@0.10.2: wanted: {"node":"0.8.x"} (current: {"node":"0.10.31","npm":"1.4.23"})
npm WARN engine hawk@0.10.2: wanted: {"node":"0.8.x"} (current: {"node":"0.10.31","npm":"1.4.23"})
npm WARN engine hawk@0.10.2: wanted: {"node":"0.8.x"} (current: {"node":"0.10.31","npm":"1.4.23"})
npm WARN engine hoek@0.7.6: wanted: {"node":"0.8.x"} (current: {"node":"0.10.31","npm":"1.4.23"})
npm WARN engine boom@0.3.8: wanted: {"node":"0.8.x"} (current: {"node":"0.10.31","npm":"1.4.23"})
npm WARN engine cryptiles@0.1.3: wanted: {"node":"0.8.x"} (current: {"node":"0.10.31","npm":"1.4.23"})
npm WARN engine sntp@0.1.4: wanted: {"node":"0.8.x"} (current: {"node":"0.10.31","npm":"1.4.23"})
npm WARN engine hoek@0.7.6: wanted: {"node":"0.8.x"} (current: {"node":"0.10.31","npm":"1.4.23"})
npm WARN engine boom@0.3.8: wanted: {"node":"0.8.x"} (current: {"node":"0.10.31","npm":"1.4.23"})
npm WARN engine cryptiles@0.1.3: wanted: {"node":"0.8.x"} (current: {"node":"0.10.31","npm":"1.4.23"})
npm WARN engine sntp@0.1.4: wanted: {"node":"0.8.x"} (current: {"node":"0.10.31","npm":"1.4.23"})
npm WARN engine hoek@0.7.6: wanted: {"node":"0.8.x"} (current: {"node":"0.10.31","npm":"1.4.23"})
npm WARN engine boom@0.3.8: wanted: {"node":"0.8.x"} (current: {"node":"0.10.31","npm":"1.4.23"})
npm WARN engine cryptiles@0.1.3: wanted: {"node":"0.8.x"} (current: {"node":"0.10.31","npm":"1.4.23"})
npm WARN engine sntp@0.1.4: wanted: {"node":"0.8.x"} (current: {"node":"0.10.31","npm":"1.4.23"})
/usr/local/bin/forever -> /usr/local/lib/node_modules/forever/bin/forever
/usr/local/bin/foreverd -> /usr/local/lib/node_modules/forever/bin/foreverd
forever@0.11.1 /usr/local/lib/node_modules/forever
├── watch@0.8.0
├── colors@0.6.2
├── pkginfo@0.3.0
├── timespan@2.3.0
├── nssocket@0.5.1 (eventemitter2@0.4.14, lazy@1.0.11)
├── optimist@0.6.1 (wordwrap@0.0.2, minimist@0.0.10)
├── utile@0.2.1 (deep-equal@0.2.1, rimraf@2.2.8, ncp@0.4.2, async@0.2.10, i@0.3.2, mkdirp@0.5.0)
├── nconf@0.6.9 (ini@1.2.1, async@0.2.9, optimist@0.6.0)
├── cliff@0.1.8 (eyes@0.1.8, winston@0.6.2)
├── winston@0.7.3 (cycle@1.0.3, stack-trace@0.0.9, async@0.2.10, eyes@0.1.8, request@2.16.6)
├── flatiron@0.3.11 (optimist@0.6.0, director@1.1.10, prompt@0.2.11, broadway@0.2.9)
└── forever-monitor@1.2.3 (watch@0.5.1, minimatch@0.2.14, utile@0.1.7, ps-tree@0.0.3, broadway@0.2.9)
 ---> f51d8fe3e51d
Removing intermediate container 4278228003b9
Successfully built f51d8fe3e51d
```

## 删除镜像

```
docker rmi <IMAGE ID>
```

### 删除所有容器和镜像

本文要做的实验全部完成,删除全部容器和镜像(建议保留官方的`ubuntu:14.04`镜像,200多MB, 下载需要时间, 由于我是在服务器上测试, 网速不是问题, 所以为了节约磁盘空间,全删除.

```
#!/bin/bash
# Delete all containers
docker rm $(docker ps -a -q)
# Delete all images
docker rmi $(docker images -q)
```





