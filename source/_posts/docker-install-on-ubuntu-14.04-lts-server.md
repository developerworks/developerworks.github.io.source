title: Docker - 在Ubuntu 14.04 Server上的安装Docker
categories:
  - docker
tags:
  - docker container
date: 2014-08-27 15:33:35
---


在 `Ubuntu 14.04 Server` 上安装过程是最简单的, 其满足了安装 `Docker`的所有要求,只需要执行如下安装脚本即可. 如果你有可能,请使用`14.04`版本的Ubuntu, 避免给自己挖坑.

<!-- more -->

# 安装

```
curl -sSL https://get.docker.io/ubuntu/ | sudo sh
```

# 测试

```
sudo docker run -i -t ubuntu /bin/bash
```

这一步会进入容器并运行一个交互式SHELL.你可以安装一些软件, 比如 `apt-get install git`,然后在克隆一个项目 `https://github.com/developerworks/bespoke-fx.git`

```
root@148bc0a45428:/root# git clone https://github.com/developerworks/bespoke-fx.git
Cloning into 'bespoke-fx'...
remote: Counting objects: 53, done.
remote: Compressing objects: 100% (37/37), done.
remote: Total 53 (delta 16), reused 47 (delta 10)
Unpacking objects: 100% (53/53), done.
Checking connectivity... done.
```

`/root` 目录下是我们刚才 `clone` 下来的一个项目

```
root@148bc0a45428:/root# ls -al
total 20
drwx------  3 root root 4096 Aug 27 07:49 .
drwxr-xr-x 21 root root 4096 Aug 27 07:39 ..
-rw-r--r--  1 root root 3106 Feb 20  2014 .bashrc
-rw-r--r--  1 root root  140 Feb 20  2014 .profile
drwxr-xr-x  7 root root 4096 Aug 27 07:49 bespoke-fx
```

现在我们 `exit` 退出容器.

我们再次执行

```
sudo docker run -i -t ubuntu /bin/bash
cd /root
ls -al
```

刚才`clone`的`bespoke-fx`目录消失了, 容器的生命周期到此结束, 其容器内的资源也一并消失.

## 使用 Docker hub 中央镜像仓库

首先需要注册一个账号, 注册账号有两种方式, 一种是通过Web表单注册, 另一种是通过命令行注册

### 通过表单注册

进入下面的链接, 转到注册页面, 填写用户名,密码,邮件. 稍后你会收到一封激活邮件, 进入优先点击激活按钮即可.

```
https://hub.docker.com/account/signup/
```

### 通过命令行注册

运行 `sudo docker login`, 按照提示填写用户名,密码和邮件.

```
$ sudo docker login
Username: fill-your-name
Password:
Email: fill-your-email-address@gmail.com
Account created. Please use the confirmation link we sent to your e-mail to activate it.
```

再次执行 `sudo docker login` 登陆 `Docker hub`

```
$ sudo docker login
Username (rainflowerpebbles):
Login Succeeded
```


## 如何检查,监控和管理容器

- `docker ps` 显示当前运行的容器, 该命令和操作系统的`ps`命令类型, `ps`命令显示当前运行的进程, 而`docker ps`显示当前运行的容器, 为了帮助理解,你可以把容器当做一个正在运行的进程.
- `docker logs` 显示一个容器的标准输出
- `docker stop` 停止一个正在运行的容器

### 显示和停止容器

打开两个终端, 在第一个终端运行 `sudo docker run -i -t ubuntu /bin/bash`

在第二个终端运行如下命令:

```
$ sudo docker ps
CONTAINER ID        IMAGE               COMMAND             CREATED             STATUS              PORTS               NAMES
3fc975a5dcaf        ubuntu:14.04        "/bin/bash"         6 seconds ago       Up 5 seconds                            drunk_perlman
$ sudo docker stop 3fc975a5dcaf
3fc975a5dcaf
```

回到第一个终端,你看到

```
root@3fc975a5dcaf:/# exit
$
```

从容器内运行的SHELL回到了操作系统SHELL


### 显示容器标准输出

还是打开两个终端, 在第一个终端执行:

```
$ sudo docker run -i -t ubuntu /bin/bash
root@2da06a3b3b8a:/# ls
bin  boot  dev	etc  home  lib	lib64  media  mnt  opt	proc  root  run  sbin  srv  sys  tmp  usr  var
```

在第二个终端执行 `sudo docker logs 2da06a3b3b8a`:

```
$ sudo docker logs 2da06a3b3b8a
root@2da06a3b3b8a:/# ls
bin  boot  dev	etc  home  lib	lib64  media  mnt  opt	proc  root  run  sbin  srv  sys  tmp  usr  var
```






