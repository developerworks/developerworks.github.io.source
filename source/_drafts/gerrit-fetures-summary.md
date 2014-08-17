title: Gerrit 
date: 2014-08-16 17:02:11
categories:
  - git
tags:
  - gerrit
---

## 启动,重启,停止

```
$ /review/bin/gerrit.sh start
$ /review/bin/gerrit.sh stop
$ /review/bin/gerrit.sh restart
```

## 配置自动启动,停止

取消`SHELL`脚本`$review_site/bin/gerrit.sh`的三行注释

```
chkconfig: 3 99 99
description: Gerrit Code Review
processname: gerrit
```

连接启动脚本到系统初始化目录:

```
sudo ln -snf `pwd`/review_site/bin/gerrit.sh /etc/init.d/gerrit
sudo ln -snf /etc/init.d/gerrit /etc/rc3.d/S90gerrit
```