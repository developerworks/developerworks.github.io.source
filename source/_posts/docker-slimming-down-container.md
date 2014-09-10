title: Docker - 给容器瘦身
categories:
  - docker
tags:
  - container
  - docker
toc: false
date: 2014-09-04 12:55:22
---


通过导出然后再导入,可以减小容器的磁盘占用大小, 首先需要通过一个镜像在后台启动一个容器

## 给容器瘦身

```
# 非交互式方式,启动后立即停止,我们通过这种方式获取一个容器的ID
root@localhost:~# ID=$(docker run -d developerworks/base /bin/bash)
# 查看容器ID
root@localhost:~# echo $ID
dbbd685a6391418a24493f8a07df62b3be6e813fc217841a80b739c96a12e426
root@localhost:~# docker export dbbd685a6391418a24493f8a07df62b3be6e813fc217841a80b739c96a12e426 | docker import - base
```

## 备份容器

```
root@localhost:~# ID=$(docker run -d developerworks/base /bin/bash)
root@localhost:~# echo $ID
dbbd685a6391418a24493f8a07df62b3be6e813fc217841a80b739c96a12e426
# 导出并压缩
root@localhost:~# docker export dbbd685a6391418a24493f8a07df62b3be6e813fc217841a80b739c96a12e426 | gzip -c > developerworks-base.tgz
```


## 参考资料

1. http://www.hnwatcher.com/r/650964/Slimming-down-Docker-containers-Intercity-Blog