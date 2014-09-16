title: Docker - 镜像依赖关系图
categories:
  - docker
tags:
  - graphviz
  - docker
toc: false
date: 2014-09-09 13:42:37
---

镜像依赖关系图的生成, 需要用到`graphviz`,需要首先安装

```
root@localhost:~# apt-get install -y graphviz
```

<!-- more -->

生成

```
root@localhost:~# docker images -viz | dot -Tpng -o docker.png
Warning: '-viz' is deprecated, it will be removed soon. See usage.
```

**下图可点击放大**

![Docker 镜像依赖关系图][1]


  [1]: /assets/images/docker-image-dependencies.png