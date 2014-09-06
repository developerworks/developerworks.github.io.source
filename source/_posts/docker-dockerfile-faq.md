title: Docker -  Dockerfile 常见问题
categories:
  - docker
tags:
  - faq
toc: false
date: 2014-09-04 23:55:05
---


- 如何解决构建镜像是 `debconf: unable to initialize frontend: Dialog` 的问题

```
RUN echo 'debconf debconf/frontend select Noninteractive' | debconf-set-selections
```

- 如何在构建镜像之前切换Ubuntu下载源

```
# 备份
RUN cp /etc/apt/sources.list /etc/apt/sources.list.bak
# 替换
RUN sed -i "s/archive.ubuntu.com/debian.ustc.edu.cn/g" /etc/apt/sources.list
```