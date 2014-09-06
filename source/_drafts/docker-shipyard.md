title: docker-shipyard
categories:
  - null
tags:
  - null
toc: false
date: 2014-09-05 14:02:13
---

项目地址: http://shipyard-project.com/

## 安装 Shipyard

Shipyard 是一个Web管理工具, 用于管理Docker相关的资源, 比如导入,构建镜像,创建,停止,启动,删除容器等操作.

```

```

## 安装 Shipyard Agent

`Shipyard Agent` 是一个代理, 用于手机Docker相关的信息并提供给`Shipyard`

```
docker run -i -t -v /var/run/docker.sock:/docker.sock \
  -e IP=`ifconfig eth0 | grep 'inet addr:' | cut -d: -f2 | awk '{print $1;}'` \
  -e URL=http://192.168.8.33:8000 -p 4500:4500 shipyard/agent
```



## 参考资料

1. http://shipyard-project.com/
2.