title: Docker - 配置通过TLS加密连接访问Docker服务
categories:
  - Docker
tags:
  - tls
  - docker
toc: false
date: 2014-09-08 19:12:07
---


## 生成证书

要能够通过TLS加密连接访问Docker服务,必须要使用证书,Docker 采用证书的方式来授权合法用户的访问.

<!-- more -->

[这里][1] 有一个Ruby脚本用来生成服务器端证书和客户端证书, 命令行如下:

```
$ gem install certificate_authority
$ ruby certgen.rb example.com
CA certificates are in ~/.docker/ca
Client certificates are in ~/.docker
Server certificates are in ~/.docker/example.com
```

## 修改启动参数


编辑配置文件 `/etc/default/docker`, 修改`DOCKER_OPTS`参数为:

1.配置启动参数
    ```
    DOCKER_OPTS="--tlsverify -H=unix:///var/run/docker.sock -H=0.0.0.0:4243 --tlscacert=/root/.docker/ca.pem --tlscert=/root/.docker/cert.pem --tlskey=/root/.docker/key.pem"
                 =========== ============================== =============== ================================ ================================ ===============================
                   |                |                           |                                  |                       |                        |
             打开TLS支持    Unix套接字,本机程序可访问  监听网络端口:4243,可通过外部网络访问            CA证书                服务器证书                     秘钥
    ```
2.重启服务
    ```
    service docker restart
    ```
3.调试日志
    如果因为配置错误导致服务启动不起来,可以查看日志文件排错.
    ```
    /var/log/upstart/docker.log
    ```



## 参考文献

1. 用TLS远程访问Docker
http://sheerun.net/2014/05/17/remote-access-to-docker-with-tls



  [1]: https://gist.github.com/sheerun/ccdeff92ea1668f3c75f