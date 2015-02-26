title: HTTP/2 尝鲜
categories:
  - HTTP2
tags:
  - H2O
toc: false
date: 2015-02-25 17:16:51
---

最近发布了HTTP/2的正式标准, 下面通过简单几个步骤尝尝鲜.

## 软件要求:

- Firefox/36.0 (36.0正式支持HTTP/2)


- H20/1.0

编译和安装

```
apt-get install -y cmake
cd /tmp
git clone https://github.com/h2o/h2o.git
cd h2o
cmake -DCMAKE_INSTALL_PREFIX=/usr/local .
make
sudo make install
```

- 启动

```
h2o -c examples/h2o/h2o.conf
```

- 打开Firefox在地址栏中输入:

```
# HTTP
http://192.168.8.200:8080
# HTTPS
https://192.168.8.200:8081
```

![It works](/assets/http2/487154F3-B4E1-45BA-B157-4B12184FF14C.png)


## 参考资料

1. HTTP/2服务器实现清单
https://github.com/http2/http2-spec/wiki/Implementations