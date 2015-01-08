title: Elixir 在Mac OS X上安装Hex.pm服务器证书不信任的问题
categories:
  - Elixir
tags:
  - Hex.pm
  - SSL
  - CURL
toc: false
date: 2015-01-08 15:32:31
---

我的Mac OS X是用的MacPorts作为第三方软件包的管理器.

wget是通过MacPorts安装的, 默认是不会包含服务器根证书的, 需要通过如下命令安装:

```
sudo port install curl-ca-bundle
```

## 参考资料:

1. http://blog.55minutes.com/2012/01/fixing-https-certificate-errors-in-wget-and-ruby/