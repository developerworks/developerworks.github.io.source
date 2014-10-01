title: Ejabberd-指南
categories:
  - Communication
tags:
  - ejabberd
  - configuration
  - guide
toc: true
date: 2014-09-29 03:02:44
---

本文参照[Ejabber的官方操作指南][Ejabberd Guide]和 [安装和操作指南(中文)][安装和操作指南],


## 安装Ejabberd

### 用ejabberd二进制安装包安装

### 用操作系统特定的包安装ejabberd

### 用CEAN安装ejabberd

### 从源码安装ejabberd

**需求**

为了在一个`类Unix`操作系统编译`ejabberd`, 你需要:

- GNU Make
- GCC
- Libexpat 1.95 或更高版本
- Erlang/OTP R15B 或更高版本.
- Libyaml 0.1.4 或更高版本.
- OpenSSL 0.9.8 或更高版本, 用于 STARTTLS, SASL 和 SSL 加密.
- Zlib 1.2.3 或更高版本, 用于支持流压缩 ([XEP-0138][XEP-0138]). 可选的.
- Erlang mysql library. 可选的. 用于支持 MySQL 验证或存储. 见 3.2.1 节.
- Erlang pgsql library. 可选的. 用于支持 PostgreSQL 验证或存储. 见 3.2.3 节.
- PAM library. 可选的. 用于F 可插拔的验证模块 (PAM). 见 3.1.5 节.
- GNU Iconv 1.8 或更高版本, 用于 IRC 网关 (mod_irc). 可选的. 在系统里不需要 GNU Libc. 见 3.3.8 节.
- ImageMagick’s 转换程序. 可选的. 用于 CAPTCHA 挑战. 见 3.1.8 节.

## 配置Ejabberd

### 基本配置

### 旧的配置文件

### 主机名称

### 虚拟主机

### 监听端口

### 身份认证

### 访问规则

### Shapers

Shapers使你能够限制一个连接的流量, 其语法为:

`shaper: { ShaperName: Rate }`

`Rate`的含义为: 以字节为单位的最大入站速率. 当超过这个速率, ejabberd将停止读取套接字数据, 知道平均速率降低到低于该值.

例子:

- 定义一个名为`normal`的shaper, 其速度限制为1000字节/秒

```
shaper:
  normal: 1000
```

- 定义一个名为`fast`的shaper,其速度限制为50,000字节/秒(~= 50KB/s)

```
shaper:
  fast: 50000
```


### 默认语言

The option `language` defines the default language of server strings that can be seen by XMPP clients. If a XMPP client does not support `xml:lang`, the specified language is used.

The option syntax is:

`language: Language`

The default value is `en`. In order to take effect there must be a translation file `Language.msg` in ejabberd's `msgs` directory.

For example, to set Russian as default language:

```
language: "ru"
```

Appendix A provides more details about internationalization and localization.

### CAPTCHA

Some `ejabberd` modules can be configured to require a CAPTCHA challenge on certain actions. If the client does not support CAPTCHA Forms ([XEP-0158][XEP-0158]), a web link is provided so the user can fill the challenge in a web browser.

An example script is provided that generates the image using ImageMagick's Convert program.

配置示例:

```
hosts: ["example.org"]
captcha_cmd: "/lib/ejabberd/priv/bin/captcha.sh"
captcha_host: "example.org:5280"
## captcha_host: "https://example.org:443"
## captcha_host: "http://example.com"
listen:
  ...
  -
    port: 5280
    module: ejabberd_http
    captcha: true
  ...
```

### STUN 和 TURN

`ejabberd`可以当做一个独立的STUN/TURN服务器([RFC 5389][RFC 5389]/[RFC 5766][RFC 5766]).

  [XEP-0158]: http://xmpp.org/extensions/xep-0158.html
  [RFC 5389]: http://tools.ietf.org/html/rfc5389
  [RFC 5766]: http://tools.ietf.org/html/rfc5766
  [XEP-0138]: http://xmpp.org/extensions/xep-0138.html
  [Ejabberd Guide]: http://www.process-one.net/docs/ejabberd/guide_en.html
  [安装和操作指南]: http://wiki.jabbercn.org/Ejabberd2:安装和操作指南
