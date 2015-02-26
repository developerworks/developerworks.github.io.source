title: 使用RefactorErl重构你的Erlang模块和应用程序
categories:
  - Erlang
tags:
  - RefactorErl
toc: true
date: 2015-02-12 11:29:17
---

> ## 序

本文是一篇介绍如何使用工具重构Erlang程序的文章. 本文以分析,阅读和重构Ejabberd 14.12作为示例, 演示如何重构一个现有的Erlang模块, 和应用程序.

本文的演示系统是基于Ubuntu 14.04, 并且带图形界面安装, 后续需要用到的重构工具都需要图形界面, 实际操作前, 请准备好相关的系统环境.

不建议使用非Linux进行本文的实验, 因为非Linux或多或少有一些软件兼容性问题, 会给实验过程带来不必要的麻烦, 如果你的操作系统是非Linux, 可安装VMWare, VirtualBox等虚拟机搭建本文的重构操作需要的实验环境.

本文是一个入门的介绍性文章, 要把RefactorErl用到你的工作中, 还需要阅读大量的相关资料,理解很多概念,请参考文章末尾的参考资料.

> ## 简介

RefactorErl 是一个源代码分析和转换,重构工具, 是欧洲几个大学的联合研究项目, 写作本文的时候, `RefactorErl`的版本为`0.9.14.09`

> ## 编译

系统环境 Ubuntu 14.04

### 安装Yaws Web服务器

Yaws Web服务器是构建基于浏览器的重构工具所需要的, 如果需要在浏览器上体验RefactorErl的功能, 需要安装Yaws Web服务器. 官方推荐使用`1.95`版本的Yaws.

```
wget http://yaws.hyber.org/download/yaws-1.95.tar.gz
tar zxf yaws-1.95.tar.gz
cd yaws-1.95
# 编辑如下文件,把1250行的 `HashBin = crypto:sha(Salted),`
# 改为 `HashBin = crypto:hash(sha, Salted),` 否则会导致编译出错.
vi src/yaws_websocket.erl
./configure
make
make install
```

### 安装graphviz

生成模块依赖图需要使用到`graphviz`

```
apt-get install -y graphviz
```

> ## 编译RefactorErl

```
wget http://plc.inf.elte.hu/erlang/dl/refactorerl-0.9.14.09.tar.gz
tar zxf refactorerl-0.9.14.09.tar.gz
cd refactorerl-0.9.14.09
make
```

> ## 运行

进入RefactorErl Shell执行各种分析操作.

```
# -db 参数标识使用的数据库引擎
# 目前支持 Mnesia, C++ graph based 和 KyotoCabinet based graph 三种存储后端
bin/referl -db kcmini

```

在RefactorErl Shell中设置用于浏览器分析界面的登陆密码

    referl_ui_web2:set_web2_pass("admin", "admin").

命令行方式直接启动Web UI

    bin/referl -web2 -yaws_path /usr/local/lib/yaws/ebin -yaws_listen 192.168.8.132 -yaws_port 8001

SHELL方式启动

```
ri:start_web2([
    {yaws_path,"/usr/local/lib/yaws/ebin"},
    {yaws_listen,"192.168.8.132"},
    {yaws_name , "192.168.8.132"},
    {yaws_port , "8001"},
    {browser_root , "/tmp/erlang"},
    {images_dir , "/tmp/graph_images"},
    {restricted_mode , true }
]).
```

在浏览器中打开: `http://192.168.8.132:8001/`

停止

    ri:stop_web2().


- 启动RefactorErl

```
bin/referl -db nif
```

- 设置Web2密码

```
referl_ui_web2:set_web2_pass("admin", "admin").
```

- 添加文件到数据库引擎


```
ri:add("/root/sources/ejabberd/deps/p1_xml/include/xml.hrl").
ri:add("/root/sources/ejabberd/deps/esip/include/esip.hrl").
ri:add("/root/sources/ejabberd/include/jlib.hrl").
ri:add("/root/sources/ejabberd/include/adhoc.hrl").
ri:add("/root/sources/ejabberd/include/ejabberd_config.hrl").
ri:add("/root/sources/ejabberd/include/ejabberd.hrl").
ri:add("/root/sources/ejabberd/include/ejabberd_web_admin.hrl").
ri:add("/root/sources/ejabberd/include/ELDAPv3.hrl").
ri:add("/root/sources/ejabberd/include/jlib.hrl").
ri:add("/root/sources/ejabberd/include/mod_muc_room.hrl").
ri:add("/root/sources/ejabberd/include/mod_proxy65.hrl").
ri:add("/root/sources/ejabberd/include/ns.hrl").
ri:add("/root/sources/ejabberd/include/XmppAddr.hrl").
ri:add("/root/sources/ejabberd/include/ejabberd_commands.hrl").
ri:add("/root/sources/ejabberd/include/ejabberd_ctl.hrl").
ri:add("/root/sources/ejabberd/include/ejabberd_http.hrl").
ri:add("/root/sources/ejabberd/include/eldap.hrl").
ri:add("/root/sources/ejabberd/include/http_bind.hrl").
ri:add("/root/sources/ejabberd/include/logger.hrl").
ri:add("/root/sources/ejabberd/include/mod_privacy.hrl").
ri:add("/root/sources/ejabberd/include/mod_roster.hrl").
ri:add("/root/sources/ejabberd/include/pubsub.hrl").
ri:add("/root/sources/ejabberd/src").
```

- 启动Web2

```
ri:start_web2([
    {yaws_path,"/usr/local/lib/yaws/ebin"},
    {yaws_listen,"192.168.8.102"},
    {yaws_name , "192.168.8.102"},
    {yaws_port , "8001"},
    {browser_root , "/tmp/erlang"},
    {images_dir , "/tmp/graph_images"},
    {restricted_mode , true }
]).
```

- 打开连接

http://192.168.8.132:8001

- 登陆Web2分析界面

用户名: admin
密码: admin


> ## 参考资料

1. 视频介绍
https://erlangcentral.org/erlang-factory-2014-refactorerl-supports-your-daily-work
2. 项目首页
http://plc.inf.elte.hu/erlang/index.html
3. 用户手册(PDF)
http://plc.inf.elte.hu/erlang/dl/manual_12_01.pdf
