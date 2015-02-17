title: 用RefactorErl分析Ejabberd源码
categories:
  - Erlang
tags:
  - RefactorErl
  - Ejabberd
toc: false
date: 2015-02-13 13:20:40
---

本文以[前面一篇文章为基础](/2015/02/12/erlang-refactoring-your-erlang-module-and-application-with-refactorerl),用RefactorErl分析Ejabberd源码的过程.

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
    {yaws_listen,"192.168.8.132"},
    {yaws_name , "192.168.8.132"},
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