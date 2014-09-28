title: Ejabberd模块快速入门
categories:
  - Erlang
tags:
  - ejabberd
toc: true
date: 2014-09-26 09:15:57
---

Ejabberd是一个以Elang编程语言开发的开源XMPP服务器.过去很长时间,XMPP以构建即时通信应用程序闻名, 很多人用它来构建实时应用程序. 依赖预Erlang平台的并发特性,在选择XMPP服务器的时候,Ejabberd天生的就适合与处理大规模并发连接的应用环境.

<!--more-->

## 安装

事先需要安装好Erlang运行环境. 这里不再赘述.

```
git clone https://github.com/processone/ejabberd.git
cd ejabberd
chmod +x autogen.sh
chmod +x rebar
./autogen.sh
./configuire
make
sudo make install
```

编译过程相关问题可参考: [编译Ejabberd遇到的问题][1]


## 相关文件

```
/etc/ejabberd/ejabberd.cfg          %% Ejabberd主配置文件
/etc/ejabberd/ejabberdctl.cfg       %% ejabberdctl命令配置文件
/sbin/ejabberdctl                   %% ejabberdctl命令
/lib/ejabberd                       %% ejabberd运行库
/var/log/ejabberd/ejabberd.log      %% 日志
```


## 编写一个内部模块

所有内部模块以`mod_`作为模块名前缀, 并实现[gen_mod][2]行为,该行为包含两个必须实现的方法`start/2`和`stop/1`:

```
start(Host,Opts) -> ok
stop(Host) -> ok
```

`Host`为运行该模块的虚拟主机名称. `Opts`为一组设置选项. 现在我们开发一个基本的模块,它在启动时打印一些信息:

```
cd $EJABBERD/src
vi mod_hello.erl
```

**mod_hello.erl**

```
-module(mod_hello).
-behavior(gen_mod).
-include("logger.hrl").
-export([
    start/2,
    stop/1
]).
start(_Host, _Opt) ->
    ?INFO_MSG("Loading module 'mod_hello' ", []).
stop(_Host) ->
    ok.
```

### 编译/安装模块

```
make
make install
```

安装后,模块位于如下位置

```
root@7db26691874d:/root/ejabberd# ll /lib/ejabberd/ebin/mod_hello.beam
-rw-r--r-- 1 root root 2004 Sep 25 20:28 /lib/ejabberd/ebin/mod_hello.beam
```

### 配置模块

```
vi /etc/ejabberd/ejabberd.cfg
```

添加模块配置

```
{modules,[
    .....
    {mod_hello, []},
    .....
    ]
}
```

### 重启/查看日志

```
ejabberdctl restart
```

```
root@7db26691874d:/# tail -f /var/log/ejabberd/ejabberd.log  |grep mod_hello
2014-09-25 20:34:27.700 [info] <0.3305.0>@mod_hello:start:9 Loading module 'mod_hello'
2014-09-25 20:34:27.734 [info] <0.3305.0>@mod_hello:start:9 Loading module 'mod_hello'
2014-09-25 20:34:27.747 [info] <0.3305.0>@mod_hello:start:9 Loading module 'mod_hello'
```


## 编写一个HTTP模块

类似预构建内部模块,区别是HTTP模块可以处理URL请求.

**mod_available_user.erl**

```
-module(mod_available_user).
-behavior(gen_mod).
-export([
    start/2,
    stop/1,
    process/2
]).
-include("ejabberd.hrl").
-include("jlib.hrl").
-include("ejabberd_http.hrl").
start(_Host, _Opts) ->
    ok.
stop(_Host) ->
    ok.
process(Path, _Request) ->
    {xmlelement, "html", [{"xmlns", "http://www.w3.org/1999/xhtml"}],
        [{xmlelement, "head", [],
            [{xmlelement, "title", [], []}]},
            {xmlelement, "body", [],
                [{xmlelement, "p", [], [{xmlcdata, is_user_exists(Path)}]}]}]}.
is_user_exists(User) ->
    Result = ejabberd_auth:is_user_exists(User, "localhost"),
    case Result of
        true -> "The username " ++ User ++ " is already taken.";
        false -> "The username " ++ User ++ " is available."
    end.
```

> 有BUG,未解决.

### 配置

配置路径/模块映射

```
  -
    port: 5280
    module: ejabberd_http
    request_handlers:
      "/pub/archive": mod_http_fileserver
      "/http-bind/": mod_http_bind
      "admin": ejabberd_web_admin
      %% 把/users路径设置为由模块mod_available_user处理
      "users": mod_available_user
    web_admin: true
    http_poll: true
    http_bind: true
    ## register: true
    captcha: true
```

设置管理员账户

```
acl:
  ##
  ## The 'admin' ACL grants administrative privileges to XMPP accounts.
  ## You can put here as many accounts as you want.
  ##
  admin:
     user:
       - "root": "xmpp.hezhiqiang.info"
       - "root": "localhost"
```

打开浏览器`http://localhost:5280/admin/server/localhost/users`增加用户, Web管理控制台需要管理权限, 如果你没有管理权限的用户可以创建一个.

```
ejabberdctl register root localhost root
            ======== ==== ========= ====
             |         |     |       |
            注册    用户名  虚拟主机  密码
```

然后用账户`root@localhost`和密码`root`登陆.

## 参考资料

1. Writing Ejabberd Modules
http://sacharya.com/writing-ejabberd-modules/
2. Getting to know ejabberd and writing modules
http://www.metabrew.com/article/getting-to-know-ejabberd-and-writing-modules

  [1]: /2014/09/25/ejabberd-compile-issues
  [2]: http://www.process-one.net/en/wiki/gen_mod

