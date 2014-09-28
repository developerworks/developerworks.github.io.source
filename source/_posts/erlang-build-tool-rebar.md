title: Erlang 构建工具 rebar
categories:
  - Erlang
tags:
  - rebar
toc: false
date: 2011-05-17 11:20:25
---


更新历史:

1. 2014-09-25 22:40:22


![下载Rebar源码并编译][1]
![rebar命令][2]
![选项][3]


[https://bitbucket.org/basho/rebar/wiki/GettingStarted][4]这篇文章是直接下载二进制的rabar,建议从源代码进行编译,可以了解rebar的更多信息.还有现成的rebar.config配置文件,下载编译过程很简单,请看图一

```
debian:~/erlang# hg clone https://bitbucket.org/basho/rebar
debian:~/erlang# cd rebar
debian:~/erlang# ./bootstrap
```

在src/myapp_app.erl文件的-export()指令后面添加如下行:

```
-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.
```

在文件的末尾添加如下行:

```
-ifdef(TEST).
simple_test() ->
    ok = application:start(myapp),
    ?assertNot(undefined == whereis(myapp_sup)).
-endif.
```

```
debian:~/erlang/rebar/myapp# ./rebat compile eunit
```

输出

```
debian:~/erlang/rebar/myapp# ./rebar compile eunit
==> myapp (compile)
==> myapp (eunit)
  Test passed.
Cover analysis: /var/root/erlang/rebar/myapp/.eunit/index.html
```

`.eunit`目录中包含了一些调试信息.

在rebar.config中添加如下行,让rebar为我们生成代码覆盖率信息.

```
{cover_enabled, true}.
```

执行:

```
debian:~/erlang/rebar/myapp# ./rebar clean
debian:~/erlang/rebar/myapp# ./rebar compile eunit
==> myapp (compile)
==> myapp (eunit)
  Test passed.
Cover analysis: /var/root/erlang/rebar/myapp/.eunit/index.html
```

覆盖率报告html文件生成了.


### 命令

Rebar提供了开发过程中大多数功能, 包括:

* 编译
* 单元测试和覆盖率分析
* 静态分析 ([Dialyzer][5] and [Xref][6]).
* 文档生成
* 依赖管理

In addition, it allows for OTP embedded system generation, taking advantage of the template processing afforded by rebar and the [reltool][7] application.

The most common commands are:

| COMMAND | DESCRIPTION |
| ------- | ----------- |
|compile|Compile all the available source in the project.|
|eunit|Perform unit testing using the Eunit application|
|doc|Generate documention using the Edoc application|
|clean|Remove any generated artifacts from compilation, unit testing, etc.|

参考:
1. http://www.cnblogs.com/musketeer/archive/2011/03/26/1996197.html
2. http://www.linezing.com/blog/?p=347
3. http://www.flatws.cn/article/program/embed/2011-05-05/24309.html

  [1]: /assets/images/git-clone-and-compile-rebar.png
  [2]: /assets/images/rebar-commands.png
  [3]: /assets/images/rebar-help.png
  [4]: https://github.com/rebar/rebar/wiki/Getting-started
  [5]: http://www.erlang.org/doc/man/dialyzer.html
  [6]: http://www.erlang.org/doc/man/xref.html
  [7]: http://www.erlang.org/doc/man/reltool.html