title: 开发Ejabberd模块
categories:
  - XMPP
tags:
  - ejabberd
toc: false
date: 2014-09-18 17:43:15
---

使用 [ejabberd][1] 最大的好处是易于扩展其服务器的功能.

每个ejabberd模块需呀实现`gen_mod`行为. erlang行为是一个函数集合, 实现了一个行为的模块必须支持行为定义的函数. `gen_mod`行为要求两个函数: `start/2`和`stop/`.当ejabberd服务器启动和停止模块的时候会调用对应的函数.

一个ejabberd模块的骨架如下:

```
-module(mod_sunshine).
-behavior(gen_mod).
-export([start/2, stop/1]).
start(_Host, _Opts) ->
    ok.
stop(_Host) ->
    ok.
```


  [1]: http://www.ejabberd.im/
