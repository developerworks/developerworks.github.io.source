title: 开发一个Ejabberd HTTP模块
categories:
  - XMPP
tags:
  - ejabberd
toc: false
date: 2014-09-18 21:17:42
---

## 基本模块结构

<!-- more -->

```
%% Module name (has to match with the filename)
-module(mod_custom).
%% Module author
-author('Gregor Uhlenheuer').
%% Module version
-vsn('1.0').
%% Debug flag
-define(EJABBERD_DEBUG, true).
%% Implement the OTP gen_mod behavior
-behavior(gen_mod).
%% Module exports
-export([start/2, stop/1, process/2]).
%%
%% INCLUDES
%%
%% base ejabberd headers
-include("ejabberd.hrl").
%% ejabberd compatibility functions
-include("jlib.hrl").
%% ejabberd HTTP headers
-include("web/ejabberd_http.hrl").
%% initialization function
start(_Host, _Opts) ->
    ok.
%% function on module unload
stop(_Host) ->
    ok.
%% process any request to "/sockets"
process(["sockets"], _Request) ->
    % FIXME: implementation goes here
    "Not implemented yet";
%% process all remaining requests
process(_Page, _Request) ->
    % FIXME: implementation goes here
    "Fallback result".
```


## 编译模块

```
erlc -I ../ejabberd/src \
     -I /lib64/ejabberd/include \
     -pa ../ejabberd/src \
     mod_custom.erl
```

模块的`.beam`文件需要放在ejabberd的`ebin`目录下

## 配置模块

- 在主配置文件中添加模块配置

原先的主配置文件中配置为
```
{5280, ejabberd_http, [http_poll, web_admin]}
```
修改后的为
```
% this will probably look like this
{5280, ejabberd_http, [http_poll, web_admin,
        {request_handlers, [
            % your request handler will respond to anything like:
            % http://example.com:5280/custom/
            {["custom"], mod_custom}
        ]}
    ]}
```

## 参考资料

1. http://uhlenheuer.net/posts/2013-01-16-writing_an_ejabberd_module.html