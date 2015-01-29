title: 自定义行为
categories:
  - Erlang
tags:
  - Behaviour
toc: false
date: 2014-11-14 18:15:11
---


```erlang
-module(some_behaviour).
-callback init(Args :: list(term())) -> 'ok'|tuple('error', Reason :: string()).
-callback handle(Event :: atom()) -> NextEvent :: atom().
-callback sync(Node :: node(), Timeout :: non_neg_integer()) -> 'ok'|tuple('error', Reason :: string()).
```

该行为要求在回调模块中定义 `init/1`, `handle/1` 和 `sync/2`三个函数, 如下:

```
-module(usage).
-behaviour(some_behaviour).
-export([init/1, handle/1, sync/2, foo/0]).
init(Config) ->
    Config.
sync(_Entry, Config) ->
    Config.
handle(Message) ->
    Message.
foo() ->
    foo_atom_returned.
```