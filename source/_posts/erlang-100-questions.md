title: Erlang 100问
categories:
  - Erlang
toc: false
date: 2014-12-13 14:11:19
---

## Supervisor 的 simple_one_for_one 有什么关键性特点?

1. Supervisor本身启动时不会启动子进程, 子进程的启动必须调用`supervisor:start_child(Sup, Args)`来启动.
2. 只能有一种子进程定义(规范)