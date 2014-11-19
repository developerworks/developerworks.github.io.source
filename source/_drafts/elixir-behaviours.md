title: Elixir 行为
categories:
  - Elixir
tags:
  - Behaviour
toc: false
date: 2014-11-12 15:03:26
---

行为的本质就是一个函数列表, 当某个模块声称实现了某个行为的时候, 该模块必须实现行为所有相关的函数. 否则会产生一个编译警告.

## 定义行为

- 使用Elixir`Behaviour`模块和`defcallback`定义行为