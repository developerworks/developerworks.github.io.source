title: 函数委派
categories:
  - Elixir
tags:
  - Elixir
toc: false
date: 2014-10-30 14:45:30
---

## 函数委派

可以通过在当前模块中通过`defdelegate`定义一个函数,并把对该函数的调用委派给`目标函数`, 一个有用的场景是:

- 把特定应用需要调用的底层模块的函数,封装到一个单独的应用模块中.

下图是`defdelegate`在Elixir内核中的[定义][defdelegate]

![`defdelegate`在Elixir内核中的定义][elixir-defdelegate.png]


 [elixir-defdelegate.png]: /assets/images/elixir-defdelegate.png
 [defdelegate]: http://elixir-lang.org/docs/stable/elixir/Kernel.html#defdelegate/2