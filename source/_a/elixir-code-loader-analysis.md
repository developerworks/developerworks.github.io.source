title: Elixir 代码加载器源码分析
categories:
  - Elixir
tags:
  - Elixir
toc: false
date: 2015-01-05 02:24:12
---

## 加载器代码注释部分

```
# This module is responsible for loading dependencies
# of the current project. This module and its functions
# are private to Mix.
```

该模块负责加载当前项目的依赖. 该模块及其函数是Mix私有的.

## childgen 函数

```
@doc """
Gets all direct children of the current `Mix.Project`
as a `Mix.Dep` struct. Umbrella project dependencies
are included as children.
By default, it will filter all dependencies that does not match
current environment, behaviour can be overridden via options.
## Options
* `:env` - filter dependencies on given environments
"""
def children(opts) do
mix_children(opts) ++ Mix.Dep.Umbrella.unloaded
end
```


## 参考资料

1. https://github.com/elixir-lang/elixir/blob/master/lib/mix/lib/mix/dep/loader.ex