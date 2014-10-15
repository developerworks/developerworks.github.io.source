title: Elixir-0003-Ecto
categories:
  - Elixir
tags:
  - Ecto
toc: false
date: 2014-10-15 01:46:41
---


## Ecto简介

Ecto是一个领域语言,用于在Elixir编程语言中编写查询,以及和数据库交互.Ecto是用Elixir开发的关系型数据库工具.

本文会介绍如何在Elixir项目中使用Ecto.

## Ecto主要组件

Ecto有三个主要的组件: 仓库, 模型, 以及查询.

### Ecto仓库

仓库是一个对对数据库的封装程序. 我们可以向下面一样定义一个仓库:

```elixir
defmodule Repo do
    use Ecto.Repo, adapter: Ecto.Adapters.Postgres
    def conf do
        parse_url "ecto://username:password@localhost/ecto_simple"
    end
end
```

Ecto当前仅支持PostgreSQL数据库,未来会支持其他数据库.

Each repository in Ecto defines a start_link/0 function that needs to be invoked before
using it in a repository. This function is not called directly,
but via a supervisor chain. Just find supervisor.ex and start a
supervisor using the init/1 function below:

在Ecto中每一个仓库定义一个`start_link/0`函数, 在仓库中使用它之前需要调用,该函数不是直接被调用的, 而是通过一个监控者链.
`supervisor.ex` and start a supervisor using the `init/1` function below:

### Ecto模型
### 查询
### 移植
## 创建Elixir项目
## 安装Ecto
## 添加一个仓库
## 添加一个模型
## 生成移植脚本
## 移植
## Operating an Ecto
## Conclusion

## 参考资料

1. https://github.com/elixir-lang/ecto
2. http://www.youtube.com/watch?v=SJRfujy9vLA
3. http://elixirsips.com/episodes/024_ecto_part_1.html