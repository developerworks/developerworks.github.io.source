title: Elixir | 第一印象
categories:
  - Elixir
tags:
  - shell
toc: false
date: 2014-10-14 01:06:12
---

Elixir 依赖与Erlang/OTP 17,需要首先安装Erlang, 这里不讲解如何安装Erlang, 这里说明如何编译和使用Elixir

# 编译

```shell
# 下载
wget https://github.com/elixir-lang/elixir/archive/v1.0.1.tar.gz
# 解压
tar zxf v1.0.1.tar.gz
cd elixir-1.0.1
make
```
# 配置路径

```
export PATH=/root/elixir-1.0.1/bin:$PATH
```

# 交互式Shell

```
root@fd4cc081e295:~/elixir-1.0.1/bin# iex
Erlang/OTP 17 [erts-6.2] [source] [64-bit] [async-threads:10] [kernel-poll:false]
Interactive Elixir (1.0.1) - press Ctrl+C to exit (type h() ENTER for help)
iex(1)>
```
输入`h()`显示如下帮助

![Elixir交互式Shell][1]

# Hell World

```elixir
IO.puts "Hello World!
```

执行

```
root@fd4cc081e295:~/elixir# elixir hello.exs
Hello World!
```

 [1]: /assets/images/94E0C021-A9FE-44CD-97C5-EA564C66BC78.png
