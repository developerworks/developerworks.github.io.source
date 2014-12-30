title: Ejabberd | 使用Elixir Mix编译Ejabberd模块
categories:
  - Communication
  - Ejabberd
tags:
  - Elixir
toc: false
date: 2014-12-29 20:15:28
---

本文是基于Github上的一个项目的实践, 该项目包含了Ejabberd 14.07的头文件, 直接把开发好的模块放在`src`目录下,并执行:

克隆项目:

```
# git clone https://github.com/scrogson/ejabberd_dev.git
Cloning into 'ejabberd_dev'...
remote: Counting objects: 39, done.
remote: Total 39 (delta 0), reused 0 (delta 0)
Unpacking objects: 100% (39/39), done.
Checking connectivity... done.
```

进入项目目录

```
cd ejabberd_dev
```

编译:

```
mix do deps.get, compile
```

即可编译模块, 编译的Beam文件生成到`_build`目录中.

## TODO:

- 开发一个Elixir Mix任务,把编译好的Beam文件复制到Ejabberd的部署目录
- 开发一个Elixir Mix任务, 用于更新Ejabberd新版本的头文件到`ejabberd_dev`项目目录

