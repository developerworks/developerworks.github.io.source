title: Git init 命令参数
categories:
  - git
tags:
  - git-init
date: 2014-08-21 23:21:52
---

初始化一个`git`仓库使用`git init`命令, 其中有几个参数值得注意

- `--shared[=(false|true|umask|group|all|world|everybody|0xxx)]`
设置仓库可被什么范围的用户访问

- `--template=<template dir>`
仓库模板目录, 此目录下的文件在初始化一个新`git`仓库是会全部复制到新创建的仓库下

- `--separate-git-dir=<git dir>`
把`.git`元数据目录分离出去

