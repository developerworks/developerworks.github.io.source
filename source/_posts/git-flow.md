title: 体验 git-flow
categories:
  - SCM
tags:
  - git
toc: true
date: 2014-08-17 00:42:59
---

## git-flow 是个什么?

- `git-flow` 是一个管理`git`代码仓库的生产力工具
- `git-flow` 是一种`git`代码管理的最佳实践
- `git-flow` 是一种能够让你傻笑一整天的玩具

<!-- more -->

![git-flow][7]

## 安装

下面介绍三个主要平台的安装过程

### 在Mac OS X上安装

本人的MBP最开始安装的`macports`, 虽说`macports`和`brew`可以共存, 但是还是不想给自己挖坑, 还是用单一的包管理工具比较好, 慢是慢了一点(我晓得,如果你的网络有啥子问题,这个不是一般的慢),但又不是天天都在装软件, 所以还是坚持使用`macports`好了.

如果说你喜欢命令行高效率的方式, 长期使用命令行管理代码, git-flow 对于你来说是再适合不过的进一步提供效率的工具了, 当你上手之后, `你是不是要偷笑了, 在我的想象中已经看到你Happy的不得了的样子了`

```
/test $ sudo port selfupdate
/test $ sudo port install git-flow
```

两行命令, `macports`轻松帮你搞定安装问题, 下面我会说一下怎么入门. 顺带一句, 你要先安装 `Xcode`, 否则`macports`是跑步起来的, 别怪我没有提醒你.

### Windows 安装

下载Cygwin, https://cygwin.com/install.html, 又32位和64位两个版本, 更具系统版本选择合适的安装程序, 下载完成,启动安装程序,一路选择默认即可, 要注意的是,要能够成功安装`git-flow`, 需要安装如下几个依赖包:

- `必须` Text/gettext
- `必须` Devel/git
- `可选` Devel/git-completion (建议安装此包,为git增加`命令自动补全`功能)
- `必须` Base/util-linux
- `必须` Web/wget

打开`Cygwin`终端, 运行:

```
wget -q -O - --no-check-certificate https://github.com/nvie/gitflow/raw/develop/contrib/gitflow-installer.sh | bash
```

### Linux 安装

开源软件的天然的平台就是Linux, 自然集成度是非常高的, 不需要特别的配置,只需要安装就可以了.

各种Linux分发版本的安装可参考如下链接:
https://github.com/nvie/gitflow/wiki/Linux

## 入门

安装完成后,从下面开始, 我会给大家演示`git-flow`的基本操作, 它为什么让我们可以如此`惬意`的管理我们的代码,`git-flow` 也是一个命令行, 下面我们来看一下 `git-flow`的命令行帮助, 大概的看一看它提供了那些功能,能为我们做哪些事情:


我们从初始化一个全新的仓库开始, 创建一个目录, 然后在其中执行`git flow init`初始化当前目录, 当然你也可以在现有的`git`项目下执行`git flow init`把`git`项目转换为`git-flow`项目:

```
/test $ mkdir git-flow-get-started && cd git-flow-get-started
/test $ git flow init
Initialized empty Git repository in /test/.git/
No branches exist yet. Base branches must be created now.
Branch name for production releases: [master]
Branch name for "next release" development: [develop]

How to name your supporting branch prefixes?
Feature branches? [feature/]
Release branches? [release/]
Hotfix branches? [hotfix/]
Support branches? [support/]
Version tag prefix? [] test
```

问你一堆问题, 使用默认的就好, 没必要折腾. 现在一个空的`git`仓库算是创建好了, 既然`git-flow`的主要用途是做分支开发, 那么我们使用`git branch`来看看当前的分支是什么样子的.

```
/test $ git branch
* develop
  master
```

怎么默认分支在 `*develop` 上面, 这没有什么奇怪的, 因为 `git-flow` 的想法就是让你在 `develop`分支上持续开发. 然后在往 `master` 分支上合并. 咦, 肿么没有刚才回答问题看到的 `feature`, `release`, `hotfix`, `support` 分支呢? 这是因为, 你的仓库现在还是空的, 就是这样.

现在我来详细解释一下这些分支, 是神马意思

> - `master`

这句话是神马意思呢? 产品发布(production releases) 顾名思义是用作产品发布的(或称为主干,类似SVN的`trunk`)分支, 和线上生产环境的代码是一样的.
当你开发的代码经过测试,测试,再测试之后木有任何问题了, 你就可以把开发分支`develop`的经过测试的代码合并到`master`分支,并准备发布到生产环境了.


## 新功能开发

> - `feature`

当我们需要为我们正在开发的App增加一个全新的功能时, 这个时候我们就需要使用到 `feature` 分支了, 增加新功能会有极大的可能会破坏之前已经比较稳定的代码, 为了隔离这种影响我们需要从`develop`切换到`feature`分支上开发. 当`feature`上的新功能开发完成, 再合并到`develop`分支上进行集成测试.

测试完成稳定后,最终要合并到 `master` 分支作为 `release`的基础.


## 创建一个新功能分支

开始一个`feature`分支, 名称为 `acl`, 这会在 `feature`分支下创建一个 `acl`目录, 表示我们需要为我们的应用程序增加访问控制功能:

```shell
/test $ git flow feature start acl
Switched to a new branch 'feature/acl'

Summary of actions:
- A new branch 'feature/acl' was created, based on 'develop'
- You are now on branch 'feature/acl'

Now, start committing on your feature. When done, use:

     git flow feature finish acl
```

现在我们来看看, 当前我们的分支是什么样子:

```
/test $ git branch
  develop
* feature/acl
  master
```

当前分支以及切换到`feature/acl`了, 我们在当前目录下(`/test`)创建一个`readme.md`文件, 然后用`git status`看一下:

```
/test $ git status
On branch feature/acl
Untracked files:
  (use "git add <file>..." to include in what will be committed)

	readme.md

nothing added to commit but untracked files present (use "git add" to track)
```

在第一行,我们同样能够看到我们当前所在的分支`feature/acl`, 现在我们要做的就和常规的`git`操作一样了.

```
/test $ git add .
/test $ git commit -m 'add file of readme.md'
[feature/acl 6ad1731] add file of readme.md
 1 file changed, 1 insertion(+)
 create mode 100644 readme.md
```

当我们的`acl`功能开发完成后,就可以通过下面的命令结束`acl` 分支的生命周期了(`feature`分支的生命周期是临时的, 它的目的就是为你提供, 开发新功能的场所).

```
/test $ git flow feature finish acl
Switched to branch 'develop'
Updating aec3c0e..6ad1731
Fast-forward
 readme.md | 1 +
 1 file changed, 1 insertion(+)
 create mode 100644 readme.md
Deleted branch feature/acl (was 6ad1731).

Summary of actions:
- The feature branch 'feature/acl' was merged into 'develop'
- Feature branch 'feature/acl' has been removed
- You are now on branch 'develop'
```

同样上面又丰富的提示信息,告诉你发生了什么事情. 运行`git branch`看看一下现在的分支状况

```
/test $ git branch
* develop
  master
```

`feature/acl`分支删除了, 当前分支又回到了`develop`, So Easy, 对吧!

## 共享你的新功能给其他人

> TODO::

```
/test $ git flow feature publish acl
/test $ git flow feature pull acl
```

## 版本发布

我们来描述一下这个开发场景, 团队决定开发一个Web端的实时聊天工具, 以增强与客户的互动性, 为客户提供更好的服务, 更好的响应速度.这个时候我们使用`feature`分支来创建一个新功能分支,这个在前面说过了.

现在的分支状态就只包括`master`, `develop`, 其中`develop`包含我们已经开发完成的实时聊天模块.

```
/test $ git flow release start 1.0.1
Switched to a new branch 'release/1.0.1'
Summary of actions:
- A new branch 'release/1.0.1' was created, based on 'develop'
- You are now on branch 'release/1.0.1'
Follow-up actions:
- Bump the version number now!
- Start committing last-minute fixes in preparing your release
- When done, run:
     git flow release finish '1.0.1'
```

上面这个命令标识,创建一个`release`分支, 版本号为`1.0.1`, 下面我们执行`git branch`来看一下分支状态:

```
/test $ git branch
  develop
  master
* release/1.0.1
```

你看到, 增加了一个`release/1.0.1`的分支



## 修订BUG

首先来看看版本情况:

```
/test $ git tag
test1.0.1
test1.0.2
test1.0.3
```

我们的软件最新版本为`test1.0.3`, 目前和`master`分支的内容是一模一样的, 同时也是生产环境中正在运行着的版本. 某天当你半夜被你的BOSS叫起来的时候, 他给你说我们的生产环境发现了一个严重的BUG, 要马上修复. 这时候就是`hoxfix`的时候了.


```
/test $ git flow hotfix start 1.0.4
```

- 把当前`master`分支的内容完整拷贝到`hotfix/1.0.4`,
- 切换分支到 `hotfix/1.0.4`
- 在`hotfix/1.0.4` 修改BUG
- `git add . && git commit -m 'bug fixed'`

```
/test $ git flow hotfix finish 1.0.4
```

- 结束`hoxfix/1.0.4`分支
- 合并到`master`
- 合并到`develop`
- 删除`hotfix/1.0.4`分支
- 切换回`develop`分支


## 参考

关于`git-flow`你还可以参考[`git-flow-cheatsheet`][1],[这里][2]是中文版. 它是一个分支管理流程图标,如果你完全理解了`git-flow`的工作方式,即使在大规模项目中,你同样能够很好的管理你的代码.

> 提示: 请仔细看`git-flow-cheatsheet`中的 `红`,`青`,`橙`,`蓝`,`绿`的五线谱, 看看`git-flow`是怎么在这个五线谱弹奏优雅的旋律的.

[一个成功的分支模型][3]
[一个成功的分支模型-英文原文][5]
[基于git的源代码管理模型——git flow][4]

  [1]: http://danielkummer.github.io/git-flow-cheatsheet/
  [2]: http://danielkummer.github.io/git-flow-cheatsheet/index.zh_CN.html
  [3]: http://www.oschina.net/translate/a-successful-git-branching-model?from=20130303
  [4]: http://www.ituring.com.cn/article/56870
  [5]: http://nvie.com/posts/a-successful-git-branching-model/
  [6]: http://www.jeffkit.info/2010/12/860/
  [7]: /assets/images/60819835-3DF4-4249-B467-DDCBBB37216C.png


