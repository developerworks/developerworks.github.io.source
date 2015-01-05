title: Erlang 用erlang.mk和relx发布Erlang应用程序
categories:
  - Erlang
tags:
  - erlang.mk
  - relx
toc: false
date: 2015-01-05 01:23:31
---

发布OTP应用程序一直是个困难的任务. 像`reltool`,`rebar`这类工具虽然简化了这类问题,但并没有解决所有问题.本文展示了一个候选方案,希望是一个更简化的方案.

发布OTP应用程序需要两个步骤.

- 首先需要构建各种需要包含在发布包中的各种OTP应用程序.
- 构建完成后, 把`Erlang运行时系统`,`节点启动脚本`,`应用程序`和`配置文件`打包为一个可发布的软件包.

[erlang.mk](https://github.com/extend/erlang.mk)解决了第一个步骤的问题.它是一个`GNU Make`包含文件. 只需要把它包含在Makefile文件中即可构建项目,获取和构建依赖,构建文档,执行静态分析等任务.

[relx](https://github.com/erlware/relx) 解决了第二个步骤的问题.它是一个发布创建工具, 用于把应用程序打包为单个可执行程序. 它并不要求配置文件, 如果你需要,那也是很小的一个配置文件.

来看一个最简单的使用erlang.mk的Makefile文件. 其只需要做一件事: 定义项目名称.

    PROJECT = my_peojct

    include erlang.mk

只需要这么定义就可以让你使用`make`命令来构建项目, 以及使用`make tests`运行测试. 如果使用`ErlyDTL`,它还可以构建`templates`目录中的`*.dtl`模板.

    PROJECT = ninenines

    DEPS = cowboy erlydtl
    dep_cowboy = https://github.com/extend/cowboy.git 0.8.5
    dep_erlydtl = https://github.com/evanmiller/erlydtl.git 4d0dc8fb

    .PHONY: release clean-release

    release: clean-release all projects
        relx -o rel/$(PROJECT)

    clean-release: clean-projects
        rm -rf rel/$(PROJECT)

    include erlang.mk

其中我们看到了如何定义依赖`DEPS = cowboy erlydtl`:

- 首先,列举所有的依赖名称
- 其次,单独定义每一个依赖的URL,提交号,标记或分支
- 随后,定义了两个目标, `release`为默认目标, 因为它是最先定义的. 你可以覆盖默认目标`all`,其作用是构建应用程序与其依赖.
- `release`目标使用`relx`在`rel/ninenines/`目录构建发布.

下面来看一下构建此发布的配置文件.

    {release, {ninenines, "1"}, [ninenines]}.

    {extended_start_script, true}.
    {sys_config, "rel/sys.config"}.

    {overlay, [
        {mkdir, "log"},
        {copy, "rel/vm.args",
            "releases/\{\{release_name\}\}-\{\{release_version\}\}/vm.args"}
    ]}.

第一行定义了一个名为`ninenines`的发布,其又一个版本号`1`, 包含一个应用程序,名称也为`ninenines`.

We then use the extended_start_script option to tell relx that we would like to have a start script that allows us to
not only start the release, but do so with the node in the background, or also to allow us to connect to a running node, and so on.
This start script has the same features as the one tools like rebar generates.

然后使用`extended_start_script`告知`relx`我们想要有一个启动脚本用于启动应用程序, .....
该启动脚本和`rebar`工具生成的作用是一样的.

## 参考资料

1. Build Erlang releases with erlang.mk and relx
http://ninenines.eu/articles/erlang.mk-and-relx/
2. erlang.mk Github项目
https://github.com/ninenines/erlang.mk