title: 译文 | Elixir 创建自定义Mix任务
categories:
  - Elixir
tags:
  - Mix
toc: true
date: 2014-11-04 16:34:35
---

原文: http://elixir-lang.readthedocs.org/en/latest/mix/3/

在Mix中,一个任务实际上是一个具有名称空间`Mix.Tasks`并实现了`run/1`函数的模块.例如,`compile`任务是一个名称为`Mix.Tasks.Compile`的模块.

创建一个简单的任务:

```elixir
defmodule Mix.Tasks.Hello do
    use Mix.Task
    @shortdoc "这是一个短文档, 看"
    @moduledoc """
    一个测试任务
    """
    def run(_) do
        IO.puts "你好,世界!"
    end
end
```
    
保存文`hello.ex`, 并编译:

	$ elixirc hello.ex
    $ mix hello
    你好,世界
    
上述模块定义了一个名称为`hello`的任务. 函数`run/1`接受一个二进制字符串参数, 该参数是从命令行传递给此任务的.

当调用命令`mix hello`时, 该任务被执行, 并输出`你好,世界`. Mix使用其第一个参数(`hello`)查找任务模块并执行`run`函数.

为什么有一个`@moduledoc`和`@shortdoc`. 这两个文档标记是被`help`任务使用来显示任务的说明文档. `@shortdoc`用在执行`mix help`的时候显示任务的简短描述, `@moduledoc`用于执行`mix help hello`是显示`hello`任务的详细描述.

除了这两个文档标签外, 还有一个`@hidden`标签, 当其设置为`true`时,该任务不显示在`mix help`的输出中, 任何没有`@shortdoc`标签的任务也不会显示.

## 常见API

当编写任务时, 需要访问一些常见的Mix功能, 如下:

- `Mix.Project.config` 返回项目配置(`mix.exs`的`project`函数), 如果当前目录不存在`mix.exs`文件, 该函数返回一个空的配置. 
- `Mix.Project.get!` 访问当前项目的模块, 需要访问项目中的特殊函数是非常有用. 如果项目未定义,将抛出异常.
- `Mix.shell` 
- `Mix.Task.run(task,args)` 从其他Mix任务中调用另一个任务; 如果该任务已经被调用, 不做任何操作.

## Namespaced Tasks

简单的任务可用于完成复杂的事情. 任务实际上也是Elixir代码,任何Elixir可以做的事情都可以放到任务中去做. 可以像分发其他库一样分发任务, 让任务可以在其他项目中重用.

要在多个项目中重用任务, 为了避免名称冲突, Mix任务支持名称空间.

示例:


```elixir
defmodule Mix.Tasks.Mytasks do
    @shortdoc "任务集合模块"
    @moduledoc """
    用于构建和部署的任务集合
    """
    defmodule Build do
        use Mix.Task
        @shortdoc "构建任务"
        @moduledoc """
        构建一个软件组件模块
        """
        def run(_) do
            IO.puts "运行子任务Build"
        end
    end
    defmodule Deploy do
        use Mix.Task

        @shortdoc "部署一个软件组件"
        @moduledoc """
        把一个软件组件部署到服务器
        """
        def run(_) do
            IO.puts "运行子任务Deploy"
        end
    end
end
```

任务模块写好了后, 可以像这样调用任务: `mix mytasks.build`, `mix mytasks.deploy`, 这功能很酷对吧?

![任务帮助][1]

## 选项解析

    OptionParser.parse(["--debug"])
    #=> { [debug: true], [] }

    OptionParser.parse(["--source", "lib"])
    #=> { [source: "lib"], [] }

    OptionParser.parse(["--source", "lib", "test/enum_test.exs", "--verbose"])
    #=> { [source: "lib", verbose: true], ["test/enum_test.exs"] }


## 分享任务

创建了任务后,如果需要在团队内分享给其他人, 或在其他项目中重用, 这章描述了几种不同的分享法师

### 作为依赖

假设创建了一个`my_tasks`项目, 其提供了众多有用的功能, 把该项目添加为其他项目的依赖, 所有在`my_tasks`项目中的任务可以被其他引用了`my_tasks`的项目使用.

### 把任务打包

Mix允许你安装和卸载本地归档包. 要为当前项目生成一个归档包, 运行:

	root@0b85dcd174f2:~/elixir/commandlinetools# mix do archive.build
    Generated archive commandlinetools-0.0.1.ez with MIX_ENV=dev

![把任务打包][2]

打包的任务可以通过文件系统路径或URL安装:

	mix archive.install http://localhost/commandlinetools-0.0.1.ez

`mix archive`命令的的详细说明可通过 `mix help archive` 查看

### MIX_PATH

最有一个中方法是使用`MIX_PATH`. 设置了`MIX_PATH`环境变量之后, 所有在其中的任务对Mix都可用, 比如:

	$ export MIX_PATH="/elixir/ebin"
    
这种方式可以单独维护需要在多个项目中使用的任务. 


  [1]: /assets/images/BAE289A3-2D50-430A-B85A-1BC3C55896F9.png
  [2]: /assets/images/71D4F33D-A3F1-4A14-A1A1-AFAE607AFFC2.png