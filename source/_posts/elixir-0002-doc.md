title: Elixir-0002
categories:
  - Elixir
tags:
  - doc
toc: false
date: 2014-10-14 10:21:11
---

## 文档是一等公民

在Elixir中,文档是一等公民, Elixir对其有内置的支持, 不需要第三方文档生成工具. 只需要使用`@doc`标签即可

### 函数文档

1. 创建一个文件`hello.exs`如下:

```
# Elixir module
defmodule Test do
    @doc """
    一个Echo方法用于输出Hello World.
    """
    def echo do
        IO.puts "Hello World!"
    end
end
```

2. 进入交互式shell

```
root# iex
```

3. 编译模块

```
iex(1)> c("hello.exs")
```

模块编译会在当前目录下生成一个`Elixir.Test.beam` BEAM文件.

4. 查看文档

![在Elixir交互式Shell中查看函数文档][1]

### 模块文档

`@moduledoc`

## 函数参数和返回类型规范

函数定义的类型规范是通过`@spec`标签定义的


## 用ExDoc生成文档

- 用`mix`创建一个新项目
    ```
    root@fd4cc081e295:~/ejabberd/elixir# mix new docs
    * creating README.md
    * creating .gitignore
    * creating mix.exs
    * creating config
    * creating config/config.exs
    * creating lib
    * creating lib/docs.ex
    * creating test
    * creating test/test_helper.exs
    * creating test/docs_test.exs
    Your mix project was created successfully.
    You can use mix to compile it, test it, and more:
        cd docs
        mix test
    Run `mix help` for more commands.
    ```
- 编辑mix.exs,添加依赖模块
    ```
    defmodule Docs.Mixfile do
      use Mix.Project
      def project do
        [app: :docs,
         version: "0.0.1",
         elixir: "~> 1.0",
         deps: deps]
      end
      # Configuration for the OTP application
      #
      # Type `mix help compile.app` for more information
      def application do
        [applications: [:logger]]
      end
      # Dependencies can be Hex packages:
      #
      #   {:mydep, "~> 0.3.0"}
      #
      # Or git/path repositories:
      #
      #   {:mydep, git: "https://github.com/elixir-lang/mydep.git", tag: "0.1.0"}
      #
      # Type `mix help deps` for more examples and options
      defp deps do
        [{:ex_doc, github: "elixir-lang/ex_doc"}]
      end
    end
    ```

- 安装依赖

```
root@fd4cc081e295:~/elixir/docs# mix deps.get
* Updating ex_doc (git://github.com/elixir-lang/ex_doc.git)
==> ex_doc
Could not find hex, which is needed to build dependency :earmark
Shall I install hex? [Yn] Y
2014-10-13 09:07:56 URL:https://s3.amazonaws.com/s3.hex.pm/installs/hex.ez [240877/240877] -> "/root/.mix/archives/hex.ez" [1]
* creating /root/.mix/archives/hex.ez
```

- 生成文档

```
mix docs
```

## ExDoc中文模板

下载地址:
https://github.com/developerworks/ex_doc

![ExDoc中文模板][2]


  [1]: /assets/images/820FF121-4358-4564-BBB9-1C5C5F7AF8BF.png
  [2]: /assets/images/61802553-AA1E-430F-9BED-2B25BC2BAF58.png


