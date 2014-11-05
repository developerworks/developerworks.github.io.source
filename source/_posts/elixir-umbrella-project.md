title: Elixir 伞形项目
categories:
  - Elixir
tags:
  - Umbrella
toc: false
date: 2014-11-04 20:23:30
---

关于伞形项目, 什么是伞形项目, 定义在[这里](https://github.com/elixir-lang-china/elixir_guide_cn/blob/master/mix/Chapter1.md#15-%E4%BC%9E%E5%BD%A2%E9%A1%B9%E7%9B%AE)

创建一个伞形项目


    ```
    root@0b85dcd174f2:~/ejabberd/elixir# mix new umbrella_project --umbrella
    * creating .gitignore
    * creating README.md
    * creating mix.exs
    * creating apps
    * creating config
    * creating config/config.exs
    Your umbrella project was created successfully.
    Inside your project, you will find an apps/ directory
    where you can create and host many apps:
        cd umbrella_project
        cd apps
        mix new my_app
    Commands like `mix compile` and `mix test` when executed
    in the umbrella project root will automatically run
    for each application in the apps/ directory.
    ```

> 在伞形项目中运行的Mix任务,会对文件夹中的每一个子项目起作用.
