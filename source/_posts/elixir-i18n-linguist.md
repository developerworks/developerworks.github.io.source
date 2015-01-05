title: Elixir | 使用linguist处理国际化字符串
categories:
  - Elixir
tags:
  - Elixir
  - I18n
toc: false
date: 2014-11-05 13:47:41
---

使用Mix创建一个项目:

    ```
    mix new i18n_example
    cd i18n_example
    ```

编辑`mix.exs`项目文件, 添加linguist依赖

    ```
    defp deps do
        [{:linguist,  github: "chrismccord/linguist"}]
    end
    ```

获取依赖

    ```
    mix deps.get
    ```

增加国际化字符串文件

    ```
    # locales/zh.exs
    [
      flash: [
        notice: [
          hello: "你好 %{first} %{last}"
        ]
      ]
    ]
    # locales/fr.exs
    [
      flash: [
        notice: [
          hello: "salut %{first} %{last}"
        ]
      ]
    ]
    ```

实现模块

    ```
    defmodule I18n do
        use Linguist.Vocabulary
        # 默认英文, 注意,这里的locale是一个宏,而不是一个函数
        locale "en", [
            flash: [
                notice: [
                    hello: "hello %{first} %{last}",
                    bye: "bye now, %{name}!"
                ]
            ],
            users: [
                title: "Users",
                profiles: [
                    title: "Profiles"
                ]
            ]
        ]
        # locale宏还可以直接读取文件
        locale "fr", Path.join([Path.dirname(__DIR__), "locales","fr.exs"])
        locale "zh", Path.join([Path.dirname(__DIR__), "locales","zh.exs"])
    end
    ```

测试

    ```
    $ iex -S mix
    iex(1)> I18n.t!("en", "flash.notice.hello", first: "chris", last: "mccord")
    "hello chris mccord"
    iex(2)> I18n.t!("zh", "flash.notice.hello", first: "chris", last: "mccord")
    "你好 chris mccord"
    ```

