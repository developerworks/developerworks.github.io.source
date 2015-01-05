title: Elixir发布管理器 Exrm
categories:
  - Elixir
tags:
  - Elixir
  - Exrm
toc: false
date: 2015-01-05 01:08:21
---

Elixir发布管理器(Elixir Release Manager)是一个Elixir任务模块,用于把一个完整Elixir项目打包为一个`.tar.gz`文件用于发布和部署.

首先, 需要在`mix.exs`文件中添加如下依赖:

```
defp deps do
    [{:exrm, "~> 0.14.16"}]
end
```

下载依赖,并编译

```
mix deps.get
mix deps.compile
```

发布项目

```
mix release
```

    Build a release for the current mix application.

    Examples

    ┃ # Build a release using defaults
    ┃ mix release
    ┃
    ┃ # Pass args to erlexec when running the release
    ┃ mix release --erl="-env TZ UTC"
    ┃
    ┃ # Enable dev mode. Make changes, compile using MIX_ENV=prod
    ┃ # and execute your release again to pick up the changes
    ┃ mix release --dev
    ┃
    ┃ # Set the verbosity level
    ┃ mix release --verbosity=[silent|quiet|normal|verbose]

    You may pass any number of arguments as needed. Make sure you pass arguments
    using --key=value, not --key value, as the args may be interpreted incorrectly
    otherwise.

