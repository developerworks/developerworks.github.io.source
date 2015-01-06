title: Elixir 把Erlang代码转换为Elixir
categories:
  - Elixir
tags:
  - Elixir
toc: false
date: 2015-01-05 02:21:41
---

项目地址:

https://github.com/developerworks/ws_cowboy


## 项目描述文件`mix.exs`

```
defmodule WsCowboy.Mixfile do
  use Mix.Project
  def project do
    [app: :ws_cowboy,
     version: "0.0.1",
     elixir: "~> 1.0",
     deps: deps]
  end
  def application do
    [
      applications: [:logger, :cowboy],
      mod: {WsCowboy, []}
    ]
  end
  defp deps do
    [{:cowboy,"~> 1.0.0"}]
  end
end
```
## Application

![Application](/assets/elixir/F12C0B50-7202-4D66-9F6D-90653C9F1BAC.png)

## Supervisor

![Supervisor](/assets/elixir/A80E3448-29CE-421F-882F-D1E6FBBBCAB1.png)

## Websocket Handler

![Websocket Handler](/assets/elixir/1A369D10-7585-4B51-805D-C289B1F8C8AD.png)

## 运行结果

![运行结果](/assets/elixir/4CB26FF2-B581-4125-9A90-82AAC005C4D5.png)

## 参考资料

1. Converting Erlang code into Elixir
http://blog.plataformatec.com.br/2014/11/converting-erlang-code-into-elixir/