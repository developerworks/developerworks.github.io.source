title: Elixir-0004 创建命令行工具
categories:
  - Elixir
tags:
  - Tools
toc: false
date: 2014-10-15 10:32:04
---


## 设置应用程序

- 创建项目
```
mix new awesome_cli
cd awesome_cli
```

```elixir lib/awesome_cli.ex
defmodule AwesomeCli do
  use Application.Behaviour
  # See http://elixir-lang.org/docs/stable/Application.Behaviour.html
  # for more information on OTP Applications
  def start(_type, _args) do
    AwesomeCli.Supervisor.start_link
  end
  def main(args) do
    IO.puts "Hello World!"
  end
end
```

## 解析参数

Lucky for us, Elixir has [OptionParser][OptionParser] for parsing CLI argument(s). We will use this module to create an awesome command line tool that takes an argument or two from the user.

First things first, we will create command line tools that will say hello to a name we provide. We will do something like: ./awesome_cli --name ElixirFriend.

Open up lib/awesome_cli.ex and add code below:

```
def main(args) do
  args |> parse_args
end
def parse_args(args) do
  {[name: name], _, _} = OptionParser.parse(args)
  IO.puts "Hello, #{name}! You're awesome!!"
end
```

## 重构代码

## 结语

## 参考资料

1. http://abstraction.killedthecat.net/create-command-line-utility-elixir-mix/
2. http://elixir-lang.org/docs/stable/elixir/OptionParser.html



  [OptionParser]: http://elixir-lang.org/docs/stable/elixir/OptionParser.html