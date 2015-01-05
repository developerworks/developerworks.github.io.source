title: Elixir提示 001
categories:
  - Elixir
tags:
  - Elixir
toc: false
date: 2015-01-05 02:12:56
---

## 模块

- Elixir代码可以组织到模块中
- 模块是函数的集合
- 模块是最小编译单元: 每个模块经过编译会生成一个`.beam`文件
- 通常在一个Elixir源文件中只定义一个模块,但也可以在一个源文件中定义多个模块,不管在源文件中有多少个模块,反正每个模块都会生成一个`.beam`文件,这种情况是一个编译输入文件产生多个编译输出文件

## unless语句

unless语句为if语句的`反义`

举例

```
defmodule Test1 do
    def run do
        if true do
            IO.puts "true"
        else
            IO.puts "false"
        end
    end
end
```

当表达式1+1==2为真时, 执行if...end之间的代码

```
defmodule Test2 do
    def run do
        unless true do
            IO.puts "true"
        else
            IO.puts "false"
        end
    end
end
```

请把Test1和Test2模块代码复制粘贴到IEx中观察却别


## 参考资料

1. http://stackoverflow.com/questions/25414347/how-do-i-run-a-beam-file-compiled-by-elixir-or-erlang