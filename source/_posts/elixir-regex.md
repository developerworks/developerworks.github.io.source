title: Elixir 正则表达式
categories:
  - Elixir
tags:
  - Regex
toc: true
date: 2015-01-02 17:29:03
---

## 定义正则表达式

- 在Elixir中定义正则表达式需要使用到一个特殊的前缀`~r`
- 匹配操作需要使用操作符`=~`


## 验证手机号码

比如我们定义一个验证11为手机号码的正则表达式,启动Elixir Shell, 定义一个手机号码的正则表达式:

```
iex(1)> regex_cellphone = ~r"^[0-9]{11,11}$"
~r/^[0-9]{11,11}$/
```

匹配:

```
iex(2)> "13912345678" =~ regex_cellphone
true
```

在判断条件中使用

```
iex(7)> cond do
...(7)>   "13912345678" =~ regex_cellphone == true ->
...(7)>     IO.puts "Cellphone number 13912345678 is a valid cell phone number"
...(7)>   true ->
...(7)>     false
...(7)> end
Cellphone number 13912345678 is a valid cell phone number
:ok
```

## 验证电子邮件地址

```
iex(13)> str_email = "riza@elixirdose.com"
"riza@elixirdose.com"
iex(14)> Regex.match?(~r/(\w+)@([\w.]+)/, str_email)
true
iex(15)> cond do
...(15)>   Regex.match?(~r/(\w+)@([\w.]+)/, str_email) == true ->
...(15)>    IO.puts "Your email is a valid email"
...(15)>   true ->
...(15)>    IO.puts "Your email address is invalid"
...(15)> end
Your email is a valid email
:ok
```

编译版本, 正则表达式有几个变化

- `Regex.compile`编译正则表达式字符串不需要使用`~r`前缀
- `\`符号需要使用`\\`替代(转义)
上面在Elixir Shell中的正则表达式`~r/(\w+)@([\w.]+)/`需要改成字符串`(\\w+)@([\\w.]+)`, 对应的编译语句为`{:ok, regex} = Regex.compile("(\\w+)@([\\w.]+)")`

```
defmodule RegExpTest do
    def run(email_address) do
        case Regex.compile("(\\w+)@([\\w.]+)") do
            {:ok, regex} ->
                if Regex.match?(regex, email_address) == true do
                    IO.puts "Your email is a valid email"
                else
                    IO.puts "Your email address is invalid"
                end
            _ ->
                IO.puts "Invalid regex definition"
        end
    end
end
```

在Elixir Shell中执行:

```
iex(4)> defmodule RegExpTest do
...(4)>     def run(email_address) do
...(4)>         case Regex.compile("(\\w+)@([\\w.]+)") do
...(4)>             {:ok, regex} ->
...(4)>                 if Regex.match?(regex, email_address) == true do
...(4)>                     IO.puts "Your email is a valid email"
...(4)>                 else
...(4)>                     IO.puts "Your email address is invalid"
...(4)>                 end
...(4)>             _ ->
...(4)>                 IO.puts "Invalid regex definition"
...(4)>         end
...(4)>     end
...(4)> end
iex:4: warning: redefining module RegExpTest
{:module, RegExpTest,
 <<70, 79, 82, 49, 0, 0, 6, 160, 66, 69, 65, 77, 69, 120, 68, 99, 0, 0, 0, 122, 131, 104, 2, 100, 0, 14, 101, 108, 105, 120, 105, 114, 95, 100, 111, 99, 115, 95, 118, 49, 108, 0, 0, 0, 2, 104, 2, ...>>,
 {:run, 1}}
iex(5)> RegExpTest.run("riza@elixirdose.com")
Your email is a valid email
:ok
```

## 验证身份证号码

规则:

- 身份证号码必须是18为字符长度,有17为数字本体码和1位校验码构成
- 本体码包含3个部分, 分别是
    - 地址(地区)码
    - 出生日期
    - 顺序码(男性奇数, 女性偶数)
    在特定应用下,比如需要用户输入身份证号码的应用场景, 我们可以通过顺序吗识别用户的性别,减少用户输入.

TODO:: 实现


## 参考资料

1. 验证邮件地址示例
https://gist.github.com/rizafahmi/b1711f7f10aaf7a7ced9
2. 身份证验证规则
http://wenku.baidu.com/link?url=lSfaWKLcnVJTF7LYXOG5qdEhY_Ns5mBWyPu372WcskB7aII2UTzQ5UakYCIBENBy2LKTz0HftEr7boaFnFZcjjbHec_8Py708uqq9nfMmEW
3. 常用语法,控制流语句
http://learnxinyminutes.com/docs/zh-cn/elixir-cn/