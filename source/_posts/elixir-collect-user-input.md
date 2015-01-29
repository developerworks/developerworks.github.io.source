title: 收集用户输入
categories:
  - Elixir
tags:
  - Elixir
  - IO
toc: false
date: 2014-10-25 15:59:06
---

在控制台程序中收集用户的输入

```
# 获取用户名
input_username = IO.gets("Please input your username: ")
username = String.strip(input)
first = String.first(username)
# 获取年龄
input_age = IO.gets("Please input your age:")
age_str = String.strip(input_age)
age = String.to_integer(age_str)
```
