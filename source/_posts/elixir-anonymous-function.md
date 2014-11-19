title: Elixir | 匿名函数
categories:
  - Elixir
tags:
  - Elixir
toc: false
date: 2014-11-10 19:56:02
---

## 快速定义

进入Elixir Shell输入:

```
iex(1)> sum = fn a, b -> a + b end
#Function<12.90072148/2 in :erl_eval.expr/5>
```

匿名函数的定义以关键字`fn`开始, 然后紧跟参数列表, 再接符号`->`, 其后为函数体, 并以关键字`end`结束匿名函数的定义

要调用匿名函数,需要使用`dot`(点)语法.

```
iex(2)> sum.(1,2)
3
```

## 匿名函数作为参数

```
defmodule MyList do
    def filter([], _func) do
        []
    end
    def filter([head|tail], func) do
        if func.(head) do
            [head | filter(tail, func)]
        else
            filter(tail, func)
        end
    end
end
## 奇数
MyList.filter([1,2,3,4,5,6], fn num -> rem(num,2) == 1 end)
```

控制台输出:

```
iex(5)> MyList.filter([1,2,3,4,5,6], fn num -> rem(num,2) == 1 end)
[1, 3, 5]
```

## 匿名函数简写

我们可以把匿名函数`fn n -> rem(n, 2) == 1 end`简写为`&(rem(&1, 2) == 1)`


```
sum = fn a, b -> a + b end
# 等同于
sum = &(&1 + &2)
```

其中第一个`&`表示匿名函数本身`&1`表示匿名函数的第一个参数, `&2`表示匿名函数的第二个参数, 以此类推. 小括号内为函数体表达式.