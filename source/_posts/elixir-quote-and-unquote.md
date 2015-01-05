title: Elixir | 元编程Quote和Unquote
categories:
  - Elixir
tags:
  - Elixir
  - Elixir
toc: false
date: 2014-11-10 22:39:17
---

## 概念介绍

- Elixir中的AST是什么?
    - 是一个Elixir Term
    - 是深度嵌套的一种表示Elixir代码结构的方法
- 如何理解?
    ```
    iex(1)> quoted = quote do 1 + 2 end
    {:+, [context: Elixir, import: Kernel], [1, 2]}
    ```
    quote用于把一个Elixir表达式转换为Elixir的AST片段
- 抽象语法树片段(AST Fragment)

才接触Quote和Unquote的时候会让人迷惑. 当你很好的理解了它后, 却是非常好用的.


<!--
一个典型的应用场景就是自动化的生成代码, 用于处理系统中高度重复性的工作
- 数据库模型对象的生成
依据数据库的定义可以自动化的生成模型对象
-->

## Quote

Quote是一个Elixir函数,用于把一个Elixir表达式转换为AST(抽象语法树). AST是一种编译器的内部表示,用于对表达式进行求值, 比如:

```
iex> quote do: 1 + 2
{:+, [context: Elixir, import: Kernel], [1, 2]}
```

表达式`1 + 2`在Elixir编译器中被表示为一个有三个元素的元组:

- 操作符 (`:+`)
- 关键字列表元数据(`[context: Elixir, import: Kernel]`)
- 参数列表 (`[1, 2]`)

如果不考虑关键字列表元数据, 其可以表示为一棵抽象语法树, 以操作符为根节点, 2个参数为树叶

![表达式1 + 2的抽象语法树](/assets/images/ast1.png)

再如, 一个稍微复杂一点的表达式`1 + 2 * 3`, 将生成一个更加复杂的抽象语法树

```
iex> quote do: 1 + 2 * 3
{:+, [context: Elixir, import: Kernel],[1, {:*, [context: Elixir, import: Kernel], [2, 3]}]}
```

![表达式1 + 2 * 3的抽象语法树](/assets/images/ast2.png)

当对表达式求值的时候, Elixir编译器将会从最左边的叶子节点开始遍历. 例如,该AST会求值为: `1 + (2 * 3)`

为了对Quote表达式求值, 需要使用`Code.eval_quoted`函数, 例如:

```
iex> Code.eval_quoted(quote do: 1 + 2 * 3)
{7, []}
```

`Code.eval_quoted`函数调用返回一个最终求得的值和一个从求值表达式产生的所有变量列表. 上述求得的值为7, 因为没有变量绑定, 所以返回一个空的列表.

Quote还可以用于函数调用, 比如:

```
iex> quote do: sum(1, 2, 3)
{:sum, [], [1, 2, 3]}
```

Quote function is like a function which is used to put an expression between a quote so that it can be used later on.

Next let's try to define some variables and use those in quote body:

```
iex> a = 1
1
iex> b = 2
2
iex> Code.eval_quoted(quote do: a + b)
** (CompileError) nofile:1: undefined function a/0
```

The `eval_quoted` function call will give you an error on undefined function a. This happens because when
`Code.eval_quoted` is called, it does not know any `a` value, because the a here is not the same variable
that we defined outside ealier. In order to refer a variable defined outside quote, unquote function needs to be used

## Unquote

So here, how it should be written if a variable is referred to outside of the scope of quote:

```
iex> a = 1
1
iex> b = 2
b
iex> quote do: unquote(a) + unquote(b)
{:+, [context: Elixir, import: Kernel], [1, 2]}
```

As you can see, the value of a and b are now evaluated correctly before Elixir construct the abstract syntax tree and
these values are actually computed at compiled time and not runtime. Now, let say, we define a function:

```
iex> a = 1
1
iex> b = 2
b
iex> fun = fn -> quote do
   unquote(a) + unquote(b)
  end
end
{:+, [context: Elixir, import: Kernel], [1, 2]}
```

Now we try to change a value and call the function again to see if the presentation will change with the new a value:

```
iex> a = 10
10
iex> fun.call()
{:+, [context: Elixir, import: Kernel], [1, 2]}
```

As you can see, although a's value is change but the funtioncal representing `a + b` is still reflecting the original
value of a and b. The way that we use quote and unquote in Elixir can be very creative and dynamic, for instance, we
can define like following to play with the real function definition at runtime.

```
iex> num1 = 5
iex> num2 = 2
iex> perform = fn fun -> Code.eval_quoted(quote do: unquote(fun)(unquote(num1), unquote(num2))) end
iex> perform.(:rem) # calculate remaining of 5 and 2
{1, []}
iex> perform.(:div) # calculate division result of 5 and 2
{2, []}
```
