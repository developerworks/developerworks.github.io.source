title: Elixir | 理解宏(Macro)第一部分
categories:
  - Elixir
tags:
  - Macro
toc: false
date: 2014-12-18 00:02:39
---

本文中的某些概念, 你最好事先了解过编译原理, 以及抽象语法树的(AST)概念, 否则本文中所描述的东西可能对你来说是看不懂的天书.

## 编译过程

![Elixir源码编译过程图示](/assets/elixir/compile-process.png)

通过上图的Elixir编译器的编译过程我们看到:

1. Elixir源码通过第一步的解析过程生成了一个AST 1的中间表述形式(以Elixir嵌套Term的形式表示)
2. AST 1通过`expansion`转换为Expanded AST
3. 展开的AST被转换成字节码

这只是一个近似的过程, 实际上Elixir编译器会生成Erlang AST, 并依赖于底层的Erlang函数把它转换成字节码.

## 创建AST片段

什么是Elixir AST ? 它是一个Elixir Term, 一个深度嵌套的层次结构, 用于表述一个语法正确的Elixir代码. 为了说得更明白一些, 举个例子. 要生成某段代码的AST, 可以使用`quote`:

```
iex(1)> quoted = quote do 1 + 2 dn
{:+, [context: Elixir, import: Kernel], [1, 2]}
```

Quote 以任意复杂的Elixir表达式作为输入,并返回相应的描述该输入代码的AST.

此例中, 生成的AST片段描述一个简单的求和操作(1+2). 通常称为`quoted expression`. 大多数时候不需要理解quoted结构的具体细节, 来看一个简单的例子. 在这种情况下, AST片段是一个包含如下元素的三元组(triplet)

- 一个操作符原子
- 表达式上下文(比如, import和aliases).大多数时候不需要理解该数据
- 操作参数(operands)

要点: `quoted expression`是一个描述代码的Elixir term. 编译器会使用它生成最终的字节码.

虽然不常见, 对一个quoted expression求值也是可以的.

```
iex(2)> Code.eval_quoted(quoted)
{3, []}
```

求值结果为一个元组, 包含表达式的求值结果, 以及构成该表达式的变量.

但是, 在AST被求值前(通常由编译器完成), quoted expression 并没有通过语义上的验证. 例如, 当我们书写如下表达式时:

```
iex(3)> a + b
** (RuntimeError) undefined function: a/0
```

得到了一个错误, 因为这里还不存在一个名为`a`的变量或函数.

相比而言, 如果quote一个表达式:

```
iex(4)> quote do a + b end
{:+, [context: Elixir, import: Kernel], [{:a, [], Elixir}, {:b, [], Elixir}]}
```

而没有发生错误, 我们有了一个表达式 `a+b`的quoted表示. 其意思是, 生成了一个描述表达式`a+b`的term, 不管表达式中的变量是否存在. 最终的代码并没有生成, 所以这里不会有错误.

如果把该表述插入到某些a和b是有效标识符的AST中, 刚才发生错误的代码`a+b`,才是正确的. 下面来试一下, 首先quote一个求和(sum)表达式:

```
iex(5)> sum_expr = quote do a + b end
```

然后创建一个quoted变量绑定表达式:

```
iex(6) bind_expr = quote do
         a = 1
         b = 2
       end
```

再说一遍, 记住这仅仅是quoted的表达式, 他们只是描述代码的简单数据, 并没有执行任何求值. 特别是, 变量 `a` 和 `b` 在当前的Elixir shell会话中并不存在.

要使这些片段能够一起工作, 必须把它们连接起来:

```
iex(7) final_expr = quote do
         unquote(bind_expr)
         unquote(sum_expr)
       end
```

这里,我们生成了一个新的quoted表达式`final_expr`, 由`bind_expr`表达式和`sum_expr`表达式构成.

下面可以对最终的AST求值:

```
iex(8)> Code.eval_quoted(final_expr)
{3, [{{:a, Elixir}, 1}, {{:b, Elixir}, 2}]}
```

求值结果由一个`表达式`, 一个`变量绑定列表`构成. 形如:

```
{expression, [{{:variable,Elixir}, value},...]}
===========      ========          ======
  |                 |                 |
表达式            变量名称           变量的值
```

从这个绑定列表中我们可以看出, 该表达式绑定了两个变量`a`和`b`, 对应的值分别为`1`和`2`

这就是在Elixir中元编程方法的核心. 当我们进行元编程的时候, 我们基本上是把各种AST片段组合起来生成新的AST. 我们通常对输入AST的内容和结构不感兴趣, 相反, 我们使用`quote`生成和组合输入片段,并生成经过修饰的代码.

## Unquoting

`unquote`上场了. 注意不管`quote`块里面包含什么, 它只是把`quote ... end`块里面的达式转换成AST片段. 这意味着我们不能以常规方式注入存在于quote外部的变量的内容. 看看上面这个例子, 它是不可能工作的:

```
quote do
  bind_expr
  sum_expr
end
```

在这个例子中, quote仅仅是简单的生成对`bind_expr`和`sum_expr`的变量引用, 但这不是我想要的结果. 我需要的效果是有一种方式能够直接注入`bind_expr`和`sum_expr`的内容到生成的AST的对应的位置.

这就是`unquote(...)`的用途 - 括号内的表达式被立即进行求值, 并就地插入到`unquote`调用的位置. 这意味着 `unquote` 的结果也必须是一个有效的AST片段.

理解`unquote`的另一种方式是, 可以把它看做是字符串插值 (`#{}`). 对于字符串你可以这样写:

```
"....#{some_expression}...."
```

类似的, 对于quote可以这样写:

```
quote do
    unquote(some_expression)
end
```

对此两种情况, 求值的表达式必须在当前上下文中是有效的, 并注入该结果到你构建的表达式中.(要么是符串, 或者是一个AST片段)

重要的时理解其含义, unquote并不是quote的反向过程. 如果需要把一个quoted expression装换为字符串, 可以使用`Macro.to_string/1`

## 例子: 追踪表达式

让我们把这些理论组合到一个简单的例子中. 我们会编写一个宏来帮助我们调试代码. 下面是这个宏的使用方式:

```
iex(1)> Tracer.trace(1 + 2)
Result of 1 + 2: 3
```

`Tracer.trace`以一个给定的表达式并打印其结果到屏幕上.

## 展开AST