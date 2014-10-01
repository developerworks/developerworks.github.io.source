title: Erlang 内置函数
categories:
  - Erlang
tags:
  - bif
toc: false
date: 2014-10-01 03:18:17
---

**timestamp()**数据类型

```
timestamp() =  {
    MegaSecs :: integer() >= 0,
    Secs :: integer() >= 0,
    MicroSecs :: integer() >= 0
}
```

`timestamp()`数据类型是一个`三元组`

- 其中第一个元素表示从1970年以来有多少个`百万秒`
- 第二个元素为妙数
- 第三个袁术为微妙数

内置函数`erlang:now()`返回的就是一个`timestamp()`数据类型.

关于时间的深度探索,请看这篇文章: [Erlang取当前时间的瓶颈以及解决方案][1]


apply(Fun, Args) -> term()

Types:

Fun = function()
Args = [term()]
Call a fun, passing the elements in Args as arguments.

Note: If the number of elements in the arguments are known at compile-time, the call is better written as Fun(Arg1, Arg2, ... ArgN).

> Warning
> Earlier, Fun could also be given as {Module, Function}, equivalent to apply(Module, Function, Args). This usage is deprecated and will stop working in a future release of Erlang/OTP.

apply(Module, Function, Args) -> term()

Types:

Module = module()
Function = atom()
Args = [term()]
Returns the result of applying Function in Module to Args. The applied function must be exported from Module. The arity of the function is the length of Args.

> apply(lists, reverse, [[a, b, c]]).
[c,b,a]
apply can be used to evaluate BIFs by using the module name erlang.

> apply(erlang, atom_to_list, ['Erlang']).
"Erlang"
Note: If the number of arguments are known at compile-time, the call is better written as Module:Function(Arg1, Arg2, ..., ArgN).

Failure: error_handler:undefined_function/3 is called if the applied function is not exported. The error handler can be redefined (see process_flag/2). If the error_handler is undefined, or if the user has redefined the default error_handler so the replacement module is undefined, an error with the reason undef is generated.


  [1]: http://blog.yufeng.info/archives/2977