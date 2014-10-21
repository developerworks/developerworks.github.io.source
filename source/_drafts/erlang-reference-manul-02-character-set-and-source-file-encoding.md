title: Erlang参考手册02
categories:
  - Erlang
tags:
  - reference-manual
toc: true
date: 2014-10-17 02:08:28
---

## 字符集和源文件编码

### 字符集

In Erlang 4.8/OTP R5A the syntax of Erlang tokens was extended to allow the use of the full ISO-8859-1 (Latin-1) character set. This is noticeable in the following ways:

All the Latin-1 printable characters can be used and are shown without the escape backslash convention.

Atoms and variables can use all Latin-1 letters

表 2.1: 字符分类

| 八进制         | 实际值         || 类别                 |
| :------------ | :----------- | :------------------- |
| 200 - 237	| 128 - 159 | 	 	| Control characters
| 240 - 277	| 160 - 191 | - ¿	| Punctuation characters
| 300 - 326	| 192 - 214 | À - Ö	| Uppercase letters
| 327	    | 215	    | ×	    | Punctuation character
| 330 - 336	| 216 - 222	| Ø - Þ	| Uppercase letters
| 337 - 366	| 223 - 246	| ß - ö	| Lowercase letters
| 367	    | 247	    | ÷	    | Punctuation character
| 370 - 377	| 248 - 255	| ø - ÿ	| Lowercase letters

在Erlang/OTP R16B中,Erlang符号语法被扩展能够支持Unicode. 开始只是对字符串的限制性支持. Erlang OTP/18期望能够处理Unicode编码的原子符号. 关于Unicode的在Erlang源代码中的使用可以查看[STDLIB's User's Guide][STDLIB's User's Guide].

  [STDLIB's User's Guide]:http://erlang.org/doc/apps/stdlib/unicode_usage.html#unicode_in_erlang


### 源代码文件编码

Erlang源代码文件编码是由源文件头两行的注释决定的. 选择第一个匹配正则表达式`coding\s*[:=]\s*([-a-zA-Z0-9])+`的字符串作为的编码. 如果匹配的字符串不是一个有效的编码,将被忽略.有效的编码为 Latin-1 和UTF-8

下面的例子选择UTF-8作为默认编码:

```
%% coding: utf-8
```

下面两个例子都把Latin-1作为默认编码:

```
%% For this file we have chosen encoding = Latin-1
```

```
%% -*- coding: latin-1 -*-
```

在Erlang OTP 17.0中,Erlang源代码文件默认编码从Latin-1变更为UTF-8.

## 数据类型

### 项式

Erlang provides a number of data types which are listed in this chapter. A piece of data of any data type is called a term.

### 数字

There are two types of numeric literals, integers and floats. Besides the conventional notation, there are two Erlang-specific notations:

- $char
ASCII value of the character char.
- base#value
Integer with the base base, which must be an integer in the range 2..36.
In Erlang 5.2/OTP R9B and earlier versions, the allowed range is 2..16.

Examples:

```
1> 42.
42
2> $A.
65
3> $\n.
10
4> 2#101.
5
5> 16#1f.
31
6> 2.3.
2.3
7> 2.3e3.
2.3e3
8> 2.3e-3.
0.0023
```

### 原子

An atom is a literal, a constant with name. An atom should be enclosed in single quotes (`'`) if it does not begin with a lower-case letter or if it contains other characters than alphanumeric characters, underscore (`_`), or `@`.

Examples:

```
hello
phone_number
'Monday'
'phone number'
```

### 比特字符串和二进制

A bit string is used to store an area of untyped memory.

Bit Strings are expressed using the bit syntax.

Bit Strings which consists of a number of bits which is evenly divisible by eight are called Binaries

Examples:

```
1> <<10,20>>.
<<10,20>>
2> <<"ABC">>.
<<"ABC">>
1> <<1:1,0:1>>.
<<2:2>>
```

More examples can be found in Programming Examples.

### 引用

A reference is a term which is unique in an Erlang runtime system, created by calling make_ref/0.

### 函数

A fun is a functional object. Funs make it possible to create an anonymous function and pass the function itself -- not its name -- as argument to other functions.

Example:

```
1> Fun1 = fun (X) -> X+1 end.
#Fun<erl_eval.6.39074546>
2> Fun1(2).
3
```
Read more about funs in Fun Expressions. More examples can be found in Programming Examples.

### 端口标识

A port identifier identifies an Erlang port. open_port/2, which is used to create ports, will return a value of this type.

Read more about ports in Ports and Port Drivers.

### 进程ID

A process identifier, pid, identifies a process. `spawn/1,2,3,4`, `spawn_link/1,2,3,4` and `spawn_opt/4`, which are used to create processes, return values of this type. Example:

```
1> spawn(m, f, []).
<0.51.0>
```

The BIF `self()` returns the pid of the calling process. Example:

```
-module(m).
-export([loop/0]).
loop() ->
    receive
        who_are_you ->
            io:format("I am ~p~n", [self()]),
            loop()
    end.
1> P = spawn(m, loop, []).
<0.58.0>
2> P ! who_are_you.
I am <0.58.0>
who_are_you
```

Read more about processes in Processes.

### 元组

Compound data type with a fixed number of terms:

```
{Term1,...,TermN}
```

Each term Term in the tuple is called an **element**. The number of elements is said to be the **size** of the tuple.

There exists a number of BIFs to manipulate tuples.

Examples:

```
1> P = {adam,24,{july,29}}.
{adam,24,{july,29}}
2> element(1,P).
adam
3> element(3,P).
{july,29}
4> P2 = setelement(2,P,25).
{adam,25,{july,29}}
5> tuple_size(P).
3
6> tuple_size({}).
0
```

### 映射

Compound data type with a variable number of key-value associations:

```
#{Key1=>Value1,...,KeyN=>ValueN}
```

Each key-value association in the map is called an **association pair**. The key and value parts of the pair are called **elements**. The number of association pairs is said to be the **size** of the map.

There exists a number of BIFs to manipulate maps.

Examples:

```
1> M1 = #{name=>adam,age=>24,date=>{july,29}}.
#{age => 24,date => {july,29},name => adam}
2> maps:get(name,M1).
adam
3> maps:get(date,M1).
{july,29}
4> M2 = maps:update(age,25,M1).
#{age => 25,date => {july,29},name => adam}
5> map_size(M).
3
6> map_size(#{}).
0
```

A collection of maps processing functions can be found in the STDLIB module maps.

Read more about Maps.

> Note
> Maps are considered experimental during OTP 17.

### 列表

Compound data type with a variable number of terms.

```
[Term1,...,TermN]
```

Each term Term in the list is called an **element**. The number of elements is said to be the **length** of the list.

Formally, a list is either the empty list [] or consists of a **head** (first element) and a **tail** (remainder of the list) which is also a list.
The latter can be expressed as `[H|T]`. The notation `[Term1,...,TermN]` above is actually shorthand for the list `[Term1|[...|[TermN|[]]]]`.

Example:

```
[] is a list, thus
[c|[]] is a list, thus
[b|[c|[]]] is a list, thus
[a|[b|[c|[]]]] is a list, or in short [a,b,c].
```

A list where the tail is a list is sometimes called a **proper list**. It is allowed to have a list where the tail is not a list, for example `[a|b]`. However, this type of list is of little practical use.

Examples:

```
1> L1 = [a,2,{c,4}].
[a,2,{c,4}]
2> [H|T] = L1.
[a,2,{c,4}]
3> H.
a
4> T.
[2,{c,4}]
5> L2 = [d|T].
[d,2,{c,4}]
6> length(L1).
3
7> length([]).
0
```

A collection of list processing functions can be found in the STDLIB module `lists`.

### 字符串

Strings are enclosed in double quotes (`"`), but is not a data type in Erlang. Instead a string `"hello"` is shorthand for the list `[$h,$e,$l,$l,$o]`, that is `[104,101,108,108,111]`.

Two adjacent string literals are concatenated into one. This is done at compile-time and does not incur any runtime overhead. Example:

```
"string" "42"
```

is equivalent to

```
"string42"
```

### 记录

A record is a data structure for storing a fixed number of elements. It has named fields and is similar to a struct in C. However, record is not a true data type. Instead record expressions are translated to tuple expressions during compilation. Therefore, record expressions are not understood by the shell unless special actions are taken. See shell(3) for details.

Examples:

```
-module(person).
-export([new/2]).

-record(person, {name, age}).

new(Name, Age) ->
    #person{name=Name, age=Age}.

1> person:new(ernie, 44).
{person,ernie,44}
```

Read more about records in Records. More examples can be found in Programming Examples.

### 布尔

There is no Boolean data type in Erlang. Instead the atoms true and false are used to denote Boolean values.

Examples:

```erlang
1> 2 =< 3.
true
2> true or false.
true
```

### 转义序列

Within strings and quoted atoms, the following escape sequences are recognized:

| Sequence	| Description    |
| --------- | ---------------|
| \b|backspace          |
| \d|delete             |
| \e|escape             |
| \f|form feed          |
| \n|newline            |
| \r|carriage return    |
| \s|space              |
| \t|tab                |
| \v|vertical tab       |
| \XYZ, \YZ, \Z|character with octal representation XYZ, YZ or Z |
| \xXY |character with hexadecimal representation XY |
| \x{X...} |character with hexadecimal representation; X... is one or more hexadecimal characters |
| \^a...\^z \^A...\^Z |control A to control Z |
| \' |single quote
| \"|double quote
| \\|backslash

Table 3.1:   Recognized Escape Sequences.

### 类型转换

There are a number of BIFs for type conversions. Examples:

```erlang
1> atom_to_list(hello).
"hello"
2> list_to_atom("hello").
hello
3> binary_to_list(<<"hello">>).
"hello"
4> binary_to_list(<<104,101,108,108,111>>).
"hello"
5> list_to_binary("hello").
<<104,101,108,108,111>>
6> float_to_list(7.0).
"7.00000000000000000000e+00"
7> list_to_float("7.000e+00").
7.0
8> integer_to_list(77).
"77"
9> list_to_integer("77").
77
10> tuple_to_list({a,b,c}).
[a,b,c]
11> list_to_tuple([a,b,c]).
{a,b,c}
12> term_to_binary({a,b,c}).
<<131,104,3,100,0,1,97,100,0,1,98,100,0,1,99>>
13> binary_to_term(<<131,104,3,100,0,1,97,100,0,1,98,100,0,1,99>>).
{a,b,c}
14> binary_to_integer(<<"77">>).
77
15> integer_to_binary(77).
<<"77">>
16> float_to_binary(7.0).
<<"7.00000000000000000000e+00">>
17> binary_to_float(<<"7.000e+00>>").
7.0
```