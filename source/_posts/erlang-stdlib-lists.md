title: Erlang标准库示例 lists
categories:
  - Erlang
tags:
  - lists
toc: false
date: 2014-10-17 11:35:17
---

>## keysearch(Key, N, TupleList) -> {value, Tuple} | false

Types:

    Key = term()
    N = integer() >= 1
    1..tuple_size(Tuple)
    TupleList = [Tuple]
    Tuple = tuple()

在一个元组列表中搜索, 列表中元组的第`N`个元素等于`Key`时返回 `{value, Tuple}`,其中`Tuple`为找到的这个元组. 如果没有找到返回`false`.

Note
    该函数的保留是为了兼容, 使用`lists:keyfind/3`(R13A引入的一个方法)更加方便.


> 例子

**lists:keysearch/3**

```
1> lists:keysearch(mail,1, [{username, "13012345678"}, {mail, "13012345678@139.com"}, {tel, 13012345678}]).
{value,{mail,"13012345678@139.com"}}
```
**lists:keyfind/3**

```
3> lists:keyfind(mail,1, [{username, "13012345678"}, {mail, "13012345678@139.com"}, {tel, 13012345678}]).
{mail,"13012345678@139.com"}
```

注意`lists:keysearch/3`和`lists:keyfind/3`返回值的差异.