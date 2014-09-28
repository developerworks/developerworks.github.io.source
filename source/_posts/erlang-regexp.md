title: Erlang 正则表达式
categories:
  - Erlang
tags:
  - regexp
toc: false
date: 2011-05-30 19:01:22
---

``` 
Eshell V5.8  (abort with ^G)
% 最长单个匹配
regexp:match("linux,windows,erlang,php,mysql", "[a-z]+").
{match,7,7}
```

```
% 首个匹配
regexp:first_match("linux,erlang,mysql,mnesia", "[a-z]+").
{match,1,5}
```

```
% 匹配,并获取linux子串
1> {M,L} = regexp:matches("linux,erlang,mysql,mnesia", "[a-z]+"),
1> {Start, Length} = lists:nth(1, L),
1> Substring = string:substr("linux,erlang,mysql,mnesia",Start,Length),
1> io:format("The first substring is ~p~n", [Substring]).
The first substring is "linux"
ok
```

```
% 替换
regexp:sub("webmaster@gmail.com", "@[a-z\.]+", "@163.com").
{ok,"webmaster@163.com",1}
```

```
% 全局替换
regexp:gsub("webmaster@gmail.com,admin@gmail.com", "@[a-z\.]+", "@163.com").
{ok,"webmaster@163.com,admin@163.com",2}
```

```
% 切分
regexp:split("webmaster@gmail.com admin@gmail.com", "\s").
{ok,["webmaster@gmail.com","admin@gmail.com"]}
```

```
% 解析(编译)一次正则表达式,使之能够到处重用,类似Python的re.compile
7> RE = regexp:parse("[a-z]+").
{ok,{pclosure,{char_class,[{97,122}]}}}
```