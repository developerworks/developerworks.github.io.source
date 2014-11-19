title: Elixir | 编码问题
categories:
  - Elixir
tags:
  - Elixir
toc: false
date: 2014-11-13 02:10:12
---



`iex`进入Elixir Shell时出现提示

![Elixir默认编码问题](/assets/images/CE4492B6-CDE2-4497-BBDD-B70D557E6DE7.png)


临时有效

```
export LANG=en_US.UTF-8
```

永久有效


```
update-locale LANG=en_US.UTF-8
```

