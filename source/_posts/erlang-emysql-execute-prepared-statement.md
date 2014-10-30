title: Emysql 执行预处理语句
categories:
  - Erlang
tags:
  - emysql
toc: false
date: 2014-10-23 16:20:32
---

```
%% 创建一个预处理语句
emysql:prepare(stmt_isbound, <<"SELECT COUNT(sn) FROM online_users WHERE sn = ? AND jid = ?">>),
%% 执行预处理语句
{_, _, _, [[Rows]], _} = emysql:execute(pool_gbox_messager, stmt_isbound, [Sn, Jid]),
?INFO_MSG("COUNT: ~p~n", [Rows])
```