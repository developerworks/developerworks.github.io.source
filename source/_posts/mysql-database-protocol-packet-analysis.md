title: MySQL数据库协议包分析
categories:
  - MySQL
tags:
  - 协议包
toc: false
date: 2014-10-09 02:11:10
---

本文的分析角度,是从Erlang语言的emsyql客户端库作为分析案例,通过对emysql库源码的阅读来分析MySQL数据库的协议包结构.并描述了MySQL协议包的结构,如何处理查询结果,如何处理错误信息等协议层内部机制.

首先从最常用的协议包开始:

## 结果集协议包

顾名思义, 该协议包定义了从MySQL数据库返回的结果集的包格式. 既然是结果集协议包,其中包含我们查询的字段列表.在emysql中定义的字段类型如下:

```erlang MySQL类型 https://github.com/Eonblast/Emysql/blob/master/include/emysql.hrl emysql.hrl
%% MYSQL TYPES
-define(FIELD_TYPE_DECIMAL, 16#00).
-define(FIELD_TYPE_TINY, 16#01).
-define(FIELD_TYPE_SHORT, 16#02).
-define(FIELD_TYPE_LONG, 16#03).
-define(FIELD_TYPE_FLOAT, 16#04).
-define(FIELD_TYPE_DOUBLE, 16#05).
-define(FIELD_TYPE_NULL, 16#06).
-define(FIELD_TYPE_TIMESTAMP, 16#07).
-define(FIELD_TYPE_LONGLONG, 16#08).
-define(FIELD_TYPE_INT24, 16#09).
-define(FIELD_TYPE_DATE, 16#0a).
-define(FIELD_TYPE_TIME, 16#0b).
-define(FIELD_TYPE_DATETIME, 16#0c).
-define(FIELD_TYPE_YEAR, 16#0d).
-define(FIELD_TYPE_NEWDATE, 16#0e).
-define(FIELD_TYPE_VARCHAR, 16#0f).
-define(FIELD_TYPE_BIT, 16#10).
-define(FIELD_TYPE_NEWDECIMAL, 16#f6).
-define(FIELD_TYPE_ENUM, 16#f7).
-define(FIELD_TYPE_SET, 16#f8).
-define(FIELD_TYPE_TINY_BLOB, 16#f9).
-define(FIELD_TYPE_MEDIUM_BLOB, 16#fa).
-define(FIELD_TYPE_LONG_BLOB, 16#fb).
-define(FIELD_TYPE_BLOB, 16#fc).
-define(FIELD_TYPE_VAR_STRING, 16#fd).
-define(FIELD_TYPE_STRING, 16#fe).
-define(FIELD_TYPE_GEOMETRY, 16#ff).
```

每一种类型,用一个16进制数字来标记. 以Erlang宏定义的方式声明了不同字段数据类型的值.


TODO::

