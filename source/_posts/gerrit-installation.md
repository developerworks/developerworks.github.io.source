title: 安装Gerrit
categories:
  - git
tags:
  - gerrit
date: 2014-08-21 22:43:12
----

Gerrit 版本: 2.9
JDK 版本: 1.7.0_45


很多东西都曾经使用过,这是其中之一,今日又需要安装,把以前没有遇到的问题记录一下

> 注意

Gerrit 手册创建配置数据库的SQL语句如下:

```
CREATE USER 'gerrit2'@'localhost' IDENTIFIED BY 'secret';
  CREATE DATABASE reviewdb;
  GRANT ALL ON reviewdb.* TO 'gerrit2'@'localhost';
  FLUSH PRIVILEGES;
```

如果你的数据库默认编码为非`latin1`, 需要执行下面的SQL语句把数据库编码修改为`latin1`否则会安装`Gerrit`的时候会出现莫名其妙的错误.

```
ALTER DATABASE review charset=latin1;
```

