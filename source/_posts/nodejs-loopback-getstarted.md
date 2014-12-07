title: Nodejs | Loopback框架入门
categories:
  - Node.js
tags:
  - Loopback
toc: false
date: 2014-11-22 21:44:48
---

## 安装strongloop框架

```shell
npm install -g strongloop
```

## 创建项目

运行 `slc loopback`, 提示输入项目信息, 包括项目名称, 目录名称等.

```shell
$ slc loopback
[?] Enter a directory name where to create the project: longfor
[?] What's the name of your application? longfor
```

上述过程创建了一个名为longfor的项目, 目录名称也为longfor.

## 创建模型

进入 longfor 目录执行 `slc loopback:model`

```shell
$ cd longfor
$ slc loopback:model
[?] Enter the model name: user
[?] Select the data-source to attach person to: db (memory)
[?] Expose person via the REST API? Yes
[?] Custom plural form (used to build REST URL): people
Let's add some person properties now.
```

## 安装数据库连接器

这里的例子, 我们以MySQL作为例子:

```shell
npm install --save loopback-connector-mysql
```

## 添加数据源

数据库连接器安装好了之后, 我们需要为我们的应用添加数据源, 通过下面的命令给应用程序longfor添加数据源, 数据源的名称为longfordb, MySQL作为连接器:

```shell
$ slc loopback:datasource longfordb
? Enter the data-source name: longfordb
? Select the connector for longfordb: (Use arrow keys)
❯ In-memory db (supported by StrongLoop)
  Email (supported by StrongLoop)
  MySQL (supported by StrongLoop)
  PostgreSQL (supported by StrongLoop)
  Oracle (supported by StrongLoop)
  Microsoft SQL (supported by StrongLoop)
  MongoDB (supported by StrongLoop)
```

数据源创建好了, 接下来就是配置数据源, 让代码可以通过我们刚才创建的这个数据源名称去和后端数据库建立连接:

## 配置数据源

数据源的配置文件在目录`$PROJECT/server/datasources.json`, 修改内容为:

```shell
{
  ...
  "longfordb": {
    "name": "longfordb",
    "connector": "mysql",
    "host": "localhost",
    "port": 3306,
    "database": "longfordb",
    "username": "root",
    "password": "root"
  }
}
```

## 关于模型

关于模型Loopback有几个内置的模型类

- `PersistedModel`
该模型类继承自`Model`类,额外支持基本查询功能和CRUD操作.


