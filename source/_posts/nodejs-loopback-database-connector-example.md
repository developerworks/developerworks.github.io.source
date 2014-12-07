title: Nodejs | Loopback 数据库连接器示例
categories:
  - Node.js
tags:
  - Loopback
toc: false
date: 2014-12-08 00:26:35
---

创建一个Loopback 应用程序

```
slc loopback # 创建项目longfor
cd longfor
```

项目创建好了之后, 在项目目录longfor下执行 `npm install --save loopback-connector-mysql` 安装MySQL数据库连接器

```shell
 $ npm install --save loopback-connector-mysql
npm http GET https://registry.npmjs.org/loopback-connector-mysql
...
...
...
loopback-connector-mysql@1.4.9 node_modules/loopback-connector-mysql
├── sl-blip@1.0.0
├── loopback-connector@1.2.0
├── async@0.9.0
├── debug@2.0.0 (ms@0.6.2)
└── mysql@2.5.3 (require-all@0.0.8, bignumber.js@1.4.1, readable-stream@1.1.13)
```

添加数据源

```
slc loopback:datasource longfordb
```

编辑数据源配置文件,添加数据库连接信息

```json
{
  "db": {
    "name": "db",
    "connector": "memory"
  },
  "longfordb": {
    "name": "longfordb",
    "connector": "mysql",
    "host": "localhost",        # Added
    "port": 3306,               # Added
    "database": "longfor",      # Added
    "username": "root",         # Added
    "password": "root"          # Added
  }
}
```

添加模型`account`,并创建模型`account`的三个属性: email, created, modified

```
slc loopback:model account
Enter an empty property name when done.
? Property name: email
   invoke   loopback:property
? Property type: string
? Required? Yes

Let's add another account property.
Enter an empty property name when done.
? Property name: created
   invoke   loopback:property
? Property type: date
? Required? Yes

Let's add another account property.
Enter an empty property name when done.
? Property name: modified
   invoke   loopback:property
? Property type: date
? Required? Yes
```

创建表并添加测试数据, 下载[create-test-data.js](https://raw.githubusercontent.com/strongloop/loopback-example-database/master/server/create-test-data.js)脚本, 修改数据源名称为longfordb:

```
# 修改
var dataSource = server.dataSources.accountDB;
# 为
var dataSource = server.dataSources.longfordb;
```

```
$ cd server
$ node create-test-data.js
Record created: { email: 'foo@bar.com',
  created: Mon Dec 08 2014 00:43:16 GMT+0800 (CST),
  modified: Mon Dec 08 2014 00:43:16 GMT+0800 (CST),
  id: 1 }
Record created: { email: 'bar@bar.com',
  created: Mon Dec 08 2014 00:43:16 GMT+0800 (CST),
  modified: Mon Dec 08 2014 00:43:16 GMT+0800 (CST),
  id: 2 }
done
```


## 参考资料

1. loopback-example-database
https://github.com/strongloop/loopback-example-database/blob/master/README.md
