title: 如何使用PM2部署一个Node.js项目
categories:
  - Node.js
tags:
  - PM2
toc: false
date: 2015-02-07 23:05:51
---


## 创建一个新项目用于部署


第一步我们需要创建一个项目

```
mkdir pm2_test && cd pm2_test
npm init
```

然后创建初始化一个Express项目

```
express init
```

第三步安装依赖模块

```
npm install
```

现在一个基本的Express项目就创建好了.


## 初始化部署配置文件

执行

```
pm2 ecosystem
```

会在当前目录下生成一个`ecosystem.json5`文件

重命名

```
mv ecosystem.json5 ecosystem.json
```

编辑`ecosystem.json`, 设置几个选项


```
  ...
  deploy : {
    production : {
      user : "root",            // 登陆用户名
      host : "servername",      // 要部署的目标服务器IP地址或域名
      ref  : "origin/master",   // 用于部署的Git仓库分支
      repo : "https://github.com/developerworks/pm2_test.git",  // Git仓库位置
      path : "/var/www/production", // 部署目标服务器文件系统位置
      "post-deploy" : "pm2 startOrRestart ecosystem.json --env production"  // 部署后启动
    },
  }
  ...
```

执行部署

```
pm2 deploy ecosystem.json production
```

更新部署

```
pm2 deploy production update
```


## 参考资料

1. https://github.com/Unitech/PM2/blob/development/ADVANCED_README.md

