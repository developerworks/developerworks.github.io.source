title: 使用node-reggie搭建超轻量级的私有npm仓库
date: 2014-08-14
categories:
  - Node.js
tags:
  - node-reggie
  - npm
---


为了提高安装npm包的速度, 之前按照npm官方的教程搭建过一个本地npm镜像仓库, 安装配置过程太过繁琐,明显不适用与敏捷的要求. 包文件存储在`couchdb`数据库中, 500多G的数据太过庞大. 管理起来巨麻烦.

`reggie` 是一个基于文件系统目录的超轻量级的npm本地仓库, 用于私有模块的版本管理, 部署还是很好用的, 下面只需要简单的几个命令行就可以跑起来. 使用感觉很Nice, 不会消耗过多的时间,简单实用.

<!-- more -->

## 安装

```
npm install -g reggie
```

## 运行

```
$ reggie-server --help
Reggie wants to serve your packages!
Usage: reggie-server
Options:
  -d, --data  Directory to store Reggie's data                                         [default: "~/.reggie/packages/package/{$package_name}/data"]
  -p, --port  Reggie's a good listener. What port should I listen on?                  [default: 8080]
  -h, --host  Which host should Reggie listen on?                                      [default: "0.0.0.0"]
  -u, --url   URL where `npm` can access registry (usually http://{hostname}:{port}/)
```

启动服务器

```
reggie-server -d ~/.reggie
```

## 发布


```
$ reggie -u http://$host:$port publish
```

### reggie 命令行参数

```
$ reggie --help
reggie publish             --> Publish current module (from module root)
reggie info <package_name> --> Show JSON info about a particular package
Options:
  -u, --url  The base URL of the Reggie server (e.g. http://reggie:8080)  [required]  [default: "http://127.0.0.1:8080"]
```


## 安装


通过本地私有仓库安装一个模块, 与安装官方npm模块不同的时,这里用`url`地址代替了包名称

```
# $package_name 为包名称
npm install http://localhost:8080/package/$package_name/1.0.0
```

### 在package.json中配置依赖

```
dependencies: {
    "$package_name": "http://<$host:$port>/$package_name/1.0.0"
}
```

### 版本号通配符


```
http://<$host:$port>/package/$package_name/1.0.x
http://<$host:$port>/package/$package_name/1.0
http://<$host:$port>/package/$package_name/1
http://<$host:$port>/package/$package_name/x
http://<$host:$port>/package/$package_name/latest
```


## 项目地址

github: https://github.com/mbrevoort/node-reggie
npmjs.org: https://www.npmjs.org/package/reggie


