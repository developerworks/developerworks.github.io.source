title: 使用node-reggie搭建超轻量级的私有npm仓库
date: 2014-08-14
categories:
- node.js
tags:
- node-reggie
- npm
---

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

## 发布包

```
$ reggie -u http://$host:$port publish
```


## 安装包

```
# $package_name 为包名称
npm install http://localhost:8080/package/$package_name/1.0.0
```

## 在package.json中配置依赖

```
dependencies: {
    "$package_name": "http://<$host:$port>/$package_name/1.0.0"
}
```

## 版本号通配符


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