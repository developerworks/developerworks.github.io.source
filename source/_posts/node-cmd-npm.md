title: NPM常用命令
categories:
  - node.js
tags:
  - npm
date: 2014-08-16
---

## 1. 初始化一个全新的模块

```
mkdir project1 && cd project1
npm init
```


![初始化一个全新的模块][1]


命令执行完成,会在当前目录下生成`package.json`文件, 这是编写一个公共模块的基本步骤, 像普通项目一样,可以`npm install $package_name`安装依赖的其他模块




## 1. 检查一个项目的依赖模块是否有新版本

```
$ npm outdated
npm http GET https://registry.npmjs.org/acl
npm http GET https://registry.npmjs.org/amqp
npm http GET https://registry.npmjs.org/epilogue
...
```

命令执行完毕后,会显示一个更新报告如下:

<!-- more -->


![检查npm依赖模块新版本][2]



字段说明:

- **Package**:
    包名称
- **Current**:
    当前项目`node_modules`目录下该模块的版本号
- **Wanted**:
    `package.json`文件`dependencies`定义期望的版本号
- **Latest**:
    `npm`仓库中该模块最新的版本号

其中模块名称标记为红色的是需要更新的, 从图中可以看出来 `Current` 的版本号是小于 `Wanted`的

  [1]: /assets/images/B1F8FA0F-178F-4338-9342-3361FCE1DC5B.png
  [2]: /assets/images/EBE0EB56-03B1-4618-A84B-C17B45619B4A.png
