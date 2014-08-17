title: 如何发布npm模块
date: 2014-08-17 00:42:59
categories:
  - node
tags:
  - npm
---


## 设置作者信息

要发布模块到npm公共仓库, 首先一个账号是必须的, 如果你在`npmjs.org`上还没有一个账号, 注册一个就是了, 发布之前还需要设置几个作者信息.

```
npm set init.author.name "developerworks"
npm set init.author.email "developerworks@163.com"
npm set init.author.url "http://developerworks.github.io"
```

下一步是执行`npm adduser` 把你的账号关联起来.

<!-- more -->