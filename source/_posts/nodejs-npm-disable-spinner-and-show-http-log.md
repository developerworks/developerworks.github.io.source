title: Nodejs | 在新版本的npm恢复老版本的下载状态风格
categories:
  - Nodejs
toc: false
date: 2014-12-15 16:51:37
---


新版本的npm在安装或更新包的时候, 显示的一个spinner一直在旋转, 特别是在网络速度不好的情况下体验感非常差.

老版本的npm安装包的时候显示的过程是这样的:


```
npm http GET https://registry.npmjs.org/repeating/-/repeating-1.1.0.tgz
npm http 304 https://registry.npmjs.org/graceful-fs
```

新版本显示的却是这样的:

![NPM新版本进度指示器](/assets/images/a5ff54aa-e577-11e3-8baa-43e1a81fba84.gif)

恢复到老版本的进度显示风格可以使用下面的命令完成:


```
npm config set spin=false
npm config set loglevel=http
```


# ElixirChina

目前[ElixirChina 网站](http://120.24.62.150:4000/)部署在阿里云上。如果要本地开发运行ElixirChina，请依次运行以下命令行指令。

`mix deps.get`
`mix ecto.create Repo`
`mix ecto.migrate Repo`
`mix phoenix.start`

在浏览器输入 `localhost:4000` 就能访问网站啦。

## 想实现的功能
- 论坛基本功能
  - [x] 发帖
  - [x] 贴内评论
  - [x] 用户注册
  - [x] 用户登录
  - [x] session验证
  - [x] 用户管理自己发的贴
  - [x] 任意用户的帖子汇总页面
  - [ ] 添加管理员

- UI设计
  - [ ] 主页帖子分页
  - [x] css美化

- 论坛高级功能
  - [x] 帖子分类
  - [x] 添加用户积分
  - [x] 评论有回复时在消息中心提示
  - [x] 帖子有回复时在消息中心提示
  - [ ] 添加版主


## 参考资料

1. https://github.com/npm/npm/issues/5340