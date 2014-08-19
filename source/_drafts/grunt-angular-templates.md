title: 使用angular模板缓存
categories:
  - grunt
tags:
  - angular
  - grunt
date: 2014-08-17 23:48:22
---

Angular的模板是通过Ajax方式异步加载的,一个完成的Angular应用程序是由无数多个模板片段构成的, 我开发过的中等规模的模板有上千个之多. 这回造成较大的网络开销, 同时增加了客户端的延迟. 已经又人想到并解决了这个问题.
那就是 `grunt-angular-templates`.


`grunt-angular-templates` 的用途是把多个`HTML`模板打包成一个`javascript`模板定义文件. 开始说明前, 首先安装需要的依赖模块.

```
/test $ npm install --save-dev grunt-angular-templates
```



