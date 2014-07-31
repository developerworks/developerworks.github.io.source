title: RESTFUL API文档生成工具及规范
date: 2014-05-11
categories:
- API
tags:
- API
- restful
- swagger
---

本文收集关于在`Node.js`项目中通过模型定义自动生成API接口文档的工具和方法


<!-- more -->
## 1. Swagger

----------

## 1.1 Swagger UI

https://github.com/wordnik/swagger-ui

## 1.2. swaggerize

从`sequelize.js`模型对象生成swagger模型定义

## 1.2.1 安装 swaggerize

```
npm install swaggerize
```

## 1.2.2 使用

```
var Sequelize = require('sequelize');
var swaggerize = require('swaggerize');
var sequelize = new Sequelize(config.database, config.username, config.password, config.options);
// 加载模型
// sequelize.import(UserModelFile);
sequelize.sync();
var swagger_model_json = swaggerize(sequelize);
```

## 2. API Blueprint

http://apiary.io/
https://github.com/wordnik/swagger-node-express/blob/master/Apps/petstore/models.js

## 3. RAML