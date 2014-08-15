title: 使用Swagger-Codegen生成服务端和客户端代码
date: 2014-07-30
categories:
- API
tags:
- RESTFUL
- API
- swagger
---

swagger-codegen 项目地址

```
https://github.com/wordnik/swagger-codegen
```

> ## 安装依赖软件

`swagger-codegen` 是用Scala语言开发的,首先需要下载一些必要的软件:

1. Java 1.7
2. Maven 3.0.3及以上
3. Scala 2.9.1 (我使用的时2.10.1)

<!-- more -->


> ## 克隆项目


```
clone https://github.com/wordnik/swagger-codegen.git
```

修改 `build.sbt`

Clone 完 `swagger-codegen` 后, 修改 `build.sbt` 中的 `scalaVersion:="2.10.1"` (在第18行左右)

> ## 编译

```
./sbt assembly
```

编译完成,会生成目录`target/scala-2.10`
这里不知道为什么,我在`build.sbt`中设置的`scalaVersion:=2.10.1`, 生成的目录却使`target/scala-2.10`
解决办法:手工修改目录名称, 如果不修改为和`scalaVersion`一样,后面生成代码的时候会报错说找不到`jar`文件

> ## 生成代码之前

这里用 `Node.js` 做例子,生成服务器端代码, 运行生成的服务器端代码有一个问题, 在Swagger-UI中查看不能正确显示参数的问题.Swagger-UI使用`parameters`, 而生成的代码却使`params`

首先找到 `Node.js` 服务端代码生成模板
```
{swagger-codegen-root}/samples/server-generator/node/templates/api.mustache
```

在27行左右, 把:

{% raw %}
```
"params" : [{{#queryParams}}
```
{% endraw %}

修改为

{% raw %}

```
"parameters" : [{{#queryParams}}
```
{% endraw %}


> ## 生成代码

运行:

```
./bin/runscala.sh samples/server-generator/node/NodeServerFromSpec.scala http://localhost:8002/api-docs
```

生成的代码默认位于目录 `samples/server-generator/node/output` 下

执行 `npm install` 安装依赖包, 并运行API服务器

```
cd `samples/server-generator/node/output`
npm install
node app/main.js
```