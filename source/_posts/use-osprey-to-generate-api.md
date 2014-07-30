title: 使用Osprey生成基于RAML规范的API接口
date: 2014-07-30 22:27:51
categories:
- API
tags:
- API
- RMAL
---

> ## Osprey指南

`Osprey`是一个JavaScript框架,基于[Node][1]和[Express][2], 与其伴生的CLI项目一起,采用API优先的方法用于快速构建通过[RAML][3](RESTFful API建模语言)描述的应用程序API. RAML API定义了应用程序和它的使用者之间的契约,Osprey以及CLI项目一起来执行和实现这个契约.

<!-- more -->

## Important

The current release of Osprey is very much a work in progress. As it is in active use within a number of rapid development projects, it too is evolving with the needs those projects uncover. While it is proving extremely beneficial, because it's still evolving rapidly we don't yet feel it meets our criteria for a first fully stable release.

We encourage you to use it and participate by raising issues, providing feedback or contributing your own code (see below)

## Coming Soon

Please check the [Osprey 1.0 里程碑][4] issues list to stay up-to-date with the immediate roadmap.

> ## 基础

主要功能包括:

- 自动验证:
    - 表单,URI,和查询参数
    - 请求/响应头
    - JSON和XML模式
- 默认参数
- 异常处理
- 为应用程序API自动生成mocks,只要在RAML文件中定义样本响应
- [API Console][5]: 自动生成的文档生成在一个交互式的Web应用中,允许开发者非常容易的调用API

## 相关项目

[Osprey-CLI][6], 脚手架工具,只需要用一行命令就可以从RAML规范文件生成基于Osprey的应用程序.

## Contributing

## Prerequisites

要使用Osprey,需要满足下面的条件

- [Node JS][7]
- [NPM][8]

> # 入门

    npm install osprey

注: 在osprey安装过程中你可以忽略警告,大多数是其使用的库抛出的警告. 如果不能正确安装osprey就需要查看这些警告.

> ## 选项A(推荐)

1. 使用Osprey-CLI创建一个应用程序骨架. 先顶一个`output`目录
2. 生成的项目骨架目录结构:
```
[output]
    |--Gruntfile.js
    |--package.json
    |--src
    |  |--app.js
    |  |--assets
    |   |--raml
    |     |--api.raml
    |-test
```
3. 熟悉基本的结构
4. 注意`output/src/assets/raml`目录. 如果你指定一个现有的RAML文件,或者包含RAML定义的目录,这些文件将被复制到这里. 否则该目录下只有一个空的`api.raml`文件.
5. 还要注意`output/src/app.js`文件. 这是应用程序主文件.

> ## 用命令行生成一个`Osprey`项目

查看命令行参数

```
$ osprey new --help
usage: osprey new [-h] [-b] [-l] [-t] -n  [-v] [-q] [raml]

Positional arguments:
  raml              A RAML file path or the path to container folder

Optional arguments:
  -h, --help        Show this help message and exit.
  -b , --baseUri    Specify base URI for your API (指定API的根URI)
  -l , --language   Specify output programming language: javascript,
                    coffeescript (指定要使用的语言,默认为`javascript`)
  -t , --target     Specify output directory (指定输出目录)
  -n , --name       Specify application name (指定应用程序名称)
  -v, --verbose     Set the verbose level of output
  -q, --quiet       Silence commands
```

生成一个新项目

```
$ osprey new specs --name osprey-project -t osprey-project
Osprey: Runtime parameters
Osprey:   - baseUri: /api
Osprey:   - language: javascript
Osprey:   - target: osprey-project
Osprey:   - name: osprey-project
Osprey:   - raml: specs
Osprey:
```

使用`tree`命令查看目录结构

```
$ tree
.
├── osprey-project
│   ├── Gruntfile.js
│   ├── package.json
│   ├── src
│   │   ├── app.js
│   │   └── assets
│   │       └── raml
│   │           └── osprey-test.raml
│   └── test
└── specs
    └── osprey-test.raml
```

**现在一个基本的`Osprey`项目目录结构已经生成好了,要运行这个例子,还需要安装依赖的模块**

在`osprey-project`目录下运行`npm install`

```
npm http GET https://registry.npmjs.org/express
npm http GET https://registry.npmjs.org/grunt-contrib-watch
npm http GET https://registry.npmjs.org/grunt-contrib-copy
npm http GET https://registry.npmjs.org/grunt-contrib-clean
npm http GET https://registry.npmjs.org/grunt
npm http GET https://registry.npmjs.org/osprey
...
```

在`osprey-projct`目录下运行`grunt`

```
Running "watch" task
Waiting...OK


Running "jshint:all" (jshint) task
>> 1 file lint free.

Running "express:development" (express) task
Starting background Express server
connect.multipart() will be removed in connect 3.0
visit https://github.com/senchalabs/connect/wiki/Connect-3.0 for alternatives
connect.limit() will be removed in connect 3.0
Osprey: Osprey::APIConsole has been initialized successfully listening at /api/console

Running "watch" task
Completed in 0.281s at Fri May 30 2014 01:05:01 GMT+0800 (CST) - Waiting...
listening on port 3000
```

在命令行中输入


```
/usr/bin/open -a "/Applications/Google Chrome.app" http://localhost:3000/api/console
```

启动Chrome浏览器打开`API Console`, 出现如下错误输出:


```
GET /api/console 200 4ms - 605b
GET /api/console/styles/app.css 304 3ms
GET /api/console/scripts/vendor.js 304 2ms
GET /api/console/scripts/app.js 304 2ms

/Users/hezhiqiang/PhpstormProjects/osprey-test/osprey-project/node_modules/osprey/dist/osprey.js:78
            data = data.toString().replace(/^baseUri:.*$/gmi, "baseUri: " + ap
                        ^
TypeError: Cannot call method 'toString' of undefined
    at /Users/hezhiqiang/PhpstormProjects/osprey-test/osprey-project/node_modules/osprey/dist/osprey.js:78:25
    at fs.js:207:20
    at Object.oncomplete (fs.js:107:15)
```

```
alias chrome="open /Applications/Google\ Chrome.app"
```

  [1]: http://nodejs.org/
  [2]: http://expressjs.com/
  [3]: http://raml.org/
  [4]: https://github.com/mulesoft/osprey/issues?milestone=1&state=open
  [5]: https://github.com/mulesoft/api-console
  [6]: https://github.com/mulesoft/osprey-cli
  [7]: http://nodejs.org/
  [8]: https://npmjs.org/