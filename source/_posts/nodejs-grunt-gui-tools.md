title: Grunt 图形管理工具
tags:
- node.js
- grunt
categories:
- node.js
date: 2014-05-13
---

## 1. grunt-peon-gui

[https://www.npmjs.org/package/grunt-peon-gui][1]

`grunt-peon-gui`是一个grunt插件,基于Web,运行`grunt-peon-gui`的时候会自动启动一个监听在`8080`端口的`Node.js`Web服务器,并自动打开浏览器显示当前项

### 1.1 安装和使用

#### 1.1.1 安装
```
npm install grunt-peon-gui --save-dev

```
<img src="https://raw.github.com/voceconnect/grunt-peon-gui/master/app/assets/img/screen.png" width="750px" />

<!-- more -->

#### 1.1.2 配置

添加如下代码到`Gruntfile.js`

```
grunt.loadNpmTasks('grunt-peon-gui');
```

#### 1.1.3 运行

```
grunt gui
```



## 2. GRUNT DEVTOOLS

[https://github.com/vladikoff/grunt-devtools][2]

`GRUNT DEVTOOLS`是一个Chrome浏览器扩展

`Chrome Developer Tools`和`Adobe Brackets`的Grunt任务运行器扩展

Grunt插件和Chrome浏览器扩展的组合使开发者能够快速的运行Grunt任务.此扩展支持后台任务,比如运行`watch`任务和服务器任务. 还支持多项目,开发者可以在`Chrome developer tools`标签运行多个项目的Grunt任务.

![http://www.vladfilippov.com/blog/2013-03-16/intro.jpg][3]

### 2.1 安装Chrome Grunt扩展


- 从Chrome Web Store下载 [Grunt Devtools extension for Chrome Developer Tools][4]
- 安装grunt插件
    - `npm install -g grunt-devtools`
    - 在`Gruntfile.js`所在的项目目录运行`grunt-devtools`
- 打开`Chrome Dev tools`, 找到`Grunt`标签. 这样你就可以使用Grunt扩展来运行Grunt任务了

<img src="http://v14d.com/i/5133941ceb6b4.jpg" width="750px" />

### 2.1 安装和配置

- 在`Gruntfile.js`所在的项目目录执行`npm install grunt-devtools`安装`grunt-devtools`
- 添加 `grunt.loadNpmTasks('grunt-devtools')`到`Gruntfile.js`
- 运行`grunt devtools`
- 打开`Chrome Dev tools`, 找到`Grunt`标签. 这样你就可以使用Grunt扩展来运行Grunt任务了

<img src="http://v14d.com/i/513393cbb7e8b.jpg" width="750px" />



  [1]: https://www.npmjs.org/package/grunt-peon-gui
  [2]: https://github.com/vladikoff/grunt-devtools
  [3]: http://www.vladfilippov.com/blog/2013-03-16/intro.jpg
  [4]: https://chrome.google.com/webstore/detail/grunt-devtools/fbiodiodggnlakggeeckkjccjhhjndnb?hl=en
