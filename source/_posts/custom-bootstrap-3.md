title: 定制Bootstrap3
tags:
  - bootstrap3
categories:
  - Web
date: 2014-08-02
---

以前开发项目都是找现成的模板, 经历多个项目后, 总是达不到理想的效果. 为了完全能够定制Web页面的外观,又不会把Bootstrap本身的文件改的面目全非,因此需要创建自定义文件结构

## 安装前端工具包

```
sudo npm install -g bower
sudo npm install -g lessc
```

本文所展示的示例是在phpstorm下完成的, 要自动编译less文件,需要用到`lessc`编译器, 打开less文件时,phpstrom会自动提示安装less插件,按提示安装即可,需要重启phpstrom.

<!--more-->

## 创建目录

创建一个新目录 `my-bootstrap`,并初始化为一个npm模块,按照提示输入相应的模块信息

```
mkdir my-bootstrap
cd my-bootstrap
npm init
```

## 安装 bootstrap 源码

有两种方式安装bootstrap源代码, 本文使用npm安装方式,和通过git方式安装路径有区别,使用时注意

```
# 通过bower安装
bower install bootstrap
# 用git安装
git clone https://github.com/twbs/bootstrap.git
```

创建一个自定义目录`custom`用于放置自定义的less文件

```
mkdir custom
cd custom
```

- 首先,在`custom`目录中创建一个自定义主的文件`my-bootstrap.less`. 此文件用于包含原始的`bootstrap.less`文件, 以及`自定义的`less模块文件.
- 其次,在`my-boostrap.less`文件中包含`bootstrap.less`文件

举例, 如果我们自定义导航栏的外观,那么首先把`navs.less`文件从
`node_modules/bootstrap/less/navs.less`复制到`costom/navs.less`

然后在`my-bootstrap.less`文件中导入`custom/navs.less`, 内容如下

```
// filename: my-bootstrap.less
@import "../bower_components/bootstrap/less/bootstrap";
@import "navs";
```

再修改`custom/navs.less`文件,定制自己需要的外观


最后在html页面中导入


![在HTML页面中导入自定义的Bootstrap CSS文件][1]

目录结构如下

![自定义Bootstrap目录结构][2]


如果要修改其他Bootstrap部件的样式和外观,复制相应的`bower_components/bootstrap/less/*.less`到`custom/`目录下, 然后在`my-bootstrap.less`文件中导入,最后修改


## 注意

`bootstrap`中有一个非常重要的文件`variables.less`, 其中定义了很多被其他文件引用的变量, 建议复制到自定义目录`custom`下, 修改其中的变量满足自定义需要.

  [1]: /images/custom-bootstrap/my-bootstrap.png
  [2]: /images/custom-bootstrap/directory-stucture.png


