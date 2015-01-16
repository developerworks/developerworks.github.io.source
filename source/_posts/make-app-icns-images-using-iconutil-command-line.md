title: 使用iconutil命令行工具制作App图标
categories:
  - Tools
tags:
  - iconutil
toc: false
date: 2015-01-15 22:09:38
---

iconutil是一个命令行工具, 用于在`.iconset`文件(实际上是一个图标集目录)和`.icns`文件之间转换.

用到的图片可以从这里下载:

https://github.com/andrevvm/appicns

比如下面的命令, 把一个`1024x1024`的`netbook.icns`文件

    iconutil -c iconset -o output.iconset netbook.icns

这个命令很简单,可以通过manpage查看:

    man iconutil

语法:

    iconutil -c {icns | iconset} [-o file] file

## 生成的图标集效果

![图标集](/assets/images/9CA1D34F-1A00-47B3-A65A-EBB0636CF617.png)