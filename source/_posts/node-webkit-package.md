title: 如何打包和分发node-webkit桌面应用程序
tags:
- node-webkit
categories:
- node.js
date: 2014-06-16
---


## 准备额外的文件

下面的子目录可以放到应用程序的根目录:

- `node_modules` 任何你想要同应用程序一起打包的Node.js模块都可以放在这里
- `plugins` NPAPI插件文件

不需要打包 `nwsnapshot` 文件

**警告**:
do not assume your `node_modules` that target one platform work as is in all platforms. For instance `node-email-templates` has specific Windows & Mac os x `npm install` commands. Besides, it requires python to install properly, which is not installed by default on Windows.

As a rule of thumb npm install your package.json on each platform you target to ensure everything works as expected.

<!-- more -->

## 第一步: 制作软件包

我们的打包系统类似于 [LÖVE][1],下面的指南是基于其 [Wiki][2]修改的.

应用程序包是一个具有`.nw`文件名扩展的zip归档文件. 三个警告:

- 必须有一个`package.json`文件来描述这个软件包, 查看[清单文件格式][3]
- `package.json`文件必须在zip归档的目录结构顶层
- 在`nw`文件中,文件和目录路径名称是区分大小写的.Windows系统不区分大小写,Mac OS X默认情况下文件系统路径也是**不区分**大小写的


  [1]: https://love2d.org/
  [2]: https://love2d.org/wiki/Game_Distribution
  [3]: https://github.com/rogerwang/node-webkit/wiki/Manifest-format



