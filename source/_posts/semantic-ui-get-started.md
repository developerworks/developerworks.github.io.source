title: Semantic UI 入门
categories:
  - Web
tags:
  - Semantic UI
toc: false
date: 2014-12-20 22:33:00
---

![Semantic UI](/assets/semantic-ui/logo.png)

## 基本用法

在`<head>`内引入`dist/semantic.min.css`和`/dist/semantic.min.js`两个文件

```
<link rel="stylesheet" type="text/css" href="/dist/semantic.min.css">
<script src="/dist/semantic.min.js"></script>
```

也可以使用单独的某个组件

```
<link rel="stylesheet" type="text/css" href="/dist/components/icon.css">
```

## 推荐用法

![gulp install](/assets/semantic-ui/gulp-install.gif)

```
git clone https://github.com/Semantic-Org/Semantic-UI.git
cd Semantic-UI
npm install
gulp
```

运行 `gulp` 进入交互式设置, 会问你一些问题, 仔细看, 仔细答.

```
gulp            // 安装后监视修改
gulp build      // 从源代码构建所有文件
gulp clean      // 清除dist目录
gulp watch      // 监视文件
gulp install    // 重新运行安装
gulp help       // 列举所有命令
```

