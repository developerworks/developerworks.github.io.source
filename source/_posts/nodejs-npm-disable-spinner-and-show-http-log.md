title: Nodejs | 在新版本的npm恢复老版本的下载状态风格
categories:
  - Node.js
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

## 参考资料

1. https://github.com/npm/npm/issues/5340