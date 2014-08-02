title: DevTools Terminal,把终端功能带入到Chrome浏览器
tags:
  - devtools
  - devtools-terminal
categories:
  - chrome
date: 2014-08-02
---

[DevTools Terminal][1] 是一个Chrome DevTools扩展, 它把操作系统终端功能带入到Chrome浏览器中. 你可以在Chrome DevTools开发者工具中使用操作系统终端来完成需要执行的shell命令和操作.

<!-- more -->

安装后出现一个警告界面

![安装后出现一个警告界面!][2]

提示说,Google放弃了对 `NPAPI` 的支持, 因此`DevTools Terminal`转而支持`Native Messaging API`, 需要手动安装 `node.js代理`

## 安装代理app

```
$ sudo npm install -g devtools-terminal
npm http GET https://registry.npmjs.org/devtools-terminal
npm http 304 https://registry.npmjs.org/devtools-terminal
...
...
...
npm http 200 https://github.com/component/global/archive/v2.0.1.tar.gz
/usr/local/bin/devtools-terminal -> /usr/local/lib/node_modules/devtools-terminal/bin/devtools-terminal
devtools-terminal@0.1.2 /usr/local/lib/node_modules/devtools-terminal
├── mkdirp@0.5.0 (minimist@0.0.8)
├── optimist@0.6.1 (wordwrap@0.0.2, minimist@0.0.10)
├── pty.js@0.2.4 (extend@1.2.1, nan@0.7.0)
└── socket.io@1.0.6 (debug@0.7.4, engine.io@1.3.1, has-binary-data@0.1.1, socket.io-parser@2.2.0, socket.io-adapter@0.2.0, socket.io-client@1.0.6)
```

## 告诉Chrome浏览器在哪里去找到它

```
$ sudo devtools-terminal --install --id=leakmhneaibbdapdoienlkifomjceknl
Chrome Native Messaging host application installed successfully
```

## 快捷键

- Mac OS X: <kbd>Option</kbd> + <kbd>Command</kbd> + <kbd>I</kbd>
- Windows : <kbd>Ctrl</kbd> + <kbd>Shift</kbd> + <kbd>I</kbd> 或 <kbd>F12</kbd>

## 显示效果

打开 Chrome DevTools 后点击 `Terminal`标签, 效果如下:

![Chrome DevTools Terminal 截图][3]

> Now you can use the chrome termial with same as Operating system terminal. This sentence is input in the Chrome terminal , it cound not be swtich the input method to chinese, only could be use en input method. -_-!

  [1]: https://github.com/petethepig/devtools-terminal
  [2]: /images/chrome-devtools-terminal-annouoncement.png
  [3]: /images/devtools-terminal.png
