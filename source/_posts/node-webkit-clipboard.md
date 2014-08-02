title: node-webkit 剪切板
tags:
- node-webkit
categories:
- node.js
date: 2014-06-16
---


Clipboard API 需要 node-webkit `>= v0.3.0` 的支持

> `Clipboard` 是一个剪切板(Windows,GTK)和粘贴板(Mac)的抽象,当前它仅支持在系统剪切板中读写纯文本.

<!-- more -->

## 简介

```
// 加载原生UI库
// Load native UI library
var gui = require('nw.gui');

// 不能创建剪切板,只能从系统获取剪切板
// We can not create a clipboard, we have to receive the system clipboard
var clipboard = gui.Clipboard.get();

// 从剪切板读取
// Read from clipboard
var text = clipboard.get('text');
console.log(text);

// 写入剪切板
// Or write something
clipboard.set('I love node-webkit :)', 'text');

// 清空剪切板的内容
// And clear it!
clipboard.clear();
```

## 参考

**get()**

返回系统剪切板
不可能创建新的剪切板,只能从操作系统获取. 注意在X11中选择剪切板不支持.

**Clipboard.set(data, [type])**
:   写入`data`到剪切板.`type`指定了数据的mine类型.当前仅支持`text`(纯文本数据)

**Clipboard.get([typ])**
:   从剪切板获取`type`类型的数据.当前仅支持`text`(纯文本数据)

**Clipboard.clear()**
:   清空剪切板

