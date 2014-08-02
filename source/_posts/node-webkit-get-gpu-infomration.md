title: node-webkit 提取GPU信息
tags:
- node-webkit
categories:
- node.js
date: 2014-06-16
---


Chromium 在`chrome://gpu`页面提供GPU相关的诊断信息,但在`node-webkit`中这个页面是坏的. 但是可以通过
`devtools`获取.

<!-- more -->

- 在`node-webkit`中打开`chrome://gpu`
- 打开`devtools`
- 进入`Console`标签,执行:

```javascript
var browserBridge = {
    onGpuInfoUpdate:function(arg){
        console.log(JSON.stringify(arg,null,1));
    }
};
```

- 然后执行获取描述GPU信息的JSON对象

```javascript
chrome.send('browserBridgeInitialized');
```

