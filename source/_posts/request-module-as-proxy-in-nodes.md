title: 使用request模块做跨域代理
date: 2014-06-18
categories:
- node.js
---

有时候跨域请求RESTFUL API做测试目的,使用`cors`模块又比较麻烦,下面的几行代码使用`request`模块作为Express的中间件实现Web代理的功能,简单,小巧.

<!-- more -->

```
var request = require('request');
var http = require('http');
var app = require('express');
app.use('/', function (req, res) {
    url = 'http://www.google.com' + req.url;
    console.log(url);
    req.pipe(request(url)).pipe(res);
});
http.createServer(app).listen(80, function () {
    console.log('Express server listening on port ' + 80);
});
```