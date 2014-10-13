title: XMPP Strophe.js插件 - 带内注册
categories:
  - Communication
tags:
  - javascript
  - strophejs
toc: false
date: 2014-10-03 18:12:35
---

## 客户端服务器交互

下图,是注册过程客户端和服务器的交互过程, 绿色的输出是客户端请求的XML节, 蓝色的输出是服务器应答的XML节.

![XMPP Stanzas][1]

## 下载插件

https://github.com/strophe/strophejs-plugins/tree/master/register

## HTML页面代码

创建两个表单元素`username`和`password`用于获取用户名和密码, 一个`Register`按钮触发注册操作.
本文所演示示例的完整代码在此: https://gist.github.com/developerworks/317ccf6eb2d3060610f8
```
<!doctype html>
<html lang="en">
<head>
  <meta charset="UTF-8">
  <title>Register a user</title>
  <!--// JQuery库-->
  <script type="text/javascript" src="jquery.min.js"></script>
  <!--// Strophe.js库-->
  <script type="text/javascript" src="strophe.js"></script>
  <!--// Strophe.js 注册插件-->
  <script type="text/javascript" src="strophe.register.js"></script>
  <!--// 注册业务逻辑-->
  <script type="text/javascript" src="register.js"></script>
</head>
<body>
  Username:<input type="text" id="username" placeholder="Please input username"/>
  Password<input type="text" id="password" placeholder="Please input your password"/>
  <br/>
  <button id="register">Register</button>
</form>
</body>
</html>
```

## 回调函数

**register.js**

```
// 调试用,XML格式化函数
function formatXml(xml) {
  var formatted = '';
  var reg = /(>)(<)(\/*)/g;
  xml = xml.replace(reg, '$1\r\n$2$3');
  var pad = 0;
  jQuery.each(xml.split('\r\n'), function (index, node) {
    var indent = 0;
    if (node.match(/.+<\/\w[^>]*>$/)) {
      indent = 0;
    } else if (node.match(/^<\/\w/)) {
      if (pad != 0) {
        pad -= 1;
      }
    } else if (node.match(/^<\w[^>]*[^\/]>.*$/)) {
      indent = 1;
    } else {
      indent = 0;
    }
    var padding = '';
    for (var i = 0; i < pad; i++) {
      padding += '  ';
    }
    formatted += padding + node + '\r\n';
    pad += indent;
  });
  return formatted;
}
// Websocket端点
var BOSH_SERVICE = 'ws://xmpp.myserver.info:5288';
// 域名
var DOMAIN_NAME = 'xmpp.myserver.info';
// XMPP连接对象
var connection = null;
// 浏览器控制台日志
function log(msg, sent) {
  if (sent) {
    console.log("%c\n" + msg, "color:green;");
  } else {
    console.log("%c\n" + msg, "color:blue;");
  }
}
function rawInput(data) {
  log(formatXml(data), false);
}
function rawOutput(data) {
  log(formatXml(data), true);
}
$(document).ready(function () {
  $('#register').bind('click', function () {
    // 创建连接
    connection = new Strophe.Connection(BOSH_SERVICE);
    // 注册事件处理器
    connection.rawInput = rawInput;
    connection.rawOutput = rawOutput;
    // 用户注册
    connection.register.connect(DOMAIN_NAME, function (status) {
      if (status === Strophe.Status.REGISTER) {
        connection.register.fields.username = $("#username").val();
        connection.register.fields.password = $("#password").val();
        console.info("registering...");
        connection.register.submit();
      }
      else if (status === Strophe.Status.REGISTERED) {
        console.info("Registerd!");
        connection.disconnect();
      }
      else if (status === Strophe.Status.CONFLICT) {
        console.error("Username already exists.")
      }
      else if (status === Strophe.Status.NOTACCEPTABLE) {
        console.error("Registration form not properly filled out.")
      }
      else if (status === Strophe.Status.REGIFAIL) {
        console.log("The Server does not support In-Band Registration")
      }
      else if (status === Strophe.Status.CONNECTED) {
        console.info("Connected.")
      }
      else if (status === Strophe.Status.DISCONNECTING) {
        console.log("Disconneting...");
      }
      else if (status === Strophe.Status.DISCONNECTED) {
        console.log("Disconneted.")
      }
      else {
      }
    }, 60, 1);
  });
});
```


  [1]: /assets/images/D6DC8BC6-6F30-49CA-B898-F47E9AD83A43.png

