title: 把Hubspot messaging 库作为Angular服务使用
date: 2014-08-15
categories:
  - Javascript
tags:
  - notification
  - angular

---


## 第一步

在`HTML`文件中引入依赖库

```
<link rel="stylesheet" href="./components/messenger/build/css/messenger.css"/>
<link rel="stylesheet" href="./components/messenger/build/css/messenger-theme-air.css"/>
<link rel="stylesheet" href="./components/messenger/build/css/messenger-theme-flat.css"/>
<link rel="stylesheet" href="./components/messenger/build/css/messenger-theme-future.css"/>
<script type="text/javascript" src="./components/messenger/build/js/messenger.min.js"></script>
<script type="text/javascript" src="./components/messenger/build/js/messenger-theme-future.js"></script>
<script type="text/javascript" src="./components/messenger/build/js/messenger-theme-flat.js"></script>
```

<!-- more -->

## 第二步

在`services`模块中定义和配置

```
/**
 * 通知功能
 * http://github.hubspot.com/messenger/docs/welcome
 */
angular.module('services').service('Notification', ['config_ui_notification', function (config_ui_notification) {
    // Messenger 内置支持3中样式, info, error, success, 可以增加自定义的样式
    // 但是需要在对应的皮肤CSS样式表中增加响应的CSS规则.
    Messenger.options = {
        extraClasses: config_ui_notification.location.top_center,
        // flat, future, block, air, ice
        theme: config_ui_notification.theme
    };
    // 信息
    this.info = function (text) {
        Message().post({message: text, type: 'info'});
    };
    // 错误
    this.error = function (msg) {
        Message().post({message: msg, type: 'error'});
    };
    // 一般消息
    this.post = function (msg) {
        Messenger().post(msg);
    };
}]);
/**
 * 配置通知
 */
angular.module('services').value('config_ui_notification', {
    // 位置, 设置通知条固定在什么位置显示
    location: {
        top_left: 'messenger-fixed messenger-on-top messenger-on-left',
        top_center: 'messenger-fixed messenger-on-top',
        top_right: 'messenger-fixed messenger-on-top message-on-right',
        bottom_left: "messenger-fixed messenger-on-bottom messenger-on-left",
        bottom_center: 'messenger-fixed messenger-on-bottom',
        bottom_right: 'messenger-fixed messenger-on-bottom messenger-on-right'
    },
    // 通知条皮肤,支持 flat, future, block, air, ice 五种官方提供的皮肤
    theme: 'air'
});
```

## 第三步

在控制器中使用

```
angular.module('controllers').controller('SoftwareVersionUploadController', [
    '$scope',
    'Notification',
    function ($scope, Notification) {
        Notification.info('已保存');
        Notification.error('网络已端口');
        Notification.sucess('已创建');
    }]);
```

