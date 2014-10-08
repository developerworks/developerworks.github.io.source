title: 检测离线应用的网络状态
categories:
  - Javascript
tags:
  - html5
  - angular
date: 2014-08-16 11:43:00
---



## 注意事项

需要知道用户什么时候重新联网, 然后与服务器进行重新同步
需要知道用户什么时候网络端口, 把服务端的请求放入等待队列, 等恢复联网后重新执行.

<!-- more -->

## 在运行阶段注册offline和online时间处理器


```
// 在$rootScope上设置网络状态, 参考stackoverflow
// http://stackoverflow.com/questions/16242389/how-to-check-internet-connection-in-angularjs
// 某些需要联网的操作,可以先判断$rootScope.online的状态
$rootScope.online = navigator.onLine;
$window.addEventListener("offline", function () {
    Notification.info('网络已断开');
    // 在控制器中可以用
    // $scope.$watch('online', function(newStatus) { ... });
    // 获取网络的状态变化
    $rootScope.$apply(function () {
        $rootScope.online = false;
    });
}, false);
$window.addEventListener("online", function () {
    Notification.info('网络已恢复连接');
    $rootScope.$apply(function () {
        $rootScope.online = true;
    });
}, false);
```

效果如下:

![离线通知效果][1]

  [1]: /assets/images/97B94168-7059-40B1-900C-05558E720106.png



