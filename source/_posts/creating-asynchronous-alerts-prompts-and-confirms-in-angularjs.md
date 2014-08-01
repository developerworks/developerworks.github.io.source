title: 在AngularJS中创建异步Alert Prompts Confirms
date: 2014-06-26
categories:
- javascript
tags:
- angularjs
---

> 本文翻译自如下文章, 因为不是文学作品而是技术文章,因此不遵照`信达雅`的原则,让人容易理解即可, 没有逐句翻译原文的语言含义,而是根据其含义用更符合汉语的语言习惯来表达.

http://www.bennadel.com/blog/2632-creating-asynchronous-alerts-prompts-and-confirms-in-angularjs.htm

## 问题

有过在前端编程经验的人都知道,调用`alert()`, `prompt()`, 和`confirm()`会阻塞浏览器执行后续的Javascript代码,直到用户做出反应. 这个问题会让前端开发变得比较棘手.
比如,一个常见的问题是当`alert()`这类阻塞操作促发的时候,假设浏览器后台每隔N秒会向服务器请求更新数据,那么后台的Ajax更新也会被阻塞, 通常解决此类问题的方式转换到使用模态对话框.这种转换并不容易.因此促使原作者创建`alert()`,`prompt()`和`confirm()`的`promise`异步处理模式的非阻塞版本.

> 理解本文的关键是要理解`promise`异步处理模式, 关于什么是`promise`异步处理模式,这里不做解释,请自行Google/百度

本文的项目地址为: [http://bennadel.github.io/JavaScript-Demos/demos/creating-asynchronous-prompts-angularjs/][1]

<!-- more -->

## 办法

解决`alert()`,`prompt()`和`confirm()`阻塞浏览器的方法是结合了Angular.js中提供的`$q`服务,把`alert()`,`prompt()`和`confirm()`的阻塞操作异步化,避免阻塞后续的Javascript代码继续执行.

## 代码

```
<!doctype html>
<html ng-app="Demo" ng-controller="AppController">
<head>
    <meta charset="utf-8"/>
    <title>Creating Asynchronous Alerts, Prompts, And Confirms In AngularJS </title>
</head>
<body>
<h1>Creating Asynchronous Alerts, Prompts, And Confirms In AngularJS </h1>
<!-- Load scripts. -->
<script type="text/javascript" src="../../vendor/jquery/jquery-2.1.0.min.js"></script>
<script type="text/javascript" src="../../vendor/angularjs/angular-1.2.16.min.js"></script>
<script type="text/javascript">
    // Create an application module for our demo.
    var app = angular.module("Demo", []);
    // -------------------------------------------------- //
    // -------------------------------------------------- //
    // I control the root of the application.
    app.controller(
        "AppController",
        function ($scope, alert, prompt, confirm) {
            // Use new, injected alert.
            alert("Hecks to the yea!").then(
                function () {
                    console.log("Alert accomplished");
                }
            );
            // Use new, injected prompt.
            prompt("Are you beginning to see the possibilities?", "Yes").then(
                function (response) {
                    console.log("Prompt accomplished with", response);
                },
                function () {
                    console.log("Prompt failed :(");
                }
            );
            // Use new, injected confirm.
            confirm("Has it come to this?").then(
                function (response) {
                    console.log("Confirm accomplished with", response);
                },
                function () {
                    console.log("Confirm failed :(");
                }
            );
            // This last console.log is here to demonstrate that the asynchronous
            // alerts, prompts, and confirms will be rejected in a future tick of
            // the event loop and will invoke logging AFTER this line of code.
            console.log("Testing completed.");
        }
    );
    // -------------------------------------------------- //
    // -------------------------------------------------- //
    // I define an asynchronous wrapper to the native alert() method. It returns a
    // promise that will be resolved in a future tick of the event loop.
    // --
    // NOTE: This promise will never be "rejected" since there is no divergent
    // behavior available to the user with the alert() method.
    app.factory(
        "alert",
        function ($window, $q) {
            // Define promise-based alert() method.
            function alert(message) {
                var defer = $q.defer();
                $window.alert(message);
                defer.resolve();
                return( defer.promise );
            }
            return( alert );
        }
    );
    // I define an asynchronous wrapper to the native prompt() method. It returns a
    // promise that will be "resolved" if the user submits the prompt; or will be
    // "rejected" if the user cancels the prompt.
    app.factory(
        "prompt",
        function ($window, $q) {
            // Define promise-based prompt() method.
            function prompt(message, defaultValue) {
                var defer = $q.defer();
                // The native prompt will return null or a string.
                var response = $window.prompt(message, defaultValue);
                if (response === null) {
                    defer.reject();
                } else {
                    defer.resolve(response);
                }
                return( defer.promise );
            }
            return( prompt );
        }
    );
    // I define an asynchronous wrapper to the native confirm() method. It returns a
    // promise that will be "resolved" if the user agrees to the confirmation; or
    // will be "rejected" if the user cancels the confirmation.
    app.factory(
        "confirm",
        function ($window, $q) {
            // Define promise-based confirm() method.
            function confirm(message) {
                var defer = $q.defer();
                // The native confirm will return a boolean.
                if ($window.confirm(message)) {
                    defer.resolve(true);
                } else {
                    defer.reject(false);
                }
                return( defer.promise );
            }
            return( confirm );
        }
    );
</script>
</body>
</html>
</pre>
```

## 总结

凡是都要有个总结,利于后续的理解和举一反三,再次碰到此类问题的时候不至于无从下手.

某些操作是同步的,这种同步操作会阻塞整个程序的执行.遇到此类问题的时候,可以试着把这些同步操作转换位异步执行.

采用的工具就是`promise`, 在很多语言中都有实现, 在`node.js`中又`q`模块, 在`Angurlar.js`中提供了`$q`服务,需要的时候可以通过Angular提供的依赖注入(DI)把需要的模块和服务注入到控制器中.

类似磁盘IO, 网络IO等这些慢操作,就可以转换成异步处理模式.

## Changelog

- 2014-06-01 首次发表


  [1]: http://bennadel.github.io/JavaScript-Demos/demos/creating-asynchronous-prompts-angularjs/