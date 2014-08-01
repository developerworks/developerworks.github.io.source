title: Angularjs 预载指示器
date: 2014-06-12
categories:
- javascript
tags:
- angularjs
---

## ngActivityIndicator

简单[Angular.js][2]应用程序的预载动画`provider`

ngActivityIndicator 是一个小巧的,一体化方案,可定制皮肤,仅依赖 Angular.js

![https://camo.githubusercontent.com/30660bc71d4b27d77c1c4c786ae50be35acc7129/68747470733a2f2f646c2e64726f70626f7875736572636f6e74656e742e636f6d2f752f3130303436333031312f6e674163746976697479496e64696361746f722d736d616c6c2e676966][3]


## 安装

可以手动下载需要的ngActivityIndicator文件,或者通过`bower`前端包管理器来安装:

```shell
bower install ngActivityIndicator
```

<!-- more -->

## 使用

需要在项目中包含 `ngActivityIndicator.js`和`ngActivityIndicator.css`文件(最小要求),然后在你的指令,控制器或服务中使用`#activityIndicator`, 例如在控制器中:

```javascript
var app = angular.module('exampleApp', ['ngActivityIndicator']);
app.controller('MainCtrl', ['$activityIndicator', '$timeout',
    function ($actvityIndicator, $timeout) {
        $actvityIndicator.startAnimating();
        $timeout(function () {
            $actvityIndicator.stopAnimating();
        }, 3000);
    }
]);
```

可以使用`ngActivityIndicator`指令来显示内置的动画, 也可以在HTML中创建自己的的预载器, 应用CSS来隐藏/显示:

```html
<body>
    <div ng-show="AILoading" class="your-preloader"></div>
    <div ng-hide="AILoading" ng-view></div>
</body>
```

Also this service can be easily used with loading SVG concept by Brett Jackson. But be aware of limited browser SVG support and user experience issues with perceptual loading time -

http://caniuse.com/svg-smil

还有该服务还可以用于加载SVG,

该服务可以加载SVG的概念由布雷特杰克逊容易使用。但要知道有限的浏览器支持SVG和感性加载时间的用户体验的问题

## API

API高度借鉴`Objective-C`的来自`UIKit`框架的`UIActivityIndicatorView`类.

- `startAnimating()`
    设置`$rootScope.AILoading`为`true`启动预载器动画.

- `stopAnimating(delay)`
    设置`$rootScope.AILoading`为`false`停止预载器动画.

- `isAnimating()`
    检查是否一个活动指示器有动画效果.

- `setActivityIndicatorStyle()`
    有多个内置的样式可以选择. 默认样式为`CircledGrey`, 可以通过如下设置来改变:

```javascript
angular.module('yourModule', ['ngActivityIndicator'])
    .config(['$activityIndicatorProvider', function ($activityIndicatorProvider) {
        $activityIndicatorProvider.setActivityIndicatorStyle('SpinnerDark');
    }]);
```


## 指令

Module is shiped with helpful directive. It is called as ngActivityIndicator and can behave differently based on the tag or restrict it's used with.

当预载器指令添加到`<body>`标签后, 预载器被添加到Angular.js应用,基于动画状态(`$rootScope.AILoading`)显示和隐藏`ng-view`.这是推荐和最常用的情况,比如:


```html
<!-- 指示器自动注入到DOM树中 -->
<body ng-activity-indicator>
    <!-- ngView 自动显示和隐藏 -->
    <div ng-view></div>
</body>
```

If you prefer to manage what to show on preloading mode by yourself, just use it as an element or attribute. Directive will handle animation status automaticly, example:

```html
<body>
    <!-- 自动演示和隐藏 -->
    <div ng-activity-indicator class="center"></div>
    <!-- 基于$rootScope.AILoading属性显示和隐藏元素 -->
    <div ng-hide="AILoading" class="some-wrapper"></div>
</body>
```

可以添加名称作为指令值来覆盖预载器的样式:

```html
<div ng-activity-indicator="CircledDark"></div>
```

## 内置样式

你可以在默认提供的样式中中选择适合你的, 如果你对默认提供的样式不满意,你也可以创建自定义风格的指示器.

样式名称列表:

```html
CircledWhite / CircledGrey (default) / CircledDark
DottedWhite / DottedGrey / DottedDark
SpinnerWhite / SpinnerGrey / SpinnerDark
```

## 如何贡献代码?

如果想要给该模块添加样式? fork仓库并执行`pull请求`即可!


  [1]: http://angularjs.org/
  [2]: http://angularjs.org/
  [3]: https://camo.githubusercontent.com/30660bc71d4b27d77c1c4c786ae50be35acc7129/68747470733a2f2f646c2e64726f70626f7875736572636f6e74656e742e636f6d2f752f3130303436333031312f6e674163746976697479496e64696361746f722d736d616c6c2e676966