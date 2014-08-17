title: AngularJS和Polymer双向绑定支持
categories:
  - polymer
tags:
  - angular
  - polymer
date: 2014-08-16 10:59:00
---

> 项目地址: https://github.com/GabiAxel/ng-polymer-elements


Web组件使`编写封装的视图片段`和`独立于任何框架的逻辑`成为可能. 目前支持核心元素(`core`)和`paper`元素.

例如, 下面的代码把`<paper-input>`元素的`inputValue`属性和`Angular`作用域的`myText`属性进行双向绑定

```
<paper-input ng-model="myText"></paper-input>
```

<!-- more -->

## 安装

`ng-polymer-elements` 可以通过 `bower` 安装:

```
bower install ng-polymer-elements
```

## 使用

添加脚本到HTML页面

```
<script type="text/javascript" src="ng-polymer-elements.js">
```

注意:

> ng-polymer-elements 要求在启动Angular应用之前加载完成. 如果使用自动启动形式, 比如 `<html ng-app="myModule">`,
> `ng-polymer-elements` 会自动处理. 如果是手动启动需要保证在 `polymer-ready` 事件之后执行.


```
window.addEventListener('polymer-ready', function() {
    angular.bootstrap(wrap(document), ['myModule']);
});
```

添加`ng-polymer-elements`模块作为angular应用程序依赖模块

```
angular.module('myModule', ['ng-polymer-elements']);
```

下面的组件通过`ng-model`支持简单值的双向绑定:

- [core-input][1]
- [paper-input][2]
- [paper-radio-group][3]
- [paper-tabs][4]       (绑定到被选择Tab的索引)
- [paper-checkbox][5]
- [paper-toggle-button][6]
- [core-overlay][7]     (绑定到已打开状态)
- [paper-dialog][8]     (绑定到已打开状态)
- [paper-toast][9]      (绑定到已打开状态)
- [paper-slider][10]


对于 [core-list][11], `ng-model`用于绑定列表数据, `ng-tap`用于绑定`tap`事件, 同时把事件`$event`暴露出来.

```
<core-list ng-model="arrayData" ng-tap="onTap($event)">
    <template>
        <div>{{text}}</div>
    </template>
</core-list>
$scope.arrayData = [{text: 'one'}, {text: 'two'}, {text: 'three'}];
$scope.onTap = function(event) {
    var tappedItem = event.details.data;
};
```

  [1]: http://www.polymer-project.org/docs/elements/core-elements.html#core-input
  [2]: http://www.polymer-project.org/docs/elements/paper-elements.html#paper-input
  [3]: http://www.polymer-project.org/docs/elements/paper-elements.html#paper-radio-group
  [4]: http://www.polymer-project.org/docs/elements/paper-elements.html#paper-tabs
  [5]: http://www.polymer-project.org/docs/elements/paper-elements.html#paper-checkbox
  [6]: http://www.polymer-project.org/docs/elements/paper-elements.html#paper-toggle-button
  [7]: http://www.polymer-project.org/docs/elements/core-elements.html#core-overlay
  [8]: http://www.polymer-project.org/docs/elements/paper-elements.html#paper-dialog
  [9]: http://www.polymer-project.org/docs/elements/paper-elements.html#paper-toast
  [10]: http://www.polymer-project.org/docs/elements/paper-elements.html#paper-slider
  [11]: http://www.polymer-project.org/docs/elements/core-elements.html#core-list

