title: Angular 和 Polymer 数据绑定
date: 2014-06-11
categories:
- javascript
tags:
- angularjs
- ploymer
---

原文: http://blog.sethladd.com/2014/02/angular-and-polymer-data-binding.html

<img height="454" src="/assets/posts/2014-06-11-angular-and-polymer-data-binding/sitting-in-a-tree.png" width="640">

呵呵! Angular和Polymer在一起荡秋千.

## Angular 和Polymer

[Angular][1] 是一个非常强大的MVVM框架, [Polymer][2] 是一个基于Web组件的对自定义元素的增强库,如何让他们和谐地共存于同一个应用. 本文将介绍通过数据绑定如何连接Angular组件和Polymer元素.

<!-- more -->

<img height="381" src="/assets/posts/2014-06-11-angular-and-polymer-data-binding/relationship.png" width="640">

这幅图也说明了Angular和Polymer的交集 - **数据绑定**

**Angular提供的功能:**

- 指令
- 依赖注入
- 测试支持
- 路由
- 服务
- 服务器通信

**Polymer提供的功能:**

- <polyer-element> 创建自定义元素
- Widgets
- 抹平(Ployfills)浏览器特性
- 封装DOM
- 基于Web标准

两个库之间有一些相同的特性. 两个库都实现了数据绑定

## 互操作

Angular 和Polymer 的互操作分为两个层次

1. 在DOM层次上的互操作
2. 在数据绑定层次上的互操作

### DOM互操作

通过DOM在自定义元素(Polymer)和Angular间进行互操作

下面是一个示例, 在Angular应用程序中包含一个Polymer自定义元素.

<img border="0" src="/assets/posts/2014-06-11-angular-and-polymer-data-binding/Dart_Editor.png" height="377" width="640">

在上图中, Angular管理`cool`变量的数据绑定. 当变量`cool`的值改变时, 所有占位符`{{cool}}`的值都会变化, 即时是绑定的自定义元素的属性.

`my-element` 仅仅是一个封装了输入字段的Polymer自定义元素,代码如下:

```
<polymer-element name="my-element">
  <template>
    <p>
      <input type="text" value="{{message}}"> Polymer binding: {{message}}
    </p>
  </template>
  <script type="application/dart" src="my_element.dart"></script>
</polymer-element>
```

Angular代码能够设置`<my-element>`元素上的属性,监听来自元素`<my-element>`的事件,即使是在`<my-element>`中插入子元素.因此Angular能向操作其他DOM元素一样操作Polymer的自定义元素

但是,有一个问题,在下面的GIF图片中,你能看得见是什么问题么?

<img border="0" src="/assets/posts/2014-06-11-angular-and-polymer-data-binding/before.gif" height="297" width="400">

在Polymer中, 任何对变量`cool`的修改不会被传播到外层DOM或Angular. 我们需要一个双向的数据绑定: Angular->Polymer方向,以及Polymer->Angular

### 数据绑定互操作

Polymer通过[Node.bind()][3]管理数据绑定. `Node.bind()` is a "new method added to all DOM nodes which instructs them to bind the named property to the data provided". Angular does not use Node.bind() out of the box, so Angular is unable to listen for changes that are initiated from Polymer.

[Justin Fagnani][4], an engineer on the Dart team, released [angular_node_bind][5], a Dart package that bridges the Angular and Polymer worlds. It is an Angular module that can listen for Node.bind() changes and propagate the changes into Angular.

Justin explains why this package can help:

- Node.bind() takes care of setting up the binding, including two-way bindings, eliminating the need for directives for every property for two-way bindings.
- Custom elements that expose properties will be properly bound, again including two-way bindings. You can use the growing collection of custom element libraries in your Angular app.

To use angular_node_bind, add the following to your app's pubspec.yaml file:

```
name: angularpolymer
description: A demo of Angular and Polymer data binding.
dependencies:
  angular: any
  angular_node_bind: any
  browser: any
  polymer: any
transformers:
- polymer:
    entry_points: web/angularpolymer.html
```

注册`NodeBindModule`


```
import 'package:polymer/polymer.dart' show initPolymer;
import 'package:angular/angular.dart' show ngBootstrap;
import 'package:angular_node_bind/angular_node_bind.dart' show NodeBindModule;
void main() {
  initPolymer().run(() {
    ngBootstrap(module: new NodeBindModule());
  });
}
```

最后改变用于绑定Polyment元素的语法,从`{ {...} }`改为`[[...]]`

```
<my-element message="[[cool]]"></my-element>
```

注意绑定表达式是`[[cool]]`而不是`{{cool}}`. `angular_node_bind` 模块查找的是`[[...]]`,并连接`Node.bind()`.

<img border="0" src="/assets/posts/2014-06-11-angular-and-polymer-data-binding/after.gif" height="275" width="400">

需要注意的是:
在Angular中所作的变更如何反映在Polymer元素中,以及在Polymer中所作的修改如何反映到Angular代码.

## 摘要

至少有两种方式可以在Angular应用程序中使用Polymer自定义元素. Polymer元素就像一般的DOM元素一样,有属性,会产生事件,并且能包含子元素.使用`angular_node_bind`包连接Polymer的数据绑定到Angular的数据绑定,实现真正双向的数据共享.

## 获取源代码

本文的示例代码可以在[这里][6]获取


  [1]: http://angulardart.org/
  [2]: http://polymer-project.org/
  [3]: http://www.polymer-project.org/platform/node_bind.html
  [4]: https://plus.google.com/+JustinFagnani
  [5]: http://pub.dartlang.org/packages/angular_node_bind
  [6]: https://github.com/sethladd/dart-angular-polymer-data-binding