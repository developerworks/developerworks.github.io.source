title: 在Angular项目中提供国际化支持
categories:
  - node.js
tags:
  - npm
date: 2014-08-16
---

文本国际化支持在angular社区中又很多开源的模块, 我筛选出一个比较好用的模块作为项目提供国际化支持

下面介绍如何使用`angular-gettext`为你的项目提供国际化支持

## 安装模块

```
bower install angular-gettext --save
```

<!-- more -->

## 引入需要的库

```
<script src="/components/angular/angular.js"></script>
<script src="/components/angular-gettext/dist/angular-gettext.min.js"></script>
<script src="translations.js"></script>
```
## angular-gettext 初始化代码

```
<script type="text/javascript">
    angular.module('GettextApp', ['gettext'])
        .controller('GettextController', [
            '$scope',
            'gettextCatalog',
            function ($scope, gettextCatalog) {
                $scope.languages = ['en', 'zh_Hans_CN'];
                $scope.language = 'zh_Hans_CN';
                $scope.$watch('language', function () {
                    console.log($scope.language);
                    gettextCatalog.setCurrentLanguage($scope.language);
                });
        }])
        .run(['gettextCatalog', function (gettextCatalog) {
            gettextCatalog.setCurrentLanguage('zh_Hans_CN');
            gettextCatalog.debug = true;
        }]);
</script>
```

## 编写HTML页面

```
<body ng-app="GettextApp">
<!--页面主容器-->
<div class="container" ng-controller="GettextController" style="padding: 5px;">
    <!--行-->
    <div class="row">
        <!--列-->
        <div class="col-md-12">
            <!--页头-->
            <div class="page-header">
                <!--面板-->
                <div class="panel panel-success disable-select">
                    <!--面板头-->
                    <div class="panel-heading">Angular.js Gettext 组件</div>
                    <!--面板主体-->
                    <div class="panel-body">
                        <select ng-model="language" ng-options="lang for lang in languages"></select>
                        <div class="entry-content">
                            <h1 translate>Introduction</h1>
                            <p translate>Our application is a product used by many global companies and thus we support multiple languages like English, French, Spanish, German and more.</p>
                        </div>
                    </div>
                </div>
            </div>
        </div>
    </div>
    <div id="footer">
        <div class="container">
            <p class="text-muted">Place sticky footer content here.</p>
        </div>
    </div>
</div>
</body>
```

## 完整的示例代码

下面的链接提供了完整的示例代码,使用的时候可以作为参考

[完整的示例代码][1]


  [1]: /assets/code/gettext/gettext.html