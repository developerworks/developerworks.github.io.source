angular.module('gettext').run(['gettextCatalog', function (gettextCatalog) {
    gettextCatalog.setStrings('zh_Hans_CN', {
        "Introduction": "简介",
        "Our application.": "我们的应用程序."
    });
}]);