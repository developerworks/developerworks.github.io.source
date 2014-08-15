angular.module('gettext').run(['gettextCatalog', function (gettextCatalog) {
    /* jshint -W100 */
    gettextCatalog.setStrings('zh_Hans_CN', {"Introduction": "简介", "Our application is a product used by many global companies and thus we support multiple languages like English, French, Spanish, German and more.": "我们的应用程序是一个被全球的公司使用的产品,并且需要支持比如英语,法语,西班牙语,德语等多国语言."});
    /* jshint +W100 */
}]);