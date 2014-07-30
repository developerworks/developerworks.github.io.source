title: Web性能测试
date: 2014-05-02
---


本文说明如何使用Google的PageSpeed Insights服务,node.js的PSI模块,以及自动build工具Grunt,Gulp把Web性能测试融入到项目构建过程中, 并生成性能分析报告.

## 安装

```shell
npm install --save psi
```

## 使用

```
var psi = require('psi');
psi({
    // key: '...', optional
    url: 'http://html5rocks.com',
    paths: '',           // optional
    locale: 'en_GB',     // optional
    strategy: 'mobile',  // optional
    threshold: 80        // optional
});
```

<!-- more -->


添加响应回调函数

```
psi(options, function(err, data){
    console.log(data.score);
    console.log(data.pageStats);
});
```

## 选项

url
:   Type: String
:   URL of the page for which the PageSpeed Insights API should generate results.

locale
:   Type: String Default: en_US
:   The locale that results should be generated in (e.g 'en_GB').

strategy
:   Type: String Default: desktop
:   The strategy to use when analyzing the page. Valid values are desktop and mobile.

threshold
:   Type: Number Default: 70
:   Threshold score that is needed to pass the pagespeed test

paths
:   Type: Array
:   An array of URL paths that are appended to the URL

key
:   Type: String Default: nokey
:   [Google API Key][1]
:   Unless Specified defaults to use the free tier on PageSpeed Insights. Good for getting a feel for how well this tool works for you.


## 命令行

You will probably want to globally install psi if using as a CLI. This can be done as follows:

```
npm install -g psi
```

You can then casually use it with your key:

```
$ psi http://www.google.com --key 'YOUR_KEY_GOES_HERE'
```

Similar to gpagespeed, the following optional flags are also supported:

```
$ psi <url> --key=<key> --prettyprint=<true> --userIp=<userIp> --locale=<locale> --strategy=<desktop|mobile>
```

```
$ psi http://www.html5rocks.com --strategy=mobile
```

## 把PSI融入到构建过程


一个使用 PSI 的例子项目: [例子项目][2]
Google PageSpeed Insights [Grunt 插件][3]


  [1]: https://code.google.com/apis/console/
  [2]: https://github.com/addyosmani/psi-gulp-sample
  [3]: https://github.com/jrcryer/grunt-pagespeed