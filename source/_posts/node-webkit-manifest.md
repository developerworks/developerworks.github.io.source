title: node-webkit Manifest 文件格式
date: 2014-06-16
---

Every app package should contain a manifest file named `package.json`, it will tell `node-webkit` how to open the app and control how the browser behaves.

<!-- more -->

## 入门

```
{
  "main": "index.html",
  "name": "nw-demo",
  "description": "demo app of node-webkit",
  "version": "0.1.0",
  "keywords": [ "demo", "node-webkit" ],
  "window": {
    "title": "node-webkit demo",
    "icon": "link.png",
    "toolbar": true,
    "frame": false,
    "width": 800,
    "height": 500,
    "position": "mouse",
    "min_width": 400,
    "min_height": 200,
    "max_width": 800,
    "max_height": 600
  },
  "webkit": {
    "plugin": true
  }
}
```

## 基本格式

`package.json` 应该以[JSON][1]格式编写.

## 要求的字段


每个包必须在它的包描述文件中提供如下所有字段:

**main**
:   (*string*) which page should be opened when `node-webkit` starts.

**name**
:   (*string*) the name of the package. This must be a unique, lowercase alpha-numeric name without spaces. It may include "." or "_" or "-" characters. It is otherwise opaque.
`name` should be globally unique since `node-webkit` will store app's data under the directory named `name`.

## 功能 参数

下面的字段控制`node-webkit`提供什么功能,以及`node-webkit`应该怎样打开主窗口.

**nodejs**
:   (*boolean*) set `nodejs` to false will disable Node support in WebKit.

**node-main**
:   (*string*) specify the path to a node.js script file, it will be executed on startup in Node context. This script will have same runtime environment with normal node.js binary. The script is ensured to run before the first DOM window load starting from v0.3.3. [See here][2] for more information. (*available after node-webkit v0.3.1*)

**single-instance**
:   (*boolean*) by default node-webkit only allows one instance of your app if your app is a standalone package (packaged with instructions of [How to package and distribute your apps][3]), if you want to allow multiple instances of your app running at the same time, specify this to `false`.

**window**
:   (*object*) controls how the main window looks, see *Window Subfields* below.

**webkit**
:   (*object*) controls what features of WebKit should be on/off, see *Webkit Subfields* below.

**user-agent**
:   *since v0.3.7*
:   (*string*) Override the `User-Agent` header in HTTP requests made from the application. The following placehoders are available:

    - `%name`: replaced by the `name` field in the manifest.
    - `%ver`: replaced by `version` field in the manifest.
    - `%nwver`: replaced by the version of node-webkit.
    - `%webkit_ver`: replaced by the version of WebKit engine.
    - `%osinfo`: replaced by the OS and CPU information you would see in browser's user agent string.

**node-remote**
:   *since v0.3.7*
:   (*string*) Enable calling Node in remote pages. The value controls for which sites this feature should be turned on. The format is the same with the "proxy bypass rules" of the browser:

:   It's a rule set, which rules are separated by `,` or `;`. Each rule can be any of the following:

```
// (1) [ URL_SCHEME "://" ] HOSTNAME_PATTERN [ ":" <port> ]
  //
  //   Match all hostnames that match the pattern HOSTNAME_PATTERN.
  //
  //   Examples:
  //     "foobar.com", "*foobar.com", "*.foobar.com", "*foobar.com:99",
  //     "https://x.*.y.com:99"
  //
  // (2) "." HOSTNAME_SUFFIX_PATTERN [ ":" PORT ]
  //
  //   Match a particular domain suffix.
  //
  //   Examples:
  //     ".google.com", ".com", "http://.google.com"
  //
  // (3) [ SCHEME "://" ] IP_LITERAL [ ":" PORT ]
  //
  //   Match URLs which are IP address literals.
  //
  //   Conceptually this is the similar to (1), but with special cases
  //   to handle IP literal canonicalization. For example matching
  //   on "[0:0:0::1]" would be the same as matching on "[::1]" since
  //   the IPv6 canonicalization is done internally.
  //
  //   Examples:
  //     "127.0.1", "[0:0::1]", "[::1]", "http://[::1]:99"
  //
  // (4)  IP_LITERAL "/" PREFIX_LENGHT_IN_BITS
  //
  //   Match any URL that is to an IP literal that falls between the
  //   given range. IP range is specified using CIDR notation.
  //
  //   Examples:
  //     "192.168.1.1/16", "fefe:13::abc/33".
  //
  // (5)  "<local>"
  //
  //   Match local addresses. The meaning of "<local>" is whether the
  //   host matches one of: "127.0.0.1", "::1", "localhost".
```

**chromium-args**
:   *since v0.4.0*
:   (*string*) Specify chromium (content shell) command line arguments. It will be useful if you want to distribute the app with some custom chromium args. For example, if you want to disable the GPU accelerated video diplay, just add "chrumium-args":"--disable-accelerated-video". If you want to add multiple arguments, separate each two argments by space. This field can take a number of flags in one argument as well, via enclosing them in single quotation marks.

A list of Chromium's command line arguments is available at [http://src.chromium.org/svn/trunk/src/content/public/common/content_switches.cc][4]

**js-flags**
:   *since v0.4.1*
:   (*string*) 指定要传递给JS引擎(v8)的标记. 例如. 打开Harmony代理和Harmony集合特性:

```
{
    "name": "nw-demo",
    "main": "index.html",
    "js-flags": "--harmony_proxies --harmony_collections"
}
```

**inject-js-start / inject-js-end**
:   (*string*) a local filename, relative to the application path, used to specify a JavaScript file to inject to the window. `inject-js-start`: The injecting JavaScript code is to be executed after any files from css, but before any other DOM is constructed or any other script is run; `inject-js-end`: The injecting JavaScript code is to be executed after the document object is loaded, before `onload` event is fired. This is mainly to be used as an option of `Window.open()` to inject JS in a new window. (*Since v0.9.0 and v0.8.5*)

**snapshot**
:   *since v0.4.2*
:   (*string*) Specify the path to the snapshot file to be loaded with the application. The snapshot file contains compiled code of your application. See [Protect JavaScript source code with v8 snapshot][5].

**dom_storage_quota**

:   *since v0.6.1*
:   (*int*) Nuber of mega bytes for the quota of the DOM storage. The suggestion is to put double the value you want.

**node-edit-menu**
:   *since v0.7.3*
:   (*boolean*) whether the default `Edit` menu should be disabled on Mac. The default value is `false`. Only effective on Mac. This is a workaround for a feature request and is expected to be replaced by something else soon.

## Window 参数

**title**
:   (*string*) `node-webkit`默认窗口标题,当应用程序启动时你可以自定义要显示的标题

**width/height**
:   (*int*) 主窗口的初始宽度/高度.

**toolbar**
:   (*boolean*) 是否显示导航栏

**icon**
:   (*string*) 窗口图片路径

**position**
:   (*string*) 可为`null`, `center` 或`mouse`, 控制启动后主窗口的初始位置

**min_width/min_height**
:   (*int*) 窗口的最小宽度/高度

**max_width/max_height**
:   (*int*) 窗口的最大宽度/高度

**as_desktop**
:   (*boolean*) 在X11环境中显示桌面背景窗口

**resizable**
:   (*boolean*) 窗口是否可以调整大小( 从node-webkit v0.3.0起可用)

**always-on-top**
:   (*boolean*) 是否让窗口保持在其他窗口的前面 ( node-webkit v0.3.4可用)

**fullscreen**
:   (*boolean*) 是否全屏显示 (自 node-webkit v0.3.0可用)

**show_in_taskbar**
:   (*boolean*) 窗口是否载任务条或Dock中显示. 默认为`true`, 自v0.9.2可用

**frame**
:   (*boolean*) 指定为`false`使窗口无边框(node-webkit v0.3.0后可用)

**show**
:   (*boolean*) 指定为`false`,让应用程序窗口在启动后隐藏(自node-webkit v0.3.0可用)

**kiosk**

:   (*boolean*) 是否使用`Kiosk`模式. 在`Kiosk`模式中,应用程序全屏化,并阻止用户离开应用程序.因此你应该在应用程序中提供一个方法来退出应用程序. 该模式主要用于演示(node-webkit v0.3.1后可用)
:   此模式通过一个Web幻灯片应用测试,左右键切换幻灯片不可用,不知道是什么原因.

## Webkit 参数

下面的参数要求 node-webkit 版本 `>= v0.3.0`

**plugin**
:  (*boolean*) 是否加载浏览器的外部插件,例如`Flash`, 默认为`false`

**java**
:  (*boolean*) 是否加载Java applets, 默认为`false`

 **page-cache**
:   (*boolean*) 是否打开页面缓存, 默认为`false`

## 其他参数

Packages/1.0 标准制定了`package.json`应该提供的但没有使用的其他字段,但你仍然可以使用.

**description**
**version**
**keywords**
**maintainers**
**contributors**
**bugs**
**licenses**
**repositories**

Array of repositories where the package can be located. Each repository is a hash with properties for the "type" and "url" location of the repository to clone/checkout the package. A "path" property may also be specified to locate the package in the repository if it does not reside at the root. For example:

```
"repositories": [
       {
            "type": "git",
            "url": "http://github.com/example.git",
            "path": "packages/mypackage"
       }
]
```

  [1]: http://www.json.org/
  [2]: https://github.com/rogerwang/node-webkit/wiki/node-main
  [3]: https://github.com/rogerwang/node-webkit/wiki/How-to-package-and-distribute-your-apps
  [4]: http://src.chromium.org/svn/trunk/src/content/public/common/content_switches.cc
  [5]: https://github.com/rogerwang/node-webkit/wiki/Protect-JavaScript-source-code-with-v8-snapshot