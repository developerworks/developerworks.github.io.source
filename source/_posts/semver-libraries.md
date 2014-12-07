title: Node.js语义版本解析库
categories:
  - Node.js
tags:
  - SemVer
toc: false
date: 2014-11-21 17:34:44
---

安装语义版本解析库

```
npm install -g semver
```

Node Shell示例

```
> var semver = require('semver');
undefined
> var version_info = semver.parse('1.2.3');
undefined
> version_info.major
1
> version_info.minor
2
> version_info.patch
3
```

验证一个版本号是否是语义化版本, 如果是一个语义化版本返回`true`, 否则返回`false`

```
semver.valid('1.2.3')
```

## 其他语言版本

- `Erlang`
https://github.com/nebularis/semver
