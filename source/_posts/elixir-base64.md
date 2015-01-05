title: Elixir | Base64编码
categories:
  - Elixir
tags:
  - Elixir
  - encoding
toc: false
date: 2014-10-23 17:30:27
---

![Elixir base64 和url,文件名安全的base64字母表][1]

一般性的Base64编码包含`+`,`/`两个符号,但是这两个符号在URL中有特殊的含义,因此需要一种方式来避免这两个符号的冲突. 在编码URL地址和文件名的时候分别使用`-`,`_`替换这两个符号.

  [1]: /assets/images/AA9701C5-0427-45AF-88CE-CB0DA1183FAB.png


## 参考资料

1. http://elixir-lang.org/docs/master/elixir/Base.html