title: Erlang通用二进制格式
categories:
  - Erlang
tags:
  - UBF
toc: false
date: 2014-10-04 18:25:55
---

`UBF`是一个让Erlang和外部世界交互的一个框架. 文档和相应的开源库基于`Joe Armstrong`最初的`UBF`站点和代码,增加了MIT许可文件,增加了大量的增强和改进.

`UBF`是一个跨网络转换和描述复杂数据结构的语言. 它包括三个部分:

- UBF(a) 是一个`语言中性的`数据描述格式, 粗略地等同于具有良好格式的XML.
- UBF(b) 是一个描述性的编程语言, 它用于描述`UBF(a)`中的类型, 以及客户端和服务器之间的协议.
- UBF(c) 是一个在UBF客户端和UBF服务器之间使用的低级协议.

`UBF`用于生产级别的部署和要求`24x7x365`可靠性的电信级系统.

## 什么是语言中性?

这是相对于`语言独立的`二进制格式而言, 它的实现是与特定的语言相关的, 支持的格式定义语言如下:

- thrift
- redis
- jsonrpc
- eep8
- bertrpc
- abnf

可以在[项目首页][1]看到多种格式的实现.

## 参考资料

1. 项目地址
https://github.com/ubf/ubf
2. UBF用户指南
http://ubf.github.io/ubf/ubf-user-guide.en.html

  [1]: https://github.com/ubf/ubf