title: 编译Ejabberd遇到的问题
categories:
  - Ejabberd
tags:
  - XMPP
  - ejabberd
toc: false
date: 2014-09-25 01:45:29
---

## 问题: 缺少`libyaml`库

办法: 安装需要的库

Ejabberd 从 13.10 开始配置文件的格式从Erlang Term转换到使用YAML格式, 需要用到libyaml来解析yaml文件.

浏览器打开: http://pyyaml.org/download/libyaml/, 找到最新版本的`libyaml`

```
wget http://pyyaml.org/download/libyaml/yaml-0.1.6.tar.gz
tar zxf yaml-0.1.6.tar.gz
./configure
make
sudo make install
```

## 问题: 网络中断导致下载了不完整的`deps/`包, 比如`deps/p1_yaml`

办法: 删除不完整的包,重新`make`

```
rm -rf deps/p1_yaml
make
```

## 问题: 编译ejabberd模块找不到头文件的问题

办法: 网上很多介绍编译模块的方法是使用erlc编译模块, 使用`erlc`编译模块需要指明头文件,依赖库的路径,容易出错,编译自定义的ejabberd并没有这么麻烦,只要把写好了的ejabberd模块文件放到`$(EJABBERD)/src`目录下, 然后运行`make`, 生成的`.beam`文件会自动生成到``$(EJABBERD)/ebin`目录下.
