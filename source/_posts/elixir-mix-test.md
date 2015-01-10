title: Elixir ExUnit测试
categories:
  - Elixir
tags:
  - Mix
  - ExUnit
toc: false
date: 2015-01-06 23:41:45
---

Mix的测试过程

- Mix首先启动应用程序
- Mix然后查找项目根目录下的`test`子目录, 查找模式为`test/**/_test.exs`, 所有匹配这个模式的Elixir脚本都本认为是测试代码.

## 给测试方法打标签

有的时候,为了加速测试过程,我们需要过滤那些依赖外部资源的测试,为此,我们可以给测试打上标签,排除那些标记为`ignore`的测试.

首先需要在测试配置文件`test/test_helper.exs`中添加如下一行,用于排除外部测试

```
ExUnit.configure exclude: [ignore: true]
```

不依赖外部资源的代码都通过测试后, 如果你还需要做一次整体测试, 可以通过参数, 把标记为`ignore`,值为`true`的测试包含进来.

```
mix test --include ignore:true
```

也可以指定所有包含标签`ignore`的测试,而不管其值是什么

```
mix test --include ignore
```

## 配置测试

- `:test_paths` 指定测试代码的位置列表, 默认为`["test"]`, 每个测试路径下面都应该包含一个`test_helper.exs`文件.
- `:test_pattern` 测试匹配模式, 默认为`*_test.exs`
- `:test_coverage` 测试覆盖率选项

## 给测试打标记

![给测试打上@ignore标记](/assets/elixir/FB52C36C-1100-4CAA-B430-C4D932CEDBBE.png)

```
mix test --trace --exclude ignore:true
```
![运行结果](/assets/elixir/36FC718B-1167-4E3B-BCB2-9B8F06AE698C.jpg)

关于测试的详细配置和说明,请参考`mix help test`


## 参考资料

1. http://stackoverflow.com/questions/26150146/how-can-i-make-mix-run-only-specific-tests-from-my-suite-of-tests
