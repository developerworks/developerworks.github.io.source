title: Git 常用别名
categories:
  - git
tags:
  - git alias
date: 2014-08-22 16:33:47
toc: false
---

- 切换分支

```
git config --global alias.co 'checkout'
```

- 显示分支

```
git config --global alias.br 'branche'
```

- 日志格式化

```
git config --global alias.logp 'log --pretty=format:"%h - %an, %ar : %s"'
git config --global alias.logpg 'log --pretty=format:"%h - %an, %ar : %s" --graph'
git config --global alias.log1 'log --pretty=oneline'
git config --global alias.log1g 'log --pretty=oneline --graph'
```

使用方法分别为:

```
git logp    # 自定义日期
git logpg   # 自定义日期,带Graph
git log1    # 一行
git log1g   # 一行,带Graph
```

- 查看远程分支

```
git config --global alias.rv 'remote -v'
```

- 查看全局配置

```
git config --global alias.cl 'config -l'
```
