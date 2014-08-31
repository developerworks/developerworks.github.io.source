title: 在一个Tomcat中运行Gerrit
categories:
  - gerrit
tags:
  - tomcat
date: 2014-08-30 19:57:20
---

## 先决条件

1. 需要设置`gerrit.init`系统属性, 只需要保证此属性存在即可, gerrit 不会使用该属性的值.
2. 如果定义了`gerrit.site_path`, gerrit 在此属性定义的路径下初始化
3. 如果未定义`gerrit.site_path`, gerrit 将会在JNDI资源`jdbc/ReviewDb`所对应的数据库中的`system_config`表中查找初始化路径
4. 如果`gerrit.site_path`和`system_config`都不能决定要初始化的路径, gerrit 将会使用`gerrit.init_path`进行初始化

## 关于插件的安装

1. 如果 `gerrit.install_plugins` 未定义, gerrit 将会安装所有插件
2. 如果 `gerrit.install_plugins` 的值是一个逗号分隔列表, gerrit 将会解析`gerrit.install_plugins`的值,并安装指定的插件
3. 如果 `gerrit.install_plugins` 是一个空字符串, gerrit 不会安装任何插件

## Tomcat 安装


全新安装

```
export CATALINA_OPTS='-Dgerrit.init -Dgerrit.site_path=/opt/gerrit'
catalina.sh start
```

升级
```
export CATALINA_OPTS='-Dgerrit.init'
catalina.sh start
```

假设`jdbc/ReviewDb`所对应的数据库中不存在`system_config`表, 下面的命令用给定的路径和该数据库创建一个新的Gerrit站点

```
export CATALINA_OPTS='-Dgerrit.init -Dgerrit.init_path=/path/to/site'
catalina.sh start
```

