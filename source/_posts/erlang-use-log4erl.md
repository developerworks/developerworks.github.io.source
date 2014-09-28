title: Erlang 使用Log4erl
categories:
  - Erlang
tags:
  - log4er
toc: false
date: 2011-05-16 04:33:49
---


更新历史:

- 2014-09-25 23:34:31

## 功能

- 支持多日志
- 当前文件Appender仅支持基于大小的日志文件滚动
- 支持默认Logger,未指定Logger时系统提供默认Logger
- 5个预定义的日志级别(debug, info, warn, error, fatal)
- 一个error_logger的日志处理器
- 支持用户指定日志级别
- 支持日志格式化
- 支持控制台日志
- 支持smtp formatter
- 支持XML格式的日志
- 支持syslog
- 支持在运行时改变Appender的格式和级别.

## Step1. Checkout from repository

你可以从 -Google code 或者- Github上获取Log4erl的源代码, *Google code上的代码已经旧了,建议从Github上clone 代码*

```
git clone git://github.com/ahmednawras/log4erl.git log4erl
```

![Clone a copy from github][1]
![Checkout a copy from google code][2]
![Log4erl目录结构][3]
![编译Log4erl][4]

## Step2. 编译

如图4, 进入Log4erl源代码目录src执行:

```
make:all([{outdir, "../ebin"}]).
```

## Step3. 安装

你要让Erlang能够找到Log4erl,两种方式

1. 把整个Log4erl目录复制到$ERLANG_HOME/lib目录下面.看上面第三张图.
2. 命令行指定

```
erl -pz /path/to/log4erl
```

## Step4. 使用

```
application:start(log4erl).
```

创建配置文件并调用log4erl:conf(file)初始化

```
log4erl:conf("priv/log4erl.conf").
```

同样你可以用编程的方式对Log4erl进行配置

```
log4erl:add_logger(messages_log).
log4erl:add_console_appender(messages_log, cmd_logs, {warn, "[%L] %l%n"}).
```

好了,现在可以使用它了.

```
log4erl:info("Information message").
```

## Log4erl配置文件格式

```
logger [<name>] {
       ...
}
```

`<name>` 指定了Logger的名称,你可以去任意你喜欢的名字.如果不指定任何名字,那么log4erl将把它作为默认的Logger使用,例如:


```
%% default logger
logger {
       ...
}
```

在一个Logger中,可以有一到多个Appender,例如

```
%% Default logger
%% it includes a file appender and a console appender
logger{
	file_appender app2{
		dir = ".",
		level = info,
		file = my_app,
		type = size,
		max = 100000,
		suffix = log,
		rotation = 5,
		format = '[%L] %I %l%n'
	}
	console_appender app1{
		level = warn,
		format = '%T %j [%L] %l%n'
	}
}
```

Appender的配置格式如下

```
<appender_type> <name> {
	...
}
```

在Appender中你可以用'property=value'的格式来设置Appender的属性,属性剑以','逗号分隔.每种Appender有不同的属性集合.

公共属性:

```
level   = <Level>     => 日志级别 (例如: warn)
format  = <F>		  => 输出格式 (查看 'Appenders.txt')
```

**file_appender**

```
dir         => 输出路径 (例如: /var/log/my_app)
file        => 日志文件名称 (例如: my_app_log)
type        => size,time. 当前仅实现了基于size的日志滚动
max         => 每次日志滚动的最大文件大小
suffix      => 日志文件后缀 (例如: log)
rotation    => 循环滚动次数,例如为5, 当滚动到第五个日志文件并且日志文件达到指定size的时候就会覆盖前面的日志文件,依次循环
```

**smtp_appender**

```
ip          => SMTP服务器IP地址
port        => SMTP服务器端口
no_auth
username    => 用户名
password    => 密码
from        => 寄信人地址
to          => 收信人地址
tilte       => 邮件标题
msg         => 邮件内容
```

**syslog_appender**

```
facility => Facility (例如: ftp)
host => 发送syslog消息的目标主机 [可选]
port => syslog 端口[可选]
```


本文源码:

https://github.com/developerworks/skypebot/tree/master/src/erlang/log4erl_example


参考资料:

1. http://code.google.com/p/log4erl/wiki/Conf_Getting_Started
2. http://code.google.com/p/log4erl/wiki/Log4erl_Manual_2


  [1]: /assets/images/clone-log4erl.png
  [2]: /assets/images/checkout-log4erl.png
  [3]: /assets/images/log4erl-directory-structure.png
  [4]: /assets/images/compile-log4erl.png