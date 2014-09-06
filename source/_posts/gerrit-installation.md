title: 安装Gerrit
categories:
  - git
tags:
  - gerrit
date: 2014-08-21 22:43:12
----

Gerrit 版本: 2.9
JDK 版本: 1.7.0_45


很多东西都曾经使用过,这是其中之一,今日又需要安装,把以前没有遇到的问题记录一下

> 注意

创建数据库之前, 设置数据库默认编码:

```
[client]
default-character-set = utf8
[mysqld]
character_set_server = utf8
default-collation= utf8_general_ci
```

Gerrit 手册创建配置数据库的SQL语句如下:

<!-- more -->

```
CREATE USER 'gerrit2'@'localhost' IDENTIFIED BY 'secret';
  CREATE DATABASE reviewdb;
  GRANT ALL ON reviewdb.* TO 'gerrit2'@'localhost';
  FLUSH PRIVILEGES;
```

如果你的数据库默认编码为非`latin1`, 需要执行下面的SQL语句把数据库编码修改为`latin1`否则会安装`Gerrit`的时候会出现莫名其妙的错误.

```
ALTER DATABASE review charset=latin1;
```
> Gerrit 把第一个用户作为管理员

## 初始化管理员用户

认证方式为 `DEVELOPMENT_BECOME_ANY_ACCOUNT`

- 打开`http://localhost:8080`
- 点击右上角`Become`跳转到账号注册页
- 点击`New Account`按钮注册新账号
- 有四个项目需要填写:
 - Full name: 账号显示名称
 - Preferred Email: 通知发送邮件地址
 - Username: 登录用户名, 作为登陆凭证
 - Add SSH Public Key: 添加公钥, 作为`git push`认证用

```
ssh -p 29418 first_user@localhost
```

## gerrit配置之中文编码和邮件

- 所有文本文件都必须存储成utf8编码
- 设置全局配置

```
git config --global core.quotepath false
git config --global i18n.logoutputencoding utf8
git config --global i18n.commitencoding utf8
```

## 邮件的问题

对gerrit的sendmail设置如下：

```
[sendemail]
        smtpServer = smtp.exmail.qq.com
        smtpServerPort = 25
        smtpEncryption = none
        smtpUser = gerrit@***.com
        smtpPass = ****
        sslVerify = false
        from=CodeReview <software@***.com>
```

## 评审的中文问题

使用`gerrit`评审代码,如果代码中有中文会出现服务器错误,这是查看`logs`中的`error_log`,看到如下错误日志,这时因为缺少解析中文的包,去http://code.google.com/p/juniversalchardet/downloads/list, 下载juniversalchardet的jar包,放在lib目录下,重新启动服务即可。

```
wget https://juniversalchardet.googlecode.com/files/juniversalchardet-1.0.3.jar
```


## 解决Gerrit2使用mysql时中文乱码问题

主要问题出在数据库上,为了保证安装时不出问题使用了latin1,但使用过程中就出现中文乱码问题了.
有时在报错如：

```
java.sql.BatchUpdateException: Incorrect string value: '\xC9\xCF' for column 'message' at row 1

或

Illegal mix of collations (latin1_bin,IMPLICIT) and (utf8_general_ci,COERCIBLE) for operation '<='
```

为此,修改如下

```
vi /etc/my.cnf
[mysqld]
default-character-set=utf8
init_connect='SET NAMES utf8'
[client]
default-character-set=utf8
```

进入数据库,执行如下命令


```
ALTER DATABASE gitdb charset=utf8;
alter table account_diff_preferences engine=innodb;
alter table account_external_ids engine=innodb;
alter table account_group_id engine=innodb;
alter table account_group_includes_by_uuid engine=innodb;
alter table account_group_includes_by_uuid_audit engine=innodb;
alter table account_group_members engine=innodb;
alter table account_group_members_audit engine=innodb;
alter table account_group_names engine=innodb;
alter table account_groups engine=innodb;
alter table account_id engine=innodb;
alter table account_patch_reviews engine=innodb;
alter table account_project_watches engine=innodb;
alter table account_ssh_keys engine=innodb;
alter table accounts engine=innodb;
alter table change_id engine=innodb;
alter table change_message_id engine=innodb;
alter table change_messages engine=innodb;
alter table changes engine=innodb;
alter table patch_comments engine=innodb;
alter table patch_set_ancestors engine=innodb;
alter table patch_set_approvals engine=innodb;
alter table patch_sets engine=innodb;
alter table schema_version engine=innodb;
alter table starred_changes engine=innodb;
alter table submodule_subscriptions engine=innodb;
alter table system_config engine=innodb;
alter table tracking_ids engine=innodb;
ALTER TABLE account_diff_preferences CONVERT TO CHARACTER SET utf8 COLLATE utf8_general_ci;
ALTER TABLE account_external_ids CONVERT TO CHARACTER SET utf8 COLLATE utf8_general_ci;
ALTER TABLE account_group_id CONVERT TO CHARACTER SET utf8 COLLATE utf8_general_ci;
ALTER TABLE account_group_includes_by_uuid CONVERT TO CHARACTER SET utf8 COLLATE utf8_general_ci;
ALTER TABLE account_group_includes_by_uuid_audit CONVERT TO CHARACTER SET utf8 COLLATE utf8_general_ci;
ALTER TABLE account_group_members CONVERT TO CHARACTER SET utf8 COLLATE utf8_general_ci;
ALTER TABLE account_group_members_audit CONVERT TO CHARACTER SET utf8 COLLATE utf8_general_ci;
ALTER TABLE account_group_names CONVERT TO CHARACTER SET utf8 COLLATE utf8_general_ci;
ALTER TABLE account_groups CONVERT TO CHARACTER SET utf8 COLLATE utf8_general_ci;
ALTER TABLE account_id CONVERT TO CHARACTER SET utf8 COLLATE utf8_general_ci;
ALTER TABLE account_patch_reviews CONVERT TO CHARACTER SET utf8 COLLATE utf8_general_ci;
ALTER TABLE account_project_watches CONVERT TO CHARACTER SET utf8 COLLATE utf8_general_ci;
ALTER TABLE account_ssh_keys CONVERT TO CHARACTER SET utf8 COLLATE utf8_general_ci;
ALTER TABLE accounts CONVERT TO CHARACTER SET utf8 COLLATE utf8_general_ci;
ALTER TABLE change_id CONVERT TO CHARACTER SET utf8 COLLATE utf8_general_ci;
ALTER TABLE change_message_id CONVERT TO CHARACTER SET utf8 COLLATE utf8_general_ci;
ALTER TABLE change_messages CONVERT TO CHARACTER SET utf8 COLLATE utf8_general_ci;
ALTER TABLE changes CONVERT TO CHARACTER SET utf8 COLLATE utf8_general_ci;
ALTER TABLE patch_comments CONVERT TO CHARACTER SET utf8 COLLATE utf8_general_ci;
ALTER TABLE patch_set_ancestors CONVERT TO CHARACTER SET utf8 COLLATE utf8_general_ci;
ALTER TABLE patch_set_approvals CONVERT TO CHARACTER SET utf8 COLLATE utf8_general_ci;
ALTER TABLE patch_sets CONVERT TO CHARACTER SET utf8 COLLATE utf8_general_ci;
ALTER TABLE schema_version CONVERT TO CHARACTER SET utf8 COLLATE utf8_general_ci;
ALTER TABLE starred_changes CONVERT TO CHARACTER SET utf8 COLLATE utf8_general_ci;
ALTER TABLE submodule_subscriptions CONVERT TO CHARACTER SET utf8 COLLATE utf8_general_ci;
ALTER TABLE system_config CONVERT TO CHARACTER SET utf8 COLLATE utf8_general_ci;
ALTER TABLE tracking_ids CONVERT TO CHARACTER SET utf8 COLLATE utf8_general_ci;
```

目的就是保证都使用utf8编码,引擎使用innodb,目的就是为了保证索引长度大于1000时依然不出错。


Gerrit 还需要设置JDBC驱动参数:

```
[database]
 type = mysql
 url = jdbc:mysql://localhost:3306/reviewdb?user=user&password=pwd&useUnicode=true&characterEncoding=utf8
```


## 参考资料

1. http://zires.info/tag/git-cherry-pick/
2. http://www.cnblogs.com/Jerryshome/archive/2012/04/19/2457170.html
3. http://ruiming.blog.sohu.com/260331384.html



