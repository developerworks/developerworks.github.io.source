title: Gerrit 邮件通知
categories:
  - gerrit
tags:
  - mail-notification
date: 2014-08-29 10:51:28
---

Gerrit 有变更是通过邮件通知相关开发者.

Gerrit 的项目设置存储在 `refs/meta/config` 分支中的 `project.config` 文件中.


```
mkdir config && cd config
git fetch ssh://developerworks@localhost:29418/project refs/meta/config
git checkout FETCH_HEAD
```

在`project.config`文件中添加配置,对一个邮件地址启用通知,可以通过命令`git config`完成:

```
git config -f project.config --add notify.team.email team@example.com
git config -f project.config --add notify.team.email project-manager@example.com
```

定义推送类型

```
git config -f project.config --add notify.team.type new_changes

```

用任意文件编辑器打开`project.config`,其内容如下:

```
[notify "team"]
	email = team@example.com
	email = project-manager@example.com
	type = new_changes
	type = new_patchsets
	type = submitted_changes
```

`project.config` 中的每个通知配置必须有唯一的名称.

提交并推送配置

```
git commit -a -m "Notify team-address@example.com of changes"
git push ssh://developerworks@localhost:29418/review HEAD:refs/meta/config
```

