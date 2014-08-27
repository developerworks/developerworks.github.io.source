title: Gerrit Prolog 变更Facts
categories:
  - gerrit
tags:
  - prolog
  - facts
date: 2014-08-27 00:22:30
---




http://gerrit-review.googlesource.com/Documentation/prolog-change-facts.html
<!-- more -->
#

| Fact | Example | Description
| ---- | ------- | -----------
| `commit_message/1` | `commit_message('Fix bug X').` | 提交的消息作为一个字符串`atom`.
| `commit_stats/3` | `commit_stats(5,20,50).` |三个参数分别为修改文件数, 插入行数, 删除行数.

# 内置Prolog断言

| Predicate | Example usage | Description
| --------- | ------------- | -----------
| `commit_stats/3` | `commit_stats(5,20,50).` |修改文件数, 插入行数, 删除行数.