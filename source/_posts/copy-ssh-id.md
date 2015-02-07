title: 复制公匙到服务器的两种方式
categories:
  - Linux
tags:
  - SSH
toc: false
date: 2015-02-07 22:59:27
---

## 第一种

在有`ssh-copy-id`工具的系统上, 比如Linux


```
ssh-copy-id username@domain
```


## 第二种

在没有`ssh-copy-id`工具的系统上, 比如Mac OS X

```
cat ~/.ssh/id_rsa.pub | ssh username@domain "cat >> ~/.ssh/authorized_keys"
```