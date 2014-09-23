title: SSH 快捷方式
categories:
  - Tools
tags:
  - ssh
toc: false
date: 2014-09-19 14:33:28
---

方便地管理服务器的两种方式.

<!-- more -->


## 使用 SSH 配置文件

编辑配置文件`~/.ssh/config`, 如果该文件不存在,创建之.

```
Host scotch
  HostName scotch.io
  User nick
Host example2
  HostName example.com
  User root
Host example3
  HostName 64.233.160.0
  User userxyz123
  Port 56000
Host amazon1
  HostName ec2.amazon.com
  User ec2-user
  IdentityFile /path/to/special/privatekey/amazon.pem
```

然后即可通过名称访问SSH服务器, 例如

```
ssh scotch
ssh example2
ssh example3
ssh amazon1
```

## 使用别名


`vim ~/.bash_aliases`

```
alias scotch='ssh nick@scotch.io'
alias example2='ssh root@example.com'
alias example3='ssh userxyz123@64.233.160.0 -p 56000'
alias amazon1='ssh ec2-user@ec2.amazon.com -i /path/to/special/privatekey/amazon.pem'
```


## 资料

1. http://scotch.io/quick-tips/how-to-create-an-ssh-shortcut