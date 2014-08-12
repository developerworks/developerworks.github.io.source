title: Ubuntu12.04打印系统(CUPS)Web管理界面403 Forbidden的问题
date: 2014-08-12
categories:
- linux
tags:
- CUPS
---



## 问题


通过如下命令安装完成`CUPS`后,登陆`http://localhost:631/admin`出现`403 Forbidden`错误.

```
sudo apt-get install cups
```

<!-- more -->

## 解决办法

参考:
http://ubuntuforums.org/showthread.php?t=1446262


### 办法一

编辑配置文件`/etc/cups/cupsd.conf`, 在所有 `<Location>` 内添加 `Allow all`

### 办法二

执行命令

```
sudo cupsctl --remote-admin
```

然后重启CUPS服务器

```
sudo /etc/init.d/cups restart
```


