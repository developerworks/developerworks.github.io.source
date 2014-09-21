title: Ejabberd 配置
categories:
  - Communication System
tags:
  - ejabberd
  - xmpp
  - bosh
toc: false
date: 2014-09-16 17:07:26
---

## 注册限制

```
## By default the frequency of account registrations from the same IP
## is limited to 1 account every 10 minutes. To disable, specify: infinity
registration_timeout: infinity
```

默认情况, 同一个IP的账号注册频率限制为每隔10分钟一次, 设置为`infinity`可禁用此限制



##
