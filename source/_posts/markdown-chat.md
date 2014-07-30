title: Markdown Chart, 一个基于Markdown语法的聊天工具
date: 2014-06-28
---

## 克隆项目仓库

```
git clone https://github.com/developerworks/markdown-chat
```

这里为了能对该项目进行定制,我把他克隆到了自己的github仓库.


## 安装依赖库

在执行这一步的时候确保以及安装好了bower前端包管理工具


```
sudo npm install -g bower
```

切换到`markdown-chat`目录, 安装项目依赖库


```
cd /path/to/markdown-chat
npm install
bower install
```

<!-- more -->

## 启动程序之前

`markdown-chat`使用了`mongodb`作为后端持久数据存储,所以要能让程序正常运行, 需要首先安装mongodb数据库

安装方法可参考: [安装MongoDB][1]

    http://docs.mongodb.org/manual/installation/
