title: Docker - 管理容器中的数据
categories:
  - docker
tags:
  - data management
  - data volume
  - docker
toc: true
date: 2014-09-05 03:03:10
---

有两种管理数据的主要方式

- 数据卷
- 数据卷容器

<!-- more -->


## 数据卷(Volume)

数据卷是一个位于容器中的`特殊目录`, 其目的是绕过[UFS文件系统][1]提供持久化和数据共享.

- 数据卷可以在容器之间共享和重用数据
- 对数据卷的修改是直接的
- 更新镜像不会包含对数据卷的数据
- 数据卷一直存在, 直到不再有容器使用


### 添加数据卷

在创建容器是通过指定`-v`选项创建数据卷. 可以使用`-v`多次用于挂载多个数据卷.

格式为`-v HOST_PATH:CONTAINER_PATH`,`:`号前是宿主系统的文件系统路径,后面是容器的文件系统路径,下面的命令在`web`容器中`mount`单个数据卷:


```
docker run -d -P --name web -v /webapp training/webapp python app.py
```

### 挂载宿主系统目录作为数据卷

除了通过`-v`选项创建数据卷之外, 还可以把宿主系统目录挂载到容器中:

挂载本地文件系统目录 `/src/webapp` 到容器目录 `/opt/webapp` 中.

```
docker run -d -P --name web -v /src/webapp:/opt/webapp training/webapp python app.py
```

- 目录必须是绝对路径
- 宿主系统中如果不存在该目录,Docker会自动创建

挂载的卷默认可以读写, 也可以用只读的方式挂载:

```
docker run -d -P --name web -v /src/webapp:/opt/webapp:ro training/webapp python app.py
```

`ro` 选项把挂载的卷 `/src/webapp` 设置为在容器中只读


### 把宿主系统中的单个文件挂载为卷

`-v` 选项也可以用于挂载单个文件, 而不仅是目录.

```
docker run --rm -it -v ~/.bash_history:/.bash_history ubuntu /bin/bash
```

This will drop you into a bash shell in a new container, you will have your bash history from the host and when you exit the container, the host will have the history of the commands typed while in the container.


### 创建和挂载一个数据卷容器


创建一个新的命名容器用于数据共享

```
docker run -d -v /dbdata --name dbdata training/postgres echo Data-only container for postgres
           ==    =======        ====== ================= =====================================
            |          |             |                 |                        |
           后台运行  目录路径     容器名称           镜像名称               SHELL命令行
```

使用`--volumes-from`标记把`/dbdata` 挂载到另外一个容器中

```
docker run -d --volumes-from dbdata --name db1 training/postgres
docker run -d --volumes-from dbdata --name db2 training/postgres
```

删除容器并会删除数据卷, 要把数据卷一并删除, 可以使用`docker rm -v`

### 备份,恢复和移植

数据卷另外一个非常实用的功能是备份,恢复和移植你的数据

```
docker run --volumes-from dbdata -v $(pwd):/backup ubuntu tar cvf /backup/backup.tar /dbdata
```

我们启动了一个新容器并从`dbdata`容器挂载数据卷. 同时我们还把当前目录挂载为容器内的`/backup`目录,最后我们使用`tar`命令来备份`dbdata`卷的内容到`/backup/backup.tar`,当命令完成,容器停止我们得到了一个宿主系统当前目录下的一个`backup.tar`数据卷备份文件.

之后,在你需要重新使用这个数据卷的时候,你可以使用如下命令把原先备份的数据卷恢复到创建的容器中继续使用.

```
docker run -v /dbdata --name dbdata2 ubuntu /bin/bash
```

然后解压备份文件到新的数据卷中.

```
docker run --volumes-from dbdata2 -v $(pwd):/backup busybox tar xvf /backup/backup.tar
```



## 参考资料

  [1]: https://docs.docker.com/terms/layer/#ufs-def