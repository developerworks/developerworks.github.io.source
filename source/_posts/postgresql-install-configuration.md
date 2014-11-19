title: PostgreSQL Ubuntu上的安装和配置
categories:
  - Database
tags:
  - PostgreSQL
toc: true
date: 2014-11-12 10:46:12
---

## 安装

执行如下命令安装PostgreSQL Server

    ```
    apt-get install -y postgresql
    ```

## 配置

- 让其他计算机可以连接到Postgresql Server

    编辑文件 `vi /etc/postgresql/9.3/main/postgresql.conf`, 定位到`#listen_addresses = 'localhost'`, 去掉注释,并修改为:

    ```
    listen_addresses = '*'
    ```

- 重启服务器

        /etc/init.d/postgresql restart

- 登录服务器并修改密码

    ```
    # 登录
    sudo -u postgres psql template1
    # 修改密码
    ALTER USER postgres with encrypted password 'postgres';
    ```

- 修改登录认证

    编辑`vi /etc/postgresql/9.3/main/pg_hba.conf`, 添加如下行:

    ```
    local   all         postgres                          md5
    ```

    再次重启:

    ```
    service postgresql restart
    ```

- 安装命令行客户端和登录验证

    ```
    sudo apt-get install postgresql-client
    psql -h localhost -U postgres -W
    ```
