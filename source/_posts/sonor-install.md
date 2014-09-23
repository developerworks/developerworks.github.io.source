title: QA - Sonor 安装
categories:
  - QA
tags:
  - sonor
toc: false
date: 2014-09-10 22:13:35
---

## 前提条件

安装需要满足如下条件:

http://docs.codehaus.org/display/SONAR/Requirements

<!--more-->


## 配置数据库

创建`sonar`的数据库用户

```
CREATE DATABASE sonar CHARACTER SET utf8 COLLATE utf8_general_ci;
CREATE USER 'sonar' IDENTIFIED BY 'sonar';
GRANT ALL ON sonar.* TO 'sonar'@'%' IDENTIFIED BY 'sonar';
GRANT ALL ON sonar.* TO 'sonar'@'localhost' IDENTIFIED BY 'sonar';
FLUSH PRIVILEGES;
```

## 下载安装

如果没有axel多线程下载工具可以通过`apt-get install -y axel`安装, 也可以直接使用`wget`

```
axel --output=/opt/sonarqube-4.4.zip --num-connections=10 http://dist.sonar.codehaus.org/sonarqube-4.4.zip
wget -O /opt/sonarqube-4.4.zip http://dist.sonar.codehaus.org/sonarqube-4.4.zip
```

安装目录

```
export SONAR_INSTALLATION=/opt/sonarqube-4.4
```

## 配置


```
vi $SONAR_INSTALLATION/conf/sonar.properties
```

- 注释掉 `sonar.jdbc.url=jdbc:h2:tcp://localhost:9092/sonar`

- 取消MySQL的注释

    ```
    #--------------------------------------------------------------------------------------------------
    # DATABASE
    #
    # IMPORTANT: the embedded H2 database is used by default. It is recommended for tests only.
    # Please use a production-ready database. Supported databases are MySQL, Oracle, PostgreSQL
    # and Microsoft SQLServer.
    # Permissions to create tables, indices and triggers must be granted to JDBC user.
    # The schema must be created first.
    sonar.jdbc.username=sonar
    sonar.jdbc.password=sonar
    #----- Embedded database H2
    # Note: it does not accept connections from remote hosts, so the
    # SonarQube server and the maven plugin must be executed on the same host.

    # Comment the following line to deactivate the default embedded database.
    # sonar.jdbc.url=jdbc:h2:tcp://localhost:9092/sonar

    # directory containing H2 database files. By default it's the /data directory in the SonarQube installation.
    #sonar.embeddedDatabase.dataDir=
    # H2 embedded database server listening port, defaults to 9092
    #sonar.embeddedDatabase.port=9092
    #----- MySQL 5.x
    # Comment the embedded database and uncomment the following line to use MySQL
    sonar.jdbc.url=jdbc:mysql://localhost:3306/sonar?useUnicode=true&characterEncoding=utf8&rewriteBatchedStatements=true
    ```

- 修改Web服务器配置

    ```
    sonar.web.host=0.0.0.0

    # Web context. When set, it must start with forward slash (for example /sonarqube).
    # The default value is root context (empty value).
    sonar.web.context=/

    # TCP port for incoming HTTP connections. Disabled when value is -1.
    sonar.web.port=9000
    ```

- 调整JDK参数

    编辑配置文件, 取消`# wrapper.java.additional.7=-server`的注释

    ```
    # RECOMMENDED : uncomment if Java Virtual Machine is a JDK but not a JRE. To know which JVM you use, execute
    # 'java -version'. JDK displays 'Server VM'.
    wrapper.java.additional.7=-server
    wrapper.java.command=/usr/lib/jvm/java-7-oracle/bin/java
    ```


