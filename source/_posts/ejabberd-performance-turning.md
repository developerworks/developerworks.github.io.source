title: Ejabberd 性能调优(草稿)
categories:
  - Ejabberd
tags:
  - Performance
toc: true
date: 2015-03-12 21:13:41
---


## 第一阶段

目标10W并发连接

## 进程限制

ulimit -u 30000

## 启动参数

Ejabberd的启动参数可以在配置文件`/etc/ejabberd/ejabberdctl.cfg`中进行调整

- `ERL_MAX_PORTS`

    每个到客户端(s2c)和服务器(s2s)的连接消耗一个`port`, `ERL_MAX_PORTS`定义了Ejabberd可以支持的并发连接数. 其可以在配置文件`/etc/ejabberd/ejabberdctl.cfg`中指定, 默认值为`32000`.

    大并发连接场景下的应用需要增大该参数的值.

- `ERL_PROCESSES`

    Erlang消耗很多轻量级进程, 如果Ejabberd比较繁忙, 可能会达到进程数上限, 这种情况会导致高延迟. 当消息延迟过高时, 需要判断是否是由于该参数引起的.

## 网络

https://www.ejabberd.im/node/23627

内核网络参数`net.ipv4.ip_local_port_range`指定了本地程序可以打开的端口范围. `61000-32768=28232`可以最多建立28232个到Ejabberd服务器的连接, 一般比这个数字小, 系统还有其他程序需要占用端口.

```
net.ipv4.ip_local_port_range = 32768	61000
```

修改端口范围
```
echo 1024 65535 > /proc/sys/net/ipv4/ip_local_port_range
```

## 排查

- 查看ulimit

    ```
    ulimit -u
    ```

- 验证进程的最大可打开文件 `Max open files`

    ```
    root@scm:~# cat /proc/18938/limits
    Limit                     Soft Limit           Hard Limit           Units
    Max cpu time              unlimited            unlimited            seconds
    Max file size             unlimited            unlimited            bytes
    Max data size             unlimited            unlimited            bytes
    Max stack size            8388608              unlimited            bytes
    Max core file size        0                    unlimited            bytes
    Max resident set          unlimited            unlimited            bytes
    Max processes             127460               127460               processes
    Max open files            60000                60000                files
    Max locked memory         65536                65536                bytes
    Max address space         unlimited            unlimited            bytes
    Max file locks            unlimited            unlimited            locks
    Max pending signals       127460               127460               signals
    Max msgqueue size         819200               819200               bytes
    Max nice priority         0                    0
    Max realtime priority     0                    0
    Max realtime timeout      unlimited            unlimited            us
    ```

## Mnesia表过载

编辑配置文件`/etc/ejabberd/ejabberdctl.cfg`, 设置选项:

```
ERL_OPTIONS="-mnesia dump_log_write_threshold 50000 -mnesia dc_dump_limit 40"
```

## 参考资料

1. 文件描述符

    http://erlangcentral.org/wiki/index.php?title=Introduction_to_Load_Testing_with_Tsung#Max_open_file_descriptor_limit

2. 虚拟接口

    http://erlangcentral.org/wiki/index.php?title=Introduction_to_Load_Testing_with_Tsung#Virtual_interfaces

3. Mnesia过载问题

    http://blog.include.io/archives/106
    http://www.tuicool.com/articles/rIBbqa

4. Tsung 常见问题

    http://tsung.erlang-projects.org/user_manual/faq.html

5. http://blog.fnil.net/index.php/archives/276/

6. 幻灯片-安装过程(需要梯子)
http://www.slideshare.net/ngocdaothanh/tsung-13985127?related=1
