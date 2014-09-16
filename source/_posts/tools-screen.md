title: 工具 - screen
categories:
  - Tools
tags:
  - tools
  - screen
toc: false
date: 2014-09-16 11:51:36
---

使用`screen`命令在多个终端之间切换,同时又不会导致退出终端会自动终止运行的进程. 三个最常用的操作

- 创建会话

    创建一个网卡流量监控的会话
    ```
    screen -S nload
    ```

- 离开

    按住 <kbd>Ctrl</kbd> 不放, 按 <kbd>A</kbd> <kbd>D</kbd>

- 恢复
    ```
    screen -r nload
    ```