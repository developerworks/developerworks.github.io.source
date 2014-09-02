title: 在Android上运行i-jetty
categories:
  - android
tags:
  - i-jetty
toc: true
date: 2014-09-01 15:47:58
---

电视盒子(Android系统)上需要一个Web服务器需要支持盒子的后台各个功能的管理, 视频点播, 处理EPG信息,所以需要一个能够处理动态内容的Servlet容器.

<!--more-->

## 安装 Android SDK

下载,解压,配置环境变量,更新SDK

```
# 下载
axel --num-connections=10 http://dl.google.com/android/android-sdk_r23.0.2-linux.tgz
# 解压
tar zxf android-sdk_r23.0.2-linux.tgz
# 环境变量设置
echo 'export ANDROID_HOME="/root/software/android-sdk-linux"
export PATH="$ANDROID_HOME/tools:$ANDROID_HOME/platform-tools:$PATH"' > /etc/profile.d/android.sh
source /etc/profile
# 更新SDK
android update sdk --no-ui
```



## 参考资料

1. http://blog.csdn.net/qylk2008/article/details/9266219