title: Linux Upstart, Node, 和 forever
categories:
  - node.js
tags:
  - upstart
  - linux
date: 2014-08-25 13:57:57
---

利用Ubuntu Upstart随系统启动node服务器.

<!-- more -->

## 安装 forever

```
$ sudo npm install -g forever
```

## Upstart 脚本启动方式

Upstart 脚本位置存放在 `/etc/init` 目录, 所有 `Upstart` 脚本以 `.conf` 后最结尾. 可以通过如下命令来管理服务器程序.

```
start application_name
status application_name
restart application_name
stop application_name
```

示例:

```
#!upstart
#
# An example upstart script for running a Node.js process as a service
# using Forever as the process monitor. For more configuration options
# associated with Forever, see: https://github.com/nodejitsu/forever
#
# You will need to set the environment variables noted below to conform to
# your use case, and should change the description.
#
description "Console Admin Server"
author "hezhiqiang"

start on runlevel [2345]
stop on starting rc RUNLEVEL=[016]

# This line is needed so that Upstart reports the pid of the Node.js process
# started by Forever rather than Forever's pid.
expect fork

# This monitors Forever, which seems gratuitous.
# TIP: Comment these out while debugging your script.
#respawn
#respawn limit 5 30

# The following environment variables must be set so as to define where Node.js
# and Forever binaries and the Node.js source code can be found.
#
# The example environment variables below assume that Node.js is installed by
# building from source with the standard settings.
#
# It should be easy enough to adapt to the paths to be appropriate to a package
# installation, but note that the packages available in the default repositories
# are far behind the times. Most users will be  building from source to get a
# more recent Node.js version.
#
# The full path to the directory containing the node and forever binaries.
# env NODE_BIN_DIR="/usr/local/bin"
# Set the NODE_PATH to the Node.js main node_modules directory.
# env NODE_PATH="/usr/local/lib/node_modules"
# The application startup Javascript file path.
# env APPLICATION_PATH="/home/node/my-application/start-my-application.js"
# Process ID file path.
# env PIDFILE="/var/run/my-application.pid"
# Log file path.
# env LOG="/var/log/my-application.log"
# Forever settings to prevent the application spinning if it fails on launch.
# env MIN_UPTIME="5000"
# env SPIN_SLEEP_TIME="2000"

env NODE_BIN_DIR="/usr/local/bin"
env NODE_PATH="/usr/local/lib/node_modules"
env APPLICATION_PATH="/root/console-admin-server/app.js"
env PIDFILE="/var/run/console-admin-server.pid"
env LOG="/var/log/console-admin-server.log"
env MIN_UPTIME="5000"
env SPIN_SLEEP_TIME="2000"

script
    export NODE_ENV="production"
    # Add the node executables to the path, which includes Forever if it is
    # installed globally, which it should be.
    PATH=$NODE_BIN_DIR:$PATH
    # The minUptime and spinSleepTime settings stop Forever from thrashing if
    # the application fails immediately on launch. This is generally necessary
    # to avoid loading development servers to the point of failure every time
    # someone makes an error in application initialization code, or bringing
    # down production servers the same way if a database or other critical
    # service suddenly becomes inaccessible.
    exec forever \
      --pidFile $PIDFILE \
      -a \
      -l $LOG \
      --minUptime $MIN_UPTIME \
      --spinSleepTime $SPIN_SLEEP_TIME \
      start $APPLICATION_PATH
end script

pre-stop script
    # Add the node executables to the path.
    PATH=$NODE_BIN_DIR:$PATH
    # Here we're using the pre-stop script to stop the Node.js application
    # process so that Forever is given a chance to do its thing and tidy up
    # its data. Note that doing it this way means that each application that
    # runs under Forever must have a different start file name, regardless of
    # which directory it is in.
    exec forever stop $APPLICATION_PATH
end script
```


## Init 脚本启动方式

```
service my-application start
service my-application status
service my-application restart
service my-application stop
```

权限设置

```
sudo su
cp application-script /etc/init.d/application-script
chmod a+x /etc/init.d/application-script
```

更新系统服务定义

```
update-rc.d my-application defaults
```

脚本如下:

```
#!/bin/bash
#
# An init.d script for running a Node.js process as a service using Forever as
# the process monitor. For more configuration options associated with Forever,
# see: https://github.com/nodejitsu/forever
#
# This was written for Debian distributions such as Ubuntu, but should still
# work on RedHat, Fedora, or other RPM-based distributions, since none of the
# built-in service functions are used. So information is provided for both.
#
### BEGIN INIT INFO
# Provides:             my-application
# Required-Start:       $syslog $remote_fs
# Required-Stop:        $syslog $remote_fs
# Should-Start:         $local_fs
# Should-Stop:          $local_fs
# Default-Start:        2 3 4 5
# Default-Stop:         0 1 6
# Short-Description:    My Application
# Description:          My Application
### END INIT INFO
#
### BEGIN CHKCONFIG INFO
# chkconfig: 2345 55 25
# description: My Application
### END CHKCONFIG INFO
#
# Based on:
# https://gist.github.com/3748766
# https://github.com/hectorcorrea/hectorcorrea.com/blob/master/etc/forever-initd-hectorcorrea.sh
# https://www.exratione.com/2011/07/running-a-nodejs-server-as-a-service-using-forever/
#
# The example environment variables below assume that Node.js is installed by
# building from source with the standard settings.
#
# It should be easy enough to adapt to the paths to be appropriate to a package
# installation, but note that the packages available in the default repositories
# are far behind the times. Most users will be building from source to get a
# suitably recent Node.js version.
#
# An application name to display in echo text.
# NAME="My Application"
# The full path to the directory containing the node and forever binaries.
# NODE_BIN_DIR="/usr/local/node/bin"
# Set the NODE_PATH to the Node.js main node_modules directory.
# NODE_PATH="/usr/local/lib/node_modules"
# The application startup Javascript file path.
# APPLICATION_PATH="/home/user/my-application/start-my-application.js"
# Process ID file path.
# PIDFILE="/var/run/my-application.pid"
# Log file path.
# LOGFILE="/var/log/my-application.log"
# Forever settings to prevent the application spinning if it fails on launch.
# MIN_UPTIME="5000"
# SPIN_SLEEP_TIME="2000"

NAME="Console Admin Server"
NODE_BIN_DIR="/usr/local/bin"
NODE_PATH="/usr/local/lib/node_modules"
APPLICATION_PATH="/root/console-admin-server/app.js"
PIDFILE="/var/run/console-admin-server.pid"
LOGFILE="/var/log/console-admin-server.log"
MIN_UPTIME="5000"
SPIN_SLEEP_TIME="2000"


# Add node to the path for situations in which the environment is passed.
PATH=$NODE_BIN_DIR:$PATH
# Export all environment variables that must be visible for the Node.js
# application process forked by Forever. It will not see any of the other
# variables defined in this script.
export NODE_PATH=$NODE_PATH

start() {
    echo "Starting $NAME"
    # We're calling forever directly without using start-stop-daemon for the
    # sake of simplicity when it comes to environment, and because this way
    # the script will work whether it is executed directly or via the service
    # utility.
    #
    # The minUptime and spinSleepTime settings stop Forever from thrashing if
    # the application fails immediately on launch. This is generally necessary to
    # avoid loading development servers to the point of failure every time
    # someone makes an error in application initialization code, or bringing down
    # production servers the same way if a database or other critical service
    # suddenly becomes inaccessible.
    #
    # The pidfile contains the child process pid, not the forever process pid.
    # We're only using it as a marker for whether or not the process is
    # running.
    #
    # Note that redirecting the output to /dev/null (or anywhere) is necessary
    # to make this script work if provisioning the service via Chef.
    forever \
      --pidFile $PIDFILE \
      -a \
      -l $LOGFILE \
      --minUptime $MIN_UPTIME \
      --spinSleepTime $SPIN_SLEEP_TIME \
      start $APPLICATION_PATH 2>&1 > /dev/null &
    RETVAL=$?
}

stop() {
    if [ -f $PIDFILE ]; then
        echo "Shutting down $NAME"
        # Tell Forever to stop the process.
        forever stop $APPLICATION_PATH 2>&1 > /dev/null
        # Get rid of the pidfile, since Forever won't do that.
        rm -f $PIDFILE
        RETVAL=$?
    else
        echo "$NAME is not running."
        RETVAL=0
    fi
}

restart() {
    stop
    start
}

status() {
    # On Ubuntu this isn't even necessary. To find out whether the service is
    # running, use "service my-application status" which bypasses this script
    # entirely provided you used the service utility to start the process.
    #
    # The commented line below is the obvious way of checking whether or not a
    # process is currently running via Forever, but in recent Forever versions
    # when the service is started during Chef provisioning a dead pipe is left
    # behind somewhere and that causes an EPIPE exception to be thrown.
    # forever list | grep -q "$APPLICATION_PATH"
    #
    # So instead we add an extra layer of indirection with this to bypass that
    # issue.
    echo `forever list` | grep -q "$APPLICATION_PATH"
    if [ "$?" -eq "0" ]; then
        echo "$NAME is running."
        RETVAL=0
    else
        echo "$NAME is not running."
        RETVAL=3
    fi
}

case "$1" in
    start)
        start
        ;;
    stop)
        stop
        ;;
    status)
        status
        ;;
    restart)
        restart
        ;;
    *)
        echo "Usage: {start|stop|status|restart}"
        exit 1
        ;;
esac
exit $RETVAL
```






