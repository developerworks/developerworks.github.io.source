title: Erlang应用程序简介
categories:
  - Erlang
tags:
  - application
toc: false
date: 2014-10-01 21:27:52
---


In OTP, application denotes a component implementing some specific functionality, that can be started and stopped as a unit, and which can be re-used in other systems as well. This module interfaces the application controller, a process started at every Erlang runtime system, and contains functions for controlling applications (for example starting and stopping applications), and functions to access information about applications (for example configuration parameters).

An application is defined by an application specification. The specification is normally located in an application resource file called Application.app, where Application is the name of the application. Refer to app(4) for more information about the application specification.

This module can also be viewed as a behaviour for an application implemented according to the OTP design principles as a supervision tree. The definition of how to start and stop the tree should be located in an application callback


在OPT中,一个应用程序表示一个实现了特定功能的组件, 可以作为独立的单元start和stop,可以在其他系统中重用.

`application`模块与`application controller`连接, `application controller` 是一个随erlang系统启动的进程.用于控制应用程序(启动,停止,访问应用程序信息).




- `application specification`
描述如何定义一个应用程序
- `application resource file`(`Application`.app)
`Application`为应用程序的名称
- `erl -man app`
查看应用程序资源文件的详细规范
