title: Ejabberd动态的重新加载(更新)修改的模块
categories:
  - Communication
  - Ejabberd
tags:
  - ejabberd
toc: true
date: 2014-10-21 15:59:04
---

有时候我们不想停止ejabberd服务,同时能够更新我们的自定义模块,ejabberd已经为我们提供了这样一个功能.通过使用ejabberd的`ejabberd_update`核心模块, 我们可以在`运行时重新加载`我们的模块新代码.

ejabberd_update核心模块为我们提供了如下三个导出的接口函数:

```
-export([
    %% Update all the modified modules
    update/0,
    %% Update only the specified modules
    update/1,
    %% Get information about the modified modules
    update_info/0
]).
```

## 示例

该示例假设你已经搭建好了Ejabberd的开发环境,如果还未搭建号开发环境,请完成开发环境的搭建.

- 首先停止ejabberd

```
ejabberdctl stop
```

- 启动到`live`模式

```
ejabberdctl live
```

- 更新一个模块文件/编译/安装

```
make && make install
```

- 查看需要更新的模块列表

打开一个新的终端执行,查看哪些模块代码需要更新

```
root@bffd81e6215e:~/ejabberd# ejabberdctl update_list
mod_gbox_messager
```

我们看到,update_list命令列出了我们需要更新的模块`mod_gbox_messager`

- 切换到live模式的窗口执行

为了清晰,下面的输出通过手工格式化

```
(ejabberd@localhost)2> ejabberd_update:update().
07:57:08.491 [debug] beam files: [mod_gbox_messager]
07:57:08.492 [debug] script: [{load_module,mod_gbox_messager}]
07:57:08.492 [debug] low level script: [
    {
        load_object_code,
        {
            ejabberd,[],[mod_gbox_messager]
        }
    },
    point_of_no_return,{
        load,{
            mod_gbox_messager,
            brutal_purge,brutal_purge
        }
    }
]
07:57:08.492 [debug] check: {ok,[]}
07:57:08.497 [debug] eval: {ok,[]}
```

- 获取更新信息


```
(ejabberd@localhost)4> ejabberd_update:update_info().
08:25:24.357 [debug] beam files: [mod_gbox_messager]
08:25:24.358 [debug] script: [{load_module,mod_gbox_messager}]
08:25:24.358 [debug] low level script: [
    {
        load_object_code,
        {ejabberd,[],[mod_gbox_messager]}
    },
    point_of_no_return,
    {load,{mod_gbox_messager,brutal_purge,brutal_purge}}
]
08:25:24.358 [debug] check: {ok,[]}
{
    ok,
    "/lib/ejabberd/ebin",
    [mod_gbox_messager],
    [{load_module,mod_gbox_messager}],
    [
        {load_object_code,{ejabberd,[],[mod_gbox_messager]}},
        point_of_no_return,
        {load,{mod_gbox_messager,brutal_purge,brutal_purge}}
    ],
    ok
}
```

`ejabberd_update:update_info()`返回一个元组,其中包含了beam文件的位置`/lib/ejabberd/ebin`, 要加载的模块列表`[{load_module,mod_gbox_messager}]`等, 我们可以在我们的HTTP模块代码中使用,比如:

```
%% 打印需要更新的模块
print_modules() ->
    {ok,_Ebin,Modules,_,_,_} = ejabberd_update:update_info,
    ?INFO_MSG("modules to update: ~p~n", [Modules]).
```




## 通过Web更新模块代码

开发一个Ejabberd的HTTP模块,(如何开发Ejabberd的HTTP模块,请参考 [开发一个Ejabberd HTTP模块][开发一个Ejabberd HTTP模块]) 并通过RESTFul接口动态地更新模块代码

下面我们来定义两个RESTFul服务的端点

```
GET http://localhost/update-modules/all
POST http://localhost/update-modules
```

- 第一个端点用于更新所有已修改的模块
- 第二个端点用于更新特定的模块列表,POST的数据格式采用JSON

为了能在HTTP模块中解码JSON数据,这里用到了Jiffy模块用于处理JSON数据的encode/decode操作. 关于Jiffy的使用,可参考 [Ejabberd中用Jiffy输出JSON数据][Ejabberd中用Jiffy输出JSON数据],有了这样一个功能,我们可以开发Ejabberd的自定义的HTTP模块通过Web动态地更新我们的模块.

如果更新成功`ejabberd_update:update/0`会返回`{ok,[]}`,可依据此判断更新过程是否成功,并返回JSON消息通知客户端更新结果.

## Bugfix

- 2014-10-23

`ejabberdctl live`启动后是没有加载`ejabberd_update`模块的, 需要执行ejabberdctl加载一次`ejabberd_update`模块, 如果是通过程序调用更新,调用时会自动加载更新模块. 所以要先执行一个`ejabberdctl update_list`手工初始化`ejabberd_update`模块.


 [Ejabberd中用Jiffy输出JSON数据]:/2014/09/28/ejabberd-jiffy
 [开发一个Ejabberd HTTP模块]:/2014/09/18/ejabberd-http-module

