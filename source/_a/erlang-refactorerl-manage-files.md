title: 如何在RefactorErl中管理文件和应用程序
categories:
  - Erlang
tags:
  - RefactorErl
toc: true
date: 2015-02-13 02:34:04
---

## 添加文件和目录

通过调用`ri:add/1`函数添加文件到`RefactorErl`数据库中.

You can add files to the RefactorErl database by calling the add function with either a filename as a string or a module name as an atom.

Note that in the latter case, "ri" defaults to the current working directory (which you may work around by including a path in your singe-quoted atom).

If you specify a directory instead of a regular filename, then it will be recursively traversed.

You may just as well give a list of atoms or strings to add more files at once. All of the following example commands would add the same file:

```
cd(dir), ri:add(modname).
ri:add('dir/modname').
ri:add(['dir/modname']).
ri:add("dir/modname.erl").
ri:add("/current/dir/modname.erl").
ri:add("path_to_dir/dir").
```

Usually, Erlang source files (having the extension .erl) are loaded into RefactorErl.
In addition, RefactorErl is also capable of loading compiled .beam files.

Note that this feature is applicable only to those .beam files that were compiled with the debug_info option.
Also note that the resulting file will be pretty printed by RefactorErl.

`RefactorErl`还可以加载`.beam`文件, `.beam`文件必须是通过`debug_info`编译选项编译的才能加载.

## 删除文件和目录

## 添加应用程序
## 通过手工配置添加应用程序

通过`ri`或`ris`模块完成手工配置

Modules can be loaded as applications, but the base of your library has to be set before:

```
ri:addenv(appbase, "path/to/my/applib").
```

If the application has additional include directories, then these directories has to be also set in advance:

```
ri:addenv(include, "path/to/my/incldir").
```

## 使用Emakefile处理函数添加应用程序
## 刷新数据库
## 生成数据库信息
