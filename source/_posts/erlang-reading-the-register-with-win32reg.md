title: Erlang Win32Reg读写注册表信息
categories:
  - Erlang
tags:
  - win32reg
toc: false
date: 2012-06-30 07:45:34
---


2012-07-29更新,
1. 从文件中读取Erlang Term 格式的环境变量,并写入注册表
2. `win32reg`现在只支持`REG_SZ`不支持`REG_EXPAND_SZ`,没有测试`REG_SZ`类型的数据,在环境变量中包含`%VAR%`形式的变量是否能够正常工作

读取保存在文件中的环境变量,并同步到注册表:

**env.erl**

```
%% Created: 2011-5-26
%% Description: TODO: Add description to env
%%
-module(env).
%% There are six entry points in the Windows registry,
%% top level keys. They can be abbreviated in the win32reg module as:
%% Abbrev.          Registry key
%% =======          ============
%% hkcr             HKEY_CLASSES_ROOT
%% current_user     HKEY_CURRENT_USER
%% hkcu             HKEY_CURRENT_USER
%% local_machine    HKEY_LOCAL_MACHINE
%% hklm             HKEY_LOCAL_MACHINE
%% users            HKEY_USERS
%% hku              HKEY_USERS
%% current_config   HKEY_CURRENT_CONFIG
%% hkcc             HKEY_CURRENT_CONFIG
%% dyn_data         HKEY_DYN_DATA
%% hkdd             HKEY_DYN_DATA
% 顶级路径必须小写
-define(CURRENT_USER, "\\hkey_current_user\\Environment").
-define(CURRENT_USER_TEST, "\\hkey_current_user\\EnvironmentTest").
%%
%% Exported Functions
%%
-export([environments/0]).
%%
%% Local Functions
%%
environments() ->
    % 以读写模式打开注册表
    {ok, Reg} = win32reg:open([write]),
    % 设置当前Key
    case win32reg:change_key(Reg, ?CURRENT_USER) of
        ok ->
            % Change key, 如果不存在就创建
            win32reg:change_key_create(Reg, ?CURRENT_USER_TEST),
            % 从文件中读取Tuples
            {ok, Envs} = file:consult("./win32reg/env.config"),
            % 逐条创建注册表Item
            Result = lists:foreach(fun({Key, Value}) ->
                win32reg:set_value(Reg, Key, Value) end, Envs),
            % 关闭注册表
            win32reg:close(Reg),
            % 返回值
            Result;
        _ ->
            false
    end.
```

保存在文件中的环境变量, 以Erlang Term的格式:

**env.config**

```
{"ANDROID","D:\\usr\\android-sdk-windows\\platform-tools"}.
{"ANT_HOME","D:\\usr\\apache-ant-1.8.0"}.
{"ARCHDIR","windows"}.
{"CAKE","D:\\chaw\\cakephp\\cake\\console"}.
{"CEAN_SERVER","http://cean.process-one.net"}.
{"ERLANG_HOME","D:\\usr\\erl5.9.1"}.
{"ffmpeg","D:\\usr\\ffmpeg-git-9d4cb45-32-bit-shared\\bin"}.
{"GETTEXT_TOOLS","D:\\usr\\gettext-tools-0.13.1.bin.woe32"}.
{"GIT","D:\\usr\\Git\\bin"}.
{"Haskell","D:\\usr\\Haskell\\2011.2.0.1"}.
{"HOME","C:\\Documents and Settings\\Administrator"}.
{"HTMLHELP","D:\\Program Files\\HTML Help Workshop"}.
{"JAVA_HOME","D:\\Program Files\\Java\\jdk1.6.0_24"}.
{"LUA_HOME","D:\\usr\\lua\\Lua\\5.1"}.
{"M2_HOME","D:\\usr\\apache-maven-3.0.3"}.
{"MAVEN_OPTS","-Xms256m -Xmx512m"}.
{"MongoDB","D:\\usr\\MongoDb\\mongodb-win32-i386-1.8.1\\bin"}.
{"MYSQL","D:\\usr\\mariadb-5.2.6-win32\\bin"}.
{"NODE_PATH","C:\\Documents and Settings\\Administrator\\Application Data\\npm\\node_modules;"}.
{"PANDOC","D:\\Program Files\\Pandoc\\bin"}.
{"PHP_HOME","D:\\usr\\php"}.
{"PROTO_BUFFER","D:\\usr\\protoc-2.4.1-win32"}.
{"PYJAMAS_HOME","G:\\pyjamas-0.8.1~+alpha1"}.
{"PYTHON_HOME","D:\\usr\\python26"}.
{"RabbitMQ","D:\\usr\\RabbitMQ Server\\rabbitmq_server-2.4.1\\sbin"}.
{"RABBITMQ_CONSOLE_LOG","new"}.
{"SVN","D:\\usr\\svn-win32-1.6.6\\bin"}.
{"TITANIUM_HOME","C:\\Documents and Settings\\Administrator\\Application Data\\Titanium\\mobilesdk\\win32\\1.7.2"}.
```

Windows的环境变量修改起来很坑,那个对话框不能最大化,看不到完整的东西.有时候变量路径又很长.

```
%% Created: 2011-5-26
%% Description: TODO: Add description to env
-module(env).
-define(CURRENT_USER, "\\hkey_current_user\\environment").
%%
%% Include files
%%
%%
%% Exported Functions
%%
-export([environments/0]).
%%
%% API Functions
%%
%%
%% Local Functions
%%
environments() ->
    % 打开注册表
    {ok, Reg} = win32reg:open([read]),
    % 设置当前Key
    case win32reg:change_key(Reg, ?CURRENT_USER) of
        ok ->
            % 读取当前Key下的所有值
            {ok, KVs} = win32reg:values(Reg),
            % 打开文件
            {ok, S} = file:open("data1.dat", write),
            lists:foreach(fun({KK,VV}) ->
                              io:format(S, "~p = ~p~n",[KK, VV]) % 写入
                       end, KVs),
            % 关闭注册表
            win32reg:close(Reg),
            % 关闭文件
            file:close(S);
        _ ->
            false
    end.
```