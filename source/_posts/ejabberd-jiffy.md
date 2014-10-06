title: Ejabberd中用Jiffy输出JSON数据
categories:
  - Communication
tags:
  - ejabberd
  - jiffy
toc: true
date: 2014-09-28 13:05:43
---

## 编译

```
cd /tmp
git clone https://github.com/processone/ejabberd.git
cd ejabberd
chmod +x autogen.sh
./autogen.sh
./configure --enable-json --enable-mysql
make
make install
```

`jiffy.so`路径有个Bug,写作本文时,该BUG暂未解决. 请参考:
https://github.com/processone/ejabberd/issues/309,
要解决此问题, 执行:

```
mv /lib/ejabberd/priv/lib/jiffy.so /lib/ejabberd/priv/jiffy.so
```




## 一个例子

```
-module(mod_online_users).
-author('hezhiqiang').
-behaviour(gen_mod).
-export([
    start/2,
    stop/1,
    process/2
]).
-include("ejabberd.hrl").
-include("jlib.hrl").
-include("ejabberd_http.hrl").
-include("logger.hrl").
%% 处理函数,直接返回要输出到浏览器的内容
process(_LocalPath, _Request) ->
    Users = mnesia:table_info(session, size),
    OnlineUsers = jiffy:encode({[{<<"onlineusers">>, Users}]}),
    {200, [], OnlineUsers}.
start(_Host, _Opts) ->
    ?INFO_MSG("===Starting module mod_online_users===", []),
    ok.
stop(_Host) ->
    ?INFO_MSG("===Stopping module mod_online_users===", []),
    ok.
```

配置`/etc/ejabberd/ejabberd.yml`


```
  -
    port: 5280
    module: ejabberd_http
    request_handlers:
      "/pub/archive": mod_http_fileserver
      "/http-bind/": mod_http_bind
      "admin": ejabberd_web_admin
      "online": mod_online_users
    web_admin: true
    http_poll: true
    http_bind: true
    captcha: true
```

重启

```
ejabberdctl restart
```

访问

```
http://192.168.8.132:5280/online/
```

![在线用户数模块][1]


## 构造复杂JSON对象

### Erlang和JSON格式对照表

```
Erlang                          JSON            Erlang
==========================================================================
null                       -> null           -> null
true                       -> true           -> true
false                      -> false          -> false
"hi"                       -> [104, 105]     -> [104, 105]
<<"hi">>                   -> "hi"           -> <<"hi">>
hi                         -> "hi"           -> <<"hi">>
1                          -> 1              -> 1
1.25                       -> 1.25           -> 1.25
[]                         -> []             -> []
[true, 1.0]                -> [true, 1.0]    -> [true, 1.0]
{[]}                       -> {}             -> {[]}
{[{foo, bar}]}             -> {"foo": "bar"} -> {[{<<"foo">>, <<"bar">>}]}
{[{<<"foo">>, <<"bar">>}]} -> {"foo": "bar"} -> {[{<<"foo">>, <<"bar">>}]}
#{<<"foo">> => <<"bar">>}  -> {"foo": "bar"} -> #{<<"foo">> -> <<"bar">>}
```

### 示例代码

```
process(_LocalPath, _Request) ->
    ConnectedUsersNumber = ejabberd_sm:connected_users_number(),
    %% 获取在线用户列表
    AllSessionList = ejabberd_sm:dirty_get_sessions_list(),
    %% 构造JSON对象数组
    AllSessions = lists:map(fun({User, Server, Resource}) ->
        {[{user, User}, {server, Server}, {resource, Resource}]}
    end, AllSessionList),
    %% 编码JSON格式
    Json = jiffy:encode({[
        {connected_users_number, ConnectedUsersNumber},
        {sessions, AllSessions}
    ]}),
    {200, [], Json}.
```

### 输出

把上面的代码和下面的输出对照理解.

![JSON输出][2]


## 变更

### 2014-10-04

调用`mnesia:system_info(all).`获取Mnesia数据库信息, 下面是返回的`Term`:

```
[
    {access_module,mnesia},
    {auto_repair,true},
    {backup_module,mnesia_backup},
    {checkpoints,[]},
    {db_nodes,[ejabberd@localhost]},
    {debug,none},
    {directory,"/var/lib/ejabberd"},
    {dump_log_load_regulation,false},
    {dump_log_time_threshold,180000},
    {dump_log_update_in_place,true},
    {dump_log_write_threshold,1000},
    {event_module,mnesia_event},
    {extra_db_nodes,[]},
    {fallback_activated,false},
    {held_locks,[]},
    {ignore_fallback_at_startup,false},
    {fallback_error_function,{mnesia,lkill}},
    {is_running,yes},
    {local_tables,[
        shaper,
        mod_register_ip,local_config,caps_features,acl,
        access,carboncopy,http_bind,reg_users_counter,pubsub_subscription,bytestream,
        privacy,passwd,irc_custom,roster,last_activity,sr_user,roster_version,
        pubsub_last_item,offline_msg,route,motd,s2s,vcard,pubsub_index,sr_group,
        session_counter,vcard_search,motd_users,schema,session,private_storage,
        pubsub_item,muc_room,pubsub_state,iq_response,temporarily_blocked,
        muc_registered,muc_online_room,pubsub_node
    ]},
    {lock_queue,[]},
    {log_version,"4.3"},
    {master_node_tables,[]},
    {max_wait_for_decision,infinity},
    {protocol_version,{8,1}},
    {running_db_nodes,[ejabberd@localhost]},
    {schema_location,opt_disc},
    {schema_version,{2,0}},
    {subscribers,[<0.15778.0>,<0.16018.0>,<0.15961.0>,<0.15954.0>]},
    {tables,[
        carboncopy,http_bind,reg_users_counter,pubsub_subscription,bytestream,
        privacy,local_config,passwd,irc_custom,shaper,roster,last_activity,
        sr_user,roster_version,pubsub_last_item,offline_msg,route,motd,
        access,acl,s2s,vcard,pubsub_index,caps_features,sr_group,session_counter,
        mod_register_ip,vcard_search,motd_users,schema,session,private_storage,
        pubsub_item,muc_room,pubsub_state,iq_response,temporarily_blocked,
        muc_registered,muc_online_room,pubsub_node
    ]},
    {transaction_commits,56},
    {transaction_failures,81},
    {transaction_log_writes,0},
    {transaction_restarts,0},
    {transactions,[]},
    {use_dir,true},
    {core_dir,false},
    {no_table_loaders,2},
    {dc_dump_limit,4},
    {send_compressed,0},
    {version,"4.12.3"}
]
```

`process/2`函数修改为:

```
process(_LocalPath, _Request) ->
    ConnectedUsersNumber = ejabberd_sm:connected_users_number(),
    %% 用户列表
    AllSessionList = ejabberd_sm:dirty_get_sessions_list(),
    ?DEBUG("All sessions ~p~n", [AllSessionList]),
    %% Mnesia 表信息
    MnesiaSystemInfo = mnesia:system_info(all),
    ?DEBUG("Mnesia Information ~p~n", [MnesiaSystemInfo]),
    %% [{<<"root">>,<<"xmpp.hezhiqiang.info">>,<<"3439698832141213525690305">>}]
    %% 构造一个JSON对象数组
    %% 对象: {[]}
    %% 数组: []
    AllSessions = lists:map(fun({User, Server, Resource}) ->
        {[{user, User}, {server, Server}, {resource, Resource}]}
    end, AllSessionList),
    RemoveElements = [subscribers, fallback_error_function],
    MnesiaSystemInfoJiffy = lists:filtermap(fun({Key, Value}) ->
        case lists:any(fun(E2) -> E2 =:= Key end, RemoveElements) of
            true ->
                false;
            false ->
                case Key of
                    directory ->
                        {true, {Key, list_to_bitstring(Value)}};
                    version ->
                        {true, {Key, list_to_bitstring(Value)}};
                    log_version ->
                        {true, {Key, list_to_bitstring(Value)}};
                    schema_version ->
                        {V1, V2} = Value,
                        {true, {Key, list_to_bitstring([integer_to_list(V1), ".", integer_to_list(V2)])}};
                    protocol_version ->
                        {V1, V2} = Value,
                        {true, {Key, list_to_bitstring([integer_to_list(V1), ".", integer_to_list(V2)])}};
                    _ ->
                        {true, {Key, Value}}
                end
        end
    end, MnesiaSystemInfo),
    Json = jiffy:encode({[
        {connected_users_number, ConnectedUsersNumber},
        {sessions, AllSessions},
        {mnesia, {MnesiaSystemInfoJiffy}}
    ]}),
    {200, [], Json}.
```

JSON输出由JSON View Formater格式化
https://chrome.google.com/webstore/detail/hdmbdioamgdkppmocchpkjhbpfmpjiei

![Mnesia系统信息JSON数据][3]

注意`lists:filtermap`返回的时元组列表, 列表中的每个元素是一个元组, 每个元组包含两个项, `lists:filtermap`函数是`lists:filter`(过滤)和`lists:map`(映射)两个函数功能上的合并:`过滤并映射`

这里例子中有部分值为空`[]`, 比如`checkpoints`, 当包含值的时候`可能`需要按照Jiffy方式转换类型.

完整代码如下:
https://gist.github.com/553129e3015995b56028

## 参考资料

1. https://github.com/davisp/jiffy
2. http://www.erlang.org/doc/man/lists.html
3. http://www.erlang.org/doc/man/mnesia.html

  [1]: /assets/images/9058AA0F-5ECA-4FCD-84BB-9CFAD161991F.png
  [2]: /assets/images/BB5D080B-5B6B-43EE-A8DF-0E7DEBE7BAF4.png
  [3]: /assets/images/3C50996B-E5DA-4574-83E7-C9B237DD9587.png