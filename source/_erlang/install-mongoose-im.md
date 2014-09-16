
```
git clone https://github.com/esl/MongooseIM.git
apt-get install -y aptitude git axel build-essential libexpat-dev libssl-dev autoconf
cd MongooseIM
make
make rel
```

注册问题

修改配置文件 `$MIM_HOME/rel/mongooseim/etc/ejabberd.cfg`

1. 修改 `hosts`, 设置实际使用的域名
2. 修改 `mod_register`配置, 把`{access_from, deny}`改为`{access_from, register}`
3. 重启MongooseIM, ``$MIM_HOME/rel/mongooseim/bin/mongooseim restart`
4. 使用Adium重新注册


配置

```
{hosts, ["xmpp.hezhiqiang.info"] }.
{mod_register, [
    %%
    %% 密码强度
    %%
    %%{password_strength, 32},
    %%
    %% After successful registration, the user receives
    %% a message with this subject and body.
    %%
    {welcome_message, {""}},
    %%
    %% When a user registers, send a notification to
    %% these XMPP accounts.
    %%
    {registration_watchers, ["admin@localhost"]},
    %%
    %% Only clients in the server machine can register accounts
    %% 需要把IP网络添加到ip_access列表中, 这里我们添加了一个 {allow,"192.168.8.0/8"}, 运行本地局域网用户注册
    {ip_access, [{allow, "127.0.0.0/8"},{allow,"192.168.8.0/8"},{deny, "0.0.0.0/0"}]},
    %%
    %% Local c2s or remote s2s users cannot register accounts
    %%
    %%{access_from, deny},
    {access_from, register},
    {access, register}
]}
```



```
root@9af821c5d811# bin/mongooseimctl
Usage: mongooseimctl [--node nodename] [--auth user host password] command [options]

Available commands in this MongooseIM node:
  backup file                                                         Store the database to backup file (only Mnesia) # 备份数据库文件
  delete_expired_messages                                             Delete expired offline messages from database   # 删除超时的离线消息
  delete_old_messages days                                            Delete offline messages older than DAYS         # 删除days天之前的离线消息
  dump file                                                           Dump the database to text file (only Mnesia)    # 导出数据库到文本文件
  dump_table file table                                               Dump a table to text file (only Mnesia)         # 导出一个表到文本文件
  get_loglevel                                                        Get the current loglevel                        # 获取当前日志级别
  help [--tags [tag] | com?*]                                         Show help (try: mongooseimctl help help)        # 显示帮助
  incoming_s2s_number                                                 Number of incoming s2s connections on the node          # 进入到当前节点的"s2s"连接数
  install_fallback file                                               Install the database from a fallback file (only Mnesia) # 从fallback文件安装数据库
  load file                                                           Restore the database from text file (only Mnesia)       # 从文本文件恢复数据库
  mnesia [info]                                                       show information of Mnesia system                  # 显示Mnesia系统信息
  mnesia_change_nodename oldnodename newnodename oldbackup newbackup  Change the erlang node name in a backup file       # 在备份文件中修改erlang节点名称
  outgoing_s2s_number                                                 Number of outgoing s2s connections on the node     # 出站的"s2s"连接数
  register user host password                                         *Register a user                                   # 注册用户
  registered_users host                                               *List all registered users in HOST                 # 列举出所有的注册用户
  restart                                                             Restart MongooseIM                                 # 重启MongooseIM
  restore file                                                        Restore the database from backup file (only Mnesia)# 从备份文件恢复数据库
  set_master nodename                                                 Set master node of the clustered Mnesia tables     # 设置集群的Mnesia表的主节点
  status                                                              Get MongooseIM status                              # 获取MongooseIM状态
  stop                                                                Stop MongooseIM                                    # 停止MongooseIM
  unregister user host                                                *Unregister a user                                 # 删除一个用户
  user_resources user host                                            List user's connected resources                    # 列举用户的连接资源

Examples:
  mongooseimctl restart
  mongooseimctl --node mongooseim@host restart

Commands to start a MongooseIM node:
  start  Start a MongooseIM node in server mode
  debug  Attach an interactive Erlang shell to a running MongooseIM node
  live   Start MongooseIM node in live (interactive) mode
```
## 示例


备份文件(仅Mnesia)

```
root@9af821c5d811# mongooseimctl backup /backup/mongooseim-backup.20140915.bak
```

删除超时的离线消息

```
bin/mongooseimctl delete_expired_messages
Problem 'error undef' occurred executing the command.
Stacktrace: [{mod_offline,remove_expired_messages,[],[]},
             {ejabberd_admin,delete_expired_messages,0,
                             [{file,"src/ejabberd_admin.erl"},{line,395}]},
             {ejabberd_ctl,call_command,3,
                           [{file,"src/ejabberd_ctl.erl"},{line,323}]},
             {ejabberd_ctl,try_call_command,3,
                           [{file,"src/ejabberd_ctl.erl"},{line,295}]},
             {ejabberd_ctl,process2,3,
                           [{file,"src/ejabberd_ctl.erl"},{line,232}]},
             {ejabberd_ctl,process,1,
                           [{file,"src/ejabberd_ctl.erl"},{line,212}]},
             {rpc,'-handle_call_call/6-fun-0-',5,
                  [{file,"rpc.erl"},{line,205}]}]
```

删除days天之前的离线消息

```
bin/mongooseimctl delete_old_messages 3
Problem 'error undef' occurred executing the command.
Stacktrace: [{mod_offline,remove_old_messages,[3],[]},
             {ejabberd_admin,delete_old_messages,1,
                             [{file,"src/ejabberd_admin.erl"},{line,400}]},
             {ejabberd_ctl,call_command,3,
                           [{file,"src/ejabberd_ctl.erl"},{line,323}]},
             {ejabberd_ctl,try_call_command,3,
                           [{file,"src/ejabberd_ctl.erl"},{line,295}]},
             {ejabberd_ctl,process2,3,
                           [{file,"src/ejabberd_ctl.erl"},{line,232}]},
             {ejabberd_ctl,process,1,
                           [{file,"src/ejabberd_ctl.erl"},{line,212}]},
             {rpc,'-handle_call_call/6-fun-0-',5,
                  [{file,"rpc.erl"},{line,205}]}]
```

导出数据库为文本文件(仅Mnesia)

```
bin/mongooseimctl dump database.txt
```


```
root@9af821c5d811:/root/MongooseIM/rel/mongooseim# bin/mongooseimctl status
The node mongooseim@localhost is started with status: started
MongooseIM version 1.4.0-269-ge00810b is running on that node
```