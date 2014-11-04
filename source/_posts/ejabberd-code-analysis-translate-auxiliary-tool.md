title: Ejabberd代码分析之-翻译辅助工具
categories:
  - Communication
  - Ejabberd
tags:
  - ejabberd
  - analysis
toc: false
date: 2014-10-06 19:20:27
---


Ejabberd支持国际化,在`<iq/>`,`<message/>`,`<presence/>`等元素上添加`xml:lang`属性,对应的英文文本会切换到指定的语言.

举个例子,模块`mod_announce.erl`代码内有如下对`translate:translate()`的使用:

```
get_title(Lang, <<"announce">>) ->
    translate:translate(Lang, <<"Announcements">>);
get_title(Lang, ?NS_ADMIN_ANNOUNCE_ALL) ->
    translate:translate(Lang, <<"Send announcement to all users">>);
get_title(Lang, ?NS_ADMIN_ANNOUNCE_ALL_ALLHOSTS) ->
    translate:translate(Lang, <<"Send announcement to all users on all hosts">>);
get_title(Lang, ?NS_ADMIN_ANNOUNCE) ->
    translate:translate(Lang, <<"Send announcement to all online users">>);
get_title(Lang, ?NS_ADMIN_ANNOUNCE_ALLHOSTS) ->
    translate:translate(Lang, <<"Send announcement to all online users on all hosts">>);
get_title(Lang, ?NS_ADMIN_SET_MOTD) ->
    translate:translate(Lang, <<"Set message of the day and send to online users">>);
get_title(Lang, ?NS_ADMIN_SET_MOTD_ALLHOSTS) ->
    translate:translate(Lang, <<"Set message of the day on all hosts and send to online users">>);
get_title(Lang, ?NS_ADMIN_EDIT_MOTD) ->
    translate:translate(Lang, <<"Update message of the day (don't send)">>);
get_title(Lang, ?NS_ADMIN_EDIT_MOTD_ALLHOSTS) ->
    translate:translate(Lang, <<"Update message of the day on all hosts (don't send)">>);
get_title(Lang, ?NS_ADMIN_DELETE_MOTD) ->
    translate:translate(Lang, <<"Delete message of the day">>);
get_title(Lang, ?NS_ADMIN_DELETE_MOTD_ALLHOSTS) ->
    translate:translate(Lang, <<"Delete message of the day on all hosts">>).
```

`translate:translate()`定义在`translate.erl`模块中.

```
-spec translate(binary(), binary()) -> binary().
translate(Lang, Msg) ->
    LLang = ascii_tolower(Lang),
    case ets:lookup(translations, {LLang, Msg}) of
      [{_, Trans}] -> Trans;
      _ ->
	  ShortLang = case str:tokens(LLang, <<"-">>) of
			[] -> LLang;
			[SL | _] -> SL
		      end,
	  case ShortLang of
	    <<"en">> -> Msg;
	    LLang -> translate(Msg);
	    _ ->
		case ets:lookup(translations, {ShortLang, Msg}) of
		  [{_, Trans}] -> Trans;
		  _ -> translate(Msg)
		end
	  end
    end.
```

当你在XML节上定义了`xml:lang='zh'`属性后,服务端的应答文本就变为中文了, 如下:

![Erlang国际化文本-服务节点列表][1]

## 增加自定义字符串

搞清楚了原理后,我就可以为我的模块添加字符串了.

首先我在我的模块中使用英文作为默认的文本描述, 如下所示:

```
#xmlel{
    name = <<"item">>,
    attrs = [
        {<<"jid">>, Server},
        {<<"node">>, <<"http://www.example.com/protocol/messager#upgrade-full">>},
        {<<"name">>, translate:translate(Lang, <<"Make a complete upgrade">>)}
    ],
    children = []
}
```

然后根据`$EJABBERD_SRC/contrib/extract_translations/README`的描述,
需要编译`$EJABBERD_SRC/contrib/extract_translations/extract_translations.erl`模块.

```
erlc $EJABBERD_SRC/contrib/extract_translations/extract_translations.erl
```

Ejabberd提供了一个SHELL脚本来从`*.po`文件生成Ejabberd需要的`*.msg`文件.

```
root@4850618fe551:~/ejabberd/contrib/extract_translations# ./prepare-translation.sh -h
Options:
  -langall
  -lang LANGUAGE_FILE
  -srcmsg2po LANGUAGE   Construct .msg file using source code to PO file
  -src2pot              Generate template POT file from source code
  -popot2po LANGUAGE    Update PO file with template POT file
  -po2msg LANGUAGE      Export PO file to MSG file
  -updateall            Generate POT and update all PO
Example:
  ./prepare-translation.sh -lang es.msg
```

首先切换到源码根目录, 执行:

```
root@4850618fe551:~/ejabberd# ./contrib/extract_translations/prepare-translation.sh -src2pot
./contrib/extract_translations/prepare-translation.sh: line 183: msguniq: command not found
```

找不到`msguniq`命令,在Ubuntu上,`msguniq`在`gettext`包种, 下面安装需要的软件包:

```
aptitude install -y gettext
```

执行下面的命令, 更新`ejabberd.pot`文件:

```
root@4850618fe551:~/ejabberd# ./contrib/extract_translations/prepare-translation.sh -src2pot
```

打开`$EJABBERD_SRC/priv/msgs/ejabberd.pot`, 翻译相关条目后, 复制粘贴到到`$EJABBERD_SRC/priv/msgs/zh.po`中,

![$EJABBERD_SRC/priv/msgs/ejabberd.pot][2]

并执行下面的命令,更新我的`zh.msg`文件, 这个文件是在Erlang代码中直接使用的文件. 后面会看到最终的效果.

```
./contrib/extract_translations/prepare-translation.sh -po2msg zh
```

更新后的消息文件在`$EJABBERD_SRC/priv/msgs/zh.msg`

![$EJABBERD_SRC/priv/msgs/zh.msg][3]

生成了我最终要的`zh.msg`文件后,重新编译一次ejabberd(之前已经编译过,这一步很快)

```
root@4850618fe551:~/ejabberd# make
/usr/lib/erlang/bin/escript rebar skip_deps=true compile
==> rel (compile)
==> ejabberd (compile)
Compiled src/mod_online_users.erl
Compiled src/mod_gbox_messager.erl
Compiled src/mod_cputime.erl
Compiled src/mod_system_information.erl
```

复制编译的新文件

```
cp ebin/*.beam /lib/ejabberd/ebin
cp priv/*.msg /lib/ejabberd/priv/msgs
```

最后重启,并测试

```
ejabberdctl restart
```

查询命令列表(注意属性`xml:lang='zh'`)

```
<iq type='get' to='xmpp.myserver.info' from='root@xmpp.myserver.info' xml:lang='zh' xmlns='jabber:client'>
  <query xmlns='http://jabber.org/protocol/disco#items' node='http://jabber.org/protocol/commands'/>
</iq>
```

国际化后的结果

![自定义Ad-Hoc Commands][4]

## 参考资料

1. $EJABBERD_SRC/src/mod_announce.erl
2. $EJABBERD_SRC/contrib/extract_translations/README
3. $EJABBERD_SRC/src/translate.erl

  [1]: /assets/images/D373E553-9009-48E3-A9C3-5FD33D7C7080.png
  [2]: /assets/images/4BD11A32-C9E4-4348-AD5A-60061B8998A5.png
  [3]: /assets/images/95BD1DE1-C5C4-47AB-BB6E-B94A3ADF2811.png
  [4]: /assets/images/1BD77F60-BE7B-48CF-B1BE-1933F3D9D839.png


