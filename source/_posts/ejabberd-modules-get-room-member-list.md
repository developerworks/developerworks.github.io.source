title: Ejabberd模块 - 获取房间成员列表
categories:
  - Ejabberd
tags:
  - XMPP
  - ejabberd
  - muc
toc: false
date: 2014-09-30 01:01:37
---

上一篇[Ejabberd 14.x版本系列模块开发过程中遇到的坑][3],一个很坑的移植过程,没找到相关的升级文档,全靠读代码. -_-!

本模块用于获取一个房间内的所有成员, 代码在[这里][1], 本模块修改自[获取房间的用户列表模块][2], 兼容`ejabberd 14.x`系列版本.

## Bugfix

1. `muc_room`表结构有变化, 下面是输出的日志,为了便于阅读,我在每个元素前附加序号作为前缀

```
[{muc_room,
    {<<"test">>,<<"conference.xmpp.myserver.info">>},
    [
        1  {title,<<232,129,138,229,164,169,229,174,164,230,160,135,233,162,152>>},
        2  {description,<<232,129,138,229,164,169,229,174,164,230,143,143,232,191,176>>},
        3  {allow_change_subj,true},
        4  {allow_query_users,true},
        5  {allow_private_messages,true},
        6  {allow_private_messages_from_visitors,anyone},
        7  {allow_visitor_status,true},
        8  {allow_visitor_nickchange,true},
        9  {public,true},
        10 {public_list,true},
        11 {persistent,true},
        12 {moderated,true},
        13 {members_by_default,true},
        14 {members_only,false},
        15 {allow_user_invites,false},
        16 {password_protected,false},
        17 {captcha_protected,false},
        18 {password,<<>>},
        19 {anonymous,true},
        20 {logging,false},
        21 {max_users,200},
        22 {allow_voice_requests,true},
        23 {voice_request_min_interval,1800},
        24 {vcard,<<>>},
        25 {captcha_whitelist,[]},
        26 {affiliations,[
            {
                {<<"hezhiqiang">>,<<"xmpp.myserver.info">>,<<>>},
                {owner,<<>>}
            }
        ]},
        27 {subject,<<>>},
        28 {subject_author,<<>>}
    ]}]
```

其中元素`affiliations`位于房间配置列表第26个元素,和[原文<<获取房间的用户列表模块>>][2]不同

## 补充

忘了一件事, 需要在`$EJABBERD/tools/xmpp_codec.spec`增加下列代码:

```
-xml(room_member_item,
     #elem{name = <<"item">>,
           xmlns = <<"http://jabber.org/protocol/muc#member-list">>,
           result = '$jid',
           attrs = [#attr{name = <<"jid">>,
                          required = true,
                          dec = {dec_jid, []},
                          enc = {enc_jid, []}}]}).
-xml(room_member,
     #elem{name = <<"query">>,
           xmlns = <<"http://jabber.org/protocol/muc#member-list">>,
           result = {room_member, '$items'},
           refs = [#ref{name = room_member_item,
                        label = '$items'}]}).
```

并执行`make spec`更新, 然后再 `make && make install`, 最后 `ejabberdctl restart`

  [1]: https://gist.github.com/developerworks/798d67182b38eda72e25
  [2]: http://blog.zlxstar.me/blog/2013/07/21/dicorvery-user-muclist
  [3]: /2014/09/29/ejabberd-modules-pitfalls

## 参考资料

1. http://blog.zlxstar.me/blog/2013/07/21/dicorvery-user-muclist
2. http://www.ibm.com/developerworks/cn/opensource/os-erlang1
3. Erlang标准库函数 `lists:any/2`
http://www.erlang.org/doc/man/lists.html#any-2
4. Erlang标准库函数 `lists:nth/2`
http://www.erlang.org/doc/man/lists.html#nth-2