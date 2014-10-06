title: XMPP XEP-0050 Ad-Hoc Commands
categories:
  - Communication
tags:
  - xmpp
toc: false
date: 2014-10-04 23:20:37
---


## 简介
### 动机
### 概念
### 先决条件
## 用例
### 发现支持
### 获取命令列表
### Announcing the Command List
### 执行命令
#### 简单扩展
#### 多阶段
#### 取消
## Implementation Notes
### Defined/Required Command Nodes
### Command Nodes
### 会话生命期

The execution of a command exists within the concept of a session.
Each session is identified by the 'sessionid' attribute, and SHOULD be
valid only between one requester/responder pair.
The responder is responsible for determining the session lifetime, with some help from the requester.

命令的执行存在一个会话概念中. 每个会话由`sessionid`属性作为标识, 并且`应该`仅在`requester/responder`成对时有效.
依赖于从`requester`得到的某些帮助,`responder`负责决定会话的生命期.


The requester starts a new session for a command by simply sending a <command/> with the 'node'
attribute (and optionally the 'status' attribute with a value of "execute").
Once the 'sessionid' attribute is given to the requester, it is the requester's responsibility
to maintain it for the session's lifetime. A session ends when the responder sends a
<command status='completed'/> or the requester sends a <command action='cancel'/> with the provided
'sessionid' value.

`requester`通过发送一个带有`node`属性的`<command/>`元素启动一个新的会话.(可选的,可以设置一个值为`execute`的`status`属性).
一定`sessionid`属性返回给`requester`,维护会话的生命期就成为了`requester`的责任.当`requester`发送一个带有`sessionid`属性的`<command status='completed'/>`
时或`<command action='cancel'/>`时,会话终止.

Once a session has ended, its 'sessionid' value SHOULD NOT be used again. It is the responder's responsibility to ensure that each 'sessionid' value is unique.

一旦会话终止, 其`sessionid`值`不应该`再被使用. 保证每一个`sessionid`的唯一性是`responder`的责任.

It may be possible for a requester to be executing more than one session of the same command with a given responder.
If the responder does not allow more than one session of the same command with the same requester,
the responder MUST return a <not-allowed/> error (see Error Condition Mappings (XEP-0086) [8]).


对于一个给定的`responder`,同一命令在多与一个会话中执行是可能的, 如果`responder`不允许来自同一`requester`的同一命令被执行多次, `responder`必须返回一个`<not-allowed/>`错误(参考 [Error Condition Mappings (XEP-0086)][1]).


### Command Actions

The result for each stage (other than the last) of a command's execution SHOULD include an <actions/> element.

The user-agent can use this information to present a more-intelligent user interface, such as a "druid" or "wizard".

一个命令的执行的每个阶段的结果(不是最后一个)应该包含一个`<actions/>`元素. 用户代理可以使用此信息展示一个更加智能的用户界面, 比如

For a user-agent, a typical interpretation of the `<actions/>` information (or lack thereof) would be the following:

1. The action `cancel` is always allowed.
2. If there is no `<actions/>` element, the user-agent can use a single-stage dialog or view.
    - The action `execute` is equivalent to the action `complete`.
3. If there is an `<actions/>` element, the user-agent usually uses a multi-stage dialog or view, such as a wizard.
    - The action `execute` is always allowed, and is equivalent to the action `next`.
    - The `prev` action is typically the `back` or `previous` button or option in a wizard. If `<prev/>` is not contained by the `<actions/>`, it is disabled.
    - The `next` action is typically the `next` button or option in a wizard. If `<next/>` is not contained by the `<actions/>`, it is disabled.
    - The `complete` action is typically the `finish` or `done` button or option in a wizard. If `<complete/>` is not contained by the `<actions/>`, it is disabled.
    - If the `<actions/>` possesses the `execute` attribute, that value is the default button or option. If the `<actions/>` does not possess the `execute` attribute, there is no default button or option.



### Command Payloads
#### 使用数据表单
### 命令成功/失败
### 国际化和本地化
## Formal Description
### <command/> Element
### <actions/> Element
### <note/> Element
### 可能的错误
## 安全考虑
## IANA Considerations
## XMPP登记处考虑
### 协议名称空间
### 服务发现标识
### Well-Known Service Discovery Nodes
### URI Query Types
## XML Schema



## Ejabberd Adhoc Command 模块分析

## Erlang记录


  [1]: http://xmpp.org/extensions/xep-0086.html

```
%% $Ejabberd/include/adhoc.hrl
%% 命令请求记录
-record(adhoc_request,
{
    lang = <<"">>      :: binary(),
    node = <<"">>      :: binary(),
    sessionid = <<"">> :: binary(),
    action = <<"">>    :: binary(),
    xdata = false      :: false | xmlel(),
    others = []        :: [xmlel()]
}).
%% 命令响应记录
-record(adhoc_response,
{
    lang = <<"">>          :: binary(),             %% 所使用的自然语言
    node = <<"">>          :: binary(),             %% 节点名称
    sessionid = <<"">>     :: binary(),             %% 会话ID
    status                 :: atom(),               %% 状态
    defaultaction = <<"">> :: binary(),             %% 描述
    actions       = []     :: [binary()],           %% 动作列表
    notes         = []     :: [{binary(), binary()}],   %% Notes
    elements      = []     :: [xmlel()]             %% 元素
}).

-type adhoc_request() :: #adhoc_request{}.
-type adhoc_response() :: #adhoc_response{}.
```




