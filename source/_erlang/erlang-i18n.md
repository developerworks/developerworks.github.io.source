title: Erlang 字符串国际化
categories:
  - Erlang
tags:
  - i18n
toc: false
date: 2014-09-26 00:21:33
---


*e_lang* 模块提供了一个便捷的方法来显示应用程序的多语言UI.

## 概述

The bigger your web service is, the more attention you should pay to make it more accessible to other users. One of the main barriers is language. Because of the MVC model we split the application into logic, behavior and view, but there is sometimes a need for a another partition.

In creating a multi-lingual service, you only need to focus on changing the content you want to display - the view will always remain the same. e_lang provides a very convenient API to keep the same views and only switch the dictionary the application uses for each request.

e_lang has been built upon key-value mapping. The basic idea is to use only the keys in your application instead of language-specific elements and place all of the translations in the separate files.

The keys for translation used within your application are strings, which can be parsed in two different ways:

- string contains : - string is split by : character and the actual key is a tuple of string - tokens created by splitting the original string
- string does not contain : - string is the key itself

语言文件在应用启动的时候读入内存,任何对其的修改都需要重新加载,这通过调用下面的函数完成:

```
e_lang:reinstall().
```

## 语言定义

语言转换过程的第一步是修改`project.conf`文件中国际化相关的配置.

```
{language files, [LanguageFileSpec]}.
%% LanguageFileSpec 为一个二元组
%% LangCode为语言代码,类型为atom()
%% PathToTranslationFile为语言文件,类型为string()
LanguageFileSpec = {LangCode, PathToTranslationFile}
LangCode = atom()
PathToTranslationFile = string()
```

创建在`project.conf`中指定的所有文件,语言文件实际上市每行定义了这样一个格式的元组

```
{Key, Translation}
```

其中`Key`可以是一个字符串或者字符串元组

## 使用

有三种方式语言文件中的字符串资源:

- 在HTML文件中只用`<wpart:lang key=Key />`在标记展开的过程中,标记将被实际定义在语言文件中的字符串替换.
- 带一个`key`作为参数在程序中调用`wpart_lang:get_translation`
- 在`.hrl`记录定义文件中的描述选项中使用元组`{key, Key}`

## 控制流

所有的翻译类型 (使用`wpart:lang`,在`.hrl`文件中指定描述, 以及直接调用翻译函数) 都有相同的控制流程

1. 保存在会话中的`e_dict`字典中的 lang key
2. `project.conf`中的`default_language`选项
3. 如果未设置上面两个任意一个选项,`e_lang`设置默认语言为英语 (语言代码为`en`)

在应用程序中使用的语言代码必须和定义在`project.conf`配置文件中的语言代码一致.

语言翻译过程如下:

1. 如果发现给定key的语言字符串,直接返回
2. 否则,如果key是一个字符串而非一个元组,返回key
3. 其他任何情况,返回字符串"no translation found"

![语言文件处理流程][1]

## 示例

嵌入到HTML

```
...
<h1><wpart:lang key="contact:header"/></h1>
erlangweb@example.org<br/>
<wpart:lang key="back"/>
...
```

此例中使用了两个翻译字符串, 一个是 *{"contact", "header"}* 另一个是 *back*.

get_translation 调用

```
...
ErrorMsg = wpart_lang:get_translation("errors:no_such_login"),
...
```

"ErrorMsg":http://wiki.erlang-web.org/ErrorMsg 变量将被绑定到对应的`{"errors", "no_such_login"}`键.

在记录文件中的描述选项

```
...
-record(login_types,
        {login = {string, [{description, {key, "login:login"}}]},
         password = {password, [{description, {key, "login:password"}}]}}).
...
```

login_types.login 和 #login_types.password 分别和`{"login", "login"}`以及`{"login", "password"}`对应.

语言配置文件 `en.conf`

```
{{"contact", "header"}, "My contact details"}.
{"back", "Go Back"}.
{{"errors", "no_such_login"}, "There is no such user in the system"}.
{{"login", "login"}, "Login"}.
{{"login", "password"}, "Password"}.
```

  [1]: /assets/images/e_lang.jpg