title: ElixirSublime 代码补全功能的异常问题.
categories:
  - Elixir
tags:
  - Sublime
toc: false
date: 2015-01-08 15:50:36
---

最开始的问题是:

```shell
Traceback (most recent call last):
  File "/Applications/Sublime Text.app/Contents/MacOS/sublime_plugin.py", line 358, in on_query_completions
    res = callback.on_query_completions(v, prefix, locations)
  File "/Users/user/Library/Application Support/Sublime Text 3/Packages/ElixirSublime/elixir_sublime.py", line 270, in on_query_completions
    if not session.send('COMPLETE', expand_selection(view, locations[0], aliases=aliases)):
  File "/Users/user/Library/Application Support/Sublime Text 3/Packages/ElixirSublime/elixir_sublime.py", line 189, in send
    self.socket.send(str.encode(cmd))
AttributeError: 'NoneType' object has no attribute 'send'
Traceback (most recent call last):
  File "/Applications/Sublime Text.app/Contents/MacOS/sublime_plugin.py", line 311, in on_activated_async
    callback.on_activated_async(v)
  File "/Users/user/Library/Application Support/Sublime Text 3/Packages/ElixirSublime/elixir_sublime.py", line 255, in on_activated_async
    self.on_load_async(view)
  File "/Users/user/Library/Application Support/Sublime Text 3/Packages/ElixirSublime/elixir_sublime.py", line 260, in on_load_async
    ElixirSession.ensure(os.path.dirname(filename))
  File "/Users/user/Library/Application Support/Sublime Text 3/Packages/ElixirSublime/elixir_sublime.py", line 160, in ensure
    session.connect()
  File "/Users/user/Library/Application Support/Sublime Text 3/Packages/ElixirSublime/elixir_sublime.py", line 179, in connect
    self.socket, _ = _socket.accept()
  File "./socket.py", line 135, in accept
socket.timeout: timed out
```

ElixirSublime日志

```
<_io.TextIOWrapper name='/var/folders/m3/vn1yyrfx7l36cq6p8msc6wt80000gn/T/ElixirSublime.log' mode='w' encoding='US-ASCII'>
```

通过Sublime的日志文件看到ElixirSublime使用了一个Mix项目`~/Library/Application Support/Sublime Text 3/Packages/ElixirSublime/sublime_completion`,
其中需要的依赖`poison`没找到, 我就进入这个执行了一下:

```
mix deps.get
```

结果又冒出一个证书问题.

![hex.pm的证书不可信](/assets/elixir/hex.im-certificate-not-trusted.jpg)

上述问题, 请参考[Elixir 在Mac OS X上安装Hex.pm服务器证书不信任的问题](/2015/01/08/elixir-hex-certificate-not-trusted)