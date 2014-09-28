title: Erlang 尾递归
categories:
  - Erlang
tags:
  - recursion
toc: false
date: 2014-09-26 00:32:22
---

![Elrang尾递归和PHP的for循环][1]

```
for(0)->
        ok;
for(N)->
    io:format("running time: ~p ms ~n",
        [merle:getkey("test")]),
    for(N-1).
```

另一种方式,采用列表,通过模式匹配`[Head|Tail]`的形式

{% raw %}
```
processWords(Words,N) ->
  case Words of
    [] -> ok;
    [Word|Rest] ->
      if length(Word) > 3 ->
        Normalise = string:to_lower(Word),
        ets:insert(indexTable,{{Normalise , N}});
    true -> ok
  end,
  processWords(Rest,N)
end.
```
{% endraw %}

  [1]: /assets/images/erlang-php-for.png
