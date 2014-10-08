title: Blockies - Javascript生成随机头像
date: 2014-08-01
categories:
  - Javascript
tags:
  - blockies
---

`Blockies` 是一个随机生成头像的javascript库, 基于随机数和色彩


<!-- more -->


## 项目地址

https://github.com/download13/blockies


## 示例代码

```
<html>
<head>
    <meta charset="utf-8">
    <style>
        canvas {
            margin: 10px;
        }
    </style>
</head>
<body>
<div id="blockies-identicon"></div>
<script src="/code/blockies/blockies.min.js"></script>
<script>

var icons_container = document.getElementById("blockies-identicon")
for (var i = 0; i < 10; i++) {
    var icon = blockies.create();
    icon.style.margin = "10px";
    document.body.appendChild(icon);
    icons_container.appendChild(icon);
}
</script>
</body>
</html>
```

## 显示效果

下面的例子是javascript生成的, 源码可以看 [这里][1]


{% raw %}
<div id="blockies-identicon"></div>
<script src="/code/blockies/blockies.min.js"></script>
<script>

var icons_container = document.getElementById("blockies-identicon")
for (var i = 0; i < 10; i++) {
    var icon = blockies.create();
    icon.style.margin = "10px";
    icons_container.appendChild(icon);
}
</script>
{% endraw %}

  [1]: /code/blockies/index.html
