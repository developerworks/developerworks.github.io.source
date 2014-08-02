title: 图片底部字幕效果
tags:
  - html
categories:
  - 前端
date: 2014-08-02
---

### 要点:

- 一个容器DIV, 类名称为`caption-wrapper`
- `caption-wrapper`中包含两个子元素 `img` 和 `div.caption-description`
- `div.caption-description` 中包含一个`p.caption-text`

### 效果

![图片底部字幕效果][1]

<!-- more -->

### HTML结构

```
<div class="caption-wrapper">
    <img src="../public/images/u311_normal.jpg" alt="" style="width: 800px;"/>
    <div class="caption-description">
        <p class="caption-text">图片底部字幕组件</p>
    </div>
</div>
```

### LESS

**overlay-caption.less**

```
.caption-wrapper{
    float:left;
    position:relative;
    .caption-description {
        position:absolute;
        bottom:0px;
        left:0px;
        width:100%;
        background-color:black;
        font-family: 'tahoma';
        font-size:15px;
        color:white;
        opacity:0.5;
        filter:alpha(opacity=60); // IE transparency
        .caption-text {
            padding:10px;
            margin:0px;
        }
    }
}
```

### So easy, right ?

  [1]: /images/overlay-caption.png