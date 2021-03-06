title: 广告统计参数和指标
tags:
  - AD
categories:
  - AD
date: 2014-07-30 23:45:12
---


> ### 点击率

来自Google的定义

点击率是指您的广告所获得的点击次数除以其展示次数所得的比值。

    简介：点击率是指用户看到广告后点击该广告的频率：
    点击率 = 广告点击次数
    展示次数（广告获得的观看次数）
    示例：如果您获得了 5 次点击和 1000 次展示，点击率就是 0.5%。

下面的链接是Google关于CTR的解释
https://support.google.com/adxbuyer/answer/1082461?hl=zh-Hans


广告请求点击率 (CTR) 的计算方法是：用广告点击次数除以广告请求的数量.
```
Ad request CTR = Clicks / Ad requests
```
例如,如果您通过 `1000` 次广告请求获得了 `7` 次点击,则您的广告请求 `CTR 为 0.7%.(7/1000 = 0.7%)`

**`点击率`** 来自于英文 **`Click-through Rate`**（点进率） 以及 **`Clicks Ratio`**（点击率）,是指网站页面上某一内容被点击的次数与被显示次数之比,即 `clicks/views`,它是一个百分比.反映了网页上某一内容的受关注程度,常常用来衡量广告的吸引程度.

$$
点击率(CTR) = \frac{点击数(Clicks)}{查看数(Views)}*100 \%
$$


<!-- more -->

> ### 如何计算广告请求数

每当您的网站请求要展示的广告时,我们就会记录一次广告请求.该指标体现的是请求了广告（针对内容广告）或搜索查询（针对搜索广告）的广告单元的数量.每当您的`广告单元`发出一次请求,我们就会报告一次广告请求.即使系统未返回任何广告,而只是展示了备用广告,情况也是如此.

> ### 展示次数份额 (Impressions Share)

http://www.gugedailishang.com/zhanghu/11215.htm


> ### 广告填充率(Fill Rate)

#### 填充率公式

为了计算填充率，你需要用广告的总展示量除以广告的总请求数，广告展示数量会因为网络问题,网站本身的问题导致丢失广告展示的机会, 计算公司如下所示：

$$
填充率 = \frac{广告展示数}{广告请求数} * 100\%
$$

导致填充率低下的因素:

- 服务器问题
    - 服务器不稳定,带宽占用过高
- 网络问题
    - 用户端网络问题
    - 用户访问的网站网络速度过慢,还未完整打开整个页面,无法忍受就关闭了页面

#### 提高填充率的优化方式

1. (应该)优化代码,提供Javascript脚本加载速度
2. (应该)压缩CSS,Javascript和HTML文件
3. (可选)有可能的情况下,图片可以进行base64编码,

```
<img src="data:image/jpg;base64,...">
```

4. (应该)使用CDN
5. (应该)使用Javascript异步加载,防止阻塞页面渲染

> ### 改善填充率需要的统计数据

1. 广告投放要么所属的网站网络质量
2. 目标网站的页面加载时间

> ### 转化率 (Conversion Rate)

广告转化率

    是用来反映网络广告对产品销售情况影响程度的指标，主要是指受网络广告影响而发生购买、
    注册或信息需求行为的浏览者占总广告点击人数的比例。

$$
转化率 = \frac{发生转化的广告浏览者人数}{点击广告的总人数} * 100\%
$$

电子商务转化率

    假设一个店铺, 本月有10000人到达店铺, 但只有581人购买了商铺,
    我们称购买人数在总到达人数中的占有比例成为转化率.

$$
转化率 = \frac{产生购买行为的人数}{所有到达访客人数} * 100\%
$$

#### 转化率计算公式





> ### 广告战役(Ad Campaign)

广告战役指在某一特定市场上为实现某一重大目标所集中进行的大规模的广告活动,是广告决战思想的一种体现,是企业之间进行市场竞争的策略之一.
http://wiki.mbalib.com/wiki/%E5%B9%BF%E5%91%8A%E6%88%98%E5%BD%B9


<script type="text/x-mathjax-config">
  MathJax.Hub.Config({
    tex2jax: {
      inlineMath: [ ['$','$'], ["\\(","\\)"] ],
      processEscapes: true
    }
  });

</script>
<script type="text/x-mathjax-config">
    MathJax.Hub.Config({
      tex2jax: {
        skipTags: ['script', 'noscript', 'style', 'textarea', 'pre', 'code']
      }
    });

</script>
<script type="text/x-mathjax-config">
    MathJax.Hub.Queue(function() {
        var all = MathJax.Hub.getAllJax(), i;
        for(i=0; i < all.length; i += 1) {
            all[i].SourceElement().parentNode.className += ' has-jax';
        }
    });

</script>
<script type="text/javascript" src="http://cdn.mathjax.org/mathjax/latest/MathJax.js?config=TeX-AMS-MML_HTMLorMML"></script>


