title: 互联网交互广告技术-规范和实现
date: 2014-07-30 23:43:12
category:
- AD
tags:
- VAST
- VPAID
- IAB
---


> ## 规范

缩写     | 全称                                                     | 中文
-------- | -------------------------------------------------------- | -----
VAST     | Video Ad Serving Template                                | 数字视频广告服务(投放)模板
VPAID    | Video Player Ad Interface Definition                     | 数字视频播放器广告接口定义
VMAP     | Video Multiple Ad Playlist                               | 数字视频多广告播放列表
N/A      | Digital Video Ad Format Guidelines and Best Practices    | 数字视频广告格式指导原则和最佳实践
N/A      | Digital Video In-Stream Ad Metrics Definitions           | 数字视频插播广告指标定义
[IAB][1] | Interactive Advertising Bureau                           | 交互式广告署
Preroll  | 无                                                       | 前置广告: 在视频/电视/电影开始播放内容前的15秒或30秒或60秒广告
MRAID    | Mobile Rich-media Ad Interface Definition                | 移动富媒体广告接口定义

<!-- more -->


> ## 术语解释

- VPAID:
    定义了数字视频`播放器`和广告`服务器`之间的通信数据格式

- VMAP:

- IAB:
    也称为互联网广告署,是支持网络业务流程的开发标准和准则行业团体.通过对网络广告的收入进行跟踪,按季度发布调查报告.
    IAB Video guidelines有六个部分组成:

- 什么是广告单元?
    一个广告单元是在应用程序/Web页面中显示广告的一个区域.一个广告单元可以显示一个或多个广告. 由你决定显示说明类型的广告,广告的外观, 以及显示说明内容类型的广告. 你可以添加不同的广告单元到应用程序/Web页面的不同位置,或者在整个应用程序中使用单个广告单元.

- 什么是数字视频广告服务(投放)模板?

    - [Video Ad Measurement Guidelines (VAMG)][2]
    - [数字视频广告服务(投放)模版 (VAST)][3]
    - [数字视频播放器广告接口定义 (VPAID)][4]
    - [数字视频多广告播放列表 (VMAP)][5]
    - [Digital Video Ad Format Guidelines and Best Practices][6]
    - [Digital Video In-Stream Ad Metrics Definitions][7]

http://msdn.microsoft.com/zh-cn/library/dn387403.aspx


> ## VAST规范的JSON格式(停止开发)


http://www.jsonpvast.org/jsonp-vast-guideline.pdf
http://www.jsonpvast.org/


> ## 开发工具和资源

- [开放视频广告项目][8]
- [Video.js][9] 一个开源HTML5视频播放器,支持VAST插件
- Video.js HTML5播放器VAST插件
    - https://github.com/PetroFrolov/vast-video-js
    - https://github.com/theonion/videojs-vast-plugin
    - http://theonion.github.io/videojs-vast-plugin/
    - http://www.beta-theta.com/test/test/
    - [iOS VAST播放器][10]
    - [HTML5视频VAST插件][11]
- [带商业支持的浏览器插件,支持任何HTML5视频播放器][12]
- [OpenX广告服务器][13]
- http://orbitopenadserver.com/
- http://www.revive-adserver.com/
- [OpenX广告服务器替代品][14]
- [面向发布商的开源移动广告服务器][15]
- [videojs-ads][16]
- [Revive Adserver广告服务器][17]

> ## 数据编码

**对于OpenRTB, 为什么Apache Avro 数据序列化是一个好选择的三个原因**
[http://blog.cloudera.com/blog/2011/05/three-reasons-why-apache-avro-data-serialization-is-a-good-choice-for-openrtb/][18]

> ## 相关技术文章

- [Working with VAST 2.0 External Companion Ads][19]
- [Using the VAST Server Ad Source - Publishers][20]
- [7个好的广告服务器方案][21]
- [学习OpenX广告管理与跟踪系统][22]
- [Openx Handbook Openx Server Optimization And Performance Tuning][23]
- [madserve 移动广告服务器/][24]

> ## 视频播放器支持

- [HTML5播放器列表][25]
- [7款很棒的HTML5视频播放器][26]
- [OVP (Open Video Player) 开放视频播放器][27]

  http://veeso.co/
  http://www.adopsinsider.com/ad-serving/how-does-ad-serving-work/
  网络视频广告 http://wenku.baidu.com/view/1a933b35b90d6c85ec3ac682.html
  图数据库Neo4j https://www.ibm.com/developerworks/cn/java/j-lo-neo4j/
  http://developer.51cto.com/art/201302/381911.htm

  reveal.js 幻灯片
  http://aseemk.com/talks/neo4j-with-nodejs#/1
  [急速拆机视频: 3分钟给iMac换SSD][28]


  [1]: http://www.iab.net
  [2]: http://www.iab.net/guidelines/508676/guidelines/dv_measurement_guidelines
  [3]: http://www.iab.net/guidelines/508676/digitalvideo/vsuite/vast
  [4]: http://www.iab.net/guidelines/508676/digitalvideo/vsuite/vpaid
  [5]: http://www.iab.net/guidelines/508676/digitalvideo/vsuite/vmap
  [6]: http://www.iab.net/guidelines/508676/digitalvideo/DV_Guidelines
  [7]: http://www.iab.net/digitalvideoinstream
  [8]: http://developer.longtailvideo.com/
  [9]: http://www.videojs.com/
  [10]: https://github.com/denivip/ios-vast-player
  [11]: http://blog.denivip.ru/index.php/2012/04/2515/?lang=en
  [12]: http://www.jsvast.com/
  [13]: http://download.openx.org/
  [14]: http://www.zenoviaexchange.com/openx-alternatives-open-source-ad-server-onramp-shuts-down/
  [15]: http://www.madserve.org/
  [16]: https://www.npmjs.org/package/videojs-ads
  [17]: http://www.revive-adserver.com/
  [18]: http://blog.cloudera.com/blog/2011/05/three-reasons-why-apache-avro-data-serialization-is-a-good-choice-for-openrtb/
  [19]: http://support.brightcove.com/en/video-cloud/docs/working-vast-20-external-companion-ads
  [20]: http://support.brightcove.com/en/video-cloud/docs/using-vast-server-ad-source-publishers
  [21]: http://www.sitepoint.com/7-great-ad-server-solutions/
  [22]: http://clientlab.github.io/studio/2014/04/16/openx/
  [23]: http://www.sherin.co.in/openxhandbook/
  [24]: http://www.madserve.org/
  [25]: http://www.reelseo.com/list-html5-video-players/
  [26]: http://www.cnblogs.com/lhb25/archive/2011/06/27/7-great-html-5-video-player-scripts.html
  [27]: http://openvideoplayer.sourceforge.net/
  [28]: http://v.youku.com/v_show/id_XMzMwMDYzODcy.html