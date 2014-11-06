title: 新一代MMO架构摘要
categories:
  - Game Server
tags:
  - Erlang
  - MMO
toc: false
date: 2014-11-01 15:32:01
---
本文阐述了一种MMO架构的实现，架构充分利用了微线程和多核处理器，使服务器能够承载更多的玩家。本方案使用erlang实现服务器集群，并将游戏世界实现分区。

## 介绍

MMOG（大型多人在线游戏）让数千计甚至数以百万计的人玩同一个游戏。新一代MMO 将提供一个高动态的游戏设计。让游戏设计者的设计充分自由将提升了技术门槛。

Achterbosch,皮尔斯,西蒙斯(2008)，对下一代MMO期望是什么这儿问题调查了122人。