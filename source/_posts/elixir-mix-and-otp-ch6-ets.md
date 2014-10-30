title: Elixir Mix 和 OTP - ETS
categories:
  - Elixir
tags:
  - Mix
  - OTP
  - ETS
toc: true
date: 2014-10-31 01:16:12
---

## ETS

每次我们需要查询一个`bucket`, 我们需要发送一个消息给`registry`. 在有些应用程序中这意味着`registry`也许变成瓶颈.

本章中,我们就爱那个学习ETS(Erlang Term Storage),以及如何把它用作一个缓存机制. 稍后我们将扩展其用途,持久化从监视进程到子进程的持久化数据,即使是在崩溃的时候.

> Warning! Don't use ETS as a cache prematurely! Log and analyze your application performance and identify which parts are bottlenecks, so you know whether you should cache, and what you should cache. This chapter is merely an example of how ETS can be used, once you've determined the need.

### 作为缓存的ETS
