title: Elixir 扩展Mix
categories:
  - Elixir
tags:
  - Mix
toc: false
date: 2014-11-02 23:48:18
---
本文讲述, 如何通过开发一个简单的自定义Mix任务扩展来增强mix的功能

    defmodule Mix.Tasks.MyTask do
        use Mix.Task
        def run(args) do
            IO.puts "Hello World!"
        end
    end

运行

    mix my_task

一个基本的扩展任务就是这样, 下面讲一个具体的有用的示例