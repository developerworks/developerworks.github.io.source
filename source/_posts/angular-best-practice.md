title: Angular 最佳实践
tags:
  - angular
categories:
  - javascript
date: 2014-06-28
---

1. 控制器和`Provider`(Service,Factory)用于修改数据模型,**而不是HTML**
2. HTML和指令定义布局,以及到模型的绑定
3. 如果要在控制器之间共享数据,那么创建一个服务或工厂 - 它们是能在整个应用程序生命周期内共享的单件对象
4. 如果你需要一个HTML Widget, 应该创建一个指令
5. 如果你有一些数据,并且需要更新HTML,NO! NO! NO!, 更新模型就可以了,保证HTML绑定到模型就可以了.


    TODO::后续有发现继续增加
