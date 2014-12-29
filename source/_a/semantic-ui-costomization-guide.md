title: Semantic UI 自定义指南
categories:
  - Web
tags:
  - Semantic UI
toc: false
date: 2014-12-21 01:44:41
---

## 简介

## 设置全局变量

## 自定义UI元素

Any changes to UI elements which are consistent across your website should be applied as a site theme on top of Semantic UI.

对任何UI元素的一致性变更,应该在Semantic UI`站点主题`层次上修改.

Site themes allow UI definitions to directly adjust the compiled CSS of Semantic,
avoiding redundant CSS that redefines what is set in the framework.
Each UI element has an individual override and variable file inside your site theme folder.
Override files are used for providing additional CSS rules which modify the baseline definition.
Variable files allow you to modify the many underlying variables of Semantic.
Any arbitrary part of a definition is defined as a variable. In semantic there are over 3000 variables
for customizing elements, and these provide the best means for consistently adjusting elements.

