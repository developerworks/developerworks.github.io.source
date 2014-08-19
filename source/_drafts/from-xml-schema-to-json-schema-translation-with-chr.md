title: 从XML模式转换到JSON模式 - 使约束处理规则(CHR)
categories:
  - json
tags:
  - json-schema
  - xsd
date: 2014-08-19 12:21:45
---


## 译序

> 这是一篇论文翻译([原文在这里][1]),这是我在寻找一种能够把XML模式直接转换为JSON模式相关的工具时发现的一篇论文, 作者是康奈尔大学的Falco Nogatz和Thom Frühwirth, 写作时间是2014年6月9日.

> 我为什么需要把XML模式转换为JSON模式? 我在研究一种方法, 能够通过XML模式直接生成数据库DDL.能够通过XML模式生成JSON模式,

> 通过JSON模式生成应用程序的CURD操作代码,以及用于执行CURD操作需要的UI界面.同时,还可以通过JSON模式文档生成基于REST的API借口代码. 提高在开发一个新Web项目时的自动化水平.



**摘要**. 尽管作为一种数据格式,特别是在Web服务方面,围绕Javscript Object Notation的软件生态系统并没有像XML一样被广泛的使用.对于这两种数据格式,都有一个存在的模式语言来指定实例文档的结构, 但是却没有方法将现有的XML模式文档转换为同等的JSON模式.

在本文中, 我们会介绍一个语言转换器实现. 它把XML模式文档作为输入,并创建等同的JSON模式文档. 我们的方式是基于`Prolog`和`CHR`. 通过展开XML模式文档为CHR约束, 用一个声明性的方式,制定具体的转换规则变得可行.

**关键字**: 约束处理规则, 语言转换器, XML模式, XSD,JSON模式

<!-- more -->

## 1. 简介

## 2. 准备工作

### 2.1 问题定义
### 2.2 问题实例

### 2.3 CHR约束

## 3. 转换过程

### 3.1 读入XML模式到Prolog
### 3.2 XML平面化 (XML Flattening)
### 3.3 设置默认值 (Setting Defaults)
### 3.4 片段转换 (Fragment Translation)

### 3.5 包装 JSON 模式
### 3.6 清理和JONS输出

## 4 结语

## 参考资料

1. Tim Bray and Jean Paoli and C Michael Sperberg-McQueen and Eve Maler and Fran ̧cois Yergeau: Extensible markup language (XML). World Wide Web Journal volume 2, number 2, pages 27–66 (1997)
2. XML Schema, Structures Part. World Wide Web Consortium (W3C), Recommen- dation October 2004), http://www.w3.org/TR/xmlschema-1 (2004)
3. Kris Zyp: A JSON Media Type for Describing the Structure and Meaning of JSON Documents (Draft 04). IETF Internet-Draft, http://tools.ietf.org/html/ draft-zyp-json-schema-04 (2013)
4. David Lee: JXON: an architecture for schema and annotation driven JSON/XML bidirectional transformations. Balisage: The Markup Conference, Balisage Series on Markup Technologies, volume 7 (2011)
5. JSON Schema and Hyperschema: Core/Validation Meta-Schema. http:// json-schema.org/schema (2013)
6. Senthil Nathan and Edward J Pring and John Morar: Convert XML to JSON in PHP. http://www.ibm.com/developerworks/xml/library/x-xml2jsonphp/ (2007)
7. Allen Brown and Matthew Fuchs and Jonathan Robie and Philip Wadler: XML Schema: Formal Description. W3C Working Draft, volume 25, pages 1–25 (2001)
8. Thom Fru ̈hwirth: Constraint Handling Rules. Cambridge University Press (2009)
9. Biron, Paul, Ashok Malhotra, and World Wide Web Consortium: XML schema part 2: Datatypes. World Wide Web Consortium Recommendation REC-xmlschema-2- 20041028 (2004)
10. JSON Schema and Hyperschema: JSON Schema Software. http://json-schema. org/implementations.html (2013)
11. Jan Wielemaker: SWI-Prolog SGML/XML Parser. SWI, University of Amsterdam, Roetersstraat, 15. Jg., p. 1018 (2005)
12. Thom Fru ̈hwirth and Jan Wielemaker and Leslie De Koninck and Markus Triska: SWI Prolog Reference Manual 6.2.2. Books on Demand (2012)
13. Falco Nogatz: From XML Schema to JSON Schema – Comparison and Translation with Constraint Handling Rules. (2013)
14. Thom Fru ̈hwirth and Frank Raiser: Constraint Handling Rules: Compilation, Ex- ecution, and Analysis. Books on Demand (2011)


  [1]: http://arxiv.org/abs/1406.2125