title: PHP面向方面编程
categories:
  - Web
tags:
  - php
  - aop
toc: false
date: 2014-09-26 00:46:02
---

h2. ChangeLogs

2011-05-31 看了看APDT的项目主页,最后一次更新在2011年1月,不知道增加了什么新功能.
2011-05-31 增加一个slideshare上的幻灯片

h2. Aspect-Oriented Programming for PHP

notextile.
<div style="width:595px" id="__ss_3585"> <strong style="display:block;margin:12px 0 4px"><a href="http://www.slideshare.net/wcandillon/aspectoriented-programming-for-php" title="Aspect-Oriented Programming for PHP">Aspect-Oriented Programming for PHP</a></strong> <iframe src="http://www.slideshare.net/slideshow/embed_code/3585" width="595" height="497" frameborder="0" marginwidth="0" marginheight="0" scrolling="no"></iframe> <div style="padding:5px 0 12px"> View more <a href="http://www.slideshare.net/">presentations</a> from <a href="http://www.slideshare.net/wcandillon">William Candillon</a> </div> </div>

h2. 概述

几年前就在关注PHP方面的AOP开发,知道有个phpAspect这样一个工具,可以是一直没有一个可用的版本,最近发现 "APDT":http://code.google.com/p/apdt/ 更新了不少内容,安装插件集成了Waved过程,代码生成算是方便了许多.

PHP是非强类型语言,通过Annotation来标记类或则方法的一些元数据, "addendum":http://code.google.com/p/addendum/ 就是这样一个工具,在PHP5中支持以JavaDoc格式编写的符号描述,addendum使用了PHP5的Reflection API.

phpAspect通过Antlr,以静态分析的方式,把方面代码注入到声明的连接点.

bc(php).
@SimpleAnnotation
@SingleValuedAnnotation(true)
@SingleValuedAnnotation(-3.141592)
@SingleValuedAnnotation('Hello World!')
@SingleValuedAnnotationWithArray({1, 2, 3})
@MultiValuedAnnotation(key = 'value', anotherKey = false, andMore = 1234)

Addendum 符号注解基础

*创建第一个符号注解*


Addendum中的符号注解是简单的类, 要创建一个Persistent符号,仅需要

bc(php).
class Persistent extends Annotation {}

注意,保证符号的类名为大写字母


*给一个类加注解*

定义好了一个符号注解后,你可以用它来对不同的类,方法,属性,进行注解,Addendum中的注解是通过创建一个类似于Java的文档快注释的语法符号完成的.

bc(php).
/** @Persistent */
class Person {
   // some code
}


*访问符号注解*

一个类的符号注解通过扩展的反射API进行访问.一个反射类可以通过两方法创建:

bc(php)..
$reflection = new ReflectionAnnotatedClass('Person'); // 通过类名

$person = new Person();
$reflection = new ReflectionAnnotatedClass($person); // 通过实例

*测试一个类是否使用Persistent符号*

bc(php).
$reflection->hasAnnotation('Persistent'); // true

访问方法/属性的符号注解,可以用 *ReflectionAnnotatedMethod* 和 *ReflectionAnnotatedProperty*

h2. 参数化的符号

*单个参数的符号*

一个符号可以存储一个值,让我们创建一个Table符号来演示这个特性:

bc(php).
class Table extends Annotation {}

现在我们用一个参数化的符号来注解一个类

bc(php).
/** @Table("people") */
class Person {
   // some code
}

参数可以通过反射API进行访问:

bc(php).
$reflection = new ReflectionAnnotatedClass('Person'); // 通过类名
$reflection->getAnnotation('Table')->value; // 包含字符串"people"

*多参数符号*

一个符号也可以存储多个参数,多参数符号可以想这样定义:

bc(php).
class Secured extends Annotation {
   public $role;
   public $level;
}

多参数符号可以这样使用:

bc(php).
/** @Secured(role = "admin", level = 2) */
class Administration {
 // some code
}

通过反射API来访问这些字段:

bc(php).
$reflection = new ReflectionAnnotatedClass('Administration'); // by class name
$annotation = $reflection->getAnnnotation('Secured');
$annotation->role; // contains string "admin"
$annotation->level; // contains integer "2"

*符号中的数组参数*

符号还可以使用{}语法存储多个参数,例如:

bc(php)..
class RolesAllowed extends Annotation {}

/** @RolesAllowed({'admin', 'web-editor'}) */
class CMS {
 // some code
}

$reflection = new ReflectionAnnotatedClass('CMS');
$annotation = $reflection->getAnnnotation('RolesAllowed');
$annotation->value; // contains array('admin', 'web-editor')


p. 当然你还可以使用关联数组:

bc(php).
@Annotation({key1 = 1, key2 = 2, key3 = 3})

甚至你可以用你喜欢的方式混合使用它们以及嵌套:

bc(php).
@Annotation({key1 = 1, 2, 3, {4, key = 5}})

h2. 谁在使用Addendum

*sfAmfPlugin*

"http://www.symfony-project.org/plugins/sfAmfPlugin/1_0_0?tab=plugin_readme":http://www.symfony-project.org/plugins/sfAmfPlugin/1_0_0?tab=plugin_readme

*APDT的类型猜测*

"http://wiki.eclipse.org/Type_inference_for_APDT#APDT_PHP_Runtime_support":http://wiki.eclipse.org/Type_inference_for_APDT#APDT_PHP_Runtime_support

*Mosquito PHP5持久性API*

Mosquito 使用PHP5开发的一个持久性API.它是一个PDO的轻型包装器,它的主要角色是通过使用反射,符号注解,以及数据库元数据使开发者减少手工输入从而把数据库的CURDL操作自动化.
Mosquito的当前版本能够在Oracle 10g, MySQL 5, 以及SQLite 3上工作

"http://code.google.com/p/mosquito-php5-persistence-api/":http://code.google.com/p/mosquito-php5-persistence-api/

*符号的介绍完成了.下面来看看符号在phpAspect面向方面的编程中具体是如何使用的.*

h2. 安装APDT

*添加更新站点*

"(lightbox)!{width:50%;height:50%}/images/uploads/2010/05/install-apdt-add-new-update-site.png(添加更新站点)!(添加更新站点)":/images/uploads/2010/05/install-apdt-add-new-update-site.png

*选择要按照的APDT插件*

"(lightbox)!{width:50%;height:50%}/images/uploads/2010/05/install-apdt-select-plugin-to-install.png(选择APDT插件)!(选择APDT插件)":/images/uploads/2010/05/install-apdt-select-plugin-to-install.png

*检测要求和依赖性*

"(lightbox)!{width:50%;height:50%}/images/uploads/2010/05/install-apdt-check-requirements-and-dependencies.png(检查要求和依赖性)!(检查要求和依赖性)":/images/uploads/2010/05/install-apdt-check-requirements-and-dependencies.png

*重启Eclipse*

*创建PHPAspect项目*

*File->New->Other->PHPAspect->PHPAspect Project*

"(lightbox)!{width:50%;height:50%}/images/uploads/2010/05/install-apdt-create-phpaspect-project.png(创建PHPASpect项目)!(创建PHPASpect项目)":/images/uploads/2010/05/install-apdt-create-phpaspect-project.png

"(lightbox)!{width:50%;height:50%}/images/uploads/2010/05/install-apdt-create-phpaspect-project-2.png(创建PHPASpect项目)!(创建PHPASpect项目)":/images/uploads/2010/05/install-apdt-create-phpaspect-project-2.png

6. 创建Aspect

*File->New->Other->PHPAspect->Aspect*

"(lightbox)!{width:50%;height:50%}/images/uploads/2010/05/install-apdt-create-aspect.png(创建一个Aspect)!(创建一个Aspect)":/images/uploads/2010/05/install-apdt-create-aspect.png

"(lightbox)!{width:50%;height:50%}/images/uploads/2010/05/install-apdt-input-aspect-name.png(输入Aspect的名称)!(输入Aspect的名称)":/images/uploads/2010/05/install-apdt-input-aspect-name.png

"(lightbox)!{width:50%;height:50%}/images/uploads/2010/05/install-apdt-ap-file.png(日志Aspect)!(日志Aspect)":/images/uploads/2010/05/install-apdt-ap-file.png

"(lightbox)!{width:50%;height:50%}/images/uploads/2010/05/install-apdt-the-audit-aspect.png(一个完整的用户登录审计Aspect)!(一个完整的用户登录审计Aspect)":/images/uploads/2010/05/install-apdt-the-audit-aspect.png

"(lightbox)!{width:50%;height:50%}/images/uploads/2010/05/install-apdt-wave-the-AuditAspect.png(对Aspect进行编织)!(对Aspect进行编织)":/images/uploads/2010/05/install-apdt-wave-the-AuditAspect.png

*编织前的User.php*

"(lightbox)!{width:50%;height:50%}/images/uploads/2010/05/install-apdt-the-user-class.png(编织前的User.php)!(编织前的User.php)":/images/uploads/2010/05/install-apdt-the-user-class.png

注意,在编织User前,一定要实例化User类.

"(lightbox)!{width:50%;height:50%}/images/uploads/2010/05/install-apdt-the-new-weaved-folder.png(生成的weaved目录)!(生成的weaved目录)":/images/uploads/2010/05/install-apdt-the-new-weaved-folder.png

*编织后的User.php*

"(lightbox)!{width:50%;height:50%}/images/uploads/2010/05/install-apdt-the-waved-user-class.png(编织后的User类)!(编织后的User类)":/images/uploads/2010/05/install-apdt-the-waved-user-class.png

*打开一个Windows 控制台*

"(lightbox)!{width:50%;height:50%}/images/uploads/2010/05/install-apdt-open-a-windows-console.png(打开Windows Console)!(打开Windows Console)":/images/uploads/2010/05/install-apdt-open-a-windows-console.png

Open Extern是一个Eclipse插件,可以通过在Google Code上的项目更新站点进行安装

项目主页 , "http://code.google.com/p/openextern/":http://code.google.com/p/openextern/

更新站点, "http://openextern.googlecode.com/svn/trunk/openextern_update":http://openextern.googlecode.com/svn/trunk/openextern_update

执行的结果,我们看到AuditAspect.php中的方法 logUserLogging() 被调用了.那么PHPAspect是如何知道在调用User::login()方法后要调用logUserLogging()方法呢?

这是通过JavaDoc风格的符号进行描述的

bc(php).
/**
 * @After(call(User->login))
 */
public function logUserLogging()
{
	echo "User logged.\n";
}


*输出结果*

"(lightbox)!{width:50%;height:50%}/images/uploads/2010/05/install-apdt-execution-result.png(执行结果)!(执行结果)":/images/uploads/2010/05/install-apdt-execution-result.png

*顺便提一下ADPT使用了ANTLR作为PHP语法识别工具*

