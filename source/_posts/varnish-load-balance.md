title: Varnish 负载均衡
categories:
  - Web
tags:
  - varnish
  - load balance
toc: false
date: 2011-06-29 22:42:20
---

Changlogs
2011-06-20 添加测试报告, 在另一台服务器上测试,1W并发,首页index.php无Cache,512M内存的VPS能达到这个程度也算不错.并发几千还是能承载的.就是没几个人来访问,哎^_*!

```
[root@localhost webbench-1.5]# ./webbench -c 10000 -t 30 http://www.hezhiqiang.info/index.php
Webbench - Simple Web Benchmark 1.5
Copyright (c) Radim Kolar 1997-2004, GPL Open Source Software.
Benchmarking: GET http://www.hezhiqiang.info/index.php
10000 clients, running 30 sec.
Speed=21030 pages/min, 3073918 bytes/sec.
Requests: 8441 susceed, 2074 failed.
```
[Varnish][1] 有内置的负载均衡支持. 如果有多个源服务器,使用起来非常简易.通过声明多个后端服务器启动Varnish,同时设置后端服务器的probe属性来监控后端服务器的健康状况.

```
/* 如果VCL定义文件内容太多可以分离到单独的文件,并包含进来 */
#include "backends.vcl";
#include "ban.vcl";
backend default {
	.host = "127.0.0.1";
	.port = "8080";
	/* Varnish 到后端服务器的连接超时, */
	.connect_timeout = 1s;
    /* 和后端服务器建立连接后,即进入等待接收响应状态,如果响应的第一个字节在5s内没有到达,被认为超时*/
	.first_byte_timeout = 5s;
    /* 第一个字节到达后, 如果后续字节在2s内没有到达,被认为是超时 */
	.between_bytes_timeout = 2s;
	.probe = {
		.url = "/";
		.interval = 5s;
		.timeout = 1 s;
		.window = 5;
		.threshold = 3;
    }
}
backend default8081 {
	.host = "127.0.0.1";
	.port = "8081";
	.connect_timeout = 1s;
	.first_byte_timeout = 5s;
	.between_bytes_timeout = 2s;
	.probe = {
		.url = "/";
		.interval = 5s;
		.timeout = 1 s;
		.window = 5;
		.threshold = 3;
    }
}
```

有了两个后端服务器定义, default 和 default8081 . probe对象指出,Varnish应该每隔5秒去获取一次 / 的内容.如果超过一秒还没有得到内容,被认为是失败.如果五次获取有三次成功,那么认为后端服务器状态被认为是健康. 健康状态检查的详细信息请查看 [backend health polling][2]

probe 在Varnish源码中定义为一个C Struct:

```
struct vrt_backend_probe {
	const char	*url;
	const char	*request;
	double		timeout;
	double		interval;
	unsigned	exp_status;
	unsigned	window;
	unsigned	threshold;
	unsigned	initial;
};
```

Director 是一个或多个后端服务器的逻辑组, 在启用健康检查的情况下,当有后端服务器不可用时, 它会自动切换到另一个后端服务器上去. 这在需要系统宕机维护, 同时又不能中断服务的情况下是非常有用的, 比如,凌晨2:00,系统维护,可以关闭一台后端服务器,维护完成后再启动,接着维护下一台,以此类推.

现在这两个后端需要被包含在一个逻辑的 director 中.被作为一个虚拟的后端,称之为 hezhiqiang:

```
directory hezhiqiang round-robin {
	{.backend = default;}
	{.backend = default8081;}
}
```

关键字 *round-robin* 指出,请求会以轮询的方式分布到后端服务器.当前(varnish 3.0)还支持另外的director, 包括 simple, hash, random, dns, client.

这些支持的后端负载均衡方式可以在源代码 *cache_backend_cfg.c* 文件中看到.

```
void
VRT_init_dir(struct cli *cli, struct director **dir, const char *name,
    int idx, const void *priv)
{
	ASSERT_CLI();
	if (!strcmp(name, "simple"))
		VRT_init_dir_simple(cli, dir, idx, priv);
	else if (!strcmp(name, "hash"))
		VRT_init_dir_hash(cli, dir, idx, priv);
	else if (!strcmp(name, "random"))
		VRT_init_dir_random(cli, dir, idx, priv);
	else if (!strcmp(name, "dns"))
		VRT_init_dir_dns(cli, dir, idx, priv);
	else if (!strcmp(name, "round-robin"))
		VRT_init_dir_round_robin(cli, dir, idx, priv);
	else if (!strcmp(name, "client"))
		VRT_init_dir_client(cli, dir, idx, priv);
	else
		INCOMPL();
}
```

现在可以把特定的请求定向到后端服务器了. 在 vcl_recv , 如下:

```
sub vcl_recv {
	# 把后端服务器指向上面定义的 directory
	set req.backend = hezhiqiang;
}
```

## Client director

Client director 在 Vanish 2.1.3以及之后版本可用.

如果基于客户端提供的信息做负载均衡,比如IP地址, HTTP 头, 或则请求URL, 那么就可以使用client director

```
sub vcl_recv {
	/* Set which backend will be used  */
	set req.backend = hezhiqiang;
	/* Load balance by user agent */
	set client.identity = req.url;
	/* Load balance by user agent */
	#set client.identity = client.ip;
	/* Load balance by user agent */
	#set client.identity = req.http.user-agent;
}
```

## DNS Director

指定384个后端服务器,其中全部在80端口监听, 0.4秒超时.

```
director directorname dns {
	.list = {
		.host_header = "www.example.com";
		.port = "80";
		.connect_timeout = 0.4;
		/* 使用掩码标识的网段作为后端 */
		"192.168.15.0"/24;
		"192.168.16.128"/25;
	}
	/* 指定DNS查询的缓存时间 */
	.ttl = 5m;
	/* 追加到客户端提供的主机头后面 */
	.suffix = "internal.example.net";
}```

## 一个完整的配置

采用round-robin双Nginx后端.搭建在Linode的Plan512 VPS节点上,同时采用XCache作为PHP的加速器,Percona MySQL 服务器.

```
/*import std;*/
backend default {
	.host = "127.0.0.1";
	.port = "8080";
	/* Varnish 到后端服务器的连接超时, */
	.connect_timeout = 1s;
    /* 和后端服务器建立连接后,即进入等待接收响应状态,如果响应的第一个字节在5s内没有到达,被认为超时*/
	.first_byte_timeout = 5s;
    /* 第一个字节到达后, 如果后续字节在2s内没有到达,被认为是超时 */
	.between_bytes_timeout = 2s;
	.probe = {
		/* 健康检查的目标URL */
		.url = "/index.html";
		/* 每次检查间隔 */
		.interval = 5s;
		/* 超时 */
		.timeout = 1 s;
		/* 检查次数 */
		.window = 5;
		/* 成功次数 */
		.threshold = 3;
    }
}
backend default8081 {
	.host = "127.0.0.1";
	.port = "8081";
	.connect_timeout = 1s;
	.first_byte_timeout = 5s;
	.between_bytes_timeout = 2s;
	.probe = {
		.url = "/";
		.interval = 5s;
		.timeout = 1 s;
		.window = 5;
		.threshold = 3;
    }
}
director hezhiqiang round-robin {
	{.backend = default;}
	{.backend = default8081;}
}
sub vcl_recv {
	set req.backend = hezhiqiang;
	# Load balance by url
	set client.identity = req.url;
	# Load balance by client ip
	#set client.identity = client.ip;
	# Load balance by user agent
	#set client.identity = req.http.user-agent;
	# std.log("fishy is going on with the vhost" + req.http.host);
	if (req.request == "GET" && req.url ~ "\.(gif|jpg|png|swf|css|js)$") {
		unset req.http.Cookie;
	}
	# do not cache the /tag, /textpattern, and /category
	if ( req.url ~ "^/tag/" ) {
		return (pass);
	}
	if ( req.url ~ "^/textpattern/") {
		return (pass);
	}
	if ( req.url ~ "^/category/" ) {
		return (pass);
	}
	if ( req.url ~ "^/html/") {
		return (pass);
	}
}
sub vcl_deliver {
	# add debugging headers, so we can see what's cached
	set resp.http.X-Served-By = server.hostname;
	if (obj.hits > 0) {
		set resp.http.X-Cache = "HIT";
		set resp.http.X-Cache-Hits = obj.hits;
	}
	else {
		set resp.http.X-Cache = "MISS";
	}
	# remove some headers added by varnish
	# unset resp.http.Via;
	# unset resp.http.X-Varnish;
}
```

  [1]: http://www.varnish-cache.org
  [2]: http://www.varnish-cache.org/trac/wiki/BackendPolling