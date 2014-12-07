title: Opencart 通过vQmod方式用Memcache替换默认的Cache
categories:
  - Opencart
tags:
  - vQmod
  - Memcache
toc: false
date: 2014-11-30 02:30:13
---

## 前提条件

用Memcache替换默认的Cache之前,需要满足如下前提条件:

- PHP需要Memcache扩展, 可以通过如下方式安装, 并在`php.ini`配置文件中添加`extension=memcache.so`

```
pecl install memcache
```

- 需要安装配置好Opencart
- 需要安装vQmod

## vQmod模块代码

```xml
<?xml version="1.0" encoding="UTF-8"?>
<modification>
    <id>Replace opencart default cache with Memcache</id>
    <version>1.0.0</version>
    <vqmver>2.5.1</vqmver>
    <author>hezhiqiang</author>
    <file name="system/library/cache.php">
        <operation>
            <search position="replace" offset="50"><![CDATA[
			<?php
			]]></search>
            <add><![CDATA[<?php
/**
 * OpenCart Ukrainian Community
 *
 * LICENSE
 *
 * This source file is subject to the GNU General Public License, Version 3
 * It is also available through the world-wide-web at this URL:
 * http://www.gnu.org/copyleft/gpl.html
 * If you did not receive a copy of the license and are unable to
 * obtain it through the world-wide-web, please send an email
 *
 * @category   OpenCart
 * @package    OCU Memcached
 * @copyright  Copyright (c) 2011 created by UncleAndy, maintained by Eugene Lifescale for
               OpenCart Ukrainian Community (http://opencart-ukraine.tumblr.com)
 * @license    http://www.gnu.org/copyleft/gpl.html
               GNU General Public License, Version 3
 */
final class Cache
{
    private $expire;
    private $memcache;
    private $ismemcache = false;
    public function __construct($exp = 3600)
    {
        $this->expire = $exp;
        if (CACHE_DRIVER == 'memcached') {
            $mc = new Memcache;
            if ($mc->pconnect(MEMCACHE_HOSTNAME, MEMCACHE_PORT)) {
                $this->memcache = $mc;
                $this->ismemcache = true;
            };
        };
        $files = glob(DIR_CACHE . 'cache.*');
        if ($files) {
            foreach ($files as $file) {
                $time = substr(strrchr($file, '.'), 1);
                if ($time < time()) {
                    if (file_exists($file)) {
                        @unlink($file);
                    }
                }
            }
        }
    }
    public function get($key)
    {
        if ((CACHE_DRIVER == 'memcached') && $this->ismemcache) {
            return ($this->memcache->get(MEMCACHE_NAMESPACE . $key, 0));
        } else {
            $files = glob(DIR_CACHE . 'cache.' . $key . '.*');
            if ($files) {
                foreach ($files as $file) {
                    $cache = '';
                    $handle = fopen($file, 'r');
                    if ($handle) {
                        $cache = fread($handle, filesize($file));
                        fclose($handle);
                    }
                    return unserialize($cache);
                }
            }
        }
    }
    public function set($key, $value)
    {
        if ((CACHE_DRIVER == 'memcached') && $this->ismemcache) {
            $this->memcache->set(MEMCACHE_NAMESPACE . $key, $value, MEMCACHE_COMPRESSED, $this->expire);
        } else {
            $this->delete($key);
            $file = DIR_CACHE . 'cache.' . $key . '.' . (time() + $this->expire);
            $handle = fopen($file, 'w');
            fwrite($handle, serialize($value));
            fclose($handle);
        };
    }
    public function delete($key)
    {
        if ((CACHE_DRIVER == 'memcached') && $this->ismemcache) {
            $this->memcache->delete(MEMCACHE_NAMESPACE . $key, 0);
        } else {
            $files = glob(DIR_CACHE . 'cache.' . $key . '.*');
            if ($files) {
                foreach ($files as $file) {
                    if (file_exists($file)) {
                        @unlink($file);
                        clearstatcache();
                    }
                }
            }
        }
    }
}
			]]></add>
        </operation>
    </file>
</modification>
```



