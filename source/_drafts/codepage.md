title: 代码页
categories:
  - Computer Basics
tags:
  - codepage
toc: false
date: 2014-09-26 00:11:46
---


## Codepage的定义和历史

- 字符内码- (charcter code)指的是用来代表字符的内码.读者在输入和存储文档时都要使用内码,内码分为
- 单字节内码-Single-Byte character sets (SBCS),可以支持256个字符编码.
- 双字节内码-Double-Byte character sets)(DBCS),可以支持65000个字符编码.主要用来对大字符集的东方文字进行编码.

## codepage

指的是一个经过挑选的以特定顺序排列的字符内码列表,对于早期的单字节内码的语种,codepage中的内码顺序使得系统可以按照此列表来根据
痰氖淙胫蹈
鲆桓龆杂Φ哪诼?对于双字节内码,则给出的是MultiByte到Unicode的对应表,这样就可以把以Unicode形式存放的字符转化为相应的字符
内码,或者反之,在Linux核心中对应的函数就是utf8_mbtowc和utf8_wctomb.

在1980年前,仍然没有任何国际标准如ISO-8859或Unicode来定义如何扩展US-ASCII编码以便非英语国家的用户使用.很多IT 厂商发明了他们自己的编码,并且使用了难以记忆的数目来标识:

例如936代表简体中文. 950代表繁体中文.

## CJK Codepage

同 Extended Unix Coding ( EUC )编码大不一样的是,下面所有的远东 codepage 都利用了C1控制码 {=80..=9F } 做为首字节, 使用ASCII值 { =40..=7E {做为第二字节,这样才能包含多达数万个双字节字符,这表明在这种编码之中小于3F的ASCII值不一定代表ASCII字符.

- CP932
Shift-JIS包含日本语 charset JIS X 0201 (每个字符一个字节) 和 JIS X 0208(每个字符两个字节),所以 JIS X0201平假名包含一个字节半宽的字符,其剩馀的60个字节被用做7076个汉字以及648个其他全宽字符的首字节.同EUC-JP编码区别的是,Shift-JIS没有包含JIS X 202中定义的5802个汉字.

- CP936
GBK 扩展了 EUC-CN 编码( GB 2312-80编码,包含 6763 个汉字)到Unicode (GB13000.1-93)中定义的20902个汉字,中国大陆使用的是简体中文zh_CN.

- CP949
UnifiedHangul (UHC) 是韩文 EUC-KR 编码(KS C 5601-1992 编码,包括2350 韩文音节和 4888 个汉字a)的超集,包含 8822个附加的韩文音节( 在C1中 )

- CP950
是代替EUC-TW (CNS 11643-1992)的 Big5 编码(13072 繁体 zh_TW 中文字) 繁体中文,这些定义都在Ken Lunde的 CJK.INF中或者 Unicode 编码表中找到.

*注意: Microsoft采用以上四种Codepage,因此要访问Microsoft的文件系统时必需采用上面的Codepage*

## IBM的远东语言Codepage

IBM的Codepage分为SBCS和DBCS两种:

IBM SBCS Codepage

- 37 (英文) *
- 290 (日文) *
- 833 (韩文) *
- 836 (简体中文) *
- 891 (韩文)
- 897 (日文)
- 903 (简体中文)
- 904 (繁体中文)IBM DBCS Codepage
- 300 (日文) *
- 301 (日文)
- 834 (韩文) *
- 835 (繁体中文) *
- 837 (简体中文) *
- 926 (韩文)
- 927 (繁体中文)
- 928 (简体中文)将SBCS的Codepage和DBCS的Codepage混合起来就成为: IBM MBCS Codepage
- 930 (日文) (Codepage 300 加 290) *
- 932 (日文) (Codepage 301 加 897)
- 933 (韩文) (Codepage 834 加 833) *
- 934 (韩文) (Codepage 926 加 891)
- 938 (繁体中文) (Codepage 927 加 904)
- 936 (简体中文) (Codepage 928 加 903)
- 5031 (简体中文) (Codepage 837 加 836) *
- 5033 (繁体中文) (Codepage 835 加 37) *

*代表采用EBCDIC编码格式由此可见,Mircosoft的CJK Codepage来源于IBM的Codepage.

## Linux下Codepage的作用

在Linux下引入对Codepage的支持主要是为了访问FAT/VFAT/FAT32/NTFS/NCPFS等文件系统下的多语种文件名的问
题,目前在NTFS和FAT32/VFAT下的文件系统上都使用了Unicode,这就需要系统在读取这些文件名时动态将其转换为相应的语言编码.因此引
入了NLS支持.其相应的程序文件在/usr/src/linux/fs/nls下:

- Config.in
- Makefile
- nls_base.c
- nls_cp437.c
- nls_cp737.c
- nls_cp775.c
- nls_cp850.c
- nls_cp852.c
- nls_cp855.c
- nls_cp857.c
- nls_cp860.c
- nls_cp861.c
- nls_cp862.c
- nls_cp863.c
- nls_cp864.c
- nls_cp865.c
- nls_cp866.c
- nls_cp869.c
- nls_cp874.c
- nls_cp936.c
- nls_cp950.c
- nls_iso8859-1.c
- nls_iso8859-15.c
- nls_iso8859-2.c
- nls_iso8859-3.c
- nls_iso8859-4.c
- nls_iso8859-5.c
- nls_iso8859-6.c
- nls_iso8859-7.c
- nls_iso8859-8.c
- nls_iso8859-9.c
- nls_koi8-r.c

实现了下列函数:

bc. 
extern int utf8_mbtowc(__u16 *, const __u8 *, int);
extern int utf8_mbstowcs(__u16 *, const __u8 *, int);
extern int utf8_wctomb(__u8 *, __u16, int);
extern int utf8_wcstombs(__u8 *, const __u16 *, int);

这样在加载相应的文件系统时就可以用下面的参数来设置Codepage:

对于Codepage 437 来说

bc. 
mount -t vfat /dev/hda1 /mnt/1 -o codepage=437,iocharset=cp437

这样在Linux下就可以正常访问不同语种的长文件名了.

## Linux下支持的Codepage
 
- nls codepage 437 — 美国/加拿大英语
- nls codepage 737 — 希腊语
- nls codepage 775 — 波罗的海语
- nls codepage 850 — 包括西欧语种(德语,西班牙语,意大利语)中的一些字符
- nls codepage 852 — Latin 2 包括中东欧语种(阿尔巴尼亚语,克罗地亚语,捷克语,英语,芬兰语,匈牙利语,爱尔兰语,德语,波兰语,罗马利亚语,塞尔维亚语,斯洛伐克语,斯洛文尼亚语,Sorbian语)
- nls codepage 855 — 斯拉夫语
- nls codepage 857 — 土耳其语
- nls codepage 860 — 葡萄牙语
- nls codepage 861 — 冰岛语
- nls codepage 862 — 希伯来语
- nls codepage 863 — 加拿大语
- nls codepage 864 — 阿拉伯语
- nls codepage 865 — 日尔曼语系
- nls codepage 866 — 斯拉夫语/俄语
- nls codepage 869 — 希腊语(2)
- nls codepage 874 — 泰语
- nls codepage 936 — 简体中文GBK
- nls codepage 950 — 繁体中文Big5
- nls iso8859-1 — 西欧语系(阿尔巴尼亚语,西班牙加泰罗尼亚语,丹麦语,荷兰语,英语,Faeroese语,芬兰语,法语,德语,加里西亚语,爱尔兰语,冰岛语,意大利语,挪威语,葡萄牙语,瑞士语.)这同时适用于美国英语.
- nls iso8859-2 — Latin 2 字符集,斯拉夫/中欧语系(捷克语,德语,匈牙利语,波兰语,罗马尼亚语,克罗地亚语,斯洛伐克语,斯洛文尼亚语)
- nls iso8859-3 — Latin 3 字符集, (世界语,加里西亚语,马耳他语,土耳其语)
- nls iso8859-4 — Latin 4 字符集, (爱莎尼亚语,拉脱维亚语,立陶宛语),是Latin 6 字符集的前序标准
- nls iso8859-5 — 斯拉夫语系(保加利亚语,Byelorussian语,马其顿语,俄语,塞尔维亚语,乌克兰语) 一般推荐使用 KOI8-R codepage
- nls iso8859-6 — 阿拉伯语.
- nls iso8859-7 — 现代希腊语
- nls iso8859-8 — 希伯来语
- nls iso8859-9 — Latin 5 字符集, (去掉了 Latin 1中不经常使用的一些冰岛语字符而代以土耳其语字符)
- nls iso8859-10 — Latin 6 字符集, (因纽特(格陵兰)语,萨摩斯岛语等Latin 4 中没有包括的北欧语种)
- nls iso8859-15 — Latin 9 字符集, 是Latin 1字符集的更新版本,去掉一些不常用的字符,增加了对爱莎尼亚语的支持,修正了法语和芬兰语部份,增加了欧元字符)
- nls koi8-r — 俄语的缺省支持

## 简体中文GBK/繁体中文Big5的Codepage

如何制作简体中文GBK/繁体中文Big5的Codepage?

1. 从 "Unicode":ftp://ftp.unicode.org/Public/MAPPINGS/VENDORS/MICSFT/WINDOWS/ 组织取得GBK/Big5的Unicode的定义.由于GBK是基于ISO 10646-1:1993标准的,而相应的日文是JIS X 0221-1995,韩文是KS C 5700-1995,他们被提交到Unicode标准的时间表为:
Unicode Version 1.0
Unicode Version 1.1 <-> ISO 10646-1:1993, JIS X 0221-1995, GB 13000.1-93
Unicode Version 2.0 <-> KS C 5700-1995从Windows 95开始均采用GBK编码. 您需要的是 CP936.TXT和 BIG5.TXT

2. 然后使用下面的程序就可以将其转化为Linux核心需要的Unicode<->GBK码表
./genmap BIG5.txt | perl uni2big5.pl
./genmap CP936.txt | perl uni2gbk.pl

3. 再修改fat/vfat/ntfs的相关函数就可以完成对核心的修改工作. 具体使用时可以使用下面的命令:
简体中文: mount -t vfat /dev/hda1 /mnt/1 -o codepage=936,iocharset=cp936
繁体中文: mount -t vfat /dev/hda1 /mnt/1 -o codepage=950,iocharset=cp936有趣的是,由于GBK包含了全部的GB2312/Big5/JIS的内码,所以使用936的Codepage也可以显示Big5的文件名.

__EOF__
