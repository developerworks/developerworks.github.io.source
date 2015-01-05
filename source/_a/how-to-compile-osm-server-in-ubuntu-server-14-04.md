title: 如何在Ubuntu Server 14.04上编译OpenStreetMap服务器
categories:
  - OpenStreetMap
toc: false
date: 2015-01-03 20:09:03
---

安装需要的软件包, 本安装和编译过程是在Linode VPS/Ubuntu Server 14.04下运行的,可能会有软件包名称等差异, 安装过程中请仔细检查

```
apt-get install build-essential libxml2-dev libgeos-dev libpq-dev libbz2-dev libtool automake libproj-dev
apt-get install gcc proj-bin libgeos-c1 git osmosis libgeos++-dev
apt-get install php5 php-pear php5-pgsql php5-json php-db
apt-get install postgresql postgis postgresql-contrib postgresql-9.3-postgis-2.1 postgresql-server-dev-9.3
apt-get install libprotobuf-c0-dev protobuf-c-compiler
```

wget http://www.nominatim.org/release/Nominatim-2.3.1.tar.bz2
tar jxvf Nominatim-2.3.1.tar.bz2
cd Nominatim
./configure
make