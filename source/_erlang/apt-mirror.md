############# config ##################
#
# set base_path    /var/spool/apt-mirror
#
# set mirror_path  $base_path/mirror
# set skel_path    $base_path/skel
# set var_path     $base_path/var
# set cleanscript $var_path/clean.sh
# set defaultarch  
# set postmirror_script $var_path/postmirror.sh
# set run_postmirror 0
set nthreads     20
set _tilde 0
#
############# end config ##############
 
# trusty 64Bit Mirror
deb-amd64 http://debian.ustc.edu.cn/ubuntu trusty main restricted universe multiverse
deb-amd64 http://debian.ustc.edu.cn/ubuntu trusty-security main restricted universe multiverse
deb-amd64 http://debian.ustc.edu.cn/ubuntu trusty-updates main restricted universe multiverse
deb-amd64 http://debian.ustc.edu.cn/ubuntu trusty-proposed main restricted universe multiverse
deb-amd64 http://debian.ustc.edu.cn/ubuntu trusty-backports main restricted universe multiverse
 
# trusty 32Bit Mirror
#deb-i386 http://debian.ustc.edu.cn/ubuntu trusty main restricted universe multiverse
#deb-i386 http://debian.ustc.edu.cn/ubuntu trusty-security main restricted universe multiverse
#deb-i386 http://debian.ustc.edu.cn/ubuntu trusty-updates main restricted universe multiverse
#deb-i386 http://debian.ustc.edu.cn/ubuntu trusty-proposed main restricted universe multiverse
#deb-i386 http://debian.ustc.edu.cn/ubuntu trusty-backports main restricted universe multiverse
 
#deb-src http://debian.ustc.edu.cn/ubuntu trusty main restricted universe multiverse
#deb-src http://debian.ustc.edu.cn/ubuntu trusty-security main restricted universe multiverse
#deb-src http://debian.ustc.edu.cn/ubuntu trusty-updates main restricted universe multiverse
#deb-src http://debian.ustc.edu.cn/ubuntu trusty-proposed main restricted universe multiverse
#deb-src http://debian.ustc.edu.cn/ubuntu trusty-backports main restricted universe multiverse
#clean http://debian.ustc.edu.cn/ubuntu
 
#deb-amd64 http://ubuntu-cloud.archive.canonical.com/ubuntu trusty-updates/grizzly main
#deb-amd64 http://ubuntu-cloud.archive.canonical.com/ubuntu trusty-proposed/grizzly main
#deb-i386 http://ubuntu-cloud.archive.canonical.com/ubuntu trusty-updates/grizzly main
#deb-i386 http://ubuntu-cloud.archive.canonical.com/ubuntu trusty-proposed/grizzly main
#clean http://ubuntu-cloud.archive.canonical.com/ubuntu
