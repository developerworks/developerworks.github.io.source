#!/bin/sh
BACKUP_PATH=/root/tmp/autobackups
TZ='Asia/Shanghai' DATE_NAME=`date +%Y_%m_%d_%H_%M_%S`
mkdir $BACKUP_PATH
mysqldump -uroot --password='mydbpassword' --databases database > $BACKUP_PATH/database.sql
tar cjf - $BACKUP_PATH |split -b 10m - /root/tmp/backup_db_$DATE_NAME.tar.bz2.

echo "DB backup $DATE_NAME" | mutt -s "DB backup" myemailaddress@gmail.com -a /root/tmp/backup_db_$DATE_NAME.tar.bz2.aa
echo "DB backup $DATE_NAME" | mutt -s "DB backup" myemailaddress@gmail.com -a /root/tmp/backup_db_$DATE_NAME.tar.bz2.ab
echo "DB backup $DATE_NAME" | mutt -s "DB backup" myemailaddress@gmail.com -a /root/tmp/backup_db_$DATE_NAME.tar.bz2.ac
echo "DB backup $DATE_NAME" | mutt -s "DB backup" myemailaddress@gmail.com -a /root/tmp/backup_db_$DATE_NAME.tar.bz2.ad
echo "DB backup $DATE_NAME" | mutt -s "DB backup" myemailaddress@gmail.com -a /root/tmp/backup_db_$DATE_NAME.tar.bz2.ae
echo "DB backup $DATE_NAME" | mutt -s "DB backup" myemailaddress@gmail.com -a /root/tmp/backup_db_$DATE_NAME.tar.bz2.af

rm -rf $BACKUP_PATH