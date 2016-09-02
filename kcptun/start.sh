#!/bin/bash
cd /root/kcptun/
./server_linux_amd64 -l ":29900" -c /root/kcptun/server-config.json > kcptun.log 2>&1 &
echo "Kcptun started."
