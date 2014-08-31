#!/bin/sh

git add . && git commit -a -m '$1' && git push
hexo g && hexo d