#!/bin/sh

git commit -a -m '$1' && git push
hexo g && hexo d
