title: Gerrit 工作流
categories:
  - 软件配置管理
tags:
  - gerrit
date: 2014-08-22 15:30:55
---

## 安装commit-msg钩子

执行

```
curl -Lo .git/hooks/commit-msg http://localhost:8080/tools/hooks/commit-msg
chmod u+x .git/hooks/commit-msg
```

或

```
scp -p -P 29418 hezhiqiang@localhost:hooks/commit-msg .git/hooks/
chmod u+x .git/hooks/commit-msg
```

<!-- more -->

为什么要安装`commit-msg`钩子, `commit-msg` 钩子在你执行 `git commit` 修改提交注释, 加入一条类似下面的一条信息:

```
Change-Id: Ibf01a5ab2da2211a13707af72e77f751bd4837e7
```

该信息可通过`git show` 查看

```
~/tmp/review $ git show
commit 6096473b0422e83cea696f5c707412a05cdb4ba6
Author: Zhiqiang,He <developerworks@163.com>
Date:   Fri Aug 22 11:08:02 2014 +0800

    first commit

    Change-Id: Ibf01a5ab2da2211a13707af72e77f751bd4837e7

diff --git a/readme.md b/readme.md
new file mode 100644
index 0000000..5ec586d
--- /dev/null
+++ b/readme.md
@@ -0,0 +1 @@
+first commit
```

如果提交信息不包含`Change-Id`行, Gerrit默认会拒绝push, 并产生如下信息:

```
! [remote rejected] HEAD -> refs/publish/master (missing Change-Id in commit
message footer)
```


## 使用 git-review

> `git-review version 1.24` 在 `Mac OS X 10.9.3` 上的问题, Python 版本 `2.7.5`,

```
~/tmp/review $ git-review
Traceback (most recent call last):
  File "/usr/local/bin/git-review", line 11, in <module>
    sys.exit(main())
  File "/Library/Python/2.7/site-packages/git_review/cmd.py", line 1132, in main
    (os.path.split(sys.argv[0])[-1], get_version()))
  File "/Library/Python/2.7/site-packages/git_review/cmd.py", line 180, in get_version
    provider = pkg_resources.get_provider(requirement)
  File "/System/Library/Frameworks/Python.framework/Versions/2.7/Extras/lib/python/pkg_resources.py", line 197, in get_provider
    return working_set.find(moduleOrReq) or require(str(moduleOrReq))[0]
  File "/System/Library/Frameworks/Python.framework/Versions/2.7/Extras/lib/python/pkg_resources.py", line 666, in require
    needed = self.resolve(parse_requirements(requirements))
  File "/System/Library/Frameworks/Python.framework/Versions/2.7/Extras/lib/python/pkg_resources.py", line 565, in resolve
    raise DistributionNotFound(req)  # XXX put more info here
pkg_resources.DistributionNotFound: git-review
```

解决办法: 升级`setuptools`

```
sudo pip install --upgrade setuptools
```


> 使用`git-review`需要在`git`项目目录下创建配置文件 `.gitreview`, 否则会出现下列提示:

```
No '.gitreview' file found in this repository. We don't know where
your gerrit is. Please manually create a remote named "gerrit" and try
again.
```

```
[gerrit]
host=localhost
port=29418
project=review
defaultbranch=master    #提交变更到 master 分支,也就是推送到 refs/for/master
defaultremote=gerrit    #默认即为gerrit
defaultrebase=0         #默认提交前不执行rebase操作
```

关于上面的配置`defaultremote=gerrit`, 你可以通过`git remote -v`查看是否存在一个名称为`gerrit`的远程分支, 该远程分支是`gerrit`管理的远程仓库, 如果不存在可以通过如下命令创建:

```
git remote add gerrit http://hezhiqiang@localhost:8080/review
```

`git-review` 默认会查找一个名为`gerrit`的远程分支, 如果该分支存在, `git-review`会提交当前分支到`HEAD:refs/for/master`.
如果`gerrit`远程分支不存在, `git-review`会在仓库的更目录下查找一个名称为`.gitreview`的文件, 假设该文件存在, `git-review`能够在第一次运行的时候自动地配置你的仓库.

## 用 git push 创建变更

```
git push ssh://hezhiqiang@localhost:29418/review HEAD:refs/for/branch
```

## SSH主机别名

SSH主机名配置文件 `.ssh/config`

```
Host scm
    Hostname    192.168.8.200
    User        git
    Port        22
    IdentityFile    ~/.ssh/id_rsa
```

Git本地仓库配置文件 `.git/config`


```
[remote "devel"]
    url = ssh://scm/review
    push = HEAD:refs/for/master
```

## 参考资料:

1. http://openwares.net/linux/gerrit_workflow.html
2. https://bugs.launchpad.net/git-review/+bug/1337701
3. https://pypi.python.org/pypi/git-review
