title: Elixir | 部署Phoenix应用程序到Ubuntu服务器
categories:
  - Elixir
tags:
  - Elixir
  - Phoenix
  - Ubuntu
toc: false
date: 2014-11-13 02:43:21
---

## 安装部署工具

```
apt-get install -y capistrano
```

## 部署

## 步骤1: 安装 capistrano 和 capify

```
gem install capistrano --version=2.15.5
capify .
mkdir config/deploy
```

## 步骤2: 添加 exrm 依赖

```
defp deps do
  [
    ...
    {:exrm, "~> 0.14.11"}
  ]
end
```

安装

```
mix deps.get
```

## 步骤3: 改变应用程序的启动方式

```
defmodule BookStore do
  use Application
  # See http://elixir-lang.org/docs/stable/elixir/Application.html
  # for more information on OTP Applications
  def start(_type, _args) do
    import Supervisor.Spec, warn: false
    children = [
      # Define workers and child supervisors to be supervised
      # worker(BookStore.Worker, [arg1, arg2, arg3])
      worker(BookStore.Repo, [])
    ]
    BookStore.Router.start
    # See http://elixir-lang.org/docs/stable/elixir/Supervisor.html
    # for other strategies and supported options
    opts = [strategy: :one_for_one, name: BookStore.Supervisor]
    Supervisor.start_link(children, opts)
  end
end
```

第12行, 添加了`BookStore.Router.start`

在开发模式中,应用程序不再能够以正常的方式启动. `mix phoenix.start`会立即崩溃, 并显示如下错误:

```
Running MyAwesomeApp.Router with Cowboy on port 4000
** (CaseClauseError) no case clause matching: {:error, {:already_started, #PID<0.149.0>}}
    (phoenix) lib/phoenix/router.ex:78: Phoenix.Router.start_adapter/2
    (phoenix) lib/mix/tasks/phoenix/start.ex:12: Mix.Tasks.Phoenix.Start.run/1
    (mix) lib/mix/cli.ex:55: Mix.CLI.run_task/2
    (elixir) src/elixir_lexical.erl:17: :elixir_lexical.run/3
    (elixir) lib/code.ex:316: Code.require_file/2
```

在开发模式中, 你需要使用下面的方式启动服务器

```
iex -S mix phoenix.start
```

## 步骤4: 把代码Push到Git仓库

```
git init
git remote add origin git@github.com:developerworks/book_store.git
```

提交代码到远程仓库

```
git add . && git commit -am "initial commit"
git push origin master
```

## 步骤5: 在Ubuntu服务器上安装Erlang和Elixir

```
$ wget http://packages.erlang-solutions.com/erlang-solutions_1.0_all.deb
$ sudo dpkg -i erlang-solutions_1.0_all.deb
$ sudo apt-get update
$ sudo apt-get install erlang
```

由于Phoenix依赖Elixir 1.0.1, 需要手动安装合适的Elixir版本.

下载,解压,编译

```
cd /root
wget https://github.com/elixir-lang/elixir/archive/v1.0.2.zip
unzip v1.0.2.zip
cd elixir-1.0.2
make
```

把程序路径添加到PATH中

```
echo 'export PATH=/root/elixir-1.0.2/bin:$PATH' >> ~/.profile
source ~/.profile
```

检查版本

```
# elixir --version
Elixir 1.0.2
```

## 步骤6: 调整Locale

```
export LANGUAGE=en_US.UTF-8
export LANG=en_US.UTF-8
export LC_ALL=en_US.UTF-8
locale-gen en_US.UTF-8
sudo apt-get install locales
sudo dpkg-reconfigure locales
```

在`~/.profile`中添加下面的配置

```
export LANGUAGE=en_US.UTF-8
export LANG=en_US.UTF-8
export LC_ALL=en_US.UTF-8
```

在`/etc/environment`添加如下配置

```
LC_ALL=en_US.UTF-8
LANG=en_US.UTF-8
```

## 步骤6: 编辑config/deploy.rb

粘贴下面的代码

```
require 'capistrano/ext/multistage'
set :stages, ["staging", "production"]
set :default_stage, "production"
set :keep_releases, 5
set :application, "My Awesome App"
set :repository,  "git@github.com:learnelixir/my-awesome-app.git"
set :scm, :git
set :branch, :master
set :use_sudo, false
set :normalize_asset_timestamps, false
set :deploy_via, :remote_cache
after "deploy:update", "deploy:cleanup"
after "deploy:update", "deploy:build", "deploy:cleanup"
namespace :assets do
  task :precompile, roles: :web do
    # do nothing
  end
end
def is_application_running?(current_path)
  pid = capture(%Q{ps ax -o pid= -o command=|
      grep "/home/app/www/book_store/current/rel/book_store/.*/[b]eam"|awk '{print $1}'})
  return pid != ""
end
namespace :deploy do
  task :is_running, roles: :web do
    is_running = is_application_running?(current_path)
    if is_running
      puts "Application is running"
    else
      puts "Application is NOT running"
    end
  end
  task :build, roles: :web do
    run "cd #{current_path} && mix deps.get && MIX_ENV=#{mix_env} mix release"
  end
  task :restart, roles: :web do
    if is_application_running?(current_path)
      run "cd #{current_path}/rel/book_store/bin && ./book_store stop"
    end
    run "cd #{current_path}/rel/book_store/bin && ./book_store start"
  end
  task :start, roles: :web do
    run "cd #{current_path}/rel/book_store/bin && ./book_store start"
  end
  task :stop, roles: :web do
    run "cd #{current_path}/rel/book_store/bin && ./book_store stop"
  end
end
```

## 步骤7: 创建`production.rb`

```
vim config/deploy/production.rb
```

内容如下

```
server "xx.xx.xx.xx", :app, :web, :db, :primary => true
set :user, '<user>'
set :branch, :master
set :mix_env, :prod
set :deploy_to, "/home/<user>/www/book_store"
set :default_environment, {
  'PATH' => "$PATH:/home/app/src/elixir/bin" # --> replace by path to your elixir bin folder
}
```

其中`xx.xx.xx.xx`为服务器IP地址,  `<user>`为用户目录, 可以用同样的方法创建`staging.rb`文件用于`staging`环境.

## 步骤8: 运行setup和deploy

为部署初始化目录结构

```
cap deploy:setup
```

执行实际部署, 如果是第一次部署, 需要下载和安装依赖包

```
cap deploy
```

## 步骤9: 把Nginx作为反向代理


```
# Phoenix服务器地址和短裤
upstream book_store {
  server 127.0.0.1:4000;
}
server {
  listen 0.0.0.0:80;
  server_name localhost;
  try_files $uri/index.html $uri @book_store;
  location @book_store {
    proxy_set_header Host $http_host;
    if (!-f $request_filename) {
      proxy_pass http://book_store;
      break;
    }
  }
  error_page 500 502 503 504 /500.html;
  access_log  /var/log/nginx/book_store.log;
  error_log  /var/log/nginx/book_store.log;
}
```

Phoenix的运行端口可以在配置文件`config/prod.exs`中修改


