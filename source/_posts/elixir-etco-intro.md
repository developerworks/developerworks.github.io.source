title: Etco 简介
categories:
  - Elixir
tags:
  - Elixir
  - Ecto
toc: true
date: 2015-02-01 12:11:23
---

## Ecto 简介

Ecto是一种领域语言(DSL), 用于在Elixir中编写数据库查询,和其他数据库操作. Ecto是用Elixir语言开发的一个关系型数据库工具.

本章主要介绍如何在Elixir项目中使用Ecto

## 主要组件

Ecto有三个主要组件: 库(Repositories), 模型(Models), 和查询(Queries)

### Ecto 库

库是一个数据库的封装, 可以通过如下方式定义一个库:

```
defmodule Repo do
    use Ecto.Repo, adapter: Ecto.Adapters.Postgres
    def conf do
        parse_url "ecto://username:password@localhost/ecto_simple"
    end
end
```

Ecto当前仅支持Postgresql数据库, 未来会支持其他数据库.

在Ecto中每个库通过定义一个`start_link/0`函数, 该函数需要在使用库(Repository)之前调用. 该函数不会直接调用, 而是通过一个`supervisor`链.
找到`supervisor.ex`并使用下面的`init/1`函数启动一个`supervisor`:

```
def init([]) do
    tree = [worker(Repo, [])]
    supervise(tree, strategy: :one_for_all)
end
```

### Ecto 模型

模型是用于定义在查询,验证和回调中使用的模式(Schema), 下面是一个模式定义的例子:

```
defmodule Weather do
    use Ecto.Model
    # weather is the DB table
    schema "weather" do
        field :city,    :string
        field :temp_lo, :integer
        field :temp_hi, :integer
        field :prcp,    :float, default: 0.0
    end
end
```

还可以定义`primary_key`, `foreign_key` `belongs_to`等. 模式的详细信息可[查看文档](http://elixir-lang.org/docs/ecto/Ecto.Model.Schema.html).

定义了模式后, Ecto自动地定义一个包含模式字段的结构:

```
weather = %Weather{temp_lo: 30}
weather.temp_lo #=> 30
```

模型和数据库交互:

```
weather = %Weather{temp_lo: 0, temp_hi: 23}
Repo.insert(weather)
```

插入数据库, 返回一个`weather`的拷贝, 可以通过返回的这个值从数据库中读取一个结构:

```
# 读取数据
weather = Repo.get Weather, 1
#=> %Weather{id: 1, ...}
# 更新
weather = %{weather | temp_lo: 10}
Repo.update(weather)
#=> :ok
# 删除
Repo.delete(weather)
```

### 查询(Query)

最后一个组件让我们可以编写查询,并向Repository执行查询.

```
import Ecto.Query
query = from w in Weather,
        where w.prcp > 0 or w.prcp == nil,
        select w
Repo.all(query)
```


### 移植(Migration)

Ecto还支持数据库移植SQL脚本. 为了生成一个新的移植脚本, 需要高数ecto移植脚本在哪里(哪个目录, 相对于项目根目录).

```
defmodule Repo do
    use Ecto.Repo, adapter: Ecto.Adapters.Postgres
    def priv do
        app_dir(:YOUR_APP_NAME, "priv/repo")
    end
end
```

然后, 可以通过`mix`工具生成移植脚本

```
mix ecto.gen.migration Repo create_posts
```

该命令会在`priv/repo/migrations`目录中生成一个新的文件. 需要编写一个SQL命令来创建表结构, 已经创建一个删除表的命令.

```
defmodule Repo.CreatePosts do
  use Ecto.Migration
  def up do
    [ "CREATE TABLE IF NOT EXISTS migrations_test(id serial primary key, name text)",
      "INSERT INTO migrations_test (name) VALUES ('inserted')" ]
  end
  def down do
    "DROP TABLE migrations_test"
  end
end
```

使用下面的命令, 运行所有PENDING状态的的移植脚本,

```
mix ecto.migrate Repo
```

回滚

```
mix ecto.rollback Repo --all
```

Ecto的更多信息可以到[Github上的Ecto项目](https://github.com/elixir-lang/ecto)上获取, 或阅读[Ecto文档](http://elixir-lang.org/docs/ecto/)


## 创建项目

简单介绍Ecto后, 是时候在项目中使用了. 下面创建一个名为 `elixir_jobs`的项目:

```
mix new elixir_jobs
cd exlixir_jobs
```

### 安装Ecto

打开 `mix.exs` 文件. 需要`postgresql`驱动作为依赖, 这里我们使用`postgrex`.

```
defp deps do
    [{:postgrex, "0.5.2"},
     {:ecto, "0.2.0"}
    ]
end
```

更新应用程序列表包含`ecto`和`postgrex`.

```
def application do
    [applications: [:postgrex, :ecto]]
end
```

终端中运行`mix deps.get`获取依赖.

### 添加库(Repository)

A repo in ecto terms is the definition of a basic interface to a database,
in this case PostgreSQL. Open up lib/elixir_jobs/repo.ex and add the code below.
If the directory doesn't exist, create it.

```
defmodule ElixirJobs.Repo do
    use Ecto.Repo, adapter: Ecto.Adapters.Postgres
    def conf do
        parse_url "ecto://postgresuser:password@localhost/elixir_jobs"
    end
    def priv do
        app_dir(:elixir_jobs, "priv/repo")
    end
end
```

我们定义了PostgreSQL连接URL.

你需要替换的时`postgresuser`, `password`和`elixir_jobs`数据库名称. 我们使用了移植功能,所以`priv`函数是必须的.

我们只需要告诉Ecto移植脚本放在什么位置, 这个位置是`priv/repo`.

接下来, 我们应该确保Repo模块随我们的应用程序一起启动. 打开`lib/elixir_jobs.ex`:

```
defmodule ElixirJobs do
    use Application

    def start(_type, _args) do
        import Supervisor.Spec
        tree = [worker(ElixirJobs,Repo, [])]
        opts = [name: ElixirJobs.Sup, strategy: :one_for_one]
        Supervisor.start_link(tree, opts)
    end
end
```

确保一切OK, 让我们编译该项目

```
mix compile
```

如果没有错误消息, 然后我们需要使用`psql`工具添加数据库`elixir_jobs`.

```
$> psql -Upostgresuser -W template1
Password for user postgresuser:
template1=# CREATE DATABASE elixir_jobs;
CREATE DATABASE
template1=# \q
```

使用`\q`退出`psql`, 然后添加一个模型用于查询.

### 添加模型(Models)

创建一个单独的文件 `lib/elixir_jobs/jobs.ex`,并输入下面的代码, 模型名称为`Jobs`.

```
defmodule ElixirJobs.Jobs do
    use Ecto.Model

    schema "jobs" do
        field :title, :string
        field :type, :string
        field :description, :string
        field :status, :string
    end
end
```

运行`mix help`可以看到一组用于移植的命令

### 生成移植(Migration)脚本

我们使用 `mix ecto.gen.migration`生成移植脚本, 首先使用下面的命令创建一个移植脚本.

```
$> mix ecto.gen.migration ElixirJobs.Repo create_job
* creating priv/repo/migrations
* creating priv/repo/migrations/20140802043823_create_job.exs
```

打开生成的`priv/repo/migrations/20140802043823_create_job.exs`文件. 在这个文件中, 我们还必须手工编写SQL命令用于创建和删除表.

文件名` 20140802043823_create_job.exs`依据生成的时间变化.

```
defmodule ElixirJobs.Repo.Migrations.CreateJob do
  use Ecto.Migration
  def up do
    "CREATE TABLE jobs(id serial primary key, title varchar(125), type varchar(50), description text, status varchar(50))"
  end
  def down do
    "DROP TABLE jobs"
  end
end
```

创建了移植脚本后, 我们就可以运行它了.

### 移植

运行移植非常容易, 像这样:

```
$> mix ecto.migrate ElixirJobs.Repo
* running UP _build/dev/lib/elixir_jobs/priv/repo/migrations/20140802043823_create_job.exs
```

就这样! 如果你打开`elixir_jobs`数据库, 你会发现一个`jobs`表, 如下:

```
 Column    |          Type          |                     Modifiers
-------------+----------------------+---------------------------------------------------
 id        | integer                | not null default nextval('jobs_id_seq'::regclass)
 title     | character varying(125) |
 type      | character varying(50)  |
 description| text                   |
 status    | character varying(50)  |
```

现在可以测试一下INSERT, UPDATE, DELETE以及SELECT操作了.

### 实际操作

启动`iex`测试我们的设置. 开始操作数据库之前, 需要使用`start_link`函数启动库(Repo), 否则会得到一个错误.

```
$> iex -S mix
Erlang/OTP 17 [erts-6.0] [source] [64-bit] [smp:4:4] [async-threads:10] [hipe] [kernel-poll:false]
Interactive Elixir (0.14.1) - press Ctrl+C to exit (type h() ENTER for help)
iex(1)> ElixirJobs.Repo.start_link
{:ok, #PID<0.108.0>}
```

如果返回`:ok`, 那么就可以使用定义模型时Ecto生成的结构插入数据了.

```
iex(2)> job = %ElixirJobs.Jobs{title: "Elixir Expert Needed", description: "ElixirDose need your help to produce a high quality Elixir article every month or two.", type: "Remote", status: "Part Time"}
iex(3)> ElixirJobs.Repo.insert(job)
%ElixirJobs.Jobs{description: "ElixirDose need your help to produce a high quality Elixir article every month or two.", id: 2, status: "Part Time", title: "Elixir Expert Needed", type: "Remote"}
```

打开`jobs`表, 就可以看到刚刚插入的数据. 现在来读取数据:

```
iex(4)> ejob = Repo.get(ElixirJobs.Jobs, 1)
%ElixirJobs.Jobs{description: "ElixirDose need your help to produce a high quality Elixir article every month or two.", id: 1, status: "Part Time", title: "Elixir Expert Needed", type: "Remote"}
iex(5)> ejob.title
"Elixir Expert Needed"
```

然后更新数据:

```
iex(6)> ejob = %{ejob | title: "Elixir Writer Needed"}
%ElixirJobs.Jobs{description: "ElixirDose need your help to produce a high quality Elixir article every month or two.", id: 1, status: "Part Time", title: "Elixir Writer Needed", type: "Remote"}
iex(7)> ElixirJobs.Repo.update(ejob)
:ok
```

验证数据已经被更新:

```
iex(8)> ElixirJobs.Repo.get(Jobs, 1)
%ElixirJobs.Jobs{description: "ElixirDose need your help to produce a high quality Elixir article every month or two.", d: 1, status: "Part Time", title: "Elixir Writer Needed", type: "Remote"}
```

现在, 删除数据:

```
iex(9)> ElixirJobs.Repo.delete(ejob)
:ok
iex(10)> ElixirJobs.Repo.get(Jobs, 1)
nil
```

## 总结

本文涵盖了Ecto的基本使用, 用DSL编写查询,以及和数据库交互.

## 参考资料

1. https://github.com/elixir-lang/ecto
2. http://www.youtube.com/watch?v=SJRfujy9vLA
3. http://elixirsips.com/episodes/024_ecto_part_1.html