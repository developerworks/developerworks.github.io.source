title: Elixir | 用Phoenix,Postgresql和Ecto创建一个书单应用
categories:
  - Elixir
tags:
  - Elixir
  - Ecto
  - Phoenix
toc: true
date: 2014-11-13 01:19:52
---

本文通过一个书单应用简要介绍使用Phoenix框架创建一个Web应用程序的基本步骤. 从这些基本步骤我们来逐步学习如何使用phoenix框架开发一个Web应用程序的基本过程.

## 修订


- 2014-12-17
    - Phoenix 从0.8.0开始 phoenix任务`phoenix.start`重命名为`phoenix.server`
    - 修改项目文件`mix.exs`的依赖版本号,更新依赖库
        - ecto          0.2.0 -> 0.2.8
        - postgrex      0.5.0 -> 0.6.0
        - phoenix       0.5.0 -> master
    - 增加命令注释说明

## 安装Phoenix

```
git clone https://github.com/phoenixframework/phoenix.git
cd phoenix
mix do deps.get, compile
```

## 创建书单项目

在`phoenix`源代码目录中运行如下命令创建一个`phoenix`项目目录结构

```
mix phoenix.new book_store ../book_store
=== =========== ========== =============
|         |            |              |
命令     任务        项目名称    新创建的项目保存位置
```

目录`../book_store`不必事先存在, `phoenix`会自动创建.

## 添加依赖库

编辑`mix.exs`文件, 修改后如下:

```
defmodule BookStore.Mixfile do
  use Mix.Project
  def project do
    [app: :book_store,
     version: "0.0.1",
     elixir: "~> 1.0",
     elixirc_paths: ["lib", "web"],
     compilers: [:phoenix] ++ Mix.compilers,
     deps: deps]
  end
  # Configuration for the OTP application
  #
  # Type `mix help compile.app` for more information
  def application do
    [mod: {BookStore, []},
     applications: [:phoenix, :cowboy, :logger, :postgrex, :ecto]]
  end
  # Specifies your project dependencies
  #
  # Type `mix help deps` for examples and options
  defp deps do
    [ {:phoenix, github: "phoenixframework/phoenix"},
      {:cowboy, "~> 1.0"},
      {:postgrex, "~> 0.6.0"},
      {:ecto, "~> 0.2.8"} ]
  end
end
```

和修改之前的`mix.exs`文件相比有两个变更处:

- 在`application`函数中增加了两个依赖的应用程序 `:postgres` 和 `:ecto` (16行)
- 在`deps`函数增加两个依赖库`{:postgrex, "~> 0.6.0"}`和`{:ecto, "~> 0.2.8"}`(24,25行)

运行

```
mix do deps.get, compile
```

## 创建一个仓库(Repo)

创建文件`web/models/repo.ex`,内容如下:

```
defmodule BookStore.Repo do
  use Ecto.Repo, adapter: Ecto.Adapters.Postgres
  def conf do
    parse_url "ecto://postgres:postgres@localhost/book_store"
  end
  def priv do
    app_dir(:book_store, "priv/repo")
  end
end
```

创建数据库:

```
createdb book_store -U postgres --encoding='utf-8' --locale=en_US.UTF-8 --template=template0
```

修改`lib/book_store.ex`为, 如下:

```
defmodule BookStore do
  use Application
  # See http://elixir-lang.org/docs/stable/elixir/Application.html
  # for more information on OTP Applications
  def start(_type, _args) do
    import Supervisor.Spec, warn: false
    children = [
      # Define workers and child supervisors to be supervised
      worker(BookStore.Repo, [])
    ]
    opts = [strategy: :one_for_one, name: BookStore.Supervisor]
    Supervisor.start_link(children, opts)
  end
end
```

编译

```
mix compile
```

## 创建模型

创建文件`web/models/books.ex`, 内容如下:

```
defmodule BookStore.Books do
  use Ecto.Model
  schema "books" do
    field :title, :string
    field :description, :string
    field :author, :string
    field :publisher, :string
  end
end
```

## 创建数据库移植脚本

```
    $ mix ecto.gen.migration Bookstore.Repo create_book
    Compiled web/models/books.ex
    Generated bookstore.app
    * creating priv/repo/migrations
    * creating priv/repo/migrations/20141112170140_create_book.exs
```

编辑生成的`priv/repo/migrations/20141112170140_create_book.exs`脚本, 内容如下:

    defmodule BookStore.Repo.Migrations.CreateBook do
      use Ecto.Migration

      def up do
        ["CREATE TABLE books(\
            id serial primary key, \
            title varchar(125), \
            description text, \
            author varchar(255), \
            publisher varchar(255))",\

         "INSERT INTO books(title, description, author, publisher) \
                 VALUES ( \
                    'Programming Elixir', \
                    'Programming Elixir: Functional |> Concurrent |> Pragmatic |> Fun', \
                    'Dave Thomas', \
                    'The Pragmatic Bookshelf')"
        ]
      end
      def down do
        "DROP TABLE books"
      end
    end

运行移植脚本

```
mix ecto.migrate BookStore.Repo
```

## 创建查询

创建文件`web/models/queries.ex`, 内容如下:

```
defmodule BookStore.Queries do
  import Ecto.Query
  def books_query do
    query = from book in BookStore.Books,
            select: book
    BookStore.Repo.all(query)
  end
end
```

## 配置路由

打开文件`web/router.ex`, 修改为如下:

```
defmodule BookStore.Router do
  use Phoenix.Router
  scope "/" do
    # Use the default browser stack.
    pipe_through :browser
    #get "/", BookStore.PageController, :index, as: :pages
    get "/", BookStore.BookController, :index, as: :books
  end
  # Other scopes may use custom stacks.
  # scope "/api" do
  #   pipe_through :api
  # end
end
```

## 创建控制器

创建文件`web/controllers/book_controller.ex`, 内容如下:

```
defmodule BookStore.BookController do
  use Phoenix.Controller
  plug :action
  def index(conn, _params) do
    books = BookStore.Queries.books_query
    render conn, "index", books: books
  end
end
```

## 创建书单视图

创建文件`web/views/book_view.ex`, 内容如下:

```
defmodule BookStore.BookView do
  use BookStore.Views
end
```

创建目录

```
mkdir web/templates/book
```

并添加文件`web/templates/book/index.html.eex`, 内容如下:

```
<h1>我的图书</h1>
<table class='table table-bodered table-striped'>
  <thead>
    <tr>
      <th>#</th>
      <th>标题</th>
      <th>描述</th>
      <th>作者</th>
      <th>出版社</th>
    </tr>
  </thead>
  <tbody>
    <%= for book <- @books do %>
      <tr>
        <td><%= book.id %></td>
        <td><%= book.title %></td>
        <td><%= book.description %></td>
        <td><%= book.author %></td>
        <td><%= book.publisher %></td>
      </tr>
    <% end %>
  </tbody>
</table>

```

启动应用,并刷新页面

```
mix phoenix.start
```

![我的书单应用](/assets/images/FF5CA944-C022-42A0-8F33-7FF6F01EBD38.png)

完成!


## 参考资料

1. Book Listing App With Elixir, Phoenix, Postgres and Ecto
http://learnelixir.com/blog/2014/10/05/build-web-app-with-elixir/