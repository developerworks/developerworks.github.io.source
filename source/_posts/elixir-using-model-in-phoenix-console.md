title: Elixir | 在Phoenix控制台中使用模型
categories:
  - Elixir
tags:
  - Phoenix
toc: false
date: 2014-11-13 02:25:18
---

## 启动Phoenix控制台

```
iex -S mix
```

## 查询

查询Id为1的图书

```
iex(16)> BookStore.Repo.get(BookStore.Books, 1)
%BookStore.Books{author: "Dave Thomas",
 description: "Programming Elixir: Functional |> Concurrent |> Pragmatic |> Fun",
 id: 1, publisher: "The Pragmatic Bookshelf", title: "Programming Elixir"}
```

查询所有图书, 返回的是一个数组

```
iex(17)> BookStore.Repo.all(BookStore.Books)
[%BookStore.Books{author: "Dave Thomas",
  description: "Programming Elixir: Functional |> Concurrent |> Pragmatic |> Fun",
  id: 1, publisher: "The Pragmatic Bookshelf", title: "Programming Elixir"}]
```

定义别名:

```
alias BookStore.Repo, as: Repo
alias BookStore.Books, as: Books
```

如果没有`as:`部分, alias将使用模型名称的最后一部分作为别名

```
alias BookStore.Repo
alias BookStore.Books
```

使用别名查询

```
iex> Repo.all(Books)
[%BookStore.Books{author: "Dave Thomas",
 description: "Programming Elixir: Functional |> Concurrent |> Pragmatic |> Fun",
  id: 1, publisher: "The Pragmatic Bookshelf", title: "Programming Elixir"}]
iex> Repo.get(Books, 1)
%BookStore.Books{author: "Dave Thomas",
 description: "Programming Elixir: Functional |> Concurrent |> Pragmatic |> Fun",
  id: 1, publisher: "The Pragmatic Bookshelf", title: "Programming Elixir"}
```

赋值给一个变量和访问模型字段

```
iex(10)> book = Repo.get(Books, 1)
%BookStore.Books{author: "Dave Thomas",
 description: "Programming Elixir: Functional |> Concurrent |> Pragmatic |> Fun",
 id: 1, publisher: "The Pragmatic Bookshelf", title: "Programming Elixir"}
iex(11)> book.author
"Dave Thomas"
iex(12)> book.description
"Programming Elixir: Functional |> Concurrent |> Pragmatic |> Fun"
iex(13)> book.id
1
iex(14)> book.publisher
"The Pragmatic Bookshelf"
iex(15)> book.title
"Programming Elixir"
```

## 更新

```
iex>  book = %{book | description: "Programming Elixir: a lot more fun", \
                      title: "Programming Elixir with fun"}
%BookStore.Books{author: "Dave Thomas",
 description: "Programming Elixir: a lot more fun", id: 1,
  publisher: "The Pragmatic Bookshelf", title: "Programming Elixir with fun"}
iex> Repo.update(book)
:ok
iex> Repo.get(Books, 1)
%BookStore.Books{author: "Dave Thomas",
 description: "Programming Elixir: a lot more fun", id: 1,
  publisher: "The Pragmatic Bookshelf", title: "Programming Elixir with fun"}
```



## 插入

```
iex> Repo.insert(%Books{author: "Simon St. Laurent, J. David Eisenberg", \
                        description: "Elixir is an excellent language if you want to \
                                      learn about functional programming, and with this hands-on \
                                      introduction",
                        publisher: "O'Reilly", title: "Introducing Elixir"})
%BookStore.Books{author: "Simon St. Laurent, J. David Eisenberg",
                 description: "Elixir is an excellent language if you want to learn about \
                               functional programming, and with this hands-on introduction", \
                 id: 18, publisher: "O'Reilly", title: "Introducing Elixir"}
```

## 删除

```
iex> introducing_elixir_book = Repo.get(Books, 2)
%BookStore.Books{author: "Simon St. Laurent, J. David Eisenberg",
 description: "Elixir is an excellent language if you want to learn \
               about functional programming, and with this hands-on introduction",
  id: 2, publisher: "O'Reilly", title: "Introducing Elixir"}
iex> Repo.delete(introducing_elixir_book)
:ok
```

## 参考资料


1. http://learnelixir.com/blog/2014/10/08/playing-with-model-in-elixir-phoenix-console/

