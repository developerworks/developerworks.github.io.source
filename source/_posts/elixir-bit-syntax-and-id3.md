title: Elixir | 从ID3中解析Mp3元数据
categories:
  - Elixir
tags:
  - Elixir
  - ID3
toc: false
date: 2014-11-19 10:49:04
---

TODO:: 判断MP3文件是否包含`TAG`标签

## MP3和ID3

ID3是一种在MP3中使用的描述MP3音频文件的元数据结构, 如下图所示:

![ID3元数据](/assets/images/vlc_metadata.png)

## ID3v1 字段结构

![字段结构](/assets/images/id3v1_blocks.gif)

![ID3v1 布局](/assets/images/id3v1.png)

## 读取Mp3文件

```
defmodule Mp3 do
  def get(filename) do
    case File.read(filename) do
        {:ok, binary} ->
            binary
        _ ->
        IO.puts "Can't not open #{filename}"
    end
  end
end
```

## 提取ID3v1 标记

ID3标记是MP3文件的最后128字节, 因此我们可以计算出ID3 Tag的起始偏移量

```
mp3_byte_size = (byte_size(binary) - 128)
```

然后通过Bit Syntax类匹配出我们需要的ID3 Tag部分的Bit字符串.

```
<< _ :: binary-size(mp3_byte_size), id3_tag :: binary >> = binary
```

## 完整代码

```
defmodule Mp3 do
  def parse(filename) do
    case File.read(filename) do
      {:ok, binary} ->
        IO.puts byte_size(binary)
        # 获取Mp3音频数据的大小,用于计算ID3的起始偏移量
        mp3_byte_size = (byte_size(binary) - 128) * 8
        # 把我们需要的部分解析出来
        << _ :: size(mp3_byte_size), id3_tag :: binary >> = binary
        case id3_tag do
          <<"TAG",tags::binary>> ->
            # 从id3_tag中匹配出标题,艺术家名称,专辑名称,发行年份, 评论, 等
            << tag     :: binary-size(3),
               title   :: binary-size(30),
               artist  :: binary-size(30),
               album   :: binary-size(30),
               year    :: binary-size(4),
               comment :: binary-size(30),
               genre   :: binary-size(1)
            >>  = id3_tag
            # 输出
            IO.puts tags
            IO.puts "TAG: #{tag}"
            IO.puts "标题名: #{title}"
            IO.puts "艺术家: #{artist}"
            IO.puts "专辑名: #{album}"
            IO.puts "年份: #{year}"
            IO.puts "评论: #{comment}"
            IO.puts "流派: #{genre}"
          _ ->
            :NO_TAG_INFORMATION
        end
      _ ->
        IO.puts "Can't not open #{filename}"
    end
  end
end
```


## 参考资料

1. 本文原文
http://benjamintanweihao.github.io/blog/2014/06/10/elixir-bit-syntax-and-id3/
2. ID3v1 结构说明
http://id3.org/ID3v1
3. Erlang 比特语法和ID3
http://www.citizen428.net/blog/2010/09/04/erlang-bit-syntax-and-id3
4. Erlang 比特语法
http://www.erlang.org/doc/programming_examples/bit_syntax.html
5. Erlang Programming - Francesco Cesarini and Simon Thompson 电子版PDF, 206页