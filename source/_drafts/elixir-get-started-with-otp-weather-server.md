title: Elixir OTP入门 - 天气服务器
categories:
  - Elixir
tags:
  - OTP
toc: false
date: 2014-11-12 01:00:15
---

本文是图书 [Études for Elixir](http://chimera.labs.oreilly.com/books/1234000001642) 第12章的一个实际操作示例, 演示了如何在Elixir使用OTP开发一个天气应用服务器.
天气服务器的源代码在 [这里](https://github.com/oreillymedia/etudes-for-elixir/blob/master/code/ch12-01/weather.ex):


## 创建项目

```
mix new weather
cd weather/lib
wget https://github.com/oreillymedia/etudes-for-elixir/blob/master/code/ch12-01/weather.ex
wget https://github.com/oreillymedia/etudes-for-elixir/blob/master/code/ch12-01/weather_sup.ex
```

## 修改代码

`atom_to_binary/1`修改为 `Atom.to_string/1`,`atom_to_list/1`  修改为 `Atom.to_char_list/1`

```elixir
defmodule Weather do
  use GenServer
  def start_link do
    GenServer.start_link(__MODULE__, [], [{:name, __MODULE__}])
  end
  @doc """
  GenServer.Behaviour行为回调函数
  """
  def init([]) do
    :inets.start()
    {:ok, []}
  end
  @doc """
  处理获取天气的同步调用
  """
  def handle_call(request, _from, state) do
    {reply, new_state} = get_weather(request, state)
    {:reply, reply, new_state}
  end
  def handle_cast(_msg, state) do
    IO.puts("Recently viewed: #{inspect(state)}")
    {:noreply, state}
  end
  def handle_info(_info, state) do
    {:noreply, state}
  end
  def terminate(_reason, _state) do
    {:ok}
  end
  def code_change(_old_version, state, _extra) do
    {:ok, state}
  end
  @doc """
  给定一个气象站名称,以及当前的服务器状态,从气象站获取天气数据,并把该气象站名称添加到服务器状态(最近查看的气象站列表)
  """
  def get_weather(station, state) do
    url = "http://w1.weather.gov/xml/current_obs/" <> station <> ".xml"
    IO.puts "正在从 " <> url <> " 获取天气数据"
    {status, data} = :httpc.request(to_char_list(url))
    IO.puts "状态: #{inspect status}"
    IO.puts "数据: #{inspect data}"
    case status do
      :error ->
        reply = {status, data}
        new_state = state
      :ok ->
        {{_http, code, _message}, _attrs, xml_as_chars} = data
        case code do
          200 ->
            xml = to_string(xml_as_chars)
            reply = {:ok, (for item <- [
                :location,
                :observation_time_rfc822,
                :weather,
                :temperature_string
            ], do: get_content(item, xml))}
            # remember only the last 10 stations
            new_state = [station | Enum.take(state, 9)]
          _ ->
            reply = {:error, code}
            new_state = state
        end
    end
    {reply, new_state}
  end
  # Given an element name (as an atom) and an XML string,
  # return a tuple with the element name and that element's text
  # content.
  @spec get_content(atom, String.t) :: {atom, String.t}
  defp get_content(element_name, xml) do
    # 从 ELIXIR V0.13.3 开始下面两个函数定义有变化
    # `atom_to_binary/1`修改为 `Atom.to_string/1`,`atom_to_list/1`  修改为 `Atom.to_char_list/1`
    {_, pattern} = Regex.compile(
    "<#{element_name}>([^<]+)</#{Atom.to_string(element_name)}>")
    result = Regex.run(pattern, xml)
    case result do
      [_all, match] -> {element_name, match}
      nil -> {element_name, nil}
    end
  end
end
```

## 获取天气数据

进入Elixir Shell

```
iex -S mix
```

![在Elixir Shell下分步运行](/assets/images/CAA2D057-327B-4857-8A5F-A09594A3878B.jpeg)

要获取一个Web页面,首先需要在`init/1`中调用`:inets.start/0`; 然后调用`:httpc.request(url)`

URL的形式为:

```
http://w1.weather.gov/xml/current_obs/NNNN.xml
```

其中`NNNN`为气象站代码,如果调用`:httpc.request/1`失败, 会得到如下形式的错误信息:

```
{:error,information}
```

如果成功,会得到一个如下形式的元组:

```
{ok,{
        {HTTP/1.1,code,code message},
        [ {HTTP header attribute,value},
          {Another attribute,another value}
        ],
        page contents
    }
}
```

得到XML数据警告格式化如下:

```
<?xml version="1.0" encoding="ISO-8859-1"?>
<?xml-stylesheet href="latest_ob.xsl" type="text/xsl"?>
<current_observation version="1.0"
                     xmlns:xsd="http://www.w3.org/2001/XMLSchema"
                     xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance"
                     xsi:noNamespaceSchemaLocation="http://www.weather.gov/view/current_observation.xsd">
  <credit>NOAA's National Weather Service</credit>
  <credit_URL>http://weather.gov/</credit_URL>
  <image>
    <url>http://weather.gov/images/xml_logo.gif</url>
    <title>NOAA's National Weather Service</title>
    <link>http://weather.gov</link>
  </image>
  <suggested_pickup>15 minutes after the hour</suggested_pickup>
  <suggested_pickup_period>60</suggested_pickup_period>
  <location>San Jose International Airport, CA</location>
  <station_id>KSJC</station_id>
  <latitude>37.37</latitude>
  <longitude>-121.93</longitude>
  <observation_time>Last Updated on Nov 11 2014, 8:53 pm PST</observation_time>
  <observation_time_rfc822>Tue, 11 Nov 2014 20:53:00 -0800</observation_time_rfc822>
  <weather>Mostly Cloudy</weather>
  <temperature_string>58.0 F (14.4 C)</temperature_string>
  <temp_f>58.0</temp_f>
  <temp_c>14.4</temp_c>
  <relative_humidity>65</relative_humidity>
  <wind_string>Variable at 3.5 MPH (3 KT)</wind_string>
  <wind_dir>Variable</wind_dir>
  <wind_degrees>999</wind_degrees>
  <wind_mph>3.5</wind_mph>
  <wind_kt>3</wind_kt>
  <pressure_string>1014.3 mb</pressure_string>
  <pressure_mb>1014.3</pressure_mb>
  <pressure_in>29.96</pressure_in>
  <dewpoint_string>46.0 F (7.8 C)</dewpoint_string>
  <dewpoint_f>46.0</dewpoint_f>
  <dewpoint_c>7.8</dewpoint_c>
  <visibility_mi>10.00</visibility_mi>
  <icon_url_base>http://forecast.weather.gov/images/wtf/small/</icon_url_base>
  <two_day_history_url>http://www.weather.gov/data/obhistory/KSJC.html</two_day_history_url>
  <icon_url_name>nbkn.png</icon_url_name>
  <ob_url>http://www.weather.gov/data/METAR/KSJC.1.txt</ob_url>
  <disclaimer_url>http://weather.gov/disclaimer.html</disclaimer_url>
  <copyright_url>http://weather.gov/disclaimer.html</copyright_url>
  <privacy_policy_url>http://weather.gov/notice.html</privacy_policy_url>
</current_observation>
```

## 解析数据

## 设置一个Supervisor


```
defmodule WeatherSup do
  use Supervisor
  def start_link do
    Supervisor.start_link(__MODULE__, [], [{:name, __MODULE__}])
  end
  def init([]) do
    child = [worker(Weather, [], [])]
    supervise(child, [{:strategy, :one_for_one}, {:max_restarts, 1},{:max_seconds, 5}])
  end
end
```

示例

```
iex(1)> {:ok, pid} = WeatherSup.start_link
Starting weather supervisor Elixir.WeatherSup
Initalizing ...
{:ok, #PID<0.89.0>}
iex(2)> Process.unlink(pid)
true
iex(3)> GenServer.call Weather, "KGAI"
正在从 http://w1.weather.gov/xml/current_obs/KGAI.xml 获取天气数据
状态: :ok
数据: {{'HTTP/1.1', 200, 'OK'}, [{'connection', 'keep-alive'}, {'date', 'Wed, 12 Nov 2014 05:55:10 GMT'}, {'accept-ranges', 'bytes'}, {'server', 'Apache/2.2.15 (Red Hat)'}, {'content-length', '2042'}, {'content-type', 'application/xml'}, {'last-modified', 'Wed, 12 Nov 2014 05:02:10 GMT'}, {'x-nids-serverid', 'www2.mo'}], '<?xml version="1.0" encoding="ISO-8859-1"?> \r\n<?xml-stylesheet href="latest_ob.xsl" type="text/xsl"?>\r\n<current_observation version="1.0"\r\n\t xmlns:xsd="http://www.w3.org/2001/XMLSchema"\r\n\t xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance"\r\n\t xsi:noNamespaceSchemaLocation="http://www.weather.gov/view/current_observation.xsd">\r\n\t<credit>NOAA\'s National Weather Service</credit>\r\n\t<credit_URL>http://weather.gov/</credit_URL>\r\n\t<image>\r\n\t\t<url>http://weather.gov/images/xml_logo.gif</url>\r\n\t\t<title>NOAA\'s National Weather Service</title>\r\n\t\t<link>http://weather.gov</link>\r\n\t</image>\r\n\t<suggested_pickup>15 minutes after the hour</suggested_pickup>\r\n\t<suggested_pickup_period>60</suggested_pickup_period>\n\t<location>Montgomery County Airpark, MD</location>\n\t<station_id>KGAI</station_id>\n\t<latitude>39.1683</latitude>\n\t<longitude>-77.166</longitude>\n\t<observation_time>Last Updated on Nov 11 2014, 11:55 pm EST</observation_time>\r\n        <observation_time_rfc822>Tue, 11 Nov 2014 23:55:00 -0500</observation_time_rfc822>\n\t<weather>Overcast</weather>\n\t<temperature_string>55.0 F (13.0 C)</temperature_string>\r\n\t<temp_f>55.0</temp_f>\r\n\t<temp_c>13.0</temp_c>\n\t<relative_humidity>94</relative_humidity>\n\t<wind_string>Calm</wind_string>\n\t<wind_dir>North</wind_dir>\n\t<wind_degrees>0</wind_degrees>\n\t<wind_mph>0.0</wind_mph>\n\t<wind_kt>0</wind_kt>\n\t<pressure_in>29.92</pressure_in>\n\t<dewpoint_string>53.6 F (12.0 C)</dewpoint_string>\r\n\t<dewpoint_f>53.6</dewpoint_f>\r\n\t<dewpoint_c>12.0</dewpoint_c>\n\t<visibility_mi>1.75</visibility_mi>\n \t<icon_url_base>http://forecast.weather.gov/images/wtf/small/</icon_url_base>\n\t<two_day_history_url>http://www.weather.gov/data/obhistory/KGAI.html</two_day_history_url>\n\t<icon_url_name>novc.png</icon_url_name>\n\t<ob_url>http://www.weather.gov/data/METAR/KGAI.1.txt</ob_url>\n\t<disclaimer_url>http://weather.gov/disclaimer.html</disclaimer_url>\r\n\t<copyright_url>http://weather.gov/disclaimer.html</copyright_url>\r\n\t<privacy_policy_url>http://weather.gov/notice.html</privacy_policy_url>\r\n</current_observation>\n'}
{:ok,
 [location: "Montgomery County Airpark, MD",
  observation_time_rfc822: "Tue, 11 Nov 2014 23:55:00 -0500",
  weather: "Overcast", temperature_string: "55.0 F (13.0 C)"]}
```

制造一个错误

```
iex(4)> GenServer.call Weather, 1234
** (exit) exited in: GenServer.call(Weather, 1234, 5000)
    ** (EXIT) an exception was raised:
        ** (ArgumentError) argument error
            :erlang.byte_size(1234)
            (weather) lib/weather.ex:38: Weather.get_weather/2
            (weather) lib/weather.ex:16: Weather.handle_call/3
            (stdlib) gen_server.erl:580: :gen_server.handle_msg/5
            (stdlib) proc_lib.erl:237: :proc_lib.init_p_do_apply/3

05:55:24.357 [error] GenServer Weather terminating
Last message: 1234
State: ["KGAI"]
** (exit) an exception was raised:
    ** (ArgumentError) argument error
        :erlang.byte_size(1234)
        (weather) lib/weather.ex:38: Weather.get_weather/2
        (weather) lib/weather.ex:16: Weather.handle_call/3
        (stdlib) gen_server.erl:580: :gen_server.handle_msg/5
        (stdlib) proc_lib.erl:237: :proc_lib.init_p_do_apply/3
    (elixir) lib/gen_server.ex:356: GenServer.call/3
```

再次调用

```
iex(4)> GenServer.call Weather, "KCMI"
正在从 http://w1.weather.gov/xml/current_obs/KCMI.xml 获取天气数据
状态: :ok
数据: {{'HTTP/1.1', 200, 'OK'}, [{'connection', 'keep-alive'}, {'date', 'Wed, 12 Nov 2014 05:55:45 GMT'}, {'accept-ranges', 'bytes'}, {'server', 'Apache/2.2.15 (Red Hat)'}, {'content-length', '2303'}, {'content-type', 'application/xml'}, {'last-modified', 'Wed, 12 Nov 2014 05:02:18 GMT'}, {'x-nids-serverid', 'www7.mo'}], '<?xml version="1.0" encoding="ISO-8859-1"?> \r\n<?xml-stylesheet href="latest_ob.xsl" type="text/xsl"?>\r\n<current_observation version="1.0"\r\n\t xmlns:xsd="http://www.w3.org/2001/XMLSchema"\r\n\t xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance"\r\n\t xsi:noNamespaceSchemaLocation="http://www.weather.gov/view/current_observation.xsd">\r\n\t<credit>NOAA\'s National Weather Service</credit>\r\n\t<credit_URL>http://weather.gov/</credit_URL>\r\n\t<image>\r\n\t\t<url>http://weather.gov/images/xml_logo.gif</url>\r\n\t\t<title>NOAA\'s National Weather Service</title>\r\n\t\t<link>http://weather.gov</link>\r\n\t</image>\r\n\t<suggested_pickup>15 minutes after the hour</suggested_pickup>\r\n\t<suggested_pickup_period>60</suggested_pickup_period>\n\t<location>Champaign / Urbana, University of Illinois-Willard, IL</location>\n\t<station_id>KCMI</station_id>\n\t<latitude>40.04</latitude>\n\t<longitude>-88.28</longitude>\n\t<observation_time>Last Updated on Nov 11 2014, 10:53 pm CST</observation_time>\r\n        <observation_time_rfc822>Tue, 11 Nov 2014 22:53:00 -0600</observation_time_rfc822>\n\t<weather>Overcast</weather>\n\t<temperature_string>33.0 F (0.6 C)</temperature_string>\r\n\t<temp_f>33.0</temp_f>\r\n\t<temp_c>0.6</temp_c>\n\t<relative_humidity>85</relative_humidity>\n\t<wind_string>Northwest at 12.7 MPH (11 KT)</wind_string>\n\t<wind_dir>Northwest</wind_dir>\n\t<wind_degrees>300</wind_degrees>\n\t<wind_mph>12.7</wind_mph>\n\t<wind_kt>11</wind_kt>\n\t<pressure_string>1022.3 mb</pressure_string>\n\t<pressure_mb>1022.3</pressure_mb>\n\t<pressure_in>30.17</pressure_in>\n\t<dewpoint_string>28.9 F (-1.7 C)</dewpoint_string>\r\n\t<dewpoint_f>28.9</dewpoint_f>\r\n\t<dewpoint_c>-1.7</dewpoint_c>\n\t<windchill_string>24 F (-4 C)</windchill_string>\r\n      \t<windchill_f>24</windchill_f>\r\n      \t<windchill_c>-4</windchill_c>\n\t<visibility_mi>10.00</visibility_mi>\n \t<icon_url_base>http://forecast.weather.gov/images/wtf/small/</icon_url_base>\n\t<two_day_history_url>http://www.weather.gov/data/obhistory/KCMI.html</two_day_history_url>\n\t<icon_url_name>novc.png</icon_url_name>\n\t<ob_url>http://www.weather.gov/data/METAR/KCMI.1.txt</ob_url>\n\t<disclaimer_url>http://weather.gov/disclaimer.html</disclaimer_url>\r\n\t<copyright_url>http://weather.gov/disclaimer.html</copyright_url>\r\n\t<privacy_policy_url>http://weather.gov/notice.html</privacy_policy_url>\r\n</current_observation>\n'}
{:ok,
 [location: "Champaign / Urbana, University of Illinois-Willard, IL",
  observation_time_rfc822: "Tue, 11 Nov 2014 22:53:00 -0600",
  weather: "Overcast", temperature_string: "33.0 F (0.6 C)"]}
```

再次得到了正确的数据, 者表示之前的错误导致Weather服务器崩溃后, Weather服务器被重启了.





## 参考资料

1. Getting Started with OTP
http://chimera.labs.oreilly.com/books/1234000001642/ch12.html

