title: 译文 | Elixir 物联网-1
categories:
  - Elixir
tags:
  - Elixir
  - IoT
toc: false
date: 2014-11-05 16:39:53
---


```
defmodule IoT do
    defp do_listen(list_socket) do
        # 接受连接
        {:ok, socket} = :ssl.transport_accept(listen_socket)
        :ok = :ssl.accept_socket(socket)
        endpoint = TcpSupervisor.start_endpoint(socket)
        :ssl.controlling_process(socket,endpoint)
        # 异步处理
        :gen_server.cast(endpoint, {:start})
        # 重新进入Accept
        do_listen(listen_socket)
    end
end
```


多个Acceptor处理每秒1000连接:


Tcp监听器Supervisor

```
defmodule Owsla.TcpListenerSupervisor do
    use Supervisor.Behaviour
    def start_link(port, acceptor_count, backlog) do
        :supervisor.start_link({ :local, :listener_sup}, __MODULE__, [port, acceptor_count, backlog])
    end
    def init([port, acceptor_count, backlog]) do
        :ssl.start()
        {:ok, listen_socket} = create_listen_socket(port, backlog)
        spawn(fn ->
            Enum.each(1..acceptor_count,
                fn (_) -> start_listener() end
                )
            end)
            tree = [ worker(Owsla.TcpAcceptor, [listen_socket], restart: :permanent) ]
            supervise(tree, strategy: :simple_one_for_one)
    end
    def create_listen_socket(port, backlog) do
         tcp_options = [
            :binary,
            {:packet, :line},
            {:reuseaddr, true},
            {:active, false},
            {:backlog, backlog}
            ]
        :gen_tcp.listen(port, tcp_options)
    end
    def start_listener() do
        :supervisor.start_child(:listener_sup, [])
    end
end
```


```
defmodule Owsla.TcpAcceptor do
    use GenServer.Behaviour

    @ssl_options
        [{:certfile, "deviceserver.crt"}, {:keyfile, "deviceserver.key"},
        {:ciphers, [
            {:dhe_rsa,:aes_256_cbc,:sha256},
            {:dhe_dss,:aes_256_cbc,:sha256},
            {:rsa,:aes_256_cbc,:sha256},
            {:dhe_rsa,:aes_128_cbc,:sha256},
            {:dhe_dss,:aes_128_cbc,:sha256},
            {:rsa,:aes_128_cbc,:sha256},
            {:dhe_rsa,:aes_256_cbc,:sha},
            {:dhe_dss,:aes_256_cbc,:sha},
            {:rsa,:aes_256_cbc,:sha},
            {:dhe_rsa,:'3des_ede_cbc',:sha},
            {:dhe_dss,:'3des_ede_cbc',:sha},
            {:rsa,:'3des_ede_cbc',:sha},
            {:dhe_rsa,:aes_128_cbc,:sha},
            {:dhe_dss,:aes_128_cbc,:sha},
            {:rsa,:aes_128_cbc,:sha},
            {:rsa,:rc4_128,:sha},
            {:rsa,:rc4_128,:md5},
            {:dhe_rsa,:des_cbc,:sha},
            {:rsa,:des_cbc,:sha}
        ]}]

    def start_link(listen_socket) do
        :gen_server.start_link(__MODULE__, listen_socket, [])
    end
    def init(listen_socket) do
        :gen_server.cast self, {:listen}
        {:ok, listen_socket}
    end

    def handle_cast( {:listen}, listen_socket) do
        do_listen(listen_socket)
    end

    defp do_listen(listen_socket) do
        case :gen_tcp.accept(listen_socket) do
       {:ok, socket} ->
                case :ssl.ssl_accept(socket, @ssl_options) do
               {:ok, ssl_socket} ->
                        endpoint = Owsla.TcpSupervisor.start_endpoint(ssl_socket)
                        :ssl.controlling_process(ssl_socket, endpoint)
                        :gen_server.cast endpoint, {:start}
                        do_listen(listen_socket)
               {:error, :closed} ->
                        do_listen(listen_socket)
                end
       {:error, :closed} -> do_listen(listen_socket)
       {:error, _} -> { :stop, :error, [] }
        end
    end
end
```