# 用 Nginx 负载均衡 EMQX 集群

Nginx 是一种高性能多功能的服务器软件，可以作为 Web 服务器和反向代理服务器，除此以外，Nginx 还可以作为负载均衡器将来自客户端的请求分发到多个后端服务器，以确保负载平衡和性能优化。Nginx 非常适用于物联网应用，因为它可以处理大量的并发请求。在物联网中，设备数量庞大，因此需要一个可以处理大量请求的服务器来保证稳定性。EMQX 原生支持由多个 MQTT 服务器组成的分布式集群架构，因此，使用 Ngnix 部署负载均衡以及 EMQX 集群，可以保证高可用性和可扩展性。

本页主要介绍了如何安装和配置 Nginx 的反向代理和负载均衡功能来搭建 MQTT 服务器从而为 EMQX 集群实现负载均衡。

## 功能与优势

使用 Nginx 负载均衡 EMQX 集群具备以下几个功能和优势：

- 作为反向代理服务器，Nginx 位于 MQTT 服务器端，代表 MQTT 客户端向 EMQX 集群发起 MQTT 连接请求，并代替 EMQX 集群处理请求，然后将 EMQX 集群的响应返回给 MQTT 客户端。这样的设置可以将多个集群隐藏起来，暴露一个接入点给 MQTT 客户端。MQTT 客户端只需要与 Nginx 通信，而不需要知道后面的集群数量和布局，这种方式可以提高系统的可维护性和可扩展性。
- Nginx 可用于终结 MQTT 客户端与 EMQX 集群之间经 SSL 加密的 MQTT 连接，减轻 EMQX 集群的加密解密负担。从而提供多种优势，如提高性能、简化证书管理和增强安全性。
- Nginx 具有灵活的负载均衡策略，以使用不同的策略来决定请求应该发送到集群中的哪个 EMQX 节点，有助于分摊流量和请求，提高性能和可靠性。例如粘性负载平衡，可将请求路由到同一后端服务器，从而提高性能和会话持久性。


## 前置准备

在开始使用之前，确保您已经创建了由以下 3 个 EMQX 节点组成的集群。需要了解如何创建 EMQX 集群，详见[创建与管理集群](./create-cluster.md)。

| 节点地址              | MQTT TCP 端口 | MQTT WebSocket 端口 |
| --------------------- | ------------- | ------------------- |
| emqx1-cluster.emqx.io | 1883          | 8083                |
| emqx2-cluster.emqx.io | 1883          | 8083                |
| emqx3-cluster.emqx.io | 1883          | 8083                |

本页中的示例将使用单个 Nginx 服务器配置为负载均衡器，将请求转发到由这 3 个 EMQX 节点组成的集群。

## 快速体验

此处提供了一个具有实际示例的 Docker Compose 配置，让您能够轻松地进行验证和测试，您可以按照以下步骤来进行操作：

1. 首先，克隆示例仓库并进入 `mqtt-lb-nginx` 目录：

```bash
git clone https://github.com/emqx/emqx-usage-example
cd emqx-usage-example/mqtt-lb-nginx
```

2. 接着，通过 Docker Compose 启动示例：

```bash
docker compose up -d
```

3. 使用 MQTTX CLI 建立 10 个 TCP 连接，模拟 MQTT 客户端连接：

```bash
mqttx bench conn -c 10
```

4. 在此之后，您可以查看 Nginx 连接监控情况以及 EMQX 客户端连接的分布情况：
    - 通过以下命令查看 Nginx 连接监控：

    ```bash
    $ curl http://localhost:8888/status                                
    Active connections: 11 
    server accepts handled requests
     60 60 65 
    Reading: 0 Writing: 1 Waiting: 0
    ```

    这将显示当前活动连接数以及服务器的请求处理统计情况，包括读取、写入和等待状态。

    - 使用以下命令分别查看每个 EMQX 节点的客户端连接情况：

    ```bash
    docker exec -it emqx1 emqx ctl broker stats | grep connections.count
    docker exec -it emqx2 emqx ctl broker stats | grep connections.count
    docker exec -it emqx3 emqx ctl broker stats | grep connections.count
    ```

    这将显示每个节点的连接数以及活动连接数，10 个连接均匀分布在集群节点：

    ```bash
    connections.count             : 3
    live_connections.count        : 3
    connections.count             : 4
    live_connections.count        : 4
    connections.count             : 3
    live_connections.count        : 3
    ```

通过以上步骤，您可以验证示例中的 Nginx 负载均衡功能，以及 EMQX 集群中客户端连接的分布情况。您也可以更改 `emqx-usage-example/mqtt-lb-nginx/nginx.conf` 文件进行自定义的配置验证。

接下来，我们将从头介绍如何安装并配置 Nginx 实现各类场景下的负载均衡需求。

## 安装 Nginx

此处使用 Ubuntu 22.04 LTS 系统，通过源码编译安装的方式安装 Nginx。您也可以使用 Docker 或二进制包安装 Nginx。

### 环境要求

在编译和安装 Nginx 前，需要确保系统中已经安装了以下依赖项：

- GNU C 和 C++ 编译器
- PCRE (Perl Compatible Regular Expressions) 库
- zlib 压缩库
- OpenSSL 库

可以使用以下命令在 Ubuntu 系统中安装以上依赖项：

```bash
sudo apt-get update
sudo apt-get install build-essential libpcre3-dev zlib1g-dev libssl-dev
```

### 下载源码

可以从 [Nginx 官方网站](https://nginx.org/en/download.html)下载最新的稳定版本，例如：

```bash
wget https://nginx.org/download/nginx-1.24.0.tar.gz
```

### 编译配置命名

下载完成后，需要解压并进入源码目录：

```bash
tar -zxvf nginx-1.24.0.tar.gz
cd nginx-1.24.0
```

使用以下命令进行编译配置：

```bash
./configure \
 --with-threads \
 --with-http_stub_status_module \
  --with-http_ssl_module \
  --with-http_realip_module \
  --with-stream \
  --with-stream_ssl_module
```

其中 `--with-http_ssl_module` 参数用于添加 SSL 功能支持，`--with-stream` 与 `--with-stream_ssl_module` 参数用于添加 TCP 反向代理支持。

### 开始编译

完成配置后，可以使用以下命令开始编译：

```bash
make
```

### 安装

编译完成后，可以使用以下命令安装 Nginx：

```bash
sudo make install
```

将 Nginx 可执行文件连接到系统 PATH 中的目录：

```bash
sudo ln -s /usr/local/nginx/sbin/nginx /usr/local/bin/nginx
```

## 使用基本命令

Nginx 的配置文件默认位于 `/usr/local/nginx/conf/nginx.conf`，将本页的配置示例添加到文件末尾即可。Nginx 基本操作命令如下：

检查配置文件：

```bash
sudo nginx -t
```

如果 Nginx 配置文件验证成功，则可以启动 Nginx：

```bash
sudo nginx
```

重新加载正在运行的 Nginx 并应用新配置，建议在操作前检查配置是否无误：

```bash
sudo nginx -s reload
```

停止 Nginx：

```bash
sudo nginx stop
```

## 配置反向代理 MQTT 连接

您可以在 Nginx 的配置文件中添加以下配置来反向代理来自客户端的 MQTT 连接请求，将请求转发至后端 MQTT 服务器。

```bash
stream {
  upstream mqtt_servers {
    # down：表示当前的 server 暂时不参与负载
    # max_fails：允许请求失败的次数；默认为 1
    # fail_timeout：失败超时时间，默认 10s, max_fails 达到次数后暂停的请求时间
    # backup：其它所有的非backup机器down或者忙的时候，请求backup机器

    server emqx1-cluster.emqx.io:1883 max_fails=2 fail_timeout=10s;
    server emqx2-cluster.emqx.io:1883 down;
    server emqx3-cluster.emqx.io:1883 backup;
  }

  server {
    listen 1883;
    proxy_pass mqtt_servers;

    # 启用此项时，对应后端监听器也需要启用 proxy_protocol
    proxy_protocol on;
    proxy_connect_timeout 10s;   
    # 默认心跳时间为 10 分钟
    proxy_timeout 1800s;
    proxy_buffer_size 3M;
    tcp_nodelay on;       
  }
}
```

## 配置反向代理 TLS 加密的 MQTT 连接

Nginx 反向代理 MQTT 并解密 TLS 连接，将客户端加密的 MQTT 请求解密后转发至后端 MQTT 服务器，以确保通信安全性。

只需在 TCP 基础配置上添加 SSL 与证书相关参数即可：

```bash
stream {
 
 upstream mqtt_servers {
    server emqx1-cluster.emqx.io:1883;
    server emqx2-cluster.emqx.io:1883;
  }

  server {
    listen 8883 ssl;

    ssl_session_cache shared:SSL:10m;
    ssl_session_timeout 10m;
    ssl_certificate /usr/local/nginx/certs/emqx.pem;
    ssl_certificate_key /usr/local/nginx/certs/emqx.key;
    ssl_verify_depth 2;
    ssl_protocols TLSv1 TLSv1.1 TLSv1.2;
    ssl_ciphers HIGH:!aNULL:!MD5;

    # 添加 CA 证书及开启验证客户端证书参数即可启用双向认证
    # ssl_client_certificate /usr/local/nginx/certs/ca.pem;
    # ssl_verify_client on;
    # ssl_verify_depth 1;

    proxy_pass mqtt_servers;

    # 启用此项时，对应后端监听器也需要启用 proxy_protocol
    proxy_protocol on;
    proxy_connect_timeout 10s;   
    # 默认心跳时间为 10 分钟
    proxy_timeout 1800s;
    proxy_buffer_size 3M;
    tcp_nodelay on;
  }
}

```

## 配置反向代理 MQTT WebSocket 连接

Nginx 在反向代理 MQTT WebSocket 连接，将客户端请求转发至后端 MQTT 服务器。需要使用  `server_name` 指定 HTTP 域名或 IP 地址。

```bash
http {
  upstream mqtt_websocket_servers {
    server emqx1-cluster.emqx.io:8083;
    server emqx2-cluster.emqx.io:8083;
  }

  server {
    listen 80;
    server_name mqtt.example.com;

    location /mqtt {
      proxy_pass http://mqtt_websocket_servers;

      proxy_http_version 1.1;
      proxy_set_header Upgrade $http_upgrade;
      proxy_set_header Connection "upgrade";

      # 禁用缓存             
      proxy_buffering off;

      proxy_connect_timeout 10s;        
      # WebSocket 连接有效时间
      # 在该时间内没有数据交互的话 WebSocket 连接会自动断开，默认为 60s 
      proxy_send_timeout 3600s;            
      proxy_read_timeout 3600s;            

      # 反向代理真实 IP            
      proxy_set_header Host $host;            
      proxy_set_header X-Real-IP $remote_addr;            
      proxy_set_header REMOTE-HOST $remote_addr;            
      proxy_set_header X-Forwarded-For $proxy_add_x_forwarded_for;
    }
  }
}
```

## 配置反向代理 TLS 加密的 MQTT WebSocket 连接

Nginx 反向代理 MQTT WebSocket 并解密 TLS 连接，将客户端加密的 MQTT 请求解密后转发至后端 MQTT 服务器，以确保通信安全性。需要使用  `server_name` 指定 HTTP 域名或 IP 地址。

只需在 WebSocket 基础配置上添加 SSL 与证书相关参数即可：

```bash
http {
  upstream mqtt_websocket_servers {
    server emqx1-cluster.emqx.io:8083;
    server emqx2-cluster.emqx.io:8083;
  }

  server {
    listen 443 ssl;
    server_name mqtt.example.com;

    ssl_session_cache shared:SSL:10m;
    ssl_session_timeout 10m;
    ssl_certificate /usr/local/nginx/certs/emqx.pem;
    ssl_certificate_key /usr/local/nginx/certs/emqx.key;
    ssl_protocols TLSv1 TLSv1.1 TLSv1.2;
    ssl_ciphers HIGH:!aNULL:!MD5;

    # 添加 CA 证书及开启验证客户端证书参数即可启用双向认证
    # ssl_client_certificate /usr/local/nginx/certs/ca.pem;
    # ssl_verify_client on;

    location /mqtt {
        proxy_pass http://mqtt_websocket_servers;
        proxy_http_version 1.1;
        proxy_set_header Upgrade $http_upgrade;
        proxy_set_header Connection "upgrade";

        # 反向代理真实 IP            
        proxy_set_header Host $host;            
        proxy_set_header X-Real-IP $remote_addr;            
        proxy_set_header REMOTE-HOST $remote_addr;            
        proxy_set_header X-Forwarded-For $proxy_add_x_forwarded_for;      
        
        # 禁用缓存             
        proxy_buffering off;
    }
  }
}
```

## 配置负载均衡策略

Nginx 提供了多种负载均衡策略，用于控制连接的分发方式。在实际使用中，根据您的服务器性能、流量需求等选择适当的负载均衡策略非常重要。

以下是一些常见的 Nginx 负载均衡策略，根据需要在 `upstream` 块中配置即可。

### 轮询（Round Robin）

这是默认的负载均衡策略，它将请求依次分配给每个后端服务器，循环往复。这样可以平均分担负载，适用于后端服务器性能差不多的情况。

```bash
upstream backend_servers {
  server emqx1-cluster.emqx.io:1883;
  server emqx2-cluster.emqx.io:1883;
  server emqx3-cluster.emqx.io:1883;
}
```

### 权重轮询（Weighted Round Robin）

在轮询基础上，为每个 EMQX 节点分配不同的权重，从而影响请求分配的比例。权重越高的服务器会获得更多的请求。

```bash
upstream backend_servers {
  server emqx1-cluster.emqx.io:1883 weight=3;
  server emqx2-cluster.emqx.io:1883 weight=2;
  server emqx3-cluster.emqx.io:1883 weight=1;
}
```

### IP 哈希（IP Hash）

根据客户端的 IP 地址进行哈希计算，然后将请求分配给一个固定的后端服务器。这确保同一个客户端的请求总是分发到同一个服务器上。

```bash
upstream backend_servers {
  ip_hash;
  server emqx1-cluster.emqx.io:1883;
  server emqx2-cluster.emqx.io:1883;
  server emqx3-cluster.emqx.io:1883;
}
```

### 最少连接数（Least Connections）

请求分发到当前连接数最少的服务器，以确保每个服务器的负载尽可能平衡。适用于后端服务器性能差异较大的情况。

```bash
upstream backend_servers {
  least_conn;
  server emqx1-cluster.emqx.io:1883;
  server emqx2-cluster.emqx.io:1883;
  server emqx3-cluster.emqx.io:1883;
}
```

请注意，上述示例配置可能需要根据您的实际情况进行调整。配置中使用的模块（如 **`ip_hash`**、**`least_conn`**）都是 Nginx 内置的模块，无需额外的模块依赖。

## 配置性能优化与监控功能

### Nginx 基础配置调整

`worker_processes`：工作进程数，设置与服务器 CPU 核心数相近的值，但要避免设置过多的进程，以防资源竞争。

`worker_connections`：单个工作进程可以允许同时建立外部连接的数量，数字越大能同时处理的连接数越多，所消耗内存也更多。这个值不超过操作系统允许的最大文件描述符数量。

```bash
worker_processes auto;

events {
 worker_connections 20480;
}
```

### Nginx 多网卡支持海量连接反向代理

在反向代理过程中，Nginx 作为客户端与后端 EMQX 节点建立连接。在这种情况下，单个 IP 地址最多能够创建约 6 万个长连接。为了支持更多的连接，你可以选择部署多个 Nginx 服务器或者配置多个 IP 地址。

以下是配置多个 IP 的示例，使用 Nginx 内置的 **`split_clients`** 模块来定义一个变量 **`$multi_ip`**，根据客户端的 IP 地址和端口号进行请求分流，需要确保所使用的 IP 地址在本地可用。

```bash
stream {
 split_clients "$remote_addr$remote_port" $multi_ip {        
    20% 10.211.55.5;        
    20% 10.211.55.20;        
    20% 10.211.55.21;        
    20% 10.211.55.22;        
    * 10.211.55.23;    
  }

  upstream mqtt_servers {
    server emqx1-cluster.emqx.io:1883;
    server emqx2-cluster.emqx.io:1883;
  }

  server {
    listen 1883;

    proxy_pass mqtt_servers;
    proxy_bind $multi_ip;
  }
}

```

### Nginx 状态监控

需要确认是否安装监控模块 `http_stub_status_module`，如果已经安装该模块，可以为 Nginx 启用状态监控：

```bash
http {
  server {
    listen 8888;
    
    location /status {            
      stub_status on;            
      access_log off;            
    }
  }
}
```

打开 `[http://localhost:8888/status](http://localhost:8888/status)` 即可查看状态数据：

```bash
$ curl http://localhost:8888/status
Active connections: 12
server accepts handled requests
 25 25 60
Reading: 0 Writing: 1 Waiting: 1
```

## 附录：主要参数说明

以下是示例配置中出现的参数说明。这些参数确保了与后端 MQTT 服务器的稳定连接，或者确保通过 Nginx 反向代理的 MQTT 通信得以加密和保护，同时采取了安全性的最佳实践来保障通信的隐私和完整性，为物联网应用提供了更高效、安全和精细控制的代理服务。

| 参数名称               | 说明                                                         |
| ---------------------- | ------------------------------------------------------------ |
| proxy_protocol         | 启用 PROXY protocol，允许 Nginx 在转发请求时在连接的开始部分附加额外的代理信息，确保 EMQX 能够获取到真实的客户端 IP。 |
| proxy_pass             | 定义了后端 MQTT 服务器的地址，表示所有来自客户端的请求将被转发到这个地址。 |
| proxy_connect_timeout  | 与后端 MQTT 服务器建立连接的超时时间，如果连接在这个时间内没有建立成功，Nginx 将会中断连接尝试。 |
| proxy_timeout          | 后端 MQTT 服务器的超时时间。这个时间内，如果没有收到来自后端的响应，Nginx 将会关闭连接。 |
| proxy_buffer_size      | 用于存储后端 MQTT 服务器接收到的数据的缓冲区大小。以确保足够的缓冲区来处理较大的数据流。 |
| tcp_nodelay            | 启用 TCP_NODELAY 选项，禁用 Nagle 算法。这可以减少数据包传输的延迟，有助于实时性要求较高的 MQTT 通信。 |
| ssl_session_cache      | 配置共享的 SSL 会话缓存。这将存储 SSL 会话的状态，以便在客户端重新连接时加速握手过程。shared:SSL:10m 指定了缓存名称和大小，这里是 10MB。 |
| ssl_session_timeout    | 设置 SSL 会话的超时时间为 10 分钟。如果会话在这段时间内没有被重用，将会被清除。 |
| ssl_certificate        | 指定 SSL 证书文件的路径。这个证书用于证明服务器的身份。      |
| ssl_certificate_key    | 指定与 SSL 证书对应的私钥文件的路径。                        |
| ssl_protocols          | 指定允许的 SSL/TLS 协议版本。                                |
| ssl_ciphers            | 配置允许的加密算法（密码套件）。HIGH:!aNULL:!MD5 这个设置指定了使用强加密套件，同时排除了一些不安全的选项，如不包括空密码套件和 MD5 散列算法。 |
| ssl_client_certificate | 指定用于验证客户端证书的 CA（证书颁发机构）证书文件的路径。这个 CA 证书用于验证客户端证书的真实性。 |
| ssl_verify_client      | 启用客户端证书验证。当设置为 "on" 时，Nginx 将会要求客户端提供有效的 SSL 证书。 |
| ssl_verify_depth       | 设置验证客户端证书的最大深度。在这里，设置为 1 表示仅验证客户端证书和 CA 证书之间的一层，不再深入验证。 |
