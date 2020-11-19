# 开始使用(GetStarted)

## emqttd 消息服务器简介

emqttd(Erlang MQTT Broker)是采用 Erlang 语言开发的开源 MQTT 消息服务器。Erlang/OTP 是出色的软实时(Soft-
Realtime)、低延时(Low-
Latency)、分布式(Distributed)的语言平台。MQTT 是轻量的(Lightweight)、发布订阅模式(PubSub)的物联网消息协议。

emqttd 设计目标是承载移动终端或物联网终端大量的 MQTT 连接，并实现在大量终端间快速低延时(Low-Latency)消息路由:

1. 稳定承载大规模的 MQTT 客户端连接，单服务器节点支持 50 万到 100 万连接。
2. 分布式节点集群，快速低延时的消息路由，单集群支持 1000 万规模的路由。
3. 消息服务器内扩展，支持定制多种认证方式、高效存储消息到后端数据库。
4. 完整支持 MQTT V3.1.1 协议，扩展支持 WebSocket、CoAP 或私有 TCP 等多协议。

## MQTT 发布订阅模式简述

MQTT 是发布订阅(Publish/Subscribe)模式的消息协议，与 HTTP 协议请求响应(Request/Response)模式不同。

MQTT 发布者与订阅者之间通过"主题"(Topic)进行消息路由，主题(Topic)格式类似 Unix 文件路径，例如:

    sensor/1/temperature

    chat/room/subject

    presence/user/feng

    sensor/1/#

    sensor/+/temperature

    uber/drivers/joe/inbox

MQTT 主题(Topic)支持'+', '#'的通配符，'+'通配一个层级，'#'通配多个层级(必须在末尾)。

MQTT 消息发布者(Publisher)只能向特定'名称主题'(不支持通配符)发布消息，订阅者(Subscriber)通过订阅'过滤主题'(支持通配符)来匹配消息。

::: tip
初接触 MQTT 协议的用户，通常会向通配符的'过滤主题'发布广播消息，MQTT 协议不支持这种模式，需从订阅侧设计广播主题(Topic)。
例如 Android 推送，向所有广州用户，推送某类本地消息，客户端获得 GIS 位置后，可订阅'news/city/guangzhou'主题。
:::

## 五分钟下载启动 emqttd

emqttd 消息服务器每个版本，会发布 Ubuntu、CentOS、FreeBSD、Mac OS X、Windows 平台的程序包。

下载地址: [ http://emqtt.com/downloads ](http://emqtt.com/downloads)

程序包下载后，可直接解压启动运行，例如 Mac 平台:

    unzip emqttd-macosx-1.1-beta-20160601.zip && cd emqttd

    # 启动emqttd
    ./bin/emqttd start

    # 检查运行状态
    ./bin/emqttd_ctl status

    # 停止emqttd
    ./bin/emqttd stop

emqttd 消息服务默认采用匿名认证，启动后 MQTT 客户端可连接 1883 端口，启动运行日志输出在 log/目录。

## 源码编译 emqttd

    git clone https://github.com/emqtt/emqttd.git

    cd emqttd && make && make dist

## Web 管理控制台(Dashboard)

emqttd 消息服务器启动后，会默认加载 Dashboard 插件，启动 Web 管理控制台。用户可通过 Web 控制台，
查看服务器运行状态、统计数据、客户端(Client)、会话(Session)、主题(Topic)、订阅(Subscription)。

控制台地址: [ http://127.0.0.1:18083](http://127.0.0.1:18083)，默认用户: admin，密码：public

![image](./_static/images/dashboard.png)

## emqttd 消息服务器功能列表

- 完整的 MQTT V3.1/V3.1.1 协议规范支持
- QoS0, QoS1, QoS2 消息支持
- 持久会话与离线消息支持
- Retained 消息支持
- Last Will 消息支持
- TCP/SSL 连接支持
- MQTT/WebSocket(SSL)支持
- HTTP 消息发布接口支持
- $SYS/#系统主题支持
- 客户端在线状态查询与订阅支持
- 客户端 ID 或 IP 地址认证支持
- 用户名密码认证支持
- LDAP 认证
- Redis、MySQL、PostgreSQL、HTTP 认证集成
- 浏览器 Cookie 认证
- 基于客户端 ID、IP 地址、用户名的访问控制(ACL)
- 多服务器节点集群(Cluster)
- 多服务器节点桥接(Bridge)
- mosquitto 桥接支持
- Stomp 协议支持
- Stomp/SockJS 支持
- 通过 Paho 兼容性测试

## emqttd 扩展模块与插件列表

### 扩展模块(Module)

| emqttd_auth_clientid    | ClientId 认证             |
| ----------------------- | ------------------------- |
| emqttd_auth_username    | 用户名密码认证            |
| emqttd_auth_ldap        | LDAP 认证                 |
| emqttd_mod_presence     | 客户端上下线状态消息发布  |
| emqttd_mod_subscription | 客户端上线自动主题订阅    |
| emqttd_mod_rewrite      | 重写客户端订阅主题(Topic) |

扩展模块通过'etc/emqttd.config'配置文件的 auth, modules 段落启用。

例如启用用户名密码认证:

    {access, [
        %% Authetication. Anonymous Default
        {auth, [
            %% Authentication with username, password
            {username, []},

            ...

启用客户端状态发布模块:

    {modules, [
        %% Client presence management module.
        %% Publish messages when client connected or disconnected
        {presence, [{qos, 0}]}

### 扩展插件(Plugin)

| [emqttd_plugin_template](https://github.com/emqtt/emqttd_plugin_template) | 插件模版与演示代码       |
| ------------------------------------------------------------------------- | ------------------------ |
| [emqttd_dashboard](https://github.com/emqtt/emqttd_dashboard)             | Web 管理控制台，默认加载 |
| [emqttd_auth_http](https://github.com/emqtt/emqttd_auth_http)             | HTTP 认证插件            |
| [emqttd_plugin_mysql](https://github.com/emqtt/emqttd_plugin_mysql)       | MySQL 认证插件           |
| [emqttd_plugin_pgsql](https://github.com/emqtt/emqttd_plugin_pgsql)       | PostgreSQL 认证插件      |
| [emqttd_plugin_redis](https://github.com/emqtt/emqttd_plugin_redis)       | Redis 认证插件           |
| [emqttd_plugin_mongo](https://github.com/emqtt/emqttd_plugin_mongo)       | MongoDB 认证插件         |
| [emqttd_stomp](https://github.com/emqtt/emqttd_stomp)                     | Stomp 协议插件           |
| [emqttd_sockjs](https://github.com/emqtt/emqttd_sockjs)                   | SockJS 插件              |
| [emqttd_recon](https://github.com/emqtt/emqttd_recon)                     | Recon 优化调测插件       |

扩展插件通过'bin/emqttd_ctl'管理命令行，加载启动运行。

例如启用 PostgreSQL 认证插件:

    ./bin/emqttd_ctl plugins load emqttd_plugin_pgsql

## 100 万线连接测试说明

::: tip
emqttd 消息服务器默认设置，允许最大客户端连接是 512，因为大部分操作系统'ulimit -n'限制为 1024。
:::

emqttd 消息服务器当前版本，连接压力测试到 130 万线，8 核心/32G 内存的 CentOS 云服务器。

操作系统内核参数、TCP 协议栈参数、Erlang 虚拟机参数、emqttd 最大允许连接数设置简述如下：

### Linux 操作系统参数

    # 2M - 系统所有进程可打开的文件数量:

    sysctl -w fs.file-max=2097152
    sysctl -w fs.nr_open=2097152

    # 1M - 系统允许当前进程打开的文件数量:

    ulimit -n 1048576

### TCP 协议栈参数

    # backlog - Socket 监听队列长度:

    sysctl -w net.core.somaxconn=65536

### Erlang 虚拟机参数

emqttd/etc/vm.args:

    ## max process numbers
    +P 2097152

    ## Sets the maximum number of simultaneously existing ports for this system
    +Q 1048576

    ## Increase number of concurrent ports/sockets
    -env ERL_MAX_PORTS 1048576

    -env ERTS_MAX_PORTS 1048576

### emqttd 最大允许连接数

emqttd/etc/emqttd.config:

    {mqtt, 1883, [
        %% Size of acceptor pool
        {acceptors, 64},

        %% Maximum number of concurrent clients
        {max_clients, 1000000},

        %% Socket Access Control
        {access, [{allow, all}]},

        %% Connection Options
        {connopts, [
            %% Rate Limit. Format is 'burst, rate', Unit is KB/Sec
            %% {rate_limit, "100,10"} %% 100K burst, 10K rate
        ]},
        ...

### 测试客户端设置

测试客户端在一个接口上，最多只能创建 65000 连接:

    sysctl -w net.ipv4.ip_local_port_range="500 65535"
    echo 1000000 > /proc/sys/fs/nr_open

### 按应用场景测试

MQTT 是一个设计得非常出色的传输层协议，在移动消息、物联网、车联网、智能硬件甚至能源勘探等领域有着广泛的应用。1 个字节报头、2 个字节心跳、消息 QoS 支持等设计，非常适合在低带宽、不可靠网络、嵌入式设备上应用。

不同的应用有不同的系统要求，用户使用 emqttd 消息服务器前，可以按自己的应用场景进行测试，而不是简单的连接压力测试:

1. Android 消息推送: 推送消息广播测试。
2. 移动即时消息应用: 消息收发确认测试。
3. 智能硬件应用: 消息的往返时延测试。
4. 物联网数据采集: 并发连接与吞吐测试。

## emqtt 开源 MQTT 客户端项目

GitHub: [ https://github.com/emqtt ](https://github.com/emqtt)

| [ emqttc ](https://github.com/emqtt/emqttc)                   | Erlang MQTT 客户端库     |
| ------------------------------------------------------------- | ------------------------ |
| [ emqtt_benchmark ](https://github.com/emqtt/emqtt_benchmark) | MQTT 连接测试工具        |
| [ CocoaMQTT ](https://github.com/emqtt/CocoaMQTT)             | Swift 语言 MQTT 客户端库 |
| [ QMQTT ](https://github.com/emqtt/qmqtt)                     | QT 框架 MQTT 客户端库    |
