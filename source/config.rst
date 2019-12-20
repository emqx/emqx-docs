
.. _configuration:

=========================
配置说明 (Configuration)
=========================

---------------
EMQ X 配置文件
---------------

*EMQ X* 消息服务器通过 etc/ 目录下配置文件进行设置，主要配置文件包括:

+----------------------------+--------------------------------------+
| 配置文件                   | 说明                                 |
+----------------------------+--------------------------------------+
| etc/emqx.conf              | EMQ X 消息服务器配置文件             |
+----------------------------+--------------------------------------+
| etc/acl.conf               | EMQ X 默认ACL规则配置文件            |
+----------------------------+--------------------------------------+
| etc/plugins/\*.conf        | EMQ X 各类插件配置文件               |
+----------------------------+--------------------------------------+

------------------
EMQ X 配置变更历史
------------------

为方便用户与插件开发者使用，*EMQ X* 配置文件经过四次调整。

1. EMQ X 1.x 版本采用 Erlang 原生配置文件格式 etc/emqttd.config:

.. code-block:: erlang

    {emqttd, [
      %% Authentication and Authorization
      {access, [
        %% Authetication. Anonymous Default
        {auth, [
            %% Authentication with username, password
            %{username, []},

            %% Authentication with clientid
            %{clientid, [{password, no}, {file, "etc/clients.config"}]},

Erlang 的原生配置格式多层级嵌套，对非 Erlang 开发者的用户很不友好。

2. EMQ X 2.0-beta.x 版本简化了原生 Erlang 配置文件，采用类似 rebar.config 或 relx.config 格式:

.. code-block:: erlang

    %% Max ClientId Length Allowed.
    {mqtt_max_clientid_len, 512}.

    %% Max Packet Size Allowed, 64K by default.
    {mqtt_max_packet_size, 65536}.

    %% Client Idle Timeout.
    {mqtt_client_idle_timeout, 30}. % Second

简化后的 Erlang 原生配置格式方便用户配置，但插件开发者不得不依赖 gen_conf 库，而不是通过 appliaton:get_env 读取配置参数。

3. EMQ X 2.0-rc.2 正式版集成了 cuttlefish 库，采用了类似 sysctl 的 `k = v` 通用格式，并在系统启动时翻译成 Erlang 原生配置格式:

.. code-block:: properties

    ## Node name
    node.name = emq@127.0.0.1

    ## Max ClientId Length Allowed.
    mqtt.max_clientid_len = 1024

4. EMQ X 3.0-beta.1 测试版正式更名 emqttd 为 emqx ，配置名称与配置信息进行相关变化:

.. code-block:: properties

    ## Profile
    etc/emq.config  ==> etc/emqx.config

    ## Node name
    原先:
    node.name = emq@127.0.0.1
    现在:
    node.name = emqx@127.0.0.1


EMQ X 启动时配置文件处理流程::

    ----------------------                                          3.0/schema/*.schema      -------------------
    | etc/emqx.conf      |                   -----------------              \|/              | data/app.config |
    |       +            | --> mergeconf --> | data/app.conf | -->  cuttlefish generate  --> |                 |
    | etc/plugins/*.conf |                   -----------------                               | data/vm.args    |
    ----------------------                                                                   -------------------

-------------------
EMQ X 环境变量
-------------------

+------------------+----------------------------------------------+
| EMQX_NODE_NAME   | Erlang 节点名称，例如: emqx@127.0.0.1        |
+------------------+----------------------------------------------+
| EMQX_NODE_COOKIE | Erlang 分布式节点通信 Cookie                 |
+------------------+----------------------------------------------+
| EMQX_MAX_PORTS   | Erlang 虚拟机最大允许打开文件 Socket 数      |
+------------------+----------------------------------------------+
| EMQX_TCP_PORT    | MQTT/TCP 监听端口，默认: 1883                |
+------------------+----------------------------------------------+
| EMQX_SSL_PORT    | MQTT/SSL 监听端口，默认: 8883                |
+------------------+----------------------------------------------+
| EMQX_WS_PORT     | MQTT/WebSocket 监听端口，默认: 8083          |
+------------------+----------------------------------------------+
| EMQX_WSS_PORT    | MQTT/WebSocket with SSL 监听端口，默认: 8084 |
+------------------+----------------------------------------------+

--------------
EMQ X 集群设置
--------------

集群名称：

.. code-block:: properties

    cluster.name = emqxcl

指定 Erlang 分布式协议:

.. code-block:: properties

    cluster.proto_dist = inet_tcp

集群发现策略：

.. code-block:: properties

    cluster.discovery = manual

启用集群自愈：

.. code-block:: properties

    cluster.autoheal = on

宕机节点自动清除周期：

.. code-block:: properties

    cluster.autoclean = 5m

------------------
EMQ X 集群自动发现
------------------

EMQ X 版本支持多种策略的节点自动发现与集群:

+-----------------+---------------------------+
| 策略            | 说明                      |
+=================+===========================+
| manual          | 手工命令创建集群          |
+-----------------+---------------------------+
| static          | 静态节点列表自动集群      |
+-----------------+---------------------------+
| mcast           | UDP 组播方式自动集群      |
+-----------------+---------------------------+
| dns             | DNS A 记录自动集群        |
+-----------------+---------------------------+
| etcd            | 通过 etcd 自动集群        |
+-----------------+---------------------------+
| k8s             | Kubernetes 服务自动集群   |
+-----------------+---------------------------+

**manual 手动创建集群**

默认配置为手动创建集群，节点通过 `./bin/emqx_ctl join <Node>` 命令加入:

.. code-block:: properties

    cluster.discovery = manual

**基于 static 节点列表自动集群**

集群发现策略为 static:

.. code-block:: properties

    cluster.discovery = static

静态节点列表:

.. code-block:: properties

    cluster.static.seeds = emqx1@127.0.0.1,emqx2@127.0.0.1

**基于 mcast 组播自动集群**

集群发现策略为 mcast:

.. code-block:: properties

    cluster.discovery = mcast

IP 组播地址:

.. code-block:: properties

    cluster.mcast.addr = 239.192.0.1

组播端口范围:

.. code-block:: properties

    cluster.mcast.ports = 4369,4370

网卡地址:

.. code-block:: properties

    cluster.mcast.iface = 0.0.0.0

组播 TTL:

.. code-block:: properties

    cluster.mcast.ttl = 255

是否循环发送组播报文:

.. code-block:: properties

    cluster.mcast.loop = on

**基于 DNS A 记录自动集群**

集群发现策略为 dns:

.. code-block:: properties

    cluster.discovery = dns

dns 名字:

.. code-block:: properties

    cluster.dns.name = localhost

用于和 IP 地址一起构建节点名字的应用名字:

.. code-block:: properties

    cluster.dns.app  = emqx

**基于 etcd 自动集群**

集群发现策略为 etcd:

.. code-block:: properties

    cluster.discovery = etcd

etcd 服务器列表，以 ``,`` 进行分隔:

.. code-block:: properties

    cluster.etcd.server = http://127.0.0.1:2379

用于 etcd 中节点路径的前缀，集群中的每个节点都会在 etcd 创建以下路径: v2/keys/<prefix>/<cluster.name>/<node.name>:

.. code-block:: properties

    cluster.etcd.prefix = emqxcl

etcd 中节点的 TTL:

.. code-block:: properties

    cluster.etcd.node_ttl = 1m

包含客户端私有 PEM 编码密钥文件的路径:

.. code-block:: properties

    cluster.etcd.ssl.keyfile = etc/certs/client-key.pem

包含客户端证书文件的路径:

.. code-block:: properties

    cluster.etcd.ssl.certfile = etc/certs/client.pem

包含 PEM 编码的CA证书文件的路径:

.. code-block:: properties

    cluster.etcd.ssl.cacertfile = etc/certs/ca.pem

**基于 Kubernetes 自动集群**

集群发现策略为 k8s:

.. code-block:: properties

    cluster.discovery = k8s

Kubernetes API 服务器列表，以 ``,`` 进行分隔:

.. code-block:: properties

    cluster.k8s.apiserver = http://10.110.111.204:8080

帮助查找集群中的 EMQ X 节点的服务名称:

.. code-block:: properties

    cluster.k8s.service_name = emqx

用于从 k8s 服务中提取 host 的地址类型:

.. code-block:: properties

    cluster.k8s.address_type = ip

EMQ X 的节点名称:

.. code-block:: properties

    cluster.k8s.app_name = emqx

Kubernetes 的命名空间:

.. code-block:: properties

    cluster.k8s.namespace = default

-------------------
EMQ X 节点与 Cookie
-------------------

Erlang 节点名称:

.. code-block:: properties

    node.name = emqx@127.0.0.1

Erlang 分布式节点间通信 Cookie:

.. code-block:: properties

    node.cookie = emqxsecretcookie

.. NOTE::

    Erlang/OTP 平台应用多由分布的 Erlang 节点(进程)组成，每个 Erlang 节点(进程)需指配一个节点名，用于节点间通信互访。
    所有互相通信的 Erlang 节点(进程)间通过一个共用的 Cookie 进行安全认证。

------------------
EMQ X 节点连接方式
------------------

*EMQ X* 节点基于 Erlang/OTP 平台的 IPv4, IPv6 或 TLS 协议连接:

.. code-block:: properties

    ## 指定 Erlang 分布式通信协议: inet_tcp | inet6_tcp | inet_tls
    cluster.proto_dist = inet_tcp

    ## 指定 Erlang 分布式通信 SSL 的参数配置
    ## node.ssl_dist_optfile = etc/ssl_dist.conf

-----------------
Erlang 虚拟机参数
-----------------

Erlang 运行时系统的心跳监控功能。注释此行以禁用心跳监控，或将值设置为 ``on`` 启用:

.. code-block:: properties

    node.heartbeat = on

异步线程池中的线程数，有效范围为 0-1024:

.. code-block:: properties

    node.async_threads = 32

Erlang 虚拟机允许的最大进程数，一个 MQTT 连接会消耗 2 个 Erlang 进程:

.. code-block:: properties

    node.process_limit = 2048000

Erlang 虚拟机允许的最大 Port 数量，一个 MQTT 连接消耗 1 个 Port:

.. code-block:: properties

    node.max_ports = 1024000

分配缓冲区繁忙限制:

.. code-block:: properties

    node.dist_buffer_size = 8MB

ETS 表的最大数量。注意，mnesia 和 SSL 将创建临时 ETS 表:

.. code-block:: properties

    node.max_ets_tables = 256000

调整 GC 以更频繁地运行:

.. code-block:: properties

    node.fullsweep_after = 1000

崩溃转储日志文件位置:

.. code-block:: properties

    node.crash_dump = log/crash.dump

Erlang 分布式使用 TLS 时存储 SSL/TLS 选项的文件:

.. code-block:: properties

    node.ssl_dist_optfile = etc/ssl_dist.conf

分布式节点的滴答时间:

.. code-block:: properties

    node.dist_net_ticktime = 60

Erlang 分布式节点间通信使用 TCP 连接的端口范围:

.. code-block:: properties

    node.dist_listen_min = 6396
    node.dist_listen_max = 6396

------------
RPC 参数配置
------------

RPC 模式 (sync | async):

.. code-block:: properties

    rpc.mode = async

RPC async 模式的最大批量消息数:

.. code-block:: properties

    rpc.async_batch_size = 256

RPC 本地监听的 TCP 端口:

.. code-block:: properties

    rpc.tcp_server_port = 5369

RPC 对端监听的 TCP 端口:

.. code-block:: properties

    rpc.tcp_client_port = 5369

RPC 的 TCP 连接个数:

.. code-block:: properties

    rpc.tcp_client_num = 32

RPC 连接超时时间:

.. code-block:: properties

    rpc.connect_timeout = 5s

RPC 发送超时时间:

.. code-block:: properties

    rpc.send_timeout = 5s

认证超时时间:

.. code-block:: properties

    rpc.authentication_timeout = 5s

同步调用超时时间:

.. code-block:: properties

    rpc.call_receive_timeout = 15s

socket 空闲时最大保持连接时间:

.. code-block:: properties

    rpc.socket_keepalive_idle = 900s

socket 保活探测间隔:

.. code-block:: properties

    rpc.socket_keepalive_interval = 75s

关闭连接前心跳探测最大失败次数:

.. code-block:: properties

    rpc.socket_keepalive_count = 9

RPC 的 TCP 发送缓存大小:

.. code-block:: properties

    rpc.socket_sndbuf = 1MB

RPC 的 TCP 发送缓存大小:

.. code-block:: properties

    rpc.socket_recbuf = 1MB

RPC 的 Socket (用户态)缓存大小:

.. code-block:: properties

    rpc.socket_buffer = 1MB

------------
日志参数配置
------------

日志输出位置，可设置写到终端或写到文件:

.. code-block:: properties

    log.to = both

设置日志级别:

.. code-block:: properties

    log.level = warning

设置 primary logger level，以及所有到文件和终端的 logger handlers 的日志级别。

设置日志文件的存储路径:

.. code-block:: properties

    log.dir = log

设置存储 “log.level” 日志的文件名:

.. code-block:: properties

    log.file = emqx.log

设置每个日志文件的最大大小:

.. code-block:: properties

    log.rotation.size = 10MB

设置循环日志记录的最大文件数量:

.. code-block:: properties

    log.rotation.count = 5

可以通过配置额外的 file logger handlers，将某个级别的日志写到单独的文件，配置格式为 log.$level.file = $filename.

例如，下面的配置将所有的大于等于 info 级别的日志额外写到 info.log 文件中:

.. code-block:: properties

    log.info.file = info.log

-------------------
匿名认证与 ACL 文件
-------------------

是否允许客户端以匿名身份通过验证:

.. code-block:: properties

    allow_anonymous = true

*EMQ X* 支持基于内置 ACL 以及 MySQL、 PostgreSQL 等插件的 ACL。

设置所有 ACL 规则都不能匹配时是否允许访问:

.. code-block:: properties

    acl_nomatch = allow

设置存储 ACL 规则的默认文件:

.. code-block:: properties

    acl_file = etc/acl.conf

设置是否允许 ACL 缓存:

.. code-block:: properties

    enable_acl_cache = on

设置每个客户端 ACL 最大缓存数量:

.. code-block:: properties

    acl_cache_max_size = 32

设置 ACL 缓存的有效时间:

.. code-block:: properties

    acl_cache_ttl = 1m

etc/acl.conf 访问控制规则定义::

    允许|拒绝  用户|IP地址|ClientID  发布|订阅  主题列表

访问控制规则采用 Erlang 元组格式，访问控制模块逐条匹配规则:

.. image:: _static/images/config_1.png

etc/acl.conf 默认访问规则设置:

允许 ``dashboard`` 用户订阅 ``$SYS/#``:

.. code-block:: erlang

    {allow, {user, "dashboard"}, subscribe, ["$SYS/#"]}.

允许本机用户发布订阅全部主题:

.. code-block:: erlang

    {allow, {ipaddr, "127.0.0.1"}, pubsub, ["$SYS/#", "#"]}.

拒绝除本机用户以外的其他用户订阅 ``$SYS/#`` 与 ``#`` 主题:

.. code-block:: erlang

    {deny, all, subscribe, ["$SYS/#", {eq, "#"}]}.

允许上述规则以外的任何情形:

.. code-block:: erlang

    {allow, all}.

.. NOTE:: 默认规则只允许本机用户订阅 $SYS/# 与 #。

*EMQ X* 消息服务器接收到 MQTT 客户端发布(Publish)或订阅(Subscribe)请求时，会逐条匹配 ACL 规则，直到匹配成功返回 allow 或 deny。

设置 flapping 的检测策略:

.. code-block:: properties

    ## flapping_detect_policy = <Threshold>, <Duration>, <Banned Interval>
    ## <Threshold>: 指定 MQTT 客户端在 <Duration> 时间内能够重复连接的最大次数
    ## <Duration>: 指定 flapping 检测的时间窗口
    ## <Banned Interval>: 指定 MQTT 客户端超出连接限制后被禁止登录的时长
    flapping_detect_policy = 30, 1m, 5m

-----------------
MQTT 协议参数配置
-----------------

MQTT 最大报文尺寸:

.. code-block:: properties

    mqtt.max_packet_size = 1MB

ClientId 最大长度:

.. code-block:: properties

    mqtt.max_clientid_len = 65535

Topic 最大层级，0 表示没有限制:

.. code-block:: properties

    mqtt.max_topic_levels = 0

允许的最大 QoS:

.. code-block:: properties

    mqtt.max_qos_allowed = 2

Topic Alias 最大数量，0 表示不支持 Topic Alias:

.. code-block:: properties

    mqtt.max_topic_alias = 0

是否支持 MQTT 保留消息:

.. code-block:: properties

    mqtt.retain_available = true

是否支持 MQTT 通配符订阅:

.. code-block:: properties

    mqtt.wildcard_subscription = true

是否支持 MQTT 共享订阅:

.. code-block:: properties

    mqtt.shared_subscription = true

是否允许消息的 loop deliver:

.. code-block:: properties

    mqtt.ignore_loop_deliver = false

此配置主要为 MQTT v3.1.1 使用，以实现 MQTT 5 中 No Local 的功能。

是否使用严格模式解析 MQTT 报文:

.. code-block:: properties

    mqtt.strict_mode = false

--------------------
MQTT Zones 参数配置
--------------------

EMQ X 使用 Zone 来管理配置组。一个 Zone 定义了一组配置项 (比如最大连接数等)，Listener 可以指定使用某个 Zone，以使用该 Zone 下的所有配置。多个 Listener 可以共享同一个 Zone。

Listener 使用配置的匹配规则如下，其优先级 Zone > Global > Default::

                       ---------              ----------              -----------
    Listeners -------> | Zone  | --nomatch--> | Global | --nomatch--> | Default |
                       ---------              ----------              -----------
                           |                       |                       |
                         match                   match                   match
                          \|/                     \|/                     \|/
                    Zone Configs            Global Configs           Default Configs

*EMQ X* 支持 ``zone.$name.xxx`` 替换成相应的 ``$name`` 的，这里的 ``zone.external.xxx`` 和 ``zone.internal.xxx`` 中的 ``$name`` 都可以换成相应的名称，也可以新增自定义 ``name`` 的 ``zone.$name.xxx``。

External Zone 参数设置
------------------------

TCP 连接建立后等待 MQTT CONNECT 报文的最长时间:

.. code-block:: properties

    zone.external.idle_timeout = 15s

发布消息速率限制:

.. code-block:: properties

    ## zone.external.publish_limit = 10,100

开启 ACL 检查:

.. code-block:: properties

    zone.external.enable_acl = on

开启黑名单检查:

.. code-block:: properties

    zone.external.enable_ban = on

是否统计每个连接的信息:

.. code-block:: properties

    zone.external.enable_stats = on

指定没有通过 ACL 检查时的动作:

.. code-block:: properties

    ## 可选值: ignore | disconnect
    zone.external.acl_deny_action = ignore

设置连接/会话进程在接收多少消息或字节后强制进行 GC:

.. code-block:: properties

    zone.external.force_gc_policy = 1000|1MB

设置连接/会话进程可使用的最大消息队列长度和堆大小，超出限制时将强制关闭进程:

.. code-block:: properties

    ## zone.external.force_shutdown_policy = 8000|800MB

MQTT 最大报文尺寸:

.. code-block:: properties

    ## zone.external.max_packet_size = 64KB

ClientId 最大长度:

.. code-block:: properties

    ## zone.external.max_clientid_len = 1024

Topic 最大层级，0 表示没有限制:

.. code-block:: properties

    ## zone.external.max_topic_levels = 7

允许的最大 QoS:

.. code-block:: properties

    ## zone.external.max_qos_allowed = 2

Topic Alias 最大数量，0 表示不支持 Topic Alias:

.. code-block:: properties

    ## zone.external.max_topic_alias = 0

是否支持 MQTT 保留消息:

.. code-block:: properties

    ## zone.external.retain_available = true

是否支持 MQTT 通配符订阅:

.. code-block:: properties

    ## zone.external.wildcard_subscription = false

是否支持 MQTT 共享订阅:

.. code-block:: properties

    ## zone.external.shared_subscription = false

服务器允许的保持连接时间，注释此行表示保持连接时间由客户端决定:

.. code-block:: properties

    ## zone.external.server_keepalive = 0

Keepalive * backoff * 2 为实际的保持连接时间:

.. code-block:: properties

    zone.external.keepalive_backoff = 0.75

允许的最大主题订阅数量，0 表示没有限制:

.. code-block:: properties

    zone.external.max_subscriptions = 0

是否允许 QoS 升级:

.. code-block:: properties

    zone.external.upgrade_qos = off

飞行窗口的最大大小:

.. code-block:: properties

    zone.external.max_inflight = 32

QoS1/2 消息的重传间隔:

.. code-block:: properties

    zone.external.retry_interval = 30s

等待 PUBREL 的 QoS2 消息最大数量(Client -> Broker)，0 表示没有限制:

.. code-block:: properties

    zone.external.max_awaiting_rel = 100

QoS2 消息(Client -> Broker)被删除前等待 PUBREL 的最大时间

.. code-block:: properties

    zone.external.await_rel_timeout = 300s

MQTT v3.1.1 连接中使用的默认会话过期时间:

.. code-block:: properties

    zone.external.session_expiry_interval = 2h

消息队列最大长度:

.. code-block:: properties

    zone.external.max_mqueue_len = 1000

主题优先级:

.. code-block:: properties

    ## zone.external.mqueue_priorities = topic/1=10,topic/2=8

消息队列是否存储 QoS0 消息:

.. code-block:: properties

    zone.external.mqueue_store_qos0 = true

是否开启 flapping 检测:

.. code-block:: properties

    zone.external.enable_flapping_detect = off

挂载点:

.. code-block:: properties

    ## zone.external.mountpoint = devicebound/

是否使用用户名作为客户端标识符:

.. code-block:: properties

    zone.external.use_username_as_clientid = false

是否接收自己发布的消息:

.. code-block:: properties

    zone.external.ignore_loop_deliver = false

是否使用严格模式解析 MQTT 报文:

.. code-block:: properties

    zone.external.strict_mode = false

Internal Zone 参数设置
------------------------

允许匿名访问:

.. code-block:: properties

    zone.internal.allow_anonymous = true

是否统计每个连接的信息:

.. code-block:: properties

    zone.internal.enable_stats = on

关闭 ACL 检查:

.. code-block:: properties

    zone.internal.enable_acl = off

指定没有通过 ACL 检查时的动作:

.. code-block:: properties

    ## 可选值: ignore | disconnect
    zone.internal.acl_deny_action = ignore

是否支持 MQTT 通配符订阅:

.. code-block:: properties

    ## zone.internal.wildcard_subscription = true

是否支持 MQTT 共享订阅:

.. code-block:: properties

    ## zone.internal.shared_subscription = true

允许的最大主题订阅数量，0 表示没有限制:

.. code-block:: properties

    zone.internal.max_subscriptions = 0

飞行窗口的最大大小:

.. code-block:: properties

    zone.internal.max_inflight = 32

等待 PUBREL 的 QoS2 消息最大数量(Client -> Broker)，0 表示没有限制:

.. code-block:: properties

    zone.internal.max_awaiting_rel = 100

消息队列最大长度:

.. code-block:: properties

    zone.internal.max_mqueue_len = 1000

消息队列是否存储 QoS0 消息:

.. code-block:: properties

    zone.internal.mqueue_store_qos0 = true

是否开启 flapping 检测:

.. code-block:: properties

    zone.internal.enable_flapping_detect = off

挂载点:

.. code-block:: properties

    ## zone.internal.mountpoint = devicebound/

是否使用用户名作为客户端标识符:

.. code-block:: properties

    zone.internal.use_username_as_clientid = false

是否接收自己发布的消息:

.. code-block:: properties

    zone.internal.ignore_loop_deliver = false

是否使用严格模式解析 MQTT 报文:

.. code-block:: properties

    zone.internal.strict_mode = false

-----------------------
MQTT Listeners 参数说明
-----------------------

*EMQ X* 消息服务器支持 MQTT、MQTT/SSL、MQTT/WS 协议服务端，可通过 `listener.tcp|ssl|ws|wss|.*` 设置端口、最大允许连接数等参数。

*EMQ X* 消息服务器默认开启的 TCP 服务端口包括:

+------+------------------------------+
| 1883 | MQTT TCP 协议端口            |
+------+------------------------------+
| 8883 | MQTT/TCP SSL 端口            |
+------+------------------------------+
| 8083 | MQTT/WebSocket 端口          |
+------+------------------------------+
| 8084 | MQTT/WebSocket with SSL 端口 |
+------+------------------------------+

Listener 参数说明:

+----------------------------------------+------------------------------------------+
| listener.tcp.${name}.acceptors         | TCP Acceptor 池                          |
+----------------------------------------+------------------------------------------+
| listener.tcp.${name}.max_connections   | 最大允许 TCP 连接数                      |
+----------------------------------------+------------------------------------------+
| listener.tcp.${name}.max_conn_rate     | 连接限制配置，例如连接1000/秒:  "1000"   |
+----------------------------------------+------------------------------------------+
| listener.tcp.${name}.zone              | 监听属于哪一个 Zone                      |
+----------------------------------------+------------------------------------------+
| listener.tcp.${name}.rate_limit        | 连接速率配置，例如限速10B/秒:  "100,200" |
+----------------------------------------+------------------------------------------+

-----------------------
MQTT/TCP 监听器 - 1883
-----------------------

*EMQ X* 版本支持配置多个 MQTT 协议监听器，例如配置名为 external、internal 两个监听器:

TCP 监听器:

.. code-block:: properties

    listener.tcp.external = 0.0.0.0:1883

接收池大小:

.. code-block:: properties

    listener.tcp.external.acceptors = 8

最大并发连接数:

.. code-block:: properties

    listener.tcp.external.max_connections = 1024000

每秒最大创建连接数:

.. code-block:: properties

    listener.tcp.external.max_conn_rate = 1000

监听器使用的 Zone:

.. code-block:: properties

    listener.tcp.external.zone = external

TCP 数据接收速率限制:

.. code-block:: properties

    ## 限制每 10s 内只能接收 100KB 数据
    ## listener.tcp.external.rate_limit = 100KB,10s

访问控制规则:

.. code-block:: properties

    listener.tcp.external.access.1 = allow all

EMQ X 集群部署在 HAProxy 或 Nginx 时，是否启用代理协议 V1/2:

.. code-block:: properties

    ## listener.tcp.external.proxy_protocol = on

代理协议的超时时间:

.. code-block:: properties

    ## listener.tcp.external.proxy_protocol_timeout = 3s

启用基于 X.509 证书的身份验证选项。EMQ X 将使用证书的公共名称作为 MQTT 用户名:

.. code-block:: properties

    ## listener.tcp.external.peer_cert_as_username = cn

挂起连接的队列的最大长度:

.. code-block:: properties

    listener.tcp.external.backlog = 1024

TCP 发送超时时间:

.. code-block:: properties

    listener.tcp.external.send_timeout = 15s

发送超时时是否关闭 TCP 连接:

.. code-block:: properties

    listener.tcp.external.send_timeout_close = on

用于 MQTT 连接的 TCP 接收缓冲区(os内核):

.. code-block:: properties

    #listener.tcp.external.recbuf = 2KB

用于 MQTT 连接的 TCP 发送缓冲区(os内核):

.. code-block:: properties

    #listener.tcp.external.sndbuf = 2KB

驱动程序使用的用户级软件缓冲区的大小，不要与选项 sndbuf 和 recbuf 混淆，
它们对应于内核套接字缓冲区。建议使用 val(buffer) >= max(val(sndbuf)，val(recbuf))
来避免不必要的复制带来的性能问题。当设置 sndbuf 或 recbuf 值时，val(buffer) 自动设置为上述最大值:

.. code-block:: properties

    #listener.tcp.external.buffer = 2KB

是否设置 buffer = max(sndbuf, recbuf):

.. code-block:: properties

    ## listener.tcp.external.tune_buffer = off

是否设置 TCP_NODELAY 标志。如果启用该选项，发送缓冲区一旦有数据就会尝试发送:

.. code-block:: properties

    listener.tcp.external.nodelay = true

是否设置 SO_REUSEADDR 标志:

.. code-block:: properties

    listener.tcp.external.reuseaddr = true

----------------------
MQTT/SSL 监听器 - 8883
----------------------

SSL 监听端口:

.. code-block:: properties

    listener.ssl.external = 8883

接收池大小:

.. code-block:: properties

    listener.ssl.external.acceptors = 16

最大并发连接数:

.. code-block:: properties

    listener.ssl.external.max_connections = 102400

每秒最大创建连接数:

.. code-block:: properties

    listener.ssl.external.max_conn_rate = 500

指定 SSL 的 {active, N} 选项:

.. code-block:: properties

    listener.ssl.external.active_n = 100

监听器使用的 Zone:

.. code-block:: properties

    listener.ssl.external.zone = external

TCP 数据接收速率限制:

.. code-block:: properties

    ## listener.ssl.external.rate_limit = 100KB,10s

访问控制规则:

.. code-block:: properties

    listener.ssl.external.access.1 = allow all

EMQ X 集群部署在 HAProxy 或 Nginx 时，是否启用代理协议 V1/2:

.. code-block:: properties

    ## listener.ssl.external.proxy_protocol = on

代理协议的超时时间:

.. code-block:: properties

    ## listener.ssl.external.proxy_protocol_timeout = 3s

TLS 版本，防止 POODLE 攻击:

.. code-block:: properties

    ## listener.ssl.external.tls_versions = tlsv1.2,tlsv1.1,tlsv1

TLS 握手超时时间:

.. code-block:: properties

    listener.ssl.external.handshake_timeout = 15s

包含用户私钥的文件的路径:

.. code-block:: properties

    listener.ssl.external.keyfile = etc/certs/key.pem

包含用户证书的文件的路径:

.. code-block:: properties

    listener.ssl.external.certfile = etc/certs/cert.pem

包含 CA 证书的文件的路径:

.. code-block:: properties

    ## listener.ssl.external.cacertfile = etc/certs/cacert.pem

包含 dh-params 的文件的路径:

.. code-block:: properties

    ## listener.ssl.external.dhfile = etc/certs/dh-params.pem

配置 verify 模式，服务器只在 verify_peer 模式下执行 x509 路径验证，并向客户端发送一个证书请求:

.. code-block:: properties

    ## listener.ssl.external.verify = verify_peer

服务器为 verify_peer 模式时，如果客户端没有要发送的证书，服务器是否返回失败:

.. code-block:: properties

    ## listener.ssl.external.fail_if_no_peer_cert = true

SSL cipher suites:

.. code-block:: properties

    listener.ssl.external.ciphers = ECDHE-ECDSA-AES256-GCM-SHA384,ECDHE-RSA-AES256-GCM-SHA384,ECDHE-ECDSA-AES256-SHA384,ECDHE-RSA-AES256-SHA384,ECDHE-ECDSA-DES-CBC3-SHA,ECDH-ECDSA-AES256-GCM-SHA384,ECDH-RSA-AES256-GCM-SHA384,ECDH-ECDSA-AES256-SHA384,ECDH-RSA-AES256-SHA384,DHE-DSS-AES256-GCM-SHA384,DHE-DSS-AES256-SHA256,AES256-GCM-SHA384,AES256-SHA256,ECDHE-ECDSA-AES128-GCM-SHA256,ECDHE-RSA-AES128-GCM-SHA256,ECDHE-ECDSA-AES128-SHA256,ECDHE-RSA-AES128-SHA256,ECDH-ECDSA-AES128-GCM-SHA256,ECDH-RSA-AES128-GCM-SHA256,ECDH-ECDSA-AES128-SHA256,ECDH-RSA-AES128-SHA256,DHE-DSS-AES128-GCM-SHA256,DHE-DSS-AES128-SHA256,AES128-GCM-SHA256,AES128-SHA256,ECDHE-ECDSA-AES256-SHA,ECDHE-RSA-AES256-SHA,DHE-DSS-AES256-SHA,ECDH-ECDSA-AES256-SHA,ECDH-RSA-AES256-SHA,AES256-SHA,ECDHE-ECDSA-AES128-SHA,ECDHE-RSA-AES128-SHA,DHE-DSS-AES128-SHA,ECDH-ECDSA-AES128-SHA,ECDH-RSA-AES128-SHA,AES128-SHA

SSL PSK cipher suites:

.. code-block:: properties

    ## listener.ssl.external.psk_ciphers = PSK-AES128-CBC-SHA,PSK-AES256-CBC-SHA,PSK-3DES-EDE-CBC-SHA,PSK-RC4-SHA

是否启动更安全的 renegotiation 机制:

.. code-block:: properties

    ## listener.ssl.external.secure_renegotiate = off

是否允许客户端重用一个已存在的会话:

.. code-block:: properties

    ## listener.ssl.external.reuse_sessions = on

是否强制根据服务器指定的顺序而不是客户端指定的顺序设置密码:

.. code-block:: properties

    ## listener.ssl.external.honor_cipher_order = on

使用客户端证书中的 CN、EN 或 CRT 字段作为用户名。注意，“verify” 应该设置为 “verify_peer”:

.. code-block:: properties

    ## listener.ssl.external.peer_cert_as_username = cn

挂起连接的队列的最大长度:

.. code-block:: properties

    ## listener.ssl.external.backlog = 1024

TCP 发送超时时间:

.. code-block:: properties

    ## listener.ssl.external.send_timeout = 15s

发送超时时是否关闭 TCP 连接:

.. code-block:: properties

    ## listener.ssl.external.send_timeout_close = on

用于 MQTT 连接的 TCP 接收缓冲区(os内核):

.. code-block:: properties

    #listener.ssl.external.recbuf = 4KB

用于 MQTT 连接的 TCP 发送缓冲区(os内核):

.. code-block:: properties

    ## listener.ssl.external.sndbuf = 4KB

驱动程序使用的用户级软件缓冲区的大小，不要与选项 sndbuf 和 recbuf 混淆，
它们对应于内核套接字缓冲区。建议使用 val(buffer) >= max(val(sndbuf)，val(recbuf))
来避免不必要的复制带来的性能问题。当设置 sndbuf 或 recbuf 值时，val(buffer) 自动设置为上述最大值:

.. code-block:: properties

    ## listener.ssl.external.buffer = 4KB

是否设置 buffer = max(sndbuf, recbuf):

.. code-block:: properties

    ## listener.ssl.external.tune_buffer = off

是否设置 TCP_NODELAY 标志。如果启用该选项，发送缓冲区一旦有数据就会尝试发送:

.. code-block:: properties

    ## listener.ssl.external.nodelay = true

是否设置 SO_REUSEADDR 标志:

.. code-block:: properties

    listener.ssl.external.reuseaddr = true

----------------------------
MQTT/WebSocket 监听器 - 8083
----------------------------

MQTT/WebSocket 监听端口:

.. code-block:: properties

    listener.ws.external = 8083

MQTT/WebSocket 端点路径:

.. code-block:: properties

    listener.ws.external.mqtt_path = /mqtt

接收池大小:

.. code-block:: properties

    listener.ws.external.acceptors = 4

最大并发连接数:

.. code-block:: properties

    listener.ws.external.max_connections = 102400

每秒最大创建连接数:

.. code-block:: properties

    listener.ws.external.max_conn_rate = 1000

TCP 数据接收速率限制:

.. code-block:: properties

    ## listener.ws.external.rate_limit = 100KB,10s

监听器使用的 Zone:

.. code-block:: properties

    listener.ws.external.zone = external

访问控制规则:

.. code-block:: properties

    listener.ws.external.access.1 = allow all

是否验证协议头是否有效:

.. code-block:: properties

    listener.ws.external.verify_protocol_header = on

EMQ X 集群部署在 NGINX 或 HAProxy 之后，使用 X-Forward-For 来识别原始 IP:

.. code-block:: properties

    ## listener.ws.external.proxy_address_header = X-Forwarded-For

EMQ X 集群部署在 NGINX 或 HAProxy 之后，使用 X-Forward-Port 来识别原始端口:

.. code-block:: properties

    ## listener.ws.external.proxy_port_header = X-Forwarded-Port

EMQ X 集群部署在 HAProxy 或 Nginx 时，是否启用代理协议 V1/2:

.. code-block:: properties

    ## listener.ws.external.proxy_protocol = on

代理协议超时时间:

.. code-block:: properties

    ## listener.ws.external.proxy_protocol_timeout = 3s

挂起连接的队列的最大长度:

.. code-block:: properties

    listener.ws.external.backlog = 1024

TCP 发送超时时间:

.. code-block:: properties

    listener.ws.external.send_timeout = 15s

发送超时时是否关闭 TCP 连接:

.. code-block:: properties

    listener.ws.external.send_timeout_close = on

用于 MQTT 连接的 TCP 接收缓冲区(os内核):

.. code-block:: properties

    ## listener.ws.external.recbuf = 2KB

用于 MQTT 连接的 TCP 发送缓冲区(os内核):

.. code-block:: properties

    ## listener.ws.external.sndbuf = 2KB

驱动程序使用的用户级软件缓冲区的大小，不要与选项 sndbuf 和 recbuf 混淆，
它们对应于内核套接字缓冲区。建议使用 val(buffer) >= max(val(sndbuf)，val(recbuf))
来避免不必要的复制带来的性能问题。当设置 sndbuf 或 recbuf 值时，val(buffer) 自动设置为上述最大值:

.. code-block:: properties

    ## listener.ws.external.buffer = 2KB

是否设置 buffer = max(sndbuf, recbuf):

.. code-block:: properties

    ## listener.ws.external.tune_buffer = off

是否设置 TCP_NODELAY 标志。如果启用该选项，发送缓冲区一旦有数据就会尝试发送:

.. code-block:: properties

    listener.ws.external.nodelay = true

是否压缩 Websocket 消息:

.. code-block:: properties

    ## listener.ws.external.compress = true

Websocket deflate 选项:

.. code-block:: properties

    ## listener.ws.external.deflate_opts.level = default
    ## listener.ws.external.deflate_opts.mem_level = 8
    ## listener.ws.external.deflate_opts.strategy = default
    ## listener.ws.external.deflate_opts.server_context_takeover = takeover
    ## listener.ws.external.deflate_opts.client_context_takeover = takeover
    ## listener.ws.external.deflate_opts.server_max_window_bits = 15
    ## listener.ws.external.deflate_opts.client_max_window_bits = 15

最大空闲时间:

.. code-block:: properties

    ## listener.ws.external.idle_timeout = 60s

最大报文大小，0 表示没有限制:

.. code-block:: properties

    ## listener.ws.external.max_frame_size = 0

-------------------------------------
MQTT/WebSocket with SSL 监听器 - 8084
-------------------------------------

MQTT/WebSocket with SSL 监听端口:

.. code-block:: properties

    listener.wss.external = 8084

接收池大小:

.. code-block:: properties

    listener.wss.external.acceptors = 4

最大并发连接数:

.. code-block:: properties

    listener.wss.external.max_connections = 16

每秒最大创建连接数:

.. code-block:: properties

    listener.wss.external.max_conn_rate = 1000

TCP 数据接收速率限制:

.. code-block:: properties

    ## listener.wss.external.rate_limit = 100KB,10s

监听器使用的 Zone:

.. code-block:: properties

    listener.wss.external.zone = external

访问控制规则:

.. code-block:: properties

    listener.wss.external.access.1 = allow all

是否验证协议头是否有效:

.. code-block:: properties

    listener.wss.external.verify_protocol_header = on

EMQ X 集群部署在 NGINX 或 HAProxy 之后，使用 X-Forward-For 来识别原始 IP:

.. code-block:: properties

    ## listener.wss.external.proxy_address_header = X-Forwarded-For

EMQ X 集群部署在 NGINX 或 HAProxy 之后，使用 X-Forward-Port 来识别原始端口:

.. code-block:: properties

    ## listener.wss.external.proxy_port_header = X-Forwarded-Port

EMQ X 集群部署在 HAProxy 或 Nginx 时，是否启用代理协议 V1/2:

.. code-block:: properties

    ## listener.wss.external.proxy_protocol = on

代理协议超时时间:

.. code-block:: properties

    ## listener.wss.external.proxy_protocol_timeout = 3s

TLS 版本，防止 POODLE 攻击:

.. code-block:: properties

    ## listener.wss.external.tls_versions = tlsv1.2,tlsv1.1,tlsv1

包含用户私钥的文件的路径:

.. code-block:: properties

    listener.wss.external.keyfile = etc/certs/key.pem

包含用户证书的文件的路径:

.. code-block:: properties

    listener.wss.external.certfile = etc/certs/cert.pem

包含 CA 证书的文件的路径:

.. code-block:: properties

    ## listener.wss.external.cacertfile = etc/certs/cacert.pem

包含 dh-params 的文件的路径:

.. code-block:: properties

    ## listener.ssl.external.dhfile = etc/certs/dh-params.pem

配置 verify 模式，服务器只在 verify_peer 模式下执行 x509 路径验证，并向客户端发送一个证书请求:

.. code-block:: properties

    ## listener.wss.external.verify = verify_peer

服务器为 verify_peer 模式时，如果客户端没有要发送的证书，服务器是否返回失败:

.. code-block:: properties

    ## listener.wss.external.fail_if_no_peer_cert = true

SSL cipher suites:

.. code-block:: properties

    ## listener.wss.external.ciphers = ECDHE-ECDSA-AES256-GCM-SHA384,ECDHE-RSA-AES256-GCM-SHA384,ECDHE-ECDSA-AES256-SHA384,ECDHE-RSA-AES256-SHA384,ECDHE-ECDSA-DES-CBC3-SHA,ECDH-ECDSA-AES256-GCM-SHA384,ECDH-RSA-AES256-GCM-SHA384,ECDH-ECDSA-AES256-SHA384,ECDH-RSA-AES256-SHA384,DHE-DSS-AES256-GCM-SHA384,DHE-DSS-AES256-SHA256,AES256-GCM-SHA384,AES256-SHA256,ECDHE-ECDSA-AES128-GCM-SHA256,ECDHE-RSA-AES128-GCM-SHA256,ECDHE-ECDSA-AES128-SHA256,ECDHE-RSA-AES128-SHA256,ECDH-ECDSA-AES128-GCM-SHA256,ECDH-RSA-AES128-GCM-SHA256,ECDH-ECDSA-AES128-SHA256,ECDH-RSA-AES128-SHA256,DHE-DSS-AES128-GCM-SHA256,DHE-DSS-AES128-SHA256,AES128-GCM-SHA256,AES128-SHA256,ECDHE-ECDSA-AES256-SHA,ECDHE-RSA-AES256-SHA,DHE-DSS-AES256-SHA,ECDH-ECDSA-AES256-SHA,ECDH-RSA-AES256-SHA,AES256-SHA,ECDHE-ECDSA-AES128-SHA,ECDHE-RSA-AES128-SHA,DHE-DSS-AES128-SHA,ECDH-ECDSA-AES128-SHA,ECDH-RSA-AES128-SHA,AES128-SHA

是否启动更安全的 renegotiation 机制:

.. code-block:: properties

    ## listener.wss.external.secure_renegotiate = off

是否允许客户端重用一个已存在的会话:

.. code-block:: properties

    ## listener.wss.external.reuse_sessions = on

是否强制根据服务器指定的顺序而不是客户端指定的顺序设置密码:

.. code-block:: properties

    ## listener.wss.external.honor_cipher_order = on

使用客户端证书中的 CN、EN 或 CRT 字段作为用户名。注意，“verify” 应该设置为 “verify_peer”:

.. code-block:: properties

    ## listener.wss.external.peer_cert_as_username = cn

挂起连接的队列的最大长度:

.. code-block:: properties

    listener.wss.external.backlog = 1024

TCP 发送超时时间:

.. code-block:: properties

    listener.wss.external.send_timeout = 15s

发送超时时是否关闭 TCP 连接:

.. code-block:: properties

    listener.wss.external.send_timeout_close = on

用于 MQTT 连接的 TCP 接收缓冲区(os内核):

.. code-block:: properties

    ## listener.wss.external.recbuf = 4KB

用于 MQTT 连接的 TCP 发送缓冲区(os内核):

.. code-block:: properties

    ## listener.wss.external.sndbuf = 4KB

驱动程序使用的用户级软件缓冲区的大小，不要与选项 sndbuf 和 recbuf 混淆，
它们对应于内核套接字缓冲区。建议使用 val(buffer) >= max(val(sndbuf)，val(recbuf))
来避免不必要的复制带来的性能问题。当设置 sndbuf 或 recbuf 值时，val(buffer) 自动设置为上述最大值:

.. code-block:: properties

    ## listener.wss.external.buffer = 4KB

是否设置 TCP_NODELAY 标志。如果启用该选项，发送缓冲区一旦有数据就会尝试发送:

.. code-block:: properties

    ## listener.wss.external.nodelay = true

是否压缩 Websocket 消息:

.. code-block:: properties

    ## listener.wss.external.compress = true

Websocket deflate 选项:

.. code-block:: properties

    ## listener.wss.external.deflate_opts.level = default
    ## listener.wss.external.deflate_opts.mem_level = 8
    ## listener.wss.external.deflate_opts.strategy = default
    ## listener.wss.external.deflate_opts.server_context_takeover = takeover
    ## listener.wss.external.deflate_opts.client_context_takeover = takeover
    ## listener.wss.external.deflate_opts.server_max_window_bits = 15
    ## listener.wss.external.deflate_opts.client_max_window_bits = 15

最大空闲时间:

.. code-block:: properties

    ## listener.wss.external.idle_timeout = 60s

最大报文大小，0 表示没有限制:

.. code-block:: properties

    ## listener.wss.external.max_frame_size = 0

--------------
Modules 模块
--------------

*EMQ X* 支持模块扩展，默认三个模块，分别为上下线消息状态发布模块、代理订阅模块、主题(Topic)重写模块。

上下线消息状态发布模块
----------------------

是否启动上下线消息状态发布模块:

.. code-block:: properties

    module.presence = on

上下线消息状态发布模块发布 MQTT 消息时使用的 QoS:

.. code-block:: properties

    module.presence.qos = 1

代理订阅模块
------------

是否启动代理订阅模块:

.. code-block:: properties

    module.subscription = off

客户端连接时自动订阅的主题与 QoS:

.. code-block:: properties

    ## Subscribe the Topics's qos
    ## module.subscription.1.topic = $client/%c
    ## module.subscription.1.qos = 0
    ## module.subscription.2.topic = $user/%u
    ## module.subscription.2.qos = 1

主题重写模块
------------

是否启动主题重写模块:

.. code-block:: properties

    module.rewrite = off

主题重写规则:

.. code-block:: properties

    ## module.rewrite.rule.1 = x/# ^x/y/(.+)$ z/y/$1
    ## module.rewrite.rule.2 = y/+/z/# ^y/(.+)/z/(.+)$ y/z/$2

----------------
扩展插件配置文件
----------------

存放插件配置文件的目录:

.. code-block:: properties

    plugins.etc_dir = etc/plugins/

存储启动时需要自动加载的插件列表的文件的路径:

.. code-block:: properties

    plugins.loaded_file = data/loaded_plugins

*EMQ X* 插件配置文件，默认在 etc/plugins/ 目录，可修改 plugins.etc_dir 来调整目录。

----------------
Broker 参数设置
----------------

系统消息的发布间隔:

.. code-block:: properties

    broker.sys_interval = 1m

系统心跳的发布间隔:

.. code-block:: properties

    broker.sys_heartbeat = 30s

是否全局注册会话:

.. code-block:: properties

    broker.enable_session_registry = on

会话锁策略:

.. code-block:: properties

    broker.session_locking_strategy = quorum

共享订阅的分发策略:

.. code-block:: properties

    broker.shared_subscription_strategy = random

共享分发时是否需要 ACK:

.. code-block:: properties

    broker.shared_dispatch_ack_enabled = false

是否开启路由批量清理功能:

.. code-block:: properties

    broker.route_batch_clean = off

---------------------
Erlang 虚拟机监控设置
---------------------

是否开启 long_gc 监控以及垃圾回收持续多久时会触发 long_gc 事件，设置为 0 表示不监控此事件:

.. code-block:: properties

    sysmon.long_gc = 0

系统中的进程或端口不间断地运行多久时会触发 long_schedule 事件，设置为 0 表示不监控此事件:

.. code-block:: properties

    sysmon.long_schedule = 240

垃圾回收导致分配的堆大小为多大时将触发 large_heap 事件:

.. code-block:: properties

    sysmon.large_heap = 8MB

系统中的进程因为发送到繁忙端口而挂起时是否触发 busy_port 事件:

.. code-block:: properties

    sysmon.busy_port = false

是否监控 Erlang 分布式端口繁忙事件:

.. code-block:: properties

    sysmon.busy_dist_port = true

cpu 占用率的检查周期:

.. code-block:: properties

    os_mon.cpu_check_interval = 60s

cpu 占用率高于多少时产生告警:

.. code-block:: properties

    os_mon.cpu_high_watermark = 80%

cpu 占用率低于多少时清除告警:

.. code-block:: properties

    os_mon.cpu_low_watermark = 60%

内存占用率的检查周期:

.. code-block:: properties

    os_mon.mem_check_interval = 60s

系统内存占用率高于多少时产生告警:

.. code-block:: properties

    os_mon.sysmem_high_watermark = 70%

单个进程内存占用率高于多少时产生告警:

.. code-block:: properties

    os_mon.procmem_high_watermark = 5%

进程数量的检查周期:

.. code-block:: properties

    vm_mon.check_interval = 30s

当前进程数量与进程数量最大限制的比率达到多少时产生告警:

.. code-block:: properties

    vm_mon.process_high_watermark = 80%

当前进程数量与进程数量最大限制的比率达到多少时清除告警:

.. code-block:: properties

    vm_mon.process_low_watermark = 60%
