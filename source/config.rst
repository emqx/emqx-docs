
.. _configuration:

=========================
配置说明 (Configuration)
=========================

--------------------
EMQ X R3.0 配置文件
--------------------

*EMQ X* R3.0 消息服务器通过 etc/ 目录下配置文件进行设置，主要配置文件包括:

+----------------------------+--------------------------------------+
| 配置文件                   | 说明                                 |
+----------------------------+--------------------------------------+
| etc/emqx.conf              | EMQ X R3.0 消息服务器配置文件        |
+----------------------------+--------------------------------------+
| etc/acl.conf               | EMQ X R3.0 默认ACL规则配置文件       |
+----------------------------+--------------------------------------+
| etc/plugins/\*.conf        | EMQ X R3.0 各类插件配置文件          |
+----------------------------+--------------------------------------+

----------------
EMQ 配置变更历史
----------------

为方便用户与插件开发者使用，*EMQ* 配置文件经过四次调整。

1. EMQ 1.x 版本采用 Erlang 原生配置文件格式 etc/emqttd.config:

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

2. EMQ 2.0-beta.x 版本简化了原生 Erlang 配置文件，采用类似 rebar.config 或 relx.config 格式:

.. code-block:: erlang

    %% Max ClientId Length Allowed.
    {mqtt_max_clientid_len, 512}.

    %% Max Packet Size Allowed, 64K by default.
    {mqtt_max_packet_size, 65536}.

    %% Client Idle Timeout.
    {mqtt_client_idle_timeout, 30}. % Second

简化后的 Erlang 原生配置格式方便用户配置，但插件开发者不得不依赖 gen_conf 库，而不是通过 appliaton:get_env 读取配置参数。

3. EMQ 2.0-rc.2 正式版集成了 cuttlefish 库，采用了类似 sysctl 的 `k = v` 通用格式，并在系统启动时翻译成 Erlang 原生配置格式:

.. code-block:: properties

    ## Node name
    node.name = emqttd@127.0.0.1
    ...
    ## Max ClientId Length Allowed.
    mqtt.max_clientid_len = 1024
    ...

4. EMQ 3.0-beta1 测试版正式更名 emqttd 为 emqx ，配置名称与配置信息进行相关变化:

.. code-block:: properties

    ## Profile
    etc/emqx.config  ==》 etc/emqx.config

    ## Node name
    原先:
    node.name = emqx@127.0.0.1
    现在:
    node.name = emqx@127.0.0.1


EMQ X R3.0 启动时配置文件处理流程::

    ----------------------                                          3.0/schema/*.schema      -------------------
    | etc/emqx.conf      |                   -----------------              \|/              | data/app.config |
    |       +            | --> mergeconf --> | data/app.conf | -->  cuttlefish generate  --> |                 |
    | etc/plugins/*.conf |                   -----------------                               | data/vm.args    |
    ----------------------                                                                   -------------------

-------------------
EMQ X R3.0 环境变量
-------------------

+--------------------+------------------------------------------+
| EMQX_NODE_NAME     | Erlang 节点名称，例如: emqx@127.0.0.1    |
+--------------------+------------------------------------------+
| EMQX_NODE_COOKIE   | Erlang 分布式节点通信 Cookie             |
+--------------------+------------------------------------------+
| EMQX_MAX_PORTS     | Erlang 虚拟机最大允许打开文件 Socket 数  |
+--------------------+------------------------------------------+
| EMQX_TCP_PORT      | MQTT/TCP 监听端口，默认: 1883            |
+--------------------+------------------------------------------+
| EMQX_SSL_PORT      | MQTT/SSL 监听端口，默认: 8883            |
+--------------------+------------------------------------------+
| EMQX_WS_PORT       | MQTT/WebSocket 监听端口，默认: 8083      |
+--------------------+------------------------------------------+
| EMQX_WSS_PORT      | MQTT/WebSocket/SSL 监听端口，默认: 8084  |
+--------------------+------------------------------------------+

--------------
EMQ X 集群设置
--------------

集群名称
--------

.. code-block:: properties

    ## Cluster name
    cluster.name = emqxcl

自动发现策略
-------------

.. code-block:: properties

    ## Cluster discovery strategy: manual | static | mcast | dns | etcd | k8s
    cluster.discovery = manual

启用集群自愈
-------------

.. code-block:: properties

    ## Cluster Autoheal: on | off
    cluster.autoheal = on

节点自动清除
------------

自动清除宕机节点:

.. code-block:: properties

    ## Clean down node of the cluster
    cluster.autoclean = 5m

------------------
EMQ X 集群自动发现
------------------

EMQ X R3.0 版本支持多种策略的节点自动发现与集群:

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

manual 手动创建集群
-------------------

默认配置为手动创建集群，节点通过 `./bin/emqx_ctl join <Node>` 命令加入:

.. code-block:: properties

    cluster.discovery = manual

基于 static 节点列表自动集群
----------------------------

配置固定的节点列表，自动发现并创建集群:

.. code-block:: properties

    cluster.discovery = static

    ##--------------------------------------------------------------------
    ## Cluster with static node list

    cluster.static.seeds = emqx1@127.0.0.1,emqx2@127.0.0.1

基于 mcast 组播自动集群
-----------------------

基于 UDP 组播自动发现并创建集群:

.. code-block:: properties

    cluster.discovery = mcast

    ##--------------------------------------------------------------------
    ## Cluster with multicast

    cluster.mcast.addr = 239.192.0.1

    cluster.mcast.ports = 4369,4370

    cluster.mcast.iface = 0.0.0.0

    cluster.mcast.ttl = 255

    cluster.mcast.loop = on

基于 DNS A 记录自动集群
-----------------------

基于 DNS A 记录自动发现并创建集群:

.. code-block:: properties

    cluster.discovery = dns

    ##--------------------------------------------------------------------
    ## Cluster with DNS

    cluster.dns.name = localhost

    cluster.dns.app  = emqx

基于 etcd 自动集群
------------------

基于 `etcd`_ 自动发现并创建集群:

.. code-block:: properties

    cluster.discovery = etcd

    ##--------------------------------------------------------------------
    ## Cluster with Etcd

    cluster.etcd.server = http://127.0.0.1:2379

    cluster.etcd.prefix = emqxcl

    cluster.etcd.node_ttl = 1m

    cluster.etcd.ssl.keyfile = etc/certs/client-key.pem

    cluster.etcd.ssl.certfile = etc/certs/client.pem

    cluster.etcd.ssl.cacertfile = etc/certs/ca.pem

基于 Kubernetes 自动集群
------------------------

`Kubernetes`_ 下自动发现并创建集群:

.. code-block:: properties

    cluster.discovery = k8s

    ##--------------------------------------------------------------------
    ## Cluster with k8s

    cluster.k8s.apiserver = http://10.110.111.204:8080

    cluster.k8s.service_name = emqx

    ## Address Type: ip | dns
    cluster.k8s.address_type = ip

    ## The Erlang application name
    cluster.k8s.app_name = emqx

    ## Kubernates Namespace
    cluster.k8s.namespace = default

-------------------
EMQ X 节点与 Cookie
-------------------

Erlang 节点名称、分布式节点间通信 Cookie:

.. code-block:: properties

    ## Node name
    node.name = emqx@127.0.0.1

    ## Cookie for distributed node
    node.cookie = emqxsecretcookie

.. NOTE::

    Erlang/OTP 平台应用多由分布的 Erlang 节点(进程)组成，每个 Erlang 节点(进程)需指配一个节点名，用于节点间通信互访。
    所有互相通信的 Erlang 节点(进程)间通过一个共用的 Cookie 进行安全认证。

------------------
EMQ X 节点连接方式
------------------

*EMQ X* 节点基于 Erlang/OTP 平台的 TCPv4, TCPv6 或 TLS 协议连接:

.. code-block:: properties

    ## Specify the erlang distributed protocol.
    ##
    ## Value: Enum
    ##  - inet_tcp: the default; handles TCP streams with IPv4 addressing.
    ##  - inet6_tcp: handles TCP with IPv6 addressing.
    ##  - inet_tls: using TLS for Erlang Distribution.
    ##
    ## vm.args: -proto_dist inet_tcp
    node.proto_dist = inet_tcp

    ## Specify SSL Options in the file if using SSL for Erlang Distribution.
    ##
    ## Value: File
    ##
    ## vm.args: -ssl_dist_optfile <File>
    ## node.ssl_dist_optfile = {{ platform_etc_dir }}/ssl_dist.conf

-----------------
Erlang 虚拟机参数
-----------------

.. code-block:: properties

    ## SMP support: enable, auto, disable
    node.smp = auto

    ## Heartbeat monitoring of an Erlang runtime system
    ## Comment the line to disable
    ## node.heartbeat = on

    ## async thread pool
    node.async_threads = 32

    ## Erlang Process Limit
    node.process_limit = 256000

    ## Sets the maximum number of simultaneously existing ports for this system
    node.max_ports = 256000

    ## Set the distribution buffer busy limit (dist_buf_busy_limit)
    node.dist_buffer_size = 8MB

    ## Max ETS Tables.
    ## Note that mnesia and SSL will create temporary ets tables.
    node.max_ets_tables = 256000

    ## Tweak GC to run more often
    node.fullsweep_after = 1000

    ## Crash dump
    node.crash_dump = log/crash.dump

    ## Specify the erlang distributed protocol.
    node.proto_dist = inet_tcp

    ## Specify SSL Options in the file if using SSL for Erlang Distribution.
    ## node.ssl_dist_optfile = etc/ssl_dist.conf

    ## Distributed node ticktime
    node.dist_net_ticktime = 60

    ## Distributed node port range
    ## node.dist_listen_min = 6396
    ## node.dist_listen_max = 6396

Erlang 虚拟机主要参数说明:

+-------------------------+--------------------------------------------------------------------------------------------------+
| node.process_limit      | Erlang 虚拟机允许的最大进程数，一个 MQTT 连接会消耗2个 Erlang 进程，所以参数值 > 最大连接数 * 2  |
+-------------------------+--------------------------------------------------------------------------------------------------+
| node.max_ports          | Erlang 虚拟机允许的最大 Port 数量，一个 MQTT 连接消耗1个 Port，所以参数值 > 最大连接数           |
+-------------------------+--------------------------------------------------------------------------------------------------+
| node.dist_listen_min    | Erlang 分布节点间通信使用 TCP 连接端口范围。注: 节点间如有防火墙，需要配置该端口段               |
+-------------------------+--------------------------------------------------------------------------------------------------+
| node.dist_listen_max    | Erlang 分布节点间通信使用 TCP 连接端口范围。注: 节点间如有防火墙，需要配置该端口段               |
+-------------------------+--------------------------------------------------------------------------------------------------+

------------
RPC 参数配置
------------

.. code-block:: properties

    ## TCP server port for RPC.
    rpc.tcp_server_port = 5369

    ## TCP port for outgoing RPC connections.
    rpc.tcp_client_port = 5369

    ## RCP Client connect timeout.
    rpc.connect_timeout = 5000

    ## TCP send timeout of RPC client and server.
    rpc.send_timeout = 5000

    ## Authentication timeout
    rpc.authentication_timeout = 5000

    ## Default receive timeout for call() functions
    rpc.call_receive_timeout = 15000

    ## Socket idle keepalive.
    rpc.socket_keepalive_idle = 900

    ## TCP Keepalive probes interval.
    rpc.socket_keepalive_interval = 75

    ## Probes lost to close the connection
    rpc.socket_keepalive_count = 9

------------
日志参数配置
------------

设置写到终端或写到文件
-------------------

.. code-block:: properties

    ## Where to emit the logs.
    log.to = both

配置日志写到什么地方，可用的选项有：

- off: 完全关闭日志
- file: 只写到文件
- console: 只写到终端(erlang shell)
- both: 同时写到终端(erlang shell) 和文件

日志级别
-------

.. code-block:: properties

    ## The log severity level.
    log.level = error

设置全局的日志级别，包括 primary logger level，以及所有到文件和终端的 logger handlers 的日志级别。

可以使用 :ref:`command_log` 为每个 logger handler 设置日志级别。

日志文件配置
----------

.. code-block:: properties

    ## The dir for log files.
    log.dir = log

    ## The log filename for logs of level specified in "log.level".
    log.file = emqx.log

    ## Maximum size of each log file.
    ## Default: 10M
    ## Supported Unit: KB | MB | G
    log.rotation.size = 10MB

    ## Maximum rotation count of log files.
    ## Default: 5
    log.rotation.count = 5

配置额外的 file logger handlers
------------------------------

可以通过配置额外的 file logger handlers，将某个级别的日志写到单独的文件。

举例，下面的配置将所有的大于等于 info 级别的日志额外写到 info.log 文件中::

    log.info.file = info.log

-------------------
匿名认证与 ACL 文件
-------------------

是否开启匿名认证
----------------

默认开启，允许任意客户端登录:

.. code-block:: properties

    ## Allow Anonymous authentication
    allow_anonymous = true

默认访问控制(ACL)文件
---------------------

*EMQ X* 支持基于 etc/acl.conf 文件或 MySQL、 PostgreSQL 等插件的访问控制规则。

.. code-block:: properties

    ## ACL nomatch. Enum: allow, deny
    acl_nomatch = allow

    ## Default ACL File
    acl_file = etc/acl.conf

    ## Enable ACL cache. Enum: on, off
    enable_acl_cache = on

    ## Default ACL cache size
    acl_cache_max_size = 32

    ## Default time-to-live of cache size
    acl_cache_ttl = 1m

etc/acl.conf 访问控制规则定义::

    允许|拒绝  用户|IP地址|ClientID  发布|订阅  主题列表

访问控制规则采用 Erlang 元组格式，访问控制模块逐条匹配规则::

              ---------              ---------              ---------
    Client -> | Rule1 | --nomatch--> | Rule2 | --nomatch--> | Rule3 | --> Default
              ---------              ---------              ---------
                  |                      |                      |
                match                  match                  match
                 \|/                    \|/                    \|/
            allow | deny           allow | deny           allow | deny

etc/acl.conf 默认访问规则设置:

.. code-block:: erlang

    %% 允许'dashboard'用户订阅 '$SYS/#'
    {allow, {user, "dashboard"}, subscribe, ["$SYS/#"]}.

    %% 允许本机用户发布订阅全部主题
    {allow, {ipaddr, "127.0.0.1"}, pubsub, ["$SYS/#", "#"]}.

    %% 拒绝用户订阅'$SYS#'与'#'主题
    {deny, all, subscribe, ["$SYS/#", {eq, "#"}]}.

    %% 上述规则无匹配，允许
    {allow, all}.

.. NOTE:: 默认规则只允许本机用户订阅'$SYS/#'与'#'

*EMQ X* 消息服务器接收到 MQTT 客户端发布(PUBLISH)或订阅(SUBSCRIBE)请求时，会逐条匹配 ACL 访问控制规则，直到匹配成功返回 allow 或 deny。

-----------------
FLAPPING 参数配置
-----------------

FLAPPING 清理间隔
-----------------

.. code-block:: properties

    ## The cleanning interval for flapping
    ##
    ## Value: Duration
    ## -d: day
    ## -h: hour
    ## -m: minute
    ## -s: second
    ##
    ## Default: 1h, 1 hour
    ## flapping_clean_interval = 1h

-----------------
MQTT 协议参数配置
-----------------

MQTT 最大报文尺寸
-----------------

.. code-block:: properties

    ## Max Packet Size Allowed, 1MB by default
    mqtt.max_packet_size = 1MB

ClientId 最大允许长度
---------------------

.. code-block:: properties

    ## Max ClientId Length Allowed
    mqtt.max_clientid_len = 65535

Topic 最大允许等级
-------------------

.. code-block:: properties

    ## Max Topic Levels Allowed
    mqtt.max_topic_levels = 0

Qos 最大允许值
----------------

.. code-block:: properties

    ## Maximum QoS allowed
    mqtt.max_qos_allowed = 2

Topic Alias 最大数量
----------------------

.. code-block:: properties

    ## Maximum Topic Alias, 0 means no limit
    mqtt.max_topic_alias = 0

启用MQTT Retain Messages
--------------------------

.. code-block:: properties

    ## Enable MQTT Retain Messages
    mqtt.retain_available = true

启用MQTT Wildcard Subscriptions
---------------------------------

.. code-block:: properties

    ## Enable MQTT Wildcard Subscriptions
    mqtt.wildcard_subscription = true

启用MQTT Shared Subscriptions
-------------------------------

.. code-block:: properties

    ## Enable MQTT Shared Subscriptions
    mqtt.shared_subscription = true

消息队列类型
-------------

.. code-block:: properties

    ## Message queue type, Enum: simple, priority
    mqtt.mqueue_type = simple

定义主题优先度
--------------

.. code-block:: properties

    ## Topic priorities, Default is 0
    ## mqtt.mqueue_priorities = topic/1=10,topic/2=8

--------------------
MQTT Zones 参数配置
--------------------

*EMQ X* 支持基于 Zone 的 Listeners 监听器组，根据不同的 Zone 定义不同的 Options 。

多个 Listener 属于一个 Zone ，当客户端属于某个 Zone 时，客户端匹配该 Zone 中的 Options 。

Listener options 模块逐条匹配规则::

                       ---------              ----------              -----------
    Listeners -------> | Zone  | --nomatch--> | Global | --nomatch--> | Default |
                       ---------              ----------              -----------
                           |                       |                       |
                         match                   match                   match
                          \|/                     \|/                     \|/
                    Zone Options            Global Options           Default Options

*EMQ X* 支持 zone.$name.xxx 替换成相应的 $name 的，这里的 zone.external.xxxx 和 zone.internal.xxxx 中的 $name 都可以换成相应的名称。
也可以新增自定义name的 zone.$name.xxx 。

External Zone 参数设置
------------------------

.. code-block:: properties

    ## Idle timeout of the external MQTT connections
    zone.external.idle_timeout = 15s

    ## Limit the external MQTT connections
    ## Default: 10 messages per second, and 100 messages burst.
    ## zone.external.publish_limit = 10,100

    ## Enable ban check
    zone.external.enable_ban = on

    ## Enable ACL check
    zone.external.enable_acl = on

    ## Enable per connection statistics,Enum: on, off
    zone.external.enable_stats = on

    ## Maximum MQTT packet size allowed
    ## Default: 1MB
    ## zone.external.max_packet_size = 64KB

    ## Maximum length of MQTT clientId allowed
    ## zone.external.max_clientid_len = 1024

    ## Maximum topic levels allowed. 0 means no limit
    ## zone.external.max_topic_levels = 7

    ## Maximum QoS allowed
    ## zone.external.max_qos_allowed = 2

    ## Maximum Topic Alias, 0 means no limit
    ## zone.external.max_topic_alias = 0

    ## Enable Server's retained messages
    ## zone.external.retain_available = true

    ## Enable Server's Wildcard Subscriptions
    ## zone.external.wildcard_subscription = false

    ## Enable Server's Shared Subscriptions
    ## zone.external.shared_subscription = false

    ## Server Keep Alive
    ## zone.external.server_keepalive = 0

    ## The backoff for MQTT keepalive timeout
    zone.external.keepalive_backoff = 0.75

    ## Maximum number of subscriptions allowed, 0 means no limit
    zone.external.max_subscriptions = 0

    ## Upgrade QoS according to subscription
    zone.external.upgrade_qos = off

    ## Maximum size of the Inflight Window storing QoS1/2 messages delivered but unacked
    zone.external.max_inflight = 32

    ## Retry interval for QoS1/2 message delivering
    zone.external.retry_interval = 20s

    ## Maximum QoS2 packets (Client -> Broker) awaiting PUBREL, 0 means no limit
    zone.external.max_awaiting_rel = 100

    ## The QoS2 messages (Client -> Broker) will be dropped if awaiting PUBREL timeout
    zone.external.await_rel_timeout = 300s

    ## Default session expiry interval for MQTT V3.1.1 connections.
    ##
    ## Value: Duration
    ## -d: day
    ## -h: hour
    ## -m: minute
    ## -s: second
    ##
    zone.external.session_expiry_interval = 2h

    ## Message queue type
    zone.external.mqueue_type = simple

    ## Maximum queue length
    zone.external.max_mqueue_len = 1000

    ## Topic priorities
    ## zone.external.mqueue_priorities = topic/1=10,topic/2=8

    ## Enable enqueue Qos0 messages
    zone.external.mqueue_store_qos0 = true

    ## Whether to turn on flapping detect
    ##
    ## Value: on | off
    zone.external.enable_flapping_detect = off

    ## The times of state change per min, specifying the threshold which is used to
    ## detect if the connection starts flapping
    ##
    ## Value: number
    zone.external.flapping_threshold = 10, 1m

    ## Flapping expiry interval for connections.
    ## This config entry is used to determine when the connection
    ## will be unbanned.
    ##
    ## Value: Duration
    ## -d: day
    ## -h: hour
    ## -m: minute
    ## -s: second
    ##
    ## Default: 1h, 1 hour
    zone.external.flapping_expiry_interval = 1h

Internal Zone 参数设置
------------------------

.. code-block:: properties

    ## 开启 Internal Zone 匿名访问
    zone.internal.allow_anonymous = true

    ## Enable per connection stats
    zone.internal.enable_stats = on

    ## Enable ACL check
    zone.internal.enable_acl = off

    ## Enable zone.$name.wildcard_subscription
    ## zone.internal.wildcard_subscription = true

    ## Enable zone.$name.shared_subscription
    ## zone.internal.shared_subscription = true

    ## Maximum number of zone.$name.subscription allowed, 0 means no limit
    zone.internal.max_subscriptions = 0

    ## Max number of QoS 1 and 2 messages that can be “inflight” at one time
    zone.internal.max_inflight = 32

    ## Max Packets that Awaiting PUBREL, 0 means no limit
    zone.internal.max_awaiting_rel = 100

    ## Maximum queue length
    zone.internal.max_mqueue_len = 1000

    ## Enable enqueue Qos0 messages
    zone.internal.mqueue_store_qos0 = true

    ## Whether to turn on flapping detect
    ##
    ## Value: on | off
    zone.internal.enable_flapping_detect = off

    ## The times of state change per min, specifying the threshold which is used to
    ## detect if the connection starts flapping
    ##
    ## Value: number
    zone.internal.flapping_threshold = 10, 1m

    ## Flapping expiry interval for connections.
    ## This config entry is used to determine when the connection
    ## will be unbanned.
    ##
    ## Value: Duration
    ## -d: day
    ## -h: hour
    ## -m: minute
    ## -s: second
    ##
    ## Default: 1h, 1 hour
    zone.internal.flapping_expiry_interval = 1h

-----------------------
MQTT Listeners 参数说明
-----------------------

*EMQ* X* 消息服务器支持 MQTT、MQTT/SSL、MQTT/WS 协议服务端，可通过 `listener.tcp|ssl|ws|wss|.*` 设置端口、最大允许连接数等参数。

*EMQ X* R3.0 消息服务器默认开启的 TCP 服务端口包括:

+-----------+-----------------------------------+
| 1883      | MQTT 协议端口                     |
+-----------+-----------------------------------+
| 8883      | MQTT/SSL 端口                     |
+-----------+-----------------------------------+
| 8083      | MQTT/WebSocket 端口               |
+-----------+-----------------------------------+
| 8080      | HTTP 管理 API 端口                |
+-----------+-----------------------------------+
| 8084      | MQTT/WebSocket/SSL 端口           |
+-----------+-----------------------------------+

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

----------------------
MQTT/TCP 监听器 - 1883
----------------------

*EMQ X* R3.0 版本支持配置多个 MQTT 协议监听器，例如配置 external、internal 两个监听器:

.. code-block:: properties

    ##--------------------------------------------------------------------
    ## External TCP Listener

    ## External TCP Listener: 1883, 127.0.0.1:1883, ::1:1883
    listener.tcp.external = 0.0.0.0:1883

    ## Size of acceptor pool
    listener.tcp.external.acceptors = 8

    ## Maximum number of concurrent connections
    listener.tcp.external.max_connections = 1024000

    ## Maximum external connections per second
    listener.tcp.external.max_conn_rate = 1000

    ## 这里配置的 external 与前面的 zone 相关联，也可以新增一个 $name 的监听器，对应前面的 $name zone ，配在相应的listener下面。
    ## Zone of the external MQTT/TCP listener belonged to
    listener.tcp.external.zone = external

    ## Mountpoint of the MQTT/TCP Listener
    ## listener.tcp.external.mountpoint = devicebound/

    ## Rate Limit. Format is 'burst,rate', Unit is Bps
    ## listener.tcp.external.rate_limit = 1024,4096

    #listener.tcp.external.access.1 = allow 192.168.0.0/24
    listener.tcp.external.access.1 = allow all

    ## Proxy Protocol V1/2
    ## listener.tcp.external.proxy_protocol = on
    ## listener.tcp.external.proxy_protocol_timeout = 3s

    ## Enable the option for X.509 certificate based authentication, Enum: cn, dn
    ## listener.tcp.external.peer_cert_as_username = cn

    ## TCP Socket Options
    listener.tcp.external.backlog = 1024

    ## TCP Send Timeout
    listener.tcp.external.send_timeout = 15s

    ## Close the TCP connection if send timeout
    listener.tcp.external.send_timeout_close = on

    #listener.tcp.external.recbuf = 2KB

    #listener.tcp.external.sndbuf = 2KB

    #listener.tcp.external.buffer = 2KB

    ## The TCP_NODELAY flag for MQTT connections
    listener.tcp.external.nodelay = true

    ## The SO_REUSEADDR flag for TCP listener
    listener.tcp.external.reuseaddr = true

----------------------
MQTT/SSL 监听器 - 8883
----------------------

.. code-block:: properties

    ##--------------------------------------------------------------------
    ## External SSL Listener
    listener.ssl.external = 8883

    ## Size of acceptor pool
    listener.ssl.external.acceptors = 16

    ## Maximum number of concurrent connections
    listener.ssl.external.max_connections = 102400

    ## Maximum MQTT/SSL connections per second
    listener.ssl.external.max_conn_rate = 500

    ## 这里配置的 external 与前面的 zone 相关联，也可以新增一个 $name 的监听器，对应前面的 $name zone ，配在相应的listener下面。
    ## Zone of the external MQTT/SSL listener belonged to
    listener.ssl.external.zone = external

    ## listener.ssl.external.mountpoint = devicebound/

    ## The access control rules for the MQTT/SSL listener
    listener.ssl.external.access.1 = allow all

    ## Rate Limit. Format is 'burst,rate', Unit is Bps
    ## listener.ssl.external.rate_limit = 1024,4096

    ## Proxy Protocol V1/2
    ## listener.ssl.external.proxy_protocol = on
    ## listener.ssl.external.proxy_protocol_timeout = 3s

    ## SSL Options
    ## listener.ssl.external.tls_versions = tlsv1.2,tlsv1.1,tlsv1
    listener.ssl.external.handshake_timeout = 15s
    listener.ssl.external.keyfile = etc/certs/key.pem
    listener.ssl.external.certfile = etc/certs/cert.pem
    ## 开启双向认证
    ## listener.ssl.external.cacertfile = etc/certs/cacert.pem
    ## listener.ssl.external.dhfile = etc/certs/dh-params.pem
    ## listener.ssl.external.verify = verify_peer
    ## listener.ssl.external.fail_if_no_peer_cert = true

    ## SSL Parameter
    ## listener.ssl.external.secure_renegotiate = off
    ## listener.ssl.external.reuse_sessions = on
    ## listener.ssl.external.honor_cipher_order = on
    ## listener.ssl.external.peer_cert_as_username = cn
    ## listener.ssl.external.backlog = 1024
    ## listener.ssl.external.send_timeout = 15s
    ## listener.ssl.external.send_timeout_close = on
    ## listener.ssl.external.recbuf = 4KB
    ## listener.ssl.external.sndbuf = 4KB
    ## listener.ssl.external.buffer = 4KB
    ## listener.ssl.external.tune_buffer = off
    ## listener.ssl.external.nodelay = true
    listener.ssl.external.reuseaddr = true

----------------------------
MQTT/WebSocket 监听器 - 8083
----------------------------

.. code-block:: properties

    ##--------------------------------------------------------------------
    ## External MQTT/WebSocket Listener

    listener.ws.external = 8083

    listener.ws.external.acceptors = 4

    listener.ws.external.max_connections = 102400

    listener.ws.external.max_conn_rate = 1000

    ## listener.ws.external.rate_limit = 1024,4096

    listener.ws.external.zone = external

    ## listener.ws.external.mountpoint = devicebound/

    listener.ws.external.access.1 = allow all

    listener.ws.external.verify_protocol_header = on

    ## listener.ws.external.proxy_address_header = X-Forwarded-For

    ## listener.ws.external.proxy_port_header = X-Forwarded-Port

    ## listener.ws.external.proxy_protocol = on

    ## listener.ws.external.proxy_protocol_timeout = 3s

    ## MQTT/WebSocket Options
    listener.ws.external.backlog = 1024
    listener.ws.external.send_timeout = 15s
    listener.ws.external.send_timeout_close = on
    ## listener.ws.external.recbuf = 2KB
    ## listener.ws.external.sndbuf = 2KB
    ## listener.ws.external.buffer = 2KB
    ## listener.ws.external.tune_buffer = off
    listener.ws.external.nodelay = true

--------------------------------
MQTT/WebSocket/SSL 监听器 - 8084
--------------------------------

.. code-block:: properties

    ##--------------------------------------------------------------------
    ## External MQTT/WebSocket/SSL Listener

    listener.wss.external = 8084

    listener.wss.external.acceptors = 4

    listener.wss.external.max_connections = 16

    listener.wss.external.max_conn_rate = 1000

    ## listener.wss.external.rate_limit = 1024,4096

    listener.wss.external.zone = external

    ## listener.wss.external.mountpoint = devicebound/

    listener.wss.external.access.1 = allow all

    listener.wss.external.verify_protocol_header = on

    ## listener.wss.external.proxy_address_header = X-Forwarded-For

    ## listener.wss.external.proxy_port_header = X-Forwarded-Port

    ## Proxy Protocol V1/2
    ## listener.wss.external.proxy_protocol = on
    ## listener.wss.external.proxy_protocol_timeout = 3s

    ## SSL Options
    ## listener.wss.external.tls_versions = tlsv1.2,tlsv1.1,tlsv1
    listener.wss.external.keyfile = etc/certs/key.pem
    listener.wss.external.certfile = etc/certs/cert.pem
    ## 开启双向认证
    ## listener.wss.external.cacertfile = etc/certs/cacert.pem
    ## listener.ssl.external.dhfile = etc/certs/dh-params.pem
    ## listener.wss.external.verify = verify_peer
    ## listener.wss.external.fail_if_no_peer_cert = true

    ## SSL Parameter
    ## listener.wss.external.ciphers =
    ## listener.wss.external.secure_renegotiate = off
    ## listener.wss.external.reuse_sessions = on
    ## listener.wss.external.honor_cipher_order = on
    ## listener.wss.external.peer_cert_as_username = cn
    listener.wss.external.backlog = 1024
    listener.wss.external.send_timeout = 15s
    listener.wss.external.send_timeout_close = on
    ## listener.wss.external.recbuf = 4KB
    ## listener.wss.external.sndbuf = 4KB
    ## listener.wss.external.buffer = 4KB
    ## listener.wss.external.nodelay = true

--------------
Bridges 桥接
--------------

*EMQ X* R3.0 支持与其它 MQTT Server 桥接，发送或者接收消息，Bridge 通过对 bridge.$name.type 参数设置，对于消息来进行发送与接收。

Bridge 模块进出规则由 type 控制::

            bridge.$name.type = in
     EDGE --------- Bridge ------------>  EMQ
                   MQTT/TLS

            bridge.$name.type = out
     EMQ  --------- Bridge ------------>  CLOUD
                   MQTT/TLS


*EMQ X* R3.0 支持 bridge.$name.xxx 替换成相应的 $name 的，这里的 bridge.edge.xxxx 和 bridge.$name.xxxx 中的 $name 都是可以换成相应的名称。
也可以新增自定义name的 bridge.$name.xxxx 。

Bridges 参数设置
--------------------------

.. code-block:: properties

    ## Bridge address: node name for local bridge, host:port for remote.
    ##
    ## Value: String
    ## Example: emqx@127.0.0.1,  127.0.0.1:1883
    bridge.aws.address = 127.0.0.1:1883
     
    ## Protocol version of the bridge.
    ##
    ## Value: Enum
    ## - mqttv5
    ## - mqttv4
    ## - mqttv3
    bridge.aws.proto_ver = mqttv4
     
    ## The ClientId of a remote bridge.
    ##
    ## Value: String
    bridge.aws.client_id = bridge_aws
     
    ## The Clean start flag of a remote bridge.
    ##
    ## Value: boolean
    ## Default: true
    ##
    ## NOTE: Some IoT platforms require clean_start
    ##       must be set to 'true'
    bridge.aws.clean_start = true
     
    ## The username for a remote bridge.
    ##
    ## Value: String
    bridge.aws.username = user
     
    ## The password for a remote bridge.
    ##
    ## Value: String
    bridge.aws.password = passwd
     
    ## Mountpoint of the bridge.
    ##
    ## Value: String
    bridge.aws.mountpoint = bridge/aws/${node}/
     
    ## Forward message topics
    ##
    ## Value: String
    ## Example: topic1/#,topic2/#
    bridge.aws.forwards = topic1/#,topic2/#
     
    ## Bribge to remote server via SSL.
    ##
    ## Value: on | off
    bridge.aws.ssl = off
     
    ## PEM-encoded CA certificates of the bridge.
    ##
    ## Value: File
    bridge.aws.cacertfile = {{ platform_etc_dir }}/certs/cacert.pem
     
    ## Client SSL Certfile of the bridge.
    ##
    ## Value: File
    bridge.aws.certfile = {{ platform_etc_dir }}/certs/client-cert.pem
     
    ## Client SSL Keyfile of the bridge.
    ##
    ## Value: File
    bridge.aws.keyfile = {{ platform_etc_dir }}/certs/client-key.pem
     
    ## SSL Ciphers used by the bridge.
    ##
    ## Value: String
    #bridge.aws.ciphers = ECDHE-ECDSA-AES256-GCM-SHA384,ECDHE-RSA-AES256-GCM-SHA384
     
    ## Ciphers for TLS PSK.
    ## Note that 'listener.ssl.external.ciphers' and 'listener.ssl.external.psk_ciphers' cannot
    ## be configured at the same time.
    ## See 'https://tools.ietf.org/html/rfc4279#section-2'.
    #bridge.aws.psk_ciphers = PSK-AES128-CBC-SHA,PSK-AES256-CBC-SHA,PSK-3DES-EDE-CBC-SHA,PSK-RC4-SHA
     
    ## Ping interval of a down bridge.
    ##
    ## Value: Duration
    ## Default: 10 seconds
    bridge.aws.keepalive = 60s
     
    ## TLS versions used by the bridge.
    ##
    ## Value: String
    bridge.aws.tls_versions = tlsv1.2,tlsv1.1,tlsv1
     
    ## Subscriptions of the bridge topic.
    ##
    ## Value: String
    bridge.aws.subscription.1.topic = cmd/topic1
     
    ## Subscriptions of the bridge qos.
    ##
    ## Value: Number
    bridge.aws.subscription.1.qos = 1
     
    ## Subscriptions of the bridge topic.
    ##
    ## Value: String
    bridge.aws.subscription.2.topic = cmd/topic2
     
    ## Subscriptions of the bridge qos.
    ##
    ## Value: Number
    bridge.aws.subscription.2.qos = 1
     
    ## Start type of the bridge.
    ##
    ## Value: enum
    ## manual
    ## auto
    bridge.aws.start_type = manual
     
    ## Bridge reconnect time.
    ##
    ## Value: Duration
    ## Default: 30 seconds
    bridge.aws.reconnect_interval = 30s
     
    ## Retry interval for bridge QoS1 message delivering.
    ##
    ## Value: Duration
    bridge.aws.retry_interval = 20s
     
    ## Inflight size.
    ##
    ## Value: Integer
    bridge.aws.max_inflight_batches = 32
     
    ## Max number of messages to collect in a batch for
    ## each send call towards emqx_bridge_connect
    ##
    ## Value: Integer
    ## default: 32
    bridge.aws.queue.batch_count_limit = 32
     
    ## Max number of bytes to collect in a batch for each
    ## send call towards emqx_bridge_connect
    ##
    ## Value: Bytesize
    ## default: 1000M
    bridge.aws.queue.batch_bytes_limit = 1000MB
     
    ## Base directory for replayq to store messages on disk
    ## If this config entry is missing or set to undefined,
    ## replayq works in a mem-only manner.
    ##
    ## Value: String
    bridge.aws.queue.replayq_dir = {{ platform_data_dir }}/emqx_aws_bridge/
     
    ## Replayq segment size
    ##
    ## Value: Bytesize
    bridge.aws.queue.replayq_seg_bytes = 10MB

--------------
Modules 模块
--------------

*EMQ X* R3.0 支持模块扩展，默认三个模块，分别为上下线消息状态发布模块、代理订阅模块、主题(Topic)重写模块。

上下线消息状态发布模块
----------------------

.. code-block:: properties

    ##--------------------------------------------------------------------
    ## Enable Presence Module
    module.presence = on

    ## Sets the QoS for presence MQTT message
    module.presence.qos = 1

代理订阅模块
------------

.. code-block:: properties

    ##--------------------------------------------------------------------
    ## Enable Subscription Module
    module.subscription = off

    ## Subscribe the Topics's qos
    ## module.subscription.1.topic = $client/%c
    ## module.subscription.1.qos = 0
    ## module.subscription.2.topic = $user/%u
    ## module.subscription.2.qos = 1

主题重写模块
------------

.. code-block:: properties

    ##--------------------------------------------------------------------
    ## Enable Rewrite Module, Enum: on, off
    module.rewrite = off

    ## {rewrite, Topic, Re, Dest}
    ## module.rewrite.rule.1 = x/# ^x/y/(.+)$ z/y/$1
    ## module.rewrite.rule.2 = y/+/z/# ^y/(.+)/z/(.+)$ y/z/$2

----------------
扩展插件配置文件
----------------

.. code-block:: properties

    ##--------------------------------------------------------------------
    ## The etc dir for plugins' config
    plugins.etc_dir =etc/plugins/

    ## The file to store loaded plugin names
    plugins.loaded_file = data/loaded_plugins

    ## File to store loaded plugin names
    plugins.expand_plugins_dir = plugins/

*EMQ X* R3.0 插件配置文件，默认在 etc/plugins/ 目录，可修改 plugins.etc_dir 来调整目录:

+----------------------------------------+-----------------------------------+
| 配置文件                               | 说明                              |
+----------------------------------------+-----------------------------------+
| etc/plugins/emqx_delayed_publish.conf  | 消息延迟发布插件                  |
+----------------------------------------+-----------------------------------+
| etc/plugins/emqx_retainer.conf         | Retain 消息存储插件               |
+----------------------------------------+-----------------------------------+
| etc/plugins/emqx_management.conf       | 管理插件                          |
+----------------------------------------+-----------------------------------+
| etc/plugins/emqx_auth_username.conf    | 用户名、密码认证插件              |
+----------------------------------------+-----------------------------------+
| etc/plugins/emqx_auth_clientid.conf    | ClientId 认证插件                 |
+----------------------------------------+-----------------------------------+
| etc/plugins/emqx_auth_http.conf        | HTTP 认证插件配置                 |
+----------------------------------------+-----------------------------------+
| etc/plugins/emqx_auth_mongo.conf       | MongoDB 认证插件配置              |
+----------------------------------------+-----------------------------------+
| etc/plugins/emqx_auth_mysql.conf       | MySQL 认证插件配置                |
+----------------------------------------+-----------------------------------+
| etc/plugins/emqx_auth_pgsql.conf       | Postgre 认证插件配置              |
+----------------------------------------+-----------------------------------+
| etc/plugins/emqx_auth_redis.conf       | Redis 认证插件配置                |
+----------------------------------------+-----------------------------------+
| etc/plugins/emqx_web_hook.conf         | Web Hook 插件配置                 |
+----------------------------------------+-----------------------------------+
| etc/plugins/emqx_lwm2m.conf            | Lwm2m 协议插件配置                |
+----------------------------------------+-----------------------------------+
| etc/plugins/emqx_coap.conf             | CoAP 协议服务器配置               |
+----------------------------------------+-----------------------------------+
| etc/plugins/emqx_dashboard.conf        | Dashboard 控制台插件配置          |
+----------------------------------------+-----------------------------------+
| etc/plugins/emqx_recon.conf            | Recon 调试插件配置                |
+----------------------------------------+-----------------------------------+
| etc/plugins/emqx_reloader.conf         | 热加载插件配置                    |
+----------------------------------------+-----------------------------------+
| etc/plugins/emqx_sn.conf               | MQTT-SN 协议插件配置              |
+----------------------------------------+-----------------------------------+
| etc/plugins/emqx_stomp.conf            | Stomp 协议插件配置                |
+----------------------------------------+-----------------------------------+
| etc/plugins/emqx_statsd.conf           | 统计管理插件配置                  |
+----------------------------------------+-----------------------------------+
| etc/plugins/emqx_auth_ldap.conf        | Ldap 认证插件配置                 |
+----------------------------------------+-----------------------------------+
| etc/plugins/emqx_auth_jwt.conf         | Jwt 认证插件配置                  |
+----------------------------------------+-----------------------------------+
| etc/plugins/emqx_plugin_template.conf  | 示例插件模版                      |
+----------------------------------------+-----------------------------------+

----------------
Broker 参数设置
----------------

.. code-block:: properties

    ## System interval of publishing $SYS messages
    broker.sys_interval = 1m

    ## Session locking strategy in a cluster
    ## Value: Enum
    ## - local
    ## - one
    ## - quorum
    ## - all
    broker.session_locking_strategy = quorum

    ## Dispatch strategy for shared subscription
    ## Value: Enum
    ## - random
    ## - round_robbin
    ## - hash
    broker.shared_subscription_strategy = random

    ## Enable batch clean for deleted routes
    broker.route_batch_clean = on

---------------------
Erlang 虚拟机监控设置
---------------------

.. code-block:: properties

    ## Enable Long GC monitoring.
    sysmon.long_gc = false

    ## Enable Long Schedule(ms) monitoring.
    sysmon.long_schedule = 240

    ## Enable Large Heap monitoring.
    sysmon.large_heap = 8MB

    ## Enable Busy Port monitoring.
    sysmon.busy_port = false

    ## Enable Busy Dist Port monitoring.
    sysmon.busy_dist_port = true

    ## The time interval for the periodic cpu check
    ## Default: 60s
    os_mon.cpu_check_interval = 60s

    ## The threshold, as percentage of system cpu
    ## for how much system cpu can be used before the corresponding alarm is set.
    os_mon.cpu_high_watermark = 80%

    ## The threshold, as percentage of system cpu
    ## for how much system cpu can be used before the corresponding alarm is clear.
    os_mon.cpu_low_watermark = 60%

    ## The time interval for the periodic memory check
    os_mon.mem_check_interval = 60s

    ## The threshold, as percentage of system memory
    ## for how much system memory can be allocated before the corresponding alarm is set.
    os_mon.sysmem_high_watermark = 70%

    ## The threshold, as percentage of system memory
    ## for how much system memory can be allocated by one Erlang process before the corresponding alarm is set.
    os_mon.procmem_high_watermark = 5%

    ## The time interval for the periodic process limit check
    vm_mon.check_interval = 30s

    ## The threshold, as percentage of processes,
    ## for how many processes can simultaneously exist
    ## at the local node before the corresponding alarm is set.
    vm_mon.process_high_watermark = 80%

    ## The threshold, as percentage of processes,
    ## for how many processes can simultaneously
    ## exist at the local node before the corresponding alarm is clear.
    vm_mon.process_low_watermark = 60%

