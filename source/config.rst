
.. _configuration:

========
配置说明
========

----------------
EMQ 2.0 配置文件
----------------

*EMQ* 2.0消息服务器通过etc/目录下配置文件进行设置，主要配置文件包括:

+----------------------------+-----------------------------------+
| 配置文件                   | 说明                              |
+----------------------------+-----------------------------------+
| etc/emq.conf               | EMQ 2.0消息服务器配置文件         |
+----------------------------+-----------------------------------+
| etc/acl.conf               | EMQ 2.0默认ACL规则配置文件        |
+----------------------------+-----------------------------------+
| etc/plugins/\*.conf        | EMQ 2.0各类插件配置文件           |
+----------------------------+-----------------------------------+

----------------
EMQ 配置变更历史
----------------

为方便用户与插件开发者使用，*EMQ* 配置文件经过三次调整。

1. EMQ 1.x版本采用Erlang原生配置文件格式etc/emqttd.config:

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

Erlang的原生配置格式多层级嵌套，对非Erlang开发者的用户很不友好。

2. EMQ 2.0-beta.x版本简化了原生Erlang配置文件，采用类似rebar.config或relx.config格式:

.. code-block:: erlang

    %% Max ClientId Length Allowed.
    {mqtt_max_clientid_len, 512}.

    %% Max Packet Size Allowed, 64K by default.
    {mqtt_max_packet_size, 65536}.

    %% Client Idle Timeout.
    {mqtt_client_idle_timeout, 30}. % Second

简化后的Erlang原生配置格式方便用户配置，但插件开发者不得不依赖gen_conf库，而不是通过appliaton:get_env读取配置参数。

3. EMQ 2.0-rc.2正式版集成了cuttlefish库，采用了类似sysctl的`k = v`通用格式，并在系统启动时翻译成Erlang原生配置格式:

.. code-block:: properties

    ## Node name
    node.name = emqttd@127.0.0.1
    ...
    ## Max ClientId Length Allowed.
    mqtt.max_clientid_len = 1024
    ...

EMQ 2.0启动时配置文件处理流程::

    ----------------------                                          2.0/schema/*.schema      -------------------
    | etc/emq.conf       |                   -----------------              \|/              | data/app.config |
    |       +            | --> mergeconf --> | data/app.conf | -->  cuttlefish generate  --> |                 |
    | etc/plugins/*.conf |                   -----------------                               | data/vm.args    |
    ----------------------                                                                   -------------------

----------------
EMQ 2.2 环境变量
----------------

+-------------------+------------------------------------------+
| EMQ_NODE_NAME     | Erlang节点名称，例如: emqttd@127.0.0.1   |
+-------------------+------------------------------------------+
| EMQ_NODE_COOKIE   | Erlang分布式节点通信Cookie               |
+-------------------+------------------------------------------+
| EMQ_MAX_PORTS     | Erlang虚拟机最大允许打开文件/Socket数    |
+-------------------+------------------------------------------+
| EMQ_TCP_PORT      | MQTT TCP监听端口，默认: 1883             |
+-------------------+------------------------------------------+
| EMQ_SSL_PORT      | MQTT SSL监听端口，默认: 8883             |
+-------------------+------------------------------------------+
| EMQ_WS_PORT       | MQTT/WebSocket监听端口，默认: 8083       |
+-------------------+------------------------------------------+
| EMQ_WSS_PORT      | MQTT/WebSocket/SSL 监听端口，默认: 8084  |
+-------------------+------------------------------------------+

------------
EMQ 集群设置
------------

集群名称
--------

.. code-block:: properties

    ## Cluster name
    cluster.name = emqcl

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

----------------
EMQ 集群自动发现
----------------

EMQ R2.3版本支持多种策略的节点自动发现与集群:

+-----------------+---------------------------+
| 策略            | 说明                      |
+=================+===========================+
| manual          | 手工命令创建集群          |
+-----------------+---------------------------+
| static          | 静态节点列表自动集群      |
+-----------------+---------------------------+
| mcast           | UDP组播方式自动集群       |
+-----------------+---------------------------+
| dns             | DNS A记录自动集群         |
+-----------------+---------------------------+
| etcd            | 通过etcd自动集群          |
+-----------------+---------------------------+
| k8s             | Kubernetes服务自动集群    |
+-----------------+---------------------------+

manual手动创建集群
------------------

默认配置为手动创建集群，节点通过'./bin/emqttd_ctl join <Node>'命令加入:

.. code-block:: properties

    cluster.discovery = manual

基于static节点列表自动集群
--------------------------

配置固定的节点列表，自动发现并创建集群:

.. code-block:: properties

    cluster.discovery = static

    ##--------------------------------------------------------------------
    ## Cluster with static node list

    cluster.static.seeds = emq1@127.0.0.1,ekka2@127.0.0.1

基于mcast组播自动集群
---------------------

基于UDP组播自动发现并创建集群:

.. code-block:: properties

    cluster.discovery = mcast

    ##--------------------------------------------------------------------
    ## Cluster with multicast

    cluster.mcast.addr = 239.192.0.1

    cluster.mcast.ports = 4369,4370

    cluster.mcast.iface = 0.0.0.0

    cluster.mcast.ttl = 255

    cluster.mcast.loop = on

基于DNS A记录自动集群
---------------------

基于DNS A记录自动发现并创建集群:

.. code-block:: properties

    cluster.discovery = dns

    ##--------------------------------------------------------------------
    ## Cluster with DNS

    cluster.dns.name = localhost

    cluster.dns.app  = ekka

基于etcd自动集群
----------------

基于 `etcd`_ 自动发现并创建集群:

.. code-block:: properties

    cluster.discovery = etcd

    ##--------------------------------------------------------------------
    ## Cluster with Etcd

    cluster.etcd.server = http://127.0.0.1:2379

    cluster.etcd.prefix = emqcl

    cluster.etcd.node_ttl = 1m

基于Kubernetes自动集群
----------------------

`Kubernetes`_ 下自动发现并创建集群:

.. code-block:: properties

    cluster.discovery = k8s

    ##--------------------------------------------------------------------
    ## Cluster with k8s

    cluster.k8s.apiserver = http://10.110.111.204:8080

    cluster.k8s.service_name = ekka

    ## Address Type: ip | dns
    cluster.k8s.address_type = ip

    ## The Erlang application name
    cluster.k8s.app_name = ekka

----------------
EMQ 节点与Cookie
----------------

Erlang节点名称、分布式节点间通信Cookie:

.. code-block:: properties

    ## Node name
    node.name = emqttd@127.0.0.1

    ## Cookie for distributed node
    node.cookie = emq_dist_cookie

.. NOTE::

    Erlang/OTP平台应用多由分布的Erlang节点(进程)组成，每个Erlang节点(进程)需指配一个节点名，用于节点间通信互访。
    所有互相通信的Erlang节点(进程)间通过一个共用的Cookie进行安全认证。

----------------
Erlang虚拟机参数
----------------

.. code-block:: properties

    ## SMP support: enable, auto, disable
    node.smp = auto

    ## Enable kernel poll
    node.kernel_poll = on

    ## async thread pool
    node.async_threads = 32

    ## Erlang Process Limit
    node.process_limit = 256000

    ## Sets the maximum number of simultaneously existing ports for this system
    node.max_ports = 65536

    ## Set the distribution buffer busy limit (dist_buf_busy_limit)
    node.dist_buffer_size = 32MB

    ## Max ETS Tables.
    ## Note that mnesia and SSL will create temporary ets tables.
    node.max_ets_tables = 256000

    ## Tweak GC to run more often
    node.fullsweep_after = 1000

    ## Crash dump
    node.crash_dump = log/crash.dump

    ## Distributed node ticktime
    node.dist_net_ticktime = 60

    ## Distributed node port range
    ## node.dist_listen_min = 6000
    ## node.dist_listen_max = 6999

Erlang虚拟机主要参数说明:

+-------------------------+---------------------------------------------------------------------------------------------+
| node.process_limit      | Erlang虚拟机允许的最大进程数，一个MQTT连接会消耗2个Erlang进程，所以参数值 > 最大连接数 * 2  |
+-------------------------+---------------------------------------------------------------------------------------------+
| node.max_ports          | Erlang虚拟机允许的最大Port数量，一个MQTT连接消耗1个Port，所以参数值 > 最大连接数            |
+-------------------------+---------------------------------------------------------------------------------------------+
| node.dist_listen_min    | Erlang分布节点间通信使用TCP连接端口范围。注: 节点间如有防火墙，需要配置该端口段             |
+-------------------------+---------------------------------------------------------------------------------------------+
| node.dist_listen_max    | Erlang分布节点间通信使用TCP连接端口范围。注: 节点间如有防火墙，需要配置该端口段             |
+-------------------------+---------------------------------------------------------------------------------------------+

------------
日志参数配置
------------

console日志
-----------

.. code-block:: properties

    ## Console log. Enum: off, file, console, both
    log.console = console

    ## Console log level. Enum: debug, info, notice, warning, error, critical, alert, emergency
    log.console.level = error

    ## Console log file
    ## log.console.file = log/console.log

error日志
---------

.. code-block:: properties

    ## Error log file
    log.error.file = log/error.log

crash日志
---------

.. code-block:: properties

    ## Enable the crash log. Enum: on, off
    log.crash = on

    log.crash.file = log/crash.log

syslog日志
----------

.. code-block:: properties

    ## Syslog. Enum: on, off
    log.syslog = on

    ##  syslog level. Enum: debug, info, notice, warning, error, critical, alert, emergency
    log.syslog.level = error

-----------------
MQTT 协议参数配置
-----------------

ClientId最大允许长度
--------------------

.. code-block:: properties

    ## Max ClientId Length Allowed.
    mqtt.max_clientid_len = 1024

MQTT最大报文尺寸
----------------

.. code-block:: properties

    ## Max Packet Size Allowed, 64K by default.
    mqtt.max_packet_size = 64KB

客户端连接闲置时间
------------------

设置MQTT客户端最大允许闲置时间(Socket连接建立，但未收到CONNECT报文):

.. code-block:: properties

    ## Client Idle Timeout (Second)
    mqtt.client.idle_timeout = 30

启用客户端连接统计
------------------

.. code-block:: properties

    ## Enable client Stats: on | off
    mqtt.client.enable_stats = off

强制GC设置
----------

.. code-block:: properties

    ## Force GC: integer. Value 0 disabled the Force GC.
    mqtt.conn.force_gc_count = 100

-----------------
匿名认证与ACL文件
-----------------

是否开启匿名认证
----------------

默认开启，允许任意客户端登录:

.. code-block:: properties

    ## Allow Anonymous authentication
    mqtt.allow_anonymous = true

默认访问控制(ACL)文件
---------------------

*EMQ* 支持基于etc/acl.conf文件或MySQL、PostgreSQL等插件的访问控制规则。

.. code-block:: properties

    ## ACL nomatch
    mqtt.acl_nomatch = allow

    ## Default ACL File
    mqtt.acl_file = etc/acl.conf

etc/acl.conf访问控制规则定义::

    允许|拒绝  用户|IP地址|ClientID  发布|订阅  主题列表

访问控制规则采用Erlang元组格式，访问控制模块逐条匹配规则::

              ---------              ---------              ---------
    Client -> | Rule1 | --nomatch--> | Rule2 | --nomatch--> | Rule3 | --> Default
              ---------              ---------              ---------
                  |                      |                      |
                match                  match                  match
                 \|/                    \|/                    \|/
            allow | deny           allow | deny           allow | deny

etc/acl.conf默认访问规则设置:

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

*EMQ* 消息服务器接收到MQTT客户端发布(PUBLISH)或订阅(SUBSCRIBE)请求时，会逐条匹配ACL访问控制规则，直到匹配成功返回allow或deny。

----------------
MQTT会话参数设置
----------------

.. code-block:: properties

    ## Upgrade QoS?
    mqtt.session.upgrade_qos = off

    ## Max number of QoS 1 and 2 messages that can be “inflight” at one time.
    ## 0 means no limit
    mqtt.session.max_inflight = 32

    ## Retry Interval for redelivering QoS1/2 messages.
    mqtt.session.retry_interval = 20s

    ## Max Packets that Awaiting PUBREL, 0 means no limit
    mqtt.session.max_awaiting_rel = 100

    ## Awaiting PUBREL Timeout
    mqtt.session.await_rel_timeout = 20s

    ## Enable Statistics: on | off
    mqtt.session.enable_stats = off

    ## Expired after 1 day:
    ## w - week
    ## d - day
    ## h - hour
    ## m - minute
    ## s - second
    mqtt.session.expiry_interval = 2h

+---------------------------+----------------------------------------------------------+
| session.upgrade_qos       | 是否根据订阅升级QoS级别                                  |
+---------------------------+----------------------------------------------------------+
| session.max_inflight      | 飞行窗口。最大允许同时下发的Qos1/2报文数，0表示没有限制。|
|                           | 窗口值越大，吞吐越高；窗口值越小，消息顺序越严格         |
+---------------------------+----------------------------------------------------------+
| session.retry_interval    | 下发QoS1/2消息未收到PUBACK响应的重试间隔                 |
+---------------------------+----------------------------------------------------------+
| session.await_rel_timeout | 收到QoS2消息，等待PUBREL报文超时时间                     |
+---------------------------+----------------------------------------------------------+
| session.max_awaiting_rel  | 最大等待PUBREL的QoS2报文数                               |
+---------------------------+----------------------------------------------------------+
| session.enable_stats      | 是否启用Session统计，off表示关闭，30s表示30秒采集一次    |
+---------------------------+----------------------------------------------------------+
| session.expiry_interval   | 持久会话到期时间，从客户端断开算起，单位：分钟           |
+---------------------------+----------------------------------------------------------+

--------------------
MQTT消息队列参数设置
--------------------

EMQ消息服务器会话通过队列缓存Qos1/Qos2消息:

1. 持久会话(Session)的离线消息

2. 飞行窗口满而延迟下发的消息

队列参数设置:

.. code-block:: properties

    ## Type: simple | priority
    mqtt.mqueue.type = simple

    ## Topic Priority: 0~255, Default is 0
    ## mqtt.mqueue.priority = topic/1=10,topic/2=8

    ## Max queue length. Enqueued messages when persistent client disconnected,
    ## or inflight window is full. 0 means no limit.
    mqtt.mqueue.max_length = 0

    ## Low-water mark of queued messages
    mqtt.mqueue.low_watermark = 20%

    ## High-water mark of queued messages
    mqtt.mqueue.high_watermark = 60%

    ## Queue Qos0 messages?
    mqtt.mqueue.store_qos0 = true

队列参数说明:

+-----------------------+---------------------------------------------------+
| mqueue.type           | 队列类型。simple: 简单队列，priority: 优先级队列  |
+-----------------------+---------------------------------------------------+
| mqueue.priority       | 主题(Topic)队列优先级设置                         |
+-----------------------+---------------------------------------------------+
| mqueue.max_length     | 队列长度, infinity表示不限制                      |
+-----------------------+---------------------------------------------------+
| mqueue.low_watermark  | 解除告警水位线                                    |
+-----------------------+---------------------------------------------------+
| mqueue.high_watermark | 队列满告警水位线                                  |
+-----------------------+---------------------------------------------------+
| mqueue.qos0           | 是否缓存QoS0消息                                  |
+-----------------------+---------------------------------------------------+

--------------
Broker参数设置
--------------

broker_sys_interval设置系统发布$SYS消息周期:

.. code-block:: properties

    ## System Interval of publishing broker $SYS Messages
    mqtt.broker.sys_interval = 60

------------------------
发布订阅(PubSub)参数设置
------------------------

.. code-block:: properties

    ## PubSub Pool Size. Default should be scheduler numbers.
    mqtt.pubsub.pool_size = 8

    mqtt.pubsub.by_clientid = true

    ## Subscribe Asynchronously
    mqtt.pubsub.async = true

--------------------
桥接(Bridge)参数设置
--------------------

.. code-block:: properties

    ## Bridge Queue Size
    mqtt.bridge.max_queue_len = 10000

    ## Ping Interval of bridge node. Unit: Second
    mqtt.bridge.ping_down_interval = 1

-----------------------
Plugins插件配置目录设置
-----------------------

.. code-block:: properties

    ## Dir of plugins' config
    mqtt.plugins.etc_dir = etc/plugins/

    ## File to store loaded plugin names.
    mqtt.plugins.loaded_file = data/loaded_plugins

----------------------
MQTT Listeners参数说明
----------------------

*EMQ* 消息服务器支持MQTT、MQTT/SSL、MQTT/WS协议服务端，可通过listener.tcp|ssl|ws|wss|.*设置端口、最大允许连接数等参数。

*EMQ* 2.2消息服务器默认开启的TCP服务端口包括:

+-----------+-----------------------------------+
| 1883      | MQTT协议端口                      |
+-----------+-----------------------------------+
| 8883      | MQTT/SSL端口                      |
+-----------+-----------------------------------+
| 8083      | MQTT/WebSocket端口                |
+-----------+-----------------------------------+
| 8080      | HTTP API端口                      |
+-----------+-----------------------------------+
| 8084      | MQTT/WebSocket/SSL端口            |
+-----------+-----------------------------------+

Listener参数说明:

+----------------------------------+------------------------------------------+
| listener.tcp.${name}.acceptors   | TCP Acceptor池                           |
+----------------------------------+------------------------------------------+
| listener.tcp.${name}.max_clients | 最大允许TCP连接数                        |
+----------------------------------+------------------------------------------+
| listener.tcp.${name}.rate_limit  | 连接限速配置，例如限速10KB/秒:  "100,10" |
+----------------------------------+------------------------------------------+

---------------------
MQTT/TCP监听器 - 1883
---------------------

EMQ 2.2版本支持配置多个MQTT协议监听器，例如配置external、internal两个监听器:

.. code-block:: properties

    ##--------------------------------------------------------------------
    ## External TCP Listener

    ## External TCP Listener: 1883, 127.0.0.1:1883, ::1:1883
    listener.tcp.external = 0.0.0.0:1883

    ## Size of acceptor pool
    listener.tcp.external.acceptors = 16

    ## Maximum number of concurrent clients
    listener.tcp.external.max_clients = 102400

    #listener.tcp.external.mountpoint = external/

    ## Rate Limit. Format is 'burst,rate', Unit is KB/Sec
    #listener.tcp.external.rate_limit = 100,10

    #listener.tcp.external.access.1 = allow 192.168.0.0/24

    listener.tcp.external.access.2 = allow all

    ## Proxy Protocol V1/2
    ## listener.tcp.external.proxy_protocol = on
    ## listener.tcp.external.proxy_protocol_timeout = 3s

    ## TCP Socket Options
    listener.tcp.external.backlog = 1024

    #listener.tcp.external.recbuf = 4KB

    #listener.tcp.external.sndbuf = 4KB

    listener.tcp.external.buffer = 4KB

    listener.tcp.external.nodelay = true

    ##--------------------------------------------------------------------
    ## Internal TCP Listener

    ## Internal TCP Listener: 11883, 127.0.0.1:11883, ::1:11883
    listener.tcp.internal = 127.0.0.1:11883

    ## Size of acceptor pool
    listener.tcp.internal.acceptors = 16

    ## Maximum number of concurrent clients
    listener.tcp.internal.max_clients = 102400

    #listener.tcp.external.mountpoint = internal/

    ## Rate Limit. Format is 'burst,rate', Unit is KB/Sec
    ## listener.tcp.internal.rate_limit = 1000,100

    ## TCP Socket Options
    listener.tcp.internal.backlog = 512

    listener.tcp.internal.tune_buffer = on

    listener.tcp.internal.buffer = 1MB

    listener.tcp.internal.recbuf = 4KB

    listener.tcp.internal.sndbuf = 1MB

    listener.tcp.internal.nodelay = true

---------------------
MQTT/SSL监听器 - 8883
---------------------

.. code-block:: properties

    ##--------------------------------------------------------------------
    ## External SSL Listener
    listener.ssl.external = 8883

    ## Size of acceptor pool
    listener.ssl.external.acceptors = 16

    ## Maximum number of concurrent clients
    listener.ssl.external.max_clients = 1024

    ## listener.ssl.external.mountpoint = inbound/

    ## Rate Limit. Format is 'burst,rate', Unit is KB/Sec
    ## listener.ssl.external.rate_limit = 100,10

    ## Proxy Protocol V1/2
    ## listener.ssl.external.proxy_protocol = on
    ## listener.ssl.external.proxy_protocol_timeout = 3s

    listener.ssl.external.access.1 = allow all

    ## SSL Options
    listener.ssl.external.handshake_timeout = 15
    listener.ssl.external.keyfile = etc/certs/key.pem
    listener.ssl.external.certfile = etc/certs/cert.pem
    ## 开启双向认证
    ## listener.ssl.external.cacertfile = etc/certs/cacert.pem
    ## listener.ssl.external.verify = verify_peer
    ## listener.ssl.external.fail_if_no_peer_cert = true

---------------------------
MQTT/WebSocket监听器 - 8083
---------------------------

.. code-block:: properties

    ##--------------------------------------------------------------------
    ## External MQTT/WebSocket Listener

    listener.ws.external = 8083

    listener.ws.external.acceptors = 4

    listener.ws.external.max_clients = 64

    listener.ws.external.access.1 = allow all

-------------------------------
MQTT/WebSocket/SSL监听器 - 8084
-------------------------------

.. code-block:: properties

    ##--------------------------------------------------------------------
    ## External MQTT/WebSocket/SSL Listener

    listener.wss.external = 8084

    listener.wss.external.acceptors = 4

    listener.wss.external.max_clients = 64

    listener.wss.external.access.1 = allow all

    ## SSL Options
    listener.wss.external.handshake_timeout = 15s

    listener.wss.external.keyfile = {{ platform_etc_dir }}/certs/key.pem

    listener.wss.external.certfile = {{ platform_etc_dir }}/certs/cert.pem

    ## listener.wss.external.cacertfile = {{ platform_etc_dir }}/certs/cacert.pem

    ## listener.wss.external.verify = verify_peer

    ## listener.wss.external.fail_if_no_peer_cert = true

---------------------
HTTP API监听器 - 8080
---------------------

.. code-block:: properties

    ##--------------------------------------------------------------------
    ## HTTP Management API Listener

    listener.api.mgmt = 127.0.0.1:8080

    listener.api.mgmt.acceptors = 4

    listener.api.mgmt.max_clients = 64

    listener.api.mgmt.access.1 = allow all

--------------------
Erlang虚拟机监控设置
--------------------

.. code-block:: properties

    ## Long GC, don't monitor in production mode for:
    sysmon.long_gc = false

    ## Long Schedule(ms)
    sysmon.long_schedule = 240

    ## 8M words. 32MB on 32-bit VM, 64MB on 64-bit VM.
    sysmon.large_heap = 8MB

    ## Busy Port
    sysmon.busy_port = false

    ## Busy Dist Port
    sysmon.busy_dist_port = true

----------------
扩展插件配置文件
----------------

*EMQ* 2.2插件配置文件，全部在etc/plugins/目录:

+----------------------------------------+-----------------------------------+
| 配置文件                               | 说明                              |
+----------------------------------------+-----------------------------------+
| etc/plugins/emq_mod_presence           | 客户端上下线状态消息发布          |
+----------------------------------------+-----------------------------------+
| etc/plugins/emq_mod_retainer           | Retain消息存储插件                |
+----------------------------------------+-----------------------------------+
| etc/plugins/emq_mod_subscription       | 客户端上线自动主题订阅            |
+----------------------------------------+-----------------------------------+
| etc/plugins/emq_auth_username.conf     | 用户名、密码认证插件              |
+----------------------------------------+-----------------------------------+
| etc/plugins/emq_auth_clientid.conf     | ClientId认证插件                  |
+----------------------------------------+-----------------------------------+
| etc/plugins/emq_auth_http.conf         | HTTP认证插件配置                  |
+----------------------------------------+-----------------------------------+
| etc/plugins/emq_auth_mongo.conf        | MongoDB认证插件配置               |
+----------------------------------------+-----------------------------------+
| etc/plugins/emq_auth_mysql.conf        | MySQL认证插件配置                 |
+----------------------------------------+-----------------------------------+
| etc/plugins/emq_auth_pgsql.conf        | Postgre认证插件配置               |
+----------------------------------------+-----------------------------------+
| etc/plugins/emq_auth_redis.conf        | Redis认证插件配置                 |
+----------------------------------------+-----------------------------------+
| etc/plugins/emq_web_hook.conf          | Web Hook 插件配置                 |
+----------------------------------------+-----------------------------------+
| etc/plugins/emq_lua_hook.conf          | Lua Hook 插件配置                 |
+----------------------------------------+-----------------------------------+
| etc/plugins/emq_coap.conf              | CoAP协议服务器配置                |
+----------------------------------------+-----------------------------------+
| etc/plugins/emq_dashboard.conf         | Dashboard控制台插件配置           |
+----------------------------------------+-----------------------------------+
| etc/plugins/emq_mod_rewrite.conf       | 主题(Topic)重写插件配置           |
+----------------------------------------+-----------------------------------+
| etc/plugins/emq_plugin_template.conf   | 示例插件模版                      |
+----------------------------------------+-----------------------------------+
| etc/plugins/emq_recon.conf             | Recon调试插件配置                 |
+----------------------------------------+-----------------------------------+
| etc/plugins/emq_reloader.conf          | 热加载插件配置                    |
+----------------------------------------+-----------------------------------+
| etc/plugins/emq_sn.conf                | MQTT-SN协议插件配置               |
+----------------------------------------+-----------------------------------+
| etc/plugins/emq_stomp.conf             | Stomp协议插件配置                 |
+----------------------------------------+-----------------------------------+

