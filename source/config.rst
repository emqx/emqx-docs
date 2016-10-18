
.. _configuration:

=======================
配置说明(Configuration)
=======================

------------
EMQ 配置文件
------------

*EMQ* 2.0消息服务器通过etc/目录下配置文件进行设置，主要配置文件包括:

+----------------------------+-----------------------------------+
| 配置文件                   | 说明                              |
+----------------------------+-----------------------------------+
| etc/emq.conf               | EMQ 2.0消息服务器配置文件         |
+----------------------------+-----------------------------------+
| etc/acl.conf               | EMQ 2.0默认ACL配置文件            |
+----------------------------+-----------------------------------+
| etc/plugins/\*.conf        | EMQ 2.0各类插件配置文件           |
+----------------------------+-----------------------------------+

*EMQ* 配置文件方式到2.0-rc.2版本为止经过三次调整，主要为了满足用户配置便捷型，同时又不影响插件开发者社区。

1. EMQ 1.x版本采用Erlang原生配置文件格式etc/emqttd.config, 内容类似::

    {emqttd, [
      %% Authentication and Authorization
      {access, [
        %% Authetication. Anonymous Default
        {auth, [
            %% Authentication with username, password
            %{username, []},
            
            %% Authentication with clientid
            %{clientid, [{password, no}, {file, "etc/clients.config"}]},

TODO: 格式问题说明...

2. EMQ 2.0-beta.x版本采用简化了的原生Erlang格式，类似rebar.config或relex.config::

TODO: 格式例子和问题说明

3. EMQ 2.0-rc.x正式版采用了类似sysctl的`k = v`通用格式，并在系统启动时翻译成Erlang原生配置格式。

TODO: 格式例子和改进声明...

------------
EMQ 环境变量
------------

*EMQ* 2.0还支持操作系统变量:

```
$EMQ_NODE_NAME" ] && EMQ_NODE_NAME=emqttd@127.0.0.1
$EMQ_NODE_COOKIE" ] && EMQ_NODE_COOKIE=emq_dist_cookie
$EMQ_MAX_PORTS" ] && EMQ_MAX_PORTS=65536
$EMQ_TCP_PORT" ] && EMQ_TCP_PORT=1883
$EMQ_SSL_PORT" ] && EMQ_SSL_PORT=8883
$EMQ_HTTP_PORT" ] && EMQ_HTTP_PORT=8083
$EMQ_HTTPS_PORT" ] && EMQ_HTTPS_PORT=8084
```
export EMQ_TCP_PORT=2883 && ./bin/emqttd start

---------------
EMQ节点与Cookie
---------------

.. code::

    ## Node name
    node.name = emqttd@127.0.0.1

    ## Cookie for distributed node
    node.cookie = emq_dist_cookie

----------------
Erlang虚拟机参数
----------------

.. code::

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

Erlang虚拟机参数说明:

+-------+----------------------------------------------------------------------------------------------+
| +P    | Erlang虚拟机允许的最大进程数，一个MQTT连接会消耗2个Erlang进程，所以参数值 > 最大连接数 * 2   |
+-------+----------------------------------------------------------------------------------------------+
| +Q    | Erlang虚拟机允许的最大Port数量，一个MQTT连接消耗1个Port，所以参数值 > 最大连接数             |
+-------+----------------------------------------------------------------------------------------------+

releases/2.0/vm.args设置Erlang节点名、节点间通信Cookie::

    -name emqttd@127.0.0.1

    ## Cookie for distributed erlang
    -setcookie emqttdsecretcookie

.. NOTE::

    Erlang/OTP平台应用多由分布的Erlang节点(进程)组成，每个Erlang节点(进程)需指配一个节点名，用于节点间通信互访。
    所有互相通信的Erlang节点(进程)间通过一个共用的Cookie进行安全认证。

---------------
console日志配置
---------------

.. code::

    ## Console log. Enum: off, file, console, both
    log.console = console

    ## Console log level. Enum: debug, info, notice, warning, error, critical, alert, emergency
    log.console.level = error

    ## Console log file
    ## log.console.file = log/console.log

-------------
error日志配置
-------------

.. code::

    ## Error log file
    log.error.file = log/error.log

-------------
crash日志配置
-------------

.. code::

    ## Enable the crash log. Enum: on, off
    log.crash = on

    log.crash.file = log/crash.log

EMQ消息服务器日志由lager应用(application)提供，日志相关设置在releases/2.0/sys.config文件的lager应用段落::

  {lager, [
    ...
  ]},

产品环境下默认只开启error日志，日志输出到logs/emqttd_error.log文件。'handlers'段落启用其他级别日志::

    {handlers, [
        {lager_console_backend, info},

        {lager_file_backend, [
            {formatter_config, [time, " ", pid, " [",severity,"] ", message, "\n"]},
            {file, "log/emqttd_info.log"},
            {level, info},
            {size, 104857600},
            {date, "$D0"},
            {count, 30}
        ]},

        {lager_file_backend, [
            {formatter_config, [time, " ", pid, " [",severity,"] ", message, "\n"]},
            {file, "log/emqttd_error.log"},
            {level, error},
            {size, 104857600},
            {date, "$D0"},
            {count, 30}
        ]}
    ]}

.. WARNING:: 过多日志打印严重影响服务器性能，产品环境下建议开启error级别日志。

------------------
MQTT 协议参数配置
------------------

------------------
MQTT 匿名认证
------------------

------------------
MQTT 默认ACL文件
------------------

------------------
MQTT 会话参数设置
------------------

------------------
MQTT 队列参数设置
------------------

------------------
MQTT Listener设置
------------------

------------------
EMQ 桥接参数设置
------------------

------------------
EMQ 插件目录设置
------------------


--------------------
etc/emq.conf配置文件
--------------------

2.0-rc.2版本重新设计了全部配置文件格式，采用类似sysctl "K = V"的通用格式，原vm.args与emqttd.conf合并为单一配置: etc/emq.conf。

----------------
ClientId最大长度
----------------

.. code::

    ## Max ClientId Length Allowed.
    mqtt.max_clientid_len = 1024

----------------
MQTT最大报文尺寸
----------------

.. code::

    ## Max Packet Size Allowed, 64K by default.
    mqtt.max_packet_size = 64KB

----------------------
MQTT客户端连接闲置时间
----------------------

设置MQTT客户端最大允许闲置时间(Socket连接建立，但未收到CONNECT报文)::

    ## Client Idle Timeout (Second)
    mqtt.client_idle_timeout = 30

------------
允许匿名认证
------------

默认开启，允许任意客户端登录::

    ## Allow Anonymous authentication
    mqtt.allow_anonymous = true

---------------------
默认访问控制(ACL)文件
---------------------

*EMQ* 消息服务器支持基于etc/acl.conf文件或MySQL、PostgreSQL插件的访问控制规则。

.. code::

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


etc/acl.conf默认访问规则设置::

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

-------------------------
MQTT会话(Session)参数设置
-------------------------

.. code::

    ## Max number of QoS 1 and 2 messages that can be “inflight” at one time.
    ## 0 means no limit
    mqtt.session.max_inflight = 100

    ## Retry interval for redelivering QoS1/2 messages.
    mqtt.session.retry_interval = 60

    ## Awaiting PUBREL Timeout
    mqtt.session.await_rel_timeout = 20

    ## Max Packets that Awaiting PUBREL, 0 means no limit
    mqtt.session.max_awaiting_rel = 0

    ## Statistics Collection Interval(seconds)
    mqtt.session.collect_interval = 0

    ## Expired after 1 day:
    ## w - week
    ## d - day
    ## h - hour
    ## m - minute
    ## s - second
    mqtt.session.expired_after = 1d

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
| session.collect_interval  | 采集会话统计数据间隔，默认0表示关闭统计                  |
+---------------------------+----------------------------------------------------------+
| session.expired_after     | 持久会话到期时间，从客户端断开算起，单位：分钟           |
+---------------------------+----------------------------------------------------------+

------------------------
MQTT消息队列(MQueue)设置
------------------------

EMQ消息服务器会话通过队列缓存Qos1/Qos2消息:

1. 持久会话(Session)的离线消息

2. 飞行窗口满而延迟下发的消息

队列参数设置::

    ## Type: simple | priority
    mqtt.queue.type = simple

    ## Topic Priority: 0~255, Default is 0
    ## mqtt.queue.priority = topic/1=10,topic/2=8

    ## Max queue length. Enqueued messages when persistent client disconnected,
    ## or inflight window is full.
    mqtt.queue.max_length = infinity

    ## Low-water mark of queued messages
    mqtt.queue.low_watermark = 20%

    ## High-water mark of queued messages
    mqtt.queue.high_watermark = 60%

    ## Queue Qos0 messages?
    mqtt.queue.qos0 = true

队列参数说明:

+----------------------+---------------------------------------------------+
| queue.type           | 队列类型。simple: 简单队列，priority: 优先级队列  |
+----------------------+---------------------------------------------------+
| queue.priority       | 主题(Topic)队列优先级设置                         |
+----------------------+---------------------------------------------------+
| queue.max_length     | 队列长度, infinity表示不限制                      |
+----------------------+---------------------------------------------------+
| queue.low_watermark  | 解除告警水位线                                    |
+----------------------+---------------------------------------------------+
| queue.high_watermark | 队列满告警水位线                                  |
+----------------------+---------------------------------------------------+
| queue.qos0           | 是否缓存QoS0消息                                  |
+----------------------+---------------------------------------------------+

--------------
Broker参数设置
--------------

broker_sys_interval设置系统发布$SYS消息周期::

    ## System Interval of publishing broker $SYS Messages
    mqtt.broker.sys_interval = 60

------------------------
发布订阅(PubSub)参数设置
------------------------

.. code::

    ## System Interval of publishing broker $SYS Messages
    mqtt.broker.sys_interval = 60

    ## PubSub Pool Size. Default should be scheduler numbers.
    mqtt.pubsub.pool_size = 8

    mqtt.pubsub.by_clientid = true

    ## Subscribe Asynchronously
    mqtt.pubsub.async = true

--------------------
桥接(bridge)参数设置
--------------------

.. code::

    ## Bridge Queue Size
    mqtt.bridge.max_queue_len = 10000

    ## Ping Interval of bridge node. Unit: Second
    mqtt.bridge.ping_down_interval = 1

----------------------
Plugins插件etc目录设置
----------------------

.. code::

    ## Dir of plugins' config
    mqtt.plugins.etc_dir = etc/plugins/

    ## File to store loaded plugin names.
    mqtt.plugins.loaded_file = data/loaded_plugins

------------------
Modules - 扩展模块
------------------

*EMQ* 消息服务器支持简单的扩展模块，用于定制服务器功能。默认支持retainer, presence、subscription模块。

----------------
启用Retainer模块
----------------

Retainer模块用于持久化MQTT Retained消息。

.. code::

    ## Enable retainer module
    mqtt.module.retainer = on

    ## disc: disc_copies, ram: ram_copies
    mqtt.module.retainer.storage_type = ram

    ## Max number of retained messages
    mqtt.module.retainer.max_message_num = 100000

    ## Max Payload Size of retained message
    mqtt.module.retainer.max_payload_size = 64KB

    ## Expired after seconds, never expired if 0
    mqtt.module.retainer.expired_after = 0

----------------
启用Presence模块
----------------

'presence'扩展模块会向$SYS主题(Topic)发布客户端上下线消息:

.. code::

    ## Enable presence module
    ## Publish presence messages when client connected or disconnected.
    mqtt.module.presence = on

    mqtt.module.presence.qos = 0

--------------------
启用Subscription模块
--------------------

'subscription'扩展模块支持客户端上线时，自动订阅或恢复订阅某些主题(Topic):

.. code::

    # Enable subscription module
    mqtt.module.subscription = on

    mqtt.module.subscription.topics = $client/%c=1,$user/%u=1

------------------
Listener监听器参数
------------------

*EMQ* 消息服务器默认开启的MQTT协议、MQTT/SSL、MQTT/WS协议服务端，可通过mqtt.listener.*设置端口、最大允许连接数等参数。

*EMQ* 2.0消息服务器默认开启的TCP服务端口包括:

+-----------+-----------------------------------+
| 1883      | MQTT协议端口                      |
+-----------+-----------------------------------+
| 8883      | MQTT(SSL)端口                     |
+-----------+-----------------------------------+
| 8083      | MQTT(WebSocket), HTTP API端口     |
+-----------+-----------------------------------+

Listener参数说明:

+-----------------------------+----------------------------------------------+
| mqtt.listener.*.acceptors   | TCP Acceptor池                               |
+-----------------------------+----------------------------------------------+
| mqtt.listener.*.max_clients | 最大允许TCP连接数                            |
+-----------------------------+----------------------------------------------+
| mqtt.listener.*.rate_limit  | 连接限速配置，例如限速10KB/秒:  "100,10"     |
+-----------------------------+----------------------------------------------+

----------------------
MQTT(TCP)监听器 - 1883
----------------------

.. code-block:: erlang

    ## TCP Listener: 1883, 127.0.0.1:1883, ::1:1883
    mqtt.listener.tcp = 1883

    ## Size of acceptor pool
    mqtt.listener.tcp.acceptors = 8

    ## Maximum number of concurrent clients
    mqtt.listener.tcp.max_clients = 1024

    ## Rate Limit. Format is 'burst,rate', Unit is KB/Sec
    ## mqtt.listener.tcp.rate_limit = 100,10

    ## TCP Socket Options
    mqtt.listener.tcp.backlog = 1024
    ## mqtt.listener.tcp.recbuf = 4096
    ## mqtt.listener.tcp.sndbuf = 4096
    ## mqtt.listener.tcp.buffer = 4096
    ## mqtt.listener.tcp.nodelay = true

----------------------
MQTT(SSL)监听器 - 8883
----------------------

.. code-block::

    ## SSL Listener: 8883, 127.0.0.1:8883, ::1:8883
    mqtt.listener.ssl = 8883

    ## Size of acceptor pool
    mqtt.listener.ssl.acceptors = 4

    ## Maximum number of concurrent clients
    mqtt.listener.ssl.max_clients = 512

    ## Rate Limit. Format is 'burst,rate', Unit is KB/Sec
    ## mqtt.listener.ssl.rate_limit = 100,10

    ## SSL Options
    mqtt.listener.ssl.handshake_timeout = 15
    mqtt.listener.ssl.keyfile = etc/certs/key.pem
    mqtt.listener.ssl.certfile = etc/certs/cert.pem
    mqtt.listener.ssl.cacertfile = etc/certs/cacert.pem
    ## mqtt.listener.ssl.verify = verify_peer
    ## mqtt.listener.ssl.failed_if_no_peer_cert = true

----------------------------
MQTT(WebSocket)监听器 - 8083
----------------------------

.. code-block::

    ## HTTP and WebSocket Listener
    mqtt.listener.http = 8083
    mqtt.listener.http.acceptors = 4
    mqtt.listener.http.max_clients = 64

--------------------------------
MQTT(WebSocket/SSL)监听器 - 8084
--------------------------------

.. code-block::

    ## HTTP(SSL) Listener
    mqtt.listener.https = 8084
    mqtt.listener.https.acceptors = 4
    mqtt.listener.https.max_clients = 64
    mqtt.listener.https.handshake_timeout = 10
    mqtt.listener.https.certfile = etc/certs/cert.pem
    mqtt.listener.https.keyfile = etc/certs/key.pem
    mqtt.listener.https.cacertfile = etc/certs/cacert.pem
    ## 开启双向认证
    ## mqtt.listener.https.verify = verify_peer
    ## mqtt.listener.https.failed_if_no_peer_cert = true

--------------------
Erlang虚拟机监控设置
--------------------

.. code::

    ## Long GC, don't monitor in production mode for:
    ## https://github.com/erlang/otp/blob/feb45017da36be78d4c5784d758ede619fa7bfd3/erts/emulator/beam/erl_gc.c#L421
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

*EMQ* 2.0全部插件配置文件，在etc/plugins/目录:

+----------------------------------------+-----------------------------------+
| 配置文件                               | 说明                              |
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
| etc/plugins/emq_sn.conf                | MQTT-SN协议服务器配置             |
+----------------------------------------+-----------------------------------+
| etc/plugins/emq_stomp.conf             | Stomp协议插件配置                 |
+----------------------------------------+-----------------------------------+

