
.. _configuration:

=======================
配置说明(Configuration)
=======================

------------
主要配置文件
------------

EMQ 2.0消息服务器通过etc/目录下配置文件进行设置，主要配置文件包括:

+-------------------+-----------------------------------+
| 配置文件          | 说明                              |
+-------------------+-----------------------------------+
| etc/vm.args       | Erlang 虚拟机的参数设置           |
+-------------------+-----------------------------------+
| etc/emqttd.conf   | EMQ 2.0消息服务器配置文件         |
+-------------------+-----------------------------------+

----------------
扩展插件配置文件
----------------

EMQ 2.0全部插件配置文件，在etc/plugins/目录:

+----------------------------------------+-----------------------------------+
| 配置文件                               | 说明                              |
+----------------------------------------+-----------------------------------+
| etc/plugins/emqttd_auth_http.conf      | HTTP认证插件配置                  |
+----------------------------------------+-----------------------------------+
| etc/plugins/emqttd_auth_mongo.conf     | MongoDB认证插件配置               |
+----------------------------------------+-----------------------------------+
| etc/plugins/emqttd_auth_mysql.conf     | MySQL认证插件配置                 |
+----------------------------------------+-----------------------------------+
| etc/plugins/emqttd_auth_pgsql.conf     | Postgre认证插件配置               |
+----------------------------------------+-----------------------------------+
| etc/plugins/emqttd_auth_redis.conf     | Redis认证插件配置                 |
+----------------------------------------+-----------------------------------+
| etc/plugins/emqttd_coap.conf           | CoAP协议服务器配置                |
+----------------------------------------+-----------------------------------+
| etc/plugins/emqttd_dashboard.conf      | Dashboard控制台插件配置           |
+----------------------------------------+-----------------------------------+
| etc/plugins/emqttd_plugin_template.conf| 示例插件模版                      |
+----------------------------------------+-----------------------------------+
| etc/plugins/emqttd_recon.conf          | Recon调试插件配置                 |
+----------------------------------------+-----------------------------------+
| etc/plugins/emqttd_reloader.conf       | 热加载插件配置                    |
+----------------------------------------+-----------------------------------+
| etc/plugins/emqttd_sn.conf             | MQTT-SN协议服务器配置             |
+----------------------------------------+-----------------------------------+
| etc/plugins/emqttd_stomp.conf          | Stomp协议插件配置                 |
+----------------------------------------+-----------------------------------+

----------------
扩展模块配置文件
----------------

EMQ 2.0扩展模块配置文件，在etc/modules/目录，在etc/emqttd.conf中引用:

+----------------------------+-----------------------------------+
| 配置文件                   | 说明                              |
+----------------------------+-----------------------------------+
| etc/modules/acl.config     | ACL(访问控制规则)设置             |
+----------------------------+-----------------------------------+
| etc/modules/client.config  | 基于ClientId认证设置              |
+----------------------------+-----------------------------------+
| etc/modules/rewrite.config | Rewrite扩展模块规则配置           |
+----------------------------+-----------------------------------+
| etc/ssl/*                  | SSL证书设置                       |
+-----------------------------+----------------------------------+

----------------
Erlang虚拟机参数
----------------

etc/vm.args文件设置Erlang虚拟机参数::

    ##-------------------------------------------------------------------------
    ## Name of the node
    ##-------------------------------------------------------------------------
    -name emqttd@127.0.0.1

    ## Cookie for distributed erlang
    -setcookie emqttdsecretcookie

    ##-------------------------------------------------------------------------
    ## Flags
    ##-------------------------------------------------------------------------

    ## Heartbeat management; auto-restarts VM if it dies or becomes unresponsive
    ## (Disabled by default..use with caution!)
    ##-heart
    -smp true

    ## Enable kernel poll and a few async threads
    +K true

    ## 12 threads/core.
    +A 48

    ## max process numbers
    +P 8192

    ## Sets the maximum number of simultaneously existing ports for this system
    +Q 8192

    ## max atom number
    ## +t

    ## Set the distribution buffer busy limit (dist_buf_busy_limit) in kilobytes.
    ## Valid range is 1-2097151. Default is 1024.
    ## +zdbbl 8192

    ## CPU Schedulers
    ## +sbt db

    ##-------------------------------------------------------------------------
    ## Env
    ##-------------------------------------------------------------------------

    ## Increase number of concurrent ports/sockets, deprecated in R17
    -env ERL_MAX_PORTS 8192

    -env ERTS_MAX_PORTS 8192

    -env ERL_MAX_ETS_TABLES 1024

    ## Tweak GC to run more often
    -env ERL_FULLSWEEP_AFTER 1000

etc/vm.args中两个最重要的参数:

+-------+----------------------------------------------------------------------------------------------+
| +P    | Erlang虚拟机允许的最大进程数，一个MQTT连接会消耗2个Erlang进程，所以参数值 > 最大连接数 * 2   |
+-------+----------------------------------------------------------------------------------------------+
| +Q    | Erlang虚拟机允许的最大Port数量，一个MQTT连接消耗1个Port，所以参数值 > 最大连接数             |
+-------+----------------------------------------------------------------------------------------------+

etc/vm.args设置Erlang节点名、节点间通信Cookie::

    -name emqttd@127.0.0.1

    ## Cookie for distributed erlang
    -setcookie emqttdsecretcookie

.. NOTE::

    Erlang/OTP平台应用多由分布的Erlang节点(进程)组成，每个Erlang节点(进程)需指配一个节点名，用于节点间通信互访。
    所有互相通信的Erlang节点(进程)间通过一个共用的Cookie进行安全认证。

-----------------
Lager日志级别配置
-----------------

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
EMQ 消息服务器配置
------------------

etc/emqttd.conf是EMQ消息服务器的核心配置文件，配置文件采用的是Erlang数据格式:

1. [ ] : 列表，逗号分隔元素

2. { } : 元组，配置元组一般两个元素{Env, Value}

3. %%  : 注释

MQTT协议参数设置
----------------

ClientId最大长度
................

%% Max ClientId Length Allowed.
{mqtt_max_clientid_len, 512}.

MQTT最大报文尺寸
................

.. code:: erlang

    %% Max Packet Size Allowed, 64K by default.
    {mqtt_max_packet_size, 65536}.

MQTT客户端连接闲置时间
......................

设置MQTT客户端最大允许闲置时间(Socket连接建立，但未收到CONNECT报文)::

    %% Client Idle Timeout.
    {mqtt_client_idle_timeout, 30}. % Second

MQTT认证模块设置
----------------

EMQ消息服务器认证由一系列认证模块(module)或插件(plugin)提供，系统默认支持用户名、ClientID、匿名(anonymouse)认证模块。

系统默认采用匿名认证(anonymous)，通过删除注释可开启其他认证方式。同时开启的多个认证模块组成认证链::

               ----------------           ----------------           ------------
    Client --> | Username认证 | -ignore-> | ClientID认证 | -ignore-> | 匿名认证 |
               ----------------           ----------------           ------------
                      |                         |                         |
                     \|/                       \|/                       \|/
                allow | deny              allow | deny              allow | deny

.. NOTE:: EMQ 2.0消息服务器还提供了MySQL、PostgreSQL、Redis、MongoDB、HTTP、LDAP认证插件，认证插件加载后认证模块失效。

用户名密码认证
..............

.. code-block:: erlang

    %% Authentication with username, password
    {auth, username, [{passwd, "etc/modules/passwd.conf"}]}.

两种方式添加用户:

1. 直接在etc/modules/passwd.conf中明文配置默认用户::

    {"user1", "passwd1"}.
    {"user2", "passwd2"}.

2. 通过'./bin/emqttd_ctl'管理命令行添加用户::

   $ ./bin/emqttd_ctl users add <Username> <Password>

ClientID认证
............

.. code-block:: erlang

    %% Authentication with clientId
    {auth, clientid, [{config, "etc/modules/client.conf"}, {password, no}]}.

etc/modules/clients.conf文件中添加ClientID::

    "testclientid0".
    {"testclientid1", "127.0.0.1"}.
    {"testclientid2", "192.168.0.1/24"}.

匿名认证
........

默认开启，允许任意客户端登录::

    %% Anonymous: Allow all
    {auth, anonymous, []}.

用户访问控制(ACL)设置
---------------------

EMQ消息服务器支持基于etc/modules/acl.conf文件或MySQL、PostgreSQL插件的访问控制规则。

默认开启基于etc/modules/acl.conf文件的访问控制::

    %% Internal ACL config
    {acl, internal, [{config, "etc/modules/acl.conf"}, {nomatch, allow}]}.

etc/modules/acl.conf访问控制规则定义::

    允许|拒绝  用户|IP地址|ClientID  发布|订阅  主题列表

访问控制规则采用Erlang元组格式，访问控制模块逐条匹配规则::

              ---------              ---------              ---------
    Client -> | Rule1 | --nomatch--> | Rule2 | --nomatch--> | Rule3 | --> Default
              ---------              ---------              ---------
                  |                      |                      |
                match                  match                  match
                 \|/                    \|/                    \|/
            allow | deny           allow | deny           allow | deny


etc/modules/acl.conf默认访问规则设置::

    %% 允许'dashboard'用户订阅 '$SYS/#'
    {allow, {user, "dashboard"}, subscribe, ["$SYS/#"]}.

    %% 允许本机用户发布订阅全部主题
    {allow, {ipaddr, "127.0.0.1"}, pubsub, ["$SYS/#", "#"]}.

    %% 拒绝用户订阅'$SYS#'与'#'主题
    {deny, all, subscribe, ["$SYS/#", {eq, "#"}]}.

    %% 上述规则无匹配，允许
    {allow, all}.

.. NOTE:: 默认规则只允许本机用户订阅'$SYS/#'与'#'

EMQ消息服务器接收到MQTT客户端发布(PUBLISH)或订阅(SUBSCRIBE)请求时，会逐条匹配ACL访问控制规则，直到匹配成功返回allow或deny。

Broker参数设置
--------------

broker_sys_interval设置系统发布$SYS消息周期::

    {broker_sys_interval, 60}.

Retained消息设置
----------------

Retained消息超期时间
..................

.. code:: erlang

    %% Expired after seconds, never expired if 0
    {retained_expired_after, 0}.

最大存储Retained消息数量
........................

.. code:: erlang

    %% Max number of retained messages
    {retained_max_message_num, 100000}.

Retained消息最大报文尺寸
........................

.. code:: erlang

    %% Max Payload Size of retained message
    {retained_max_playload_size, 65536}.

MQTT会话(Session)参数设置
-------------------------

.. code:: erlang

    %% Max number of QoS 1 and 2 messages that can be “inflight” at one time.
    %% 0 means no limit
    {session_max_inflight, 100}.

    %% Retry interval for redelivering QoS1/2 messages.
    {session_unack_retry_interval, 60}.

    %% Awaiting PUBREL Timeout
    {session_await_rel_timeout, 20}.

    %% Max Packets that Awaiting PUBREL, 0 means no limit
    {session_max_awaiting_rel, 0}.

    %% Statistics Collection Interval(seconds)
    {session_collect_interval, 0}.

    %% Expired after 2 day (unit: minute)
    {session_expired_after, 2880}.

+------------------------------+----------------------------------------------------------+
| session_max_inflight         | 飞行窗口。最大允许同时下发的Qos1/2报文数，0表示没有限制。|
|                              | 窗口值越大，吞吐越高；窗口值越小，消息顺序越严格         |
+------------------------------+----------------------------------------------------------+
| session_unack_retry_interval | 下发QoS1/2消息未收到PUBACK响应的重试间隔                 |
+------------------------------+----------------------------------------------------------+
| session_await_rel_timeout    | 收到QoS2消息，等待PUBREL报文超时时间                     |
+------------------------------+----------------------------------------------------------+
| session_max_awaiting_rel     | 最大等待PUBREL的QoS2报文数                               |
+------------------------------+----------------------------------------------------------+
| session_collect_interval     | 采集会话统计数据间隔，默认0表示关闭统计                  |
+------------------------------+----------------------------------------------------------+
| session_expired_after        | 持久会话到期时间，从客户端断开算起，单位：分钟           |
+------------------------------+----------------------------------------------------------+

MQTT消息队列(MQueue)设置
------------------------

EMQ消息服务器会话通过队列缓存Qos1/Qos2消息:

1. 持久会话(Session)的离线消息

2. 飞行窗口满而延迟下发的消息

队列参数设置::

    %% Type: simple | priority
    {queue_type, simple}.

    %% Topic Priority: 0~255, Default is 0
    %% {queue_priority, [{"topic/1", 10}, {"topic/2", 8}]}.

    %% Max queue length. Enqueued messages when persistent client disconnected,
    %% or inflight window is full.
    {queue_max_length, infinity}.

    %% Low-water mark of queued messages
    {queue_low_watermark, 0.2}.

    %% High-water mark of queued messages
    {queue_high_watermark, 0.6}.

    %% Queue Qos0 messages?
    {queue_qos0, true}.

队列参数说明:

+----------------------+---------------------------------------------------+
| queue_type           | 队列类型。simple: 简单队列，priority: 优先级队列  |
+----------------------+---------------------------------------------------+
| queue_priority       | 主题(Topic)队列优先级设置                         |
+----------------------+---------------------------------------------------+
| queue_max_length     | 队列长度, infinity表示不限制                      |
+----------------------+---------------------------------------------------+
| queue_low_watermark  | 解除告警水位线                                    |
+----------------------+---------------------------------------------------+
| queue_high_watermark | 队列满告警水位线                                  |
+----------------------+---------------------------------------------------+
| queue_qos0           | 是否缓存QoS0消息                                  |
+----------------------+---------------------------------------------------+

发布订阅(PubSub)参数设置
------------------------

PubSub进程池
............

.. code:: erlang

    %% PubSub Pool Size. Default should be scheduler numbers.
    {pubsub_pool_size, 8}.

ClientId订阅
............

MQTT会话通过ClientId订阅。

.. code:: erlang

    {pubsub_by_clientid, true}.

是否异步订阅
............

.. code:: erlang

    %% Subscribe Asynchronously
    {pubsub_async, true}.

EMQ桥接(bridge)参数设置
-----------------------

桥接最大缓存报文数
..................

.. code:: erlang

    %% TODO: Bridge Queue Size
    {bridge_max_queue_len, 10000}.

桥接节点宕机检测周期
....................

.. code:: erlang

    %% Ping Interval of bridge node
    {bridge_ping_down_interval, 1}. % second

Plugins插件目录设置
-------------------

插件配置文件目录
................

.. code:: erlang

    %% Dir of plugins' config
    {plugins_etc_dir, "etc/plugins/"}.

已加载插件存储文件
.................

.. code:: erlang

    %% File to store loaded plugin names.
    {plugins_loaded_file, "data/loaded_plugins"}.

Modules扩展模块设置
-------------------

EMQ消息服务器支持简单的扩展模块，用于定制服务器功能。默认支持presence、subscription、rewrite模块。

Presence模块设置
................

'presence'扩展模块会向$SYS主题(Topic)发布客户端上下线消息:

.. code:: erlang

    %% Client presence management module. Publish presence messages when 
    %% client connected or disconnected.
    {module, presence, [{qos, 0}]}.

Subscription模块配置
....................

'subscription'扩展模块支持客户端上线时，自动订阅或恢复订阅某些主题(Topic)::

.. code:: erlang

    %% Subscribe topics automatically when client connected
    {module, subscription, [{"$client/$c", 1}]}.

Rewrite模块配置
...............

'rewrite'扩展模块支持重写主题(Topic)路径, 重写规则定义在etc/rewrite.conf文件:

.. code:: erlang

    %% [Rewrite](https://github.com/emqtt/emqttd/wiki/Rewrite)
    {module, rewrite, [{config, "etc/modules/rewrite.conf"}]}.

etc/modules/rewrite.conf扩展模块的规则配置文件，示例配置::

    {topic, "x/#", [
        {rewrite, "^x/y/(.+)$", "z/y/$1"},
        {rewrite, "^x/(.+)$", "y/$1"}
    ]}.

    {topic, "y/+/z/#", [
        {rewrite, "^y/(.+)/z/(.+)$", "y/z/$2"}
    ]}.

Listener监听器设置
------------------

EMQ消息服务器开启的MQTT协议、HTTP协议服务端，可通过listener设置TCP服务端口、最大允许连接数等参数。

EMQ 2.0消息服务器默认开启的TCP服务端口包括:

+-----------+-----------------------------------+
| 1883      | MQTT协议端口                      |
+-----------+-----------------------------------+
| 8883      | MQTT(SSL)端口                     |
+-----------+-----------------------------------+
| 8083      | MQTT(WebSocket), HTTP API端口     |
+-----------+-----------------------------------+

listener参数说明:

+-------------+-----------------------------------------------------------+
| acceptors   | TCP Acceptor池                                            |
+-------------+-----------------------------------------------------------+
| max_clients | 最大允许TCP连接数                                         |
+-------------+-----------------------------------------------------------+
| access      | 允许访问的IP地址段设置，例如: [{allow, "192.168.1.0/24"}] |
+-------------+-----------------------------------------------------------+
| connopts    | 连接限速配置，例如限速10KB/秒: {rate_limit, "100,10"}     |
+-------------+-----------------------------------------------------------+
| sockopts    | Socket参数设置                                            |
+-------------+-----------------------------------------------------------+

1883 - MQTT协议端口
...................

.. code-block:: erlang

    %% Plain MQTT
    {listener, mqtt, 1883, [
        %% Size of acceptor pool
        {acceptors, 16},

        %% Maximum number of concurrent clients
        {max_clients, 512},

        %% Mount point prefix
        %% {mount_point, "prefix/"},

        %% Socket Access Control
        {access, [{allow, all}]},

        %% Connection Options
        {connopts, [
            %% Rate Limit. Format is 'burst, rate', Unit is KB/Sec
            %% {rate_limit, "100,10"} %% 100K burst, 10K rate
        ]},

        %% Socket Options
        {sockopts, [
            %Set buffer if hight thoughtput
            %{recbuf, 4096},
            %{sndbuf, 4096},
            %{buffer, 4096},
            %{nodelay, true},
            {backlog, 1024}
        ]}
    ]}.

8883 - MQTT(SSL)端口
.....................

.. code-block:: erlang

    %% MQTT/SSL
    {listener, mqtts, 8883, [
        %% Size of acceptor pool
        {acceptors, 4},

        %% Maximum number of concurrent clients
        {max_clients, 512},

        %% Mount point prefix
        %% {mount_point, "secure/"},

        %% Socket Access Control
        {access, [{allow, all}]},

        %% SSL certificate and key files
        {ssl, [{certfile, "etc/ssl/ssl.crt"},
               {keyfile,  "etc/ssl/ssl.key"}]},

        %% Socket Options
        {sockopts, [
            {backlog, 1024}
            %{buffer, 4096},
        ]}
    ]}.

8083 - MQTT(WebSocket)端口
..........................

.. code-block:: erlang

    %% HTTP and WebSocket Listener
    {listener, http, 8083, [
        %% Size of acceptor pool
        {acceptors, 4},

        %% Maximum number of concurrent clients
        {max_clients, 64},

        %% Socket Access Control
        {access, [{allow, all}]},

        %% Socket Options
        {sockopts, [
            {backlog, 1024}
            %{buffer, 4096},
        ]}
    ]}.

Erlang虚拟机监控设置
--------------------

.. code:: erlang

    %% Long GC, don't monitor in production mode for:
    %% https://github.com/erlang/otp/blob/feb45017da36be78d4c5784d758ede619fa7bfd3/erts/emulator/beam/erl_gc.c#L421

    {sysmon_long_gc, false}.

    %% Long Schedule(ms)
    {sysmon_long_schedule, 240}.

    %% 8M words. 32MB on 32-bit VM, 64MB on 64-bit VM.
    %% 8 * 1024 * 1024
    {sysmon_large_heap, 8388608}.

    %% Busy Port
    {sysmon_busy_port, false}.

    %% Busy Dist Port
    {sysmon_busy_dist_port, true}.

