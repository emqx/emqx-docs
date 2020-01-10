
.. _commands:

===================
管理命令 (Commands)
===================

*EMQ X* 消息服务器提供了 ``./bin/emqx_ctl`` 的管理命令行。

-----------
status 命令
-----------

查询 *EMQ X* 消息服务器运行状态::

    $ ./bin/emqx_ctl status

    Node 'emqx@127.0.0.1' is started
    emqx v4.0.0 is running

---------
mgmt 命令
---------

mgmt 命令查询应用程序。

+------------------------------+-----------------------------+
| mgmt list                    | 列出应用程序列表            |
+------------------------------+-----------------------------+
| mgmt insert <AppId> <Name>   | 添加 REST API 的应用程序    |
+------------------------------+-----------------------------+
| mgmt update <AppId> <status> | 更新 REST API 的应用程序    |
+------------------------------+-----------------------------+
| mgmt lookup <AppId>          | 获取 REST API 的应用程序详情|
+------------------------------+-----------------------------+
| mgmt delete <AppId>          | 删除 REST API 的应用程序    |
+------------------------------+-----------------------------+

mgmt list
---------

列出应用程序列表::

    $ ./bin/emqx_ctl mgmt list
    app_id: 901abdba8eb8c, secret: MjgzMzQ5MjM1MzUzMTc4MjgyMjE3NzU4ODcwMDg0NjQ4OTG, name: hello, desc: , status: true, expired: undefined

mgmt insert <AppId> <Name>
--------------------------

添加REST API的应用程序::

    $ ./bin/emqx_ctl mgmt insert dbcb6e023370b world
    AppSecret: MjgzMzQ5MjYyMTY3ODk4MjA5NzMwODExODMxMDM1NDk0NDA

mgmt update <AppId> <status>
-----------------------------

更新 REST API 的应用程序::

    $ ./bin/emqx_ctl mgmt update dbcb6e023370b stop
    update successfully.

mgmt lookup <AppId>
---------------------

获取 REST API 的应用程序详情::

    $ ./bin/emqx_ctl mgmt lookup dbcb6e023370b
    app_id: dbcb6e023370b
    secret: MjgzMzQ5MjYyMTY3ODk4MjA5NzMwODExODMxMDM1NDk0NDA
    name: world
    desc: Application user
    status: stop
    expired: undefined

mgmt delete <AppId>
--------------------

删除REST API的应用程序::

    $ ./bin/emqx_ctl mgmt delete dbcb6e023370b
    ok

-----------
broker 命令
-----------

broker 命令查询服务器基本信息，启动时间，统计数据与性能数据。

+----------------+---------------------------------------------------+
| broker         | 查询 EMQ X 消息服务器描述、版本、启动时间         |
+----------------+---------------------------------------------------+
| broker stats   | 查询连接(Connection)、会话(Session)、主题(Topic)、|
|                | 订阅(Subscription)、路由(Route)统计信息           |
+----------------+---------------------------------------------------+
| broker metrics | 查询 MQTT 报文(Packet)、消息(Message)收发统计     |
+----------------+---------------------------------------------------+

查询 *EMQ X* 消息服务器基本信息包括版本、启动时间等::

    $ ./bin/emqx_ctl broker

    sysdescr  : EMQ X Broker
    version   : v4.0.0
    uptime    : 25 seconds
    datetime  : 2019-12-19 14:34:19

broker stats
------------

查询服务器客户端连接(Connections)、会话(Sessions)、主题(Topics)、订阅(Subscriptions)、路由(Routes)统计::

    $ ./bin/emqx_ctl broker stats

    actions.count                 : 5
    actions.max                   : 5
    channels.count                : 0
    channels.max                  : 0
    connections.count             : 0
    connections.max               : 0
    resources.count               : 0
    resources.max                 : 0
    retained.count                : 3
    retained.max                  : 3
    routes.count                  : 0
    routes.max                    : 0
    rules.count                   : 0
    rules.max                     : 0
    sessions.count                : 0
    sessions.max                  : 0
    suboptions.count              : 0
    suboptions.max                : 0
    subscribers.count             : 0
    subscribers.max               : 0
    subscriptions.count           : 0
    subscriptions.max             : 0
    subscriptions.shared.count    : 0
    subscriptions.shared.max      : 0
    topics.count                  : 0
    topics.max                    : 0

broker metrics
--------------

查询服务器流量(Bytes)、MQTT报文(Packets)、消息(Messages)收发统计::

    $ ./bin/emqx_ctl broker metrics

    actions.success               : 0
    bytes.received                : 0
    bytes.sent                    : 0
    client.auth.anonymous         : 0
    client.authenticate           : 0
    client.check_acl              : 0
    client.connack                : 0
    client.connect                : 0
    client.connected              : 0
    client.disconnected           : 0
    client.subscribe              : 0
    client.unsubscribe            : 0
    delivery.dropped              : 0
    delivery.dropped.expired      : 0
    delivery.dropped.no_local     : 0
    delivery.dropped.qos0_msg     : 0
    delivery.dropped.queue_full   : 0
    delivery.dropped.too_large    : 0
    messages.acked                : 0
    messages.delayed              : 0
    messages.delivered            : 0
    messages.dropped              : 0
    messages.dropped.expired      : 0
    messages.dropped.no_subscriber: 0
    messages.forward              : 0
    messages.publish              : 0
    messages.qos0.received        : 0
    messages.qos0.sent            : 0
    messages.qos1.received        : 0
    messages.qos1.sent            : 0
    messages.qos2.received        : 0
    messages.qos2.sent            : 0
    messages.received             : 0
    messages.retained             : 3
    messages.sent                 : 0
    packets.auth.received         : 0
    packets.auth.sent             : 0
    packets.connack.auth_error    : 0
    packets.connack.error         : 0
    packets.connack.sent          : 0
    packets.connect.received      : 0
    packets.disconnect.received   : 0
    packets.disconnect.sent       : 0
    packets.pingreq.received      : 0
    packets.pingresp.sent         : 0
    packets.puback.inuse          : 0
    packets.puback.missed         : 0
    packets.puback.received       : 0
    packets.puback.sent           : 0
    packets.pubcomp.inuse         : 0
    packets.pubcomp.missed        : 0
    packets.pubcomp.received      : 0
    packets.pubcomp.sent          : 0
    packets.publish.auth_error    : 0
    packets.publish.dropped       : 0
    packets.publish.error         : 0
    packets.publish.received      : 0
    packets.publish.sent          : 0
    packets.pubrec.inuse          : 0
    packets.pubrec.missed         : 0
    packets.pubrec.received       : 0
    packets.pubrec.sent           : 0
    packets.pubrel.missed         : 0
    packets.pubrel.received       : 0
    packets.pubrel.sent           : 0
    packets.received              : 0
    packets.sent                  : 0
    packets.suback.sent           : 0
    packets.subscribe.auth_error  : 0
    packets.subscribe.error       : 0
    packets.subscribe.received    : 0
    packets.unsuback.sent         : 0
    packets.unsubscribe.error     : 0
    packets.unsubscribe.received  : 0
    rules.matched                 : 0
    session.created               : 0
    session.discarded             : 0
    session.resumed               : 0
    session.takeovered            : 0
    session.terminated            : 0

------------
cluster 命令
------------

cluster 命令集群多个 *EMQ X* 消息服务器节点(进程):

+----------------------------+---------------------+
| cluster join <Node>        | 加入集群            |
+----------------------------+---------------------+
| cluster leave              | 离开集群            |
+----------------------------+---------------------+
| cluster force-leave <Node> | 从集群删除节点      |
+----------------------------+---------------------+
| cluster status             | 查询集群状态        |
+----------------------------+---------------------+

cluster 命令集群本机两个 *EMQ X* 节点示例:

+-----------+---------------------+-------------+
| 目录      | 节点名              | MQTT 端口   |
+-----------+---------------------+-------------+
| emqx1     | emqx1@127.0.0.1     | 1883        |
+-----------+---------------------+-------------+
| emqx2     | emqx2@127.0.0.1     | 2883        |
+-----------+---------------------+-------------+

启动 emqx1 ::

    $ cd emqx1 && ./bin/emqx start

启动 emqx2 ::

    $ cd emqx2 && ./bin/emqx start

emqx2 节点与 emqx1 集群，emqx2 目录下::

    $ ./bin/emqx_ctl cluster join emqx1@127.0.0.1

    Join the cluster successfully.
    Cluster status: [{running_nodes,['emqx1@127.0.0.1','emqx2@127.0.0.1']}]

任意节点目录下查询集群状态::

    $ ./bin/emqx_ctl cluster status

    Cluster status: [{running_nodes,['emqx2@127.0.0.1','emqx1@127.0.0.1']}]

集群消息路由测试::

    # emqx1节点上订阅x
    $ mosquitto_sub -t x -q 1 -p 1883

    # emqx2节点上向x发布消息
    $ mosquitto_pub -t x -q 1 -p 2883 -m hello

emqx2 节点离开集群::

    $ cd emqx2 && ./bin/emqx_ctl cluster leave

emqx1 节点下删除 emqx2::

    $ cd emqx1 && ./bin/emqx_ctl cluster force-leave emqx2@127.0.0.1

.. note:: 不支持一个已经在 A 集群中的节点加入另外一个集群，因为这会导致两个集群数据不一致

--------
acl 命令
--------

重新加载 acl 配置文件::

    $ ./bin/emqx_ctl acl reload

------------
clients 命令
------------

clients 命令查询连接的 MQTT 客户端。

+-------------------------+-----------------------------+
| clients list            | 查询全部客户端连接          |
+-------------------------+-----------------------------+
| clients show <ClientId> | 根据 ClientId 查询客户端    |
+-------------------------+-----------------------------+
| clients kick <ClientId> | 根据 ClientId 踢出客户端    |
+-------------------------+-----------------------------+

clients list
------------

查询全部客户端连接::

    $ ./bin/emqx_ctl clients list

    Client(mosqsub/43832-airlee.lo, username=test1, peername=127.0.0.1:62135, clean_start=true, keepalive=60, session_expiry_interval=0, subscriptions=0, inflight=0, awaiting_rel=0, delivered_msgs=0, enqueued_msgs=0, dropped_msgs=0, connected=true, created_at=1576477947, connected_at=1576477947)
    Client(mosqsub/44011-airlee.lo, username=test2, peername=127.0.0.1:64961, clean_start=true, keepalive=60, session_expiry_interval=0, subscriptions=0, inflight=0, awaiting_rel=0, delivered_msgs=0, enqueued_msgs=0, dropped_msgs=0, connected=true, created_at=1576477950, connected_at=1576477950)
    ...

返回 Client 对象的属性:

+-------------------------+------------------------------------------+
| username                | 用户名                                   |
+-------------------------+------------------------------------------+
| peername                | 客户端 IP 与端口                         |
+-------------------------+------------------------------------------+
| clean_start             | MQTT Clean Start                         |
+-------------------------+------------------------------------------+
| keepalive               | MQTT KeepAlive                           |
+-------------------------+------------------------------------------+
| session_expiry_interval | 会话过期间隔                             |
+-------------------------+------------------------------------------+
| subscriptions           | 当前订阅数量                             |
+-------------------------+------------------------------------------+
| inflight                | 当前正在下发的消息数                     |
+-------------------------+------------------------------------------+
| awaiting_rel            | 等待客户端发送 PUBREL 的 QoS2 消息数     |
+-------------------------+------------------------------------------+
| delivered_msgs          | EMQ X 向此客户端转发的消息数量(包含重传) |
+-------------------------+------------------------------------------+
| enqueued_msgs           | 消息队列当前长度                         |
+-------------------------+------------------------------------------+
| dropped_msgs            | 消息队列达到最大长度后丢弃的消息数量     |
+-------------------------+------------------------------------------+
| connected               | 是否在线                                 |
+-------------------------+------------------------------------------+
| created_at              | 会话创建时间                             |
+-------------------------+------------------------------------------+
| connected_at            | 客户端连接时间                           |
+-------------------------+------------------------------------------+

clients show <ClientId>
-----------------------

根据 ClientId 查询客户端::

    $ ./bin/emqx_ctl clients show "mosqsub/43832-airlee.lo"

    Client(mosqsub/43832-airlee.lo, username=test1, peername=127.0.0.1:62747, clean_start=false, keepalive=60, session_expiry_interval=7200, subscriptions=0, inflight=0, awaiting_rel=0, delivered_msgs=0, enqueued_msgs=0, dropped_msgs=0, connected=true, created_at=1576479557, connected_at=1576479557)

clients kick <ClientId>
-----------------------

根据 ClientId 踢出客户端::

    $ ./bin/emqx_ctl clients kick "clientid"

-----------
routes 命令
-----------

routes 命令查询路由表。

+---------------------+---------------------+
| routes list         | 查询全部路由        |
+---------------------+---------------------+
| routes show <Topic> | 根据 Topic 查询路由 |
+---------------------+---------------------+

routes list
-----------

查询全部路由::

    $ ./bin/emqx_ctl routes list

    t2/# -> emqx2@127.0.0.1
    t/+/x -> emqx2@127.0.0.1,emqx@127.0.0.1

routes show <Topic>
-------------------

根据 Topic 查询一条路由::

    $ ./bin/emqx_ctl routes show t/+/x

    t/+/x -> emqx2@127.0.0.1,emqx@127.0.0.1

------------------
subscriptions 命令
------------------

subscriptions 命令查询消息服务器的订阅(Subscription)表。

+--------------------------------------------+--------------------------+
| subscriptions list                         | 查询全部订阅             |
+--------------------------------------------+--------------------------+
| subscriptions show <ClientId>              | 查询某个 ClientId 的订阅 |
+--------------------------------------------+--------------------------+
| subscriptions add <ClientId> <Topic> <QoS> | 手动添加静态订阅         |
+--------------------------------------------+--------------------------+
| subscriptions del <ClientId> <Topic>       | 手动删除静态订阅         |
+--------------------------------------------+--------------------------+

subscriptions list
------------------

查询全部订阅::

    $ ./bin/emqx_ctl subscriptions list

    mosqsub/91042-airlee.lo -> t/y:1
    mosqsub/90475-airlee.lo -> t/+/x:2

subscriptions show <ClientId>
-----------------------------

查询某个 Client 的订阅::

    $ ./bin/emqx_ctl subscriptions show 'mosqsub/90475-airlee.lo'

    mosqsub/90475-airlee.lo -> t/+/x:2

subscriptions add <ClientId> <Topic> <QoS>
------------------------------------------

手动添加订阅关系::

    $ ./bin/emqx_ctl subscriptions add 'mosqsub/90475-airlee.lo' '/world' 1

    ok

subscriptions del <ClientId> <Topic>
------------------------------------

手动删除订阅关系::

    $ ./bin/emqx_ctl subscriptions del 'mosqsub/90475-airlee.lo' '/world'

    ok

------------
plugins 命令
------------

plugins 命令用于加载、卸载、查询插件应用。 *EMQ X* 消息服务器通过插件扩展认证、定制功能，插件置于 plugins/ 目录下。

+---------------------------+-------------------------+
| plugins list              | 列出全部插件(Plugin)    |
+---------------------------+-------------------------+
| plugins load <Plugin>     | 加载插件(Plugin)        |
+---------------------------+-------------------------+
| plugins unload <Plugin>   | 卸载插件(Plugin)        |
+---------------------------+-------------------------+
| plugins reload <Plugin>   | 重载插件(Plugin)        |
+---------------------------+-------------------------+

.. note:: 当修改完成某插件的配置文件时，若需要立即生效则需要执行 ``reload`` 命令。因为 ``unload/load`` 命令不会编译新的配置文件

plugins list
------------

列出全部插件::

    $ ./bin/emqx_ctl plugins list

    Plugin(emqx_auth_clientid, version=v4.0.0, description=EMQ X Authentication with ClientId/Password, active=false)
    Plugin(emqx_auth_http, version=v4.0.0, description=EMQ X Authentication/ACL with HTTP API, active=false)
    Plugin(emqx_auth_jwt, version=v4.0.0, description=EMQ X Authentication with JWT, active=false)
    Plugin(emqx_auth_ldap, version=v4.0.0, description=EMQ X Authentication/ACL with LDAP, active=false)
    Plugin(emqx_auth_mongo, version=v4.0.0, description=EMQ X Authentication/ACL with MongoDB, active=false)
    Plugin(emqx_auth_mysql, version=v4.0.0, description=EMQ X Authentication/ACL with MySQL, active=false)
    Plugin(emqx_auth_pgsql, version=v4.0.0, description=EMQ X Authentication/ACL with PostgreSQL, active=false)
    Plugin(emqx_auth_redis, version=v4.0.0, description=EMQ X Authentication/ACL with Redis, active=false)
    Plugin(emqx_auth_username, version=v4.0.0, description=EMQ X Authentication with Username and Password, active=false)
    Plugin(emqx_bridge_mqtt, version=v4.0.0, description=EMQ X Bridge to MQTT Broker, active=false)
    Plugin(emqx_coap, version=v4.0.0, description=EMQ X CoAP Gateway, active=false)
    Plugin(emqx_dashboard, version=v4.0.0, description=EMQ X Web Dashboard, active=true)
    Plugin(emqx_delayed_publish, version=v4.0.0, description=EMQ X Delayed Publish, active=false)
    Plugin(emqx_lua_hook, version=v4.0.0, description=EMQ X Lua Hooks, active=false)
    Plugin(emqx_lwm2m, version=v4.0.0, description=EMQ X LwM2M Gateway, active=false)
    Plugin(emqx_management, version=v4.0.0, description=EMQ X Management API and CLI, active=true)
    Plugin(emqx_plugin_template, version=v4.0.0, description=EMQ X Plugin Template, active=false)
    Plugin(emqx_psk_file, version=v4.0.0, description=EMQX PSK Plugin from File, active=false)
    Plugin(emqx_recon, version=v4.0.0, description=EMQ X Recon Plugin, active=true)
    Plugin(emqx_reloader, version=v4.0.0, description=EMQ X Reloader Plugin, active=false)
    Plugin(emqx_retainer, version=v4.0.0, description=EMQ X Retainer, active=true)
    Plugin(emqx_rule_engine, version=v4.0.0, description=EMQ X Rule Engine, active=true)
    Plugin(emqx_sn, version=v4.0.0, description=EMQ X MQTT SN Plugin, active=false)
    Plugin(emqx_statsd, version=v4.0.0, description=Statsd for EMQ X, active=false)
    Plugin(emqx_stomp, version=v4.0.0, description=EMQ X Stomp Protocol Plugin, active=false)
    Plugin(emqx_web_hook, version=v4.0.0, description=EMQ X Webhook Plugin, active=false)

插件属性:

+-------------+-----------------+
| version     | 插件版本        |
+-------------+-----------------+
| description | 插件描述        |
+-------------+-----------------+
| active      | 是否已加载      |
+-------------+-----------------+

plugins load <Plugin>
---------------------

加载插件::

    $ ./bin/emqx_ctl plugins load emqx_lua_hook

    Plugin emqx_lua_hook loaded successfully.

plugins unload <Plugin>
-----------------------

卸载插件::

    $ ./bin/emqx_ctl plugins unload emqx_lua_hook

    Plugin emqx_lua_hook unloaded successfully.

plugins reload <Plugin>
-----------------------

重载插件::

    $ ./bin/emqx_ctl plugins reload emqx_lua_hook

    Plugin emqx_lua_hook reloaded successfully.

-------
vm 命令
-------

vm 命令用于查询 Erlang 虚拟机负载、内存、进程、IO 信息。

+-------------+-----------------------------+
| vm          | 等同于 vm all               |
+-------------+-----------------------------+
| vm all      | 查询 VM 全部信息            |
+-------------+-----------------------------+
| vm load     | 查询 VM 负载                |
+-------------+-----------------------------+
| vm memory   | 查询 VM 内存                |
+-------------+-----------------------------+
| vm process  | 查询 VM Erlang 进程数量     |
+-------------+-----------------------------+
| vm io       | 查询 VM io 最大文件句柄     |
+-------------+-----------------------------+
| vm ports    | 查询 VM 的端口              |
+-------------+-----------------------------+

vm all
------

查询 VM 全部信息，包括负载、内存、Erlang 进程数量等::

    cpu/load1               : 4.22
    cpu/load5               : 3.29
    cpu/load15              : 3.16
    memory/total            : 99995208
    memory/processes        : 38998248
    memory/processes_used   : 38938520
    memory/system           : 60996960
    memory/atom             : 1189073
    memory/atom_used        : 1173808
    memory/binary           : 100336
    memory/code             : 25439961
    memory/ets              : 7161128
    process/limit           : 2097152
    process/count           : 315
    io/max_fds              : 10240
    io/active_fds           : 0
    ports/count             : 18
    ports/limit             : 1048576

vm load
-------

查询 VM 负载::

    $ ./bin/emqx_ctl vm load

    cpu/load1               : 2.21
    cpu/load5               : 2.60
    cpu/load15              : 2.36

vm memory
---------

查询 VM 内存::

    $ ./bin/emqx_ctl vm memory

    memory/total            : 23967736
    memory/processes        : 3594216
    memory/processes_used   : 3593112
    memory/system           : 20373520
    memory/atom             : 512601
    memory/atom_used        : 491955
    memory/binary           : 51432
    memory/code             : 13401565
    memory/ets              : 1082848

vm process
----------

查询 Erlang 进程数量::

    $ ./bin/emqx_ctl vm process

    process/limit           : 2097152
    process/count           : 314

vm io
-----

查询 IO 最大句柄数::

    $ ./bin/emqx_ctl vm io

    io/max_fds              : 10240
    io/active_fds           : 0

vm ports
--------

查询 VM 的端口::

    $ ./bin/emqx_ctl vm ports

    ports/count           : 18
    ports/limit           : 1048576

-----------
mnesia 命令
-----------

查询 mnesia 数据库系统状态。

--------
log 命令
--------

log 命令用于设置日志等级。访问 `Documentation of logger <http://erlang.org/doc/apps/kernel/logger_chapter.html>`_ 以获取详细信息

+--------------------------------------------+----------------------------------------------------+
| log set-level <Level>                      | 设置主日志等级和所有 Handlers 日志等级             |
+--------------------------------------------+----------------------------------------------------+
| log primary-level                          | 查看主日志等级                                     |
+--------------------------------------------+----------------------------------------------------+
| log primary-lelvel <Level>                 | 设置主日志等级                                     |
+--------------------------------------------+----------------------------------------------------+
| log handlers list                          | 查看当前安装的所有 Hanlders                        |
+--------------------------------------------+----------------------------------------------------+
| log handlers set-level <HandlerId> <Level> | 设置指定 Hanlder 的日志等级                        |
+--------------------------------------------+----------------------------------------------------+

log set-level <Level>
---------------------

设置主日志等级和所有 Handlers 日志等级::

    $ ./bin/emqx_ctl log set-level debug

    debug

log primary-level
-----------------

查看主日志等级::

    $ ./bin/emqx_ctl log primary-level

    debug

log primary-level <Level>
--------------------------

设置主日志等级::

    $ ./bin/emqx_ctl log primary-level info

    info

log handlers list
-----------------

查看当前安装的所有 Hanlders::

    $ ./bin/emqx_ctl log handlers list

    LogHandler(id=emqx_logger_handler, level=debug, destination=unknown)
    LogHandler(id=file, level=debug, destination=log/emqx.log)
    LogHandler(id=default, level=debug, destination=console)

log handlers set-level <HandlerId> <Level>
------------------------------------------

设置指定 Hanlder 的日志等级::

    $ ./bin/emqx_ctl log handlers set-level emqx_logger_handler error

    error

----------
trace 命令
----------

trace 命令用于追踪某个客户端或 Topic，打印日志信息到文件。

+------------------------------------------------+------------------------------------------------+
| trace list                                     | 查询全部开启的追踪                             |
+------------------------------------------------+------------------------------------------------+
| trace start client <ClientId> <File> [<Level>] | 开启 Client 追踪，存储指定等级的日志到文件     |
+------------------------------------------------+------------------------------------------------+
| trace stop client <ClientId>                   | 关闭 Client 追踪                               |
+------------------------------------------------+------------------------------------------------+
| trace start topic <Topic> <File> [<Level>]     | 开启 Topic 追踪，存储指定等级的日志到文件      |
+------------------------------------------------+------------------------------------------------+
| trace stop topic <Topic>                       | 关闭 Topic 追踪                                |
+------------------------------------------------+------------------------------------------------+

.. note:: 使用 trace 之前，需要将主日志等级(primary logger level) 设置成足够低的值。为提高系统运行性能，默认的主日志等级是 error。

trace start client <ClientId> <File> [<Level>]
----------------------------------------------

开启 Client 追踪::

    $ ./bin/emqx_ctl log primary-level debug

    debug

    $ ./bin/emqx_ctl trace start client clientid log/clientid_trace.log

    trace clientid clientid successfully

    $ ./bin/emqx_ctl trace start client clientid2 log/clientid2_trace.log error

    trace clientid clientid2 successfully

trace stop client <ClientId>
----------------------------

关闭 Client 追踪::

    $ ./bin/emqx_ctl trace stop client clientid

    stop tracing clientid clientid successfully

trace start topic <Topic> <File> [<Level>]
------------------------------------------

开启 Topic 追踪::

    $ ./bin/emqx_ctl log primary-level debug

    debug

    $ ./bin/emqx_ctl trace start topic topic log/topic_trace.log

    trace topic topic successfully

    $ ./bin/emqx_ctl trace start topic topic2 log/topic2_trace.log error

    trace topic topic2 successfully

trace stop topic <Topic>
------------------------

关闭 Topic 追踪::

    $ ./bin/emqx_ctl trace topic topic off

    stop tracing topic topic successfully

trace list
----------

查询全部开启的追踪::

    $ ./bin/emqx_ctl trace list

    Trace(clientid=clientid2, level=error, destination="log/clientid2_trace.log")
    Trace(topic=topic2, level=error, destination="log/topic2_trace.log")

---------
listeners
---------

listeners 命令用于查询开启的 TCP 服务监听器

+-----------------------------------+-----------------------------------+
| listeners                         | 查询开启的 TCP 服务监听器         |
+-----------------------------------+-----------------------------------+
| listeners stop <Proto> <Port>     | 停止监听端口                      |
+-----------------------------------+-----------------------------------+

listeners list
--------------

查询开启的 TCP 服务监听器::

    $ ./bin/emqx_ctl listeners

    listener on mqtt:ssl:8883
      acceptors       : 16
      max_conns       : 102400
      current_conn    : 0
      shutdown_count  : []
    listener on mqtt:tcp:0.0.0.0:1883
      acceptors       : 8
      max_conns       : 1024000
      current_conn    : 0
      shutdown_count  : []
    listener on mqtt:tcp:127.0.0.1:11883
      acceptors       : 4
      max_conns       : 1024000
      current_conn    : 2
      shutdown_count  : []
    listener on http:dashboard:18083
      acceptors       : 2
      max_conns       : 512
      current_conn    : 0
      shutdown_count  : []
    listener on http:management:8081
      acceptors       : 2
      max_conns       : 512
      current_conn    : 0
      shutdown_count  : []
    listener on mqtt:ws:8083
      acceptors       : 2
      max_conns       : 102400
      current_conn    : 0
      shutdown_count  : []
    listener on mqtt:wss:8084
      acceptors       : 2
      max_conns       : 16
      current_conn    : 0
      shutdown_count  : []

listener 参数说明:

+-----------------+-----------------------------------+
| acceptors       | TCP Acceptor 池                   |
+-----------------+-----------------------------------+
| max_conns       | 最大允许连接数                    |
+-----------------+-----------------------------------+
| current_conns   | 当前连接数                        |
+-----------------+-----------------------------------+
| shutdown_count  | Socket 关闭原因统计               |
+-----------------+-----------------------------------+

listeners stop <Proto> <Port>
------------------------------

停止监听端口::

    $ ./bin/emqx_ctl listeners stop mqtt:tcp 0.0.0.0:1883

    Stop mqtt:tcp listener on 0.0.0.0:1883 successfully.

----------------------------
规则引擎(rule engine) 命令
----------------------------

----------
rules 命令
----------

+-----------------------------------------------------------+----------------+
| rules list                                                | List all rules |
+-----------------------------------------------------------+----------------+
| rules show <RuleId>                                       | Show a rule    |
+-----------------------------------------------------------+----------------+
| rules create <name> <hook> <sql> <actions> [-d [<descr>]] | Create a rule  |
+-----------------------------------------------------------+----------------+
| rules delete <RuleId>                                     | Delete a rule  |
+-----------------------------------------------------------+----------------+

rules create
------------

创建一个新的规则::

    ## 创建一个测试规则，简单打印所有发送到 't/a' 主题的消息内容
    $ ./bin/emqx_ctl rules create \
      'test1' \
      'message.publish' \
      'select * from "t/a"' \
      '[{"name":"built_in:inspect_action", "params": {"a": 1}}]' \
      -d 'Rule for debug'

    Rule test1:1556242324634254201 created

.. note:: 一个规则由系统生成的规则 ID 标识，所以如果用相同的名字重复添加规则，会生成多个 ID 不同的规则。

rules list
----------

列出当前所有的规则::

    $ ./bin/emqx_ctl rules list

    rule(id='test1:1556242324634254201', name='test1', for='message.publish', rawsql='select * from "t/a"', actions=[{"name":"built_in:inspect_action","params":{"a":1}}], enabled='true', description='Rule for debug')

rules show
----------

查询规则::

    ## 查询 RuleID 为 'test1:1556242324634254201' 的规则
    $ ./bin/emqx_ctl rules show 'test1:1556242324634254201'

    rule(id='test1:1556242324634254201', name='test1', for='message.publish', rawsql='select * from "t/a"', actions=[{"name":"built_in:inspect_action","params":{"a":1}}], enabled='true', description='Rule for debug')

rules delete
------------

删除规则::

    ## 删除 RuleID 为 'test1:1556242324634254201' 的规则
    $ ./bin/emqx_ctl rules delete 'test1:1556242324634254201'

    ok

------------------
rule-actions 命令
------------------

+-----------------------------------------------+--------------------+
| rule-actions list [-t [<type>]] [-k [<hook>]] | List all actions   |
+-----------------------------------------------+--------------------+
| rule-actions show <ActionId>                  | Show a rule action |
+-----------------------------------------------+--------------------+

.. note:: 动作可以由 emqx 内置(称为系统内置动作)，或者由 emqx 插件编写，但不能通过 CLI/API 添加或删除。

rule-actions show
-----------------

查询动作::

    ## 查询名为 'built_in:inspect_action' 动作
    $ ./bin/emqx_ctl rule-actions show 'built_in:inspect_action'

    action(name='built_in:inspect_action', app='emqx_rule_engine', for='$any', type='built_in', params=#{}, description='Inspect the details of action params for debug purpose')

rule-actions list
-----------------

列出符合条件的动作::

    ## 列出当前所有的动作
    $ ./bin/emqx_ctl rule-actions list

    action(name='built_in:republish_action', app='emqx_rule_engine', for='message.publish', type='built_in', params=#{target_topic => #{description => <<"Repubilsh the message to which topic">>,format => topic,required => true,title => <<"To Which Topic">>,type => string}}, description='Republish a MQTT message to a another topic')
    action(name='web_hook:event_action', app='emqx_web_hook', for='$events', type='web_hook', params=#{'$resource' => #{description => <<"Bind a resource to this action">>,required => true,title => <<"Resource ID">>,type => string},template => #{description => <<"The payload template to be filled with variables before sending messages">>,required => false,schema => #{},title => <<"Payload Template">>,type => object}}, description='Forward Events to Web Server')
    action(name='web_hook:publish_action', app='emqx_web_hook', for='message.publish', type='web_hook', params=#{'$resource' => #{description => <<"Bind a resource to this action">>,required => true,title => <<"Resource ID">>,type => string}}, description='Forward Messages to Web Server')
    action(name='built_in:inspect_action', app='emqx_rule_engine', for='$any', type='built_in', params=#{}, description='Inspect the details of action params for debug purpose')

    ## 列出所有资源类型为 web_hook 的动作
    $ ./bin/emqx_ctl rule-actions list -t web_hook

    action(name='web_hook:event_action', app='emqx_web_hook', for='$events', type='web_hook', params=#{'$resource' => #{description => <<"Bind a resource to this action">>,required => true,title => <<"Resource ID">>,type => string},template => #{description => <<"The payload template to be filled with variables before sending messages">>,required => false,schema => #{},title => <<"Payload Template">>,type => object}}, description='Forward Events to Web Server')
    action(name='web_hook:publish_action', app='emqx_web_hook', for='message.publish', type='web_hook', params=#{'$resource' => #{description => <<"Bind a resource to this action">>,required => true,title => <<"Resource ID">>,type => string}}, description='Forward Messages to Web Server')

    ## 列出所有 Hook 类型匹配 'client.connected' 的动作
    $ ./bin/emqx_ctl rule-actions list -k 'client.connected'

    action(name='built_in:inspect_action', app='emqx_rule_engine', for='$any', type='built_in', params=#{}, description='Inspect the details of action params for debug purpose')

----------------
resources 命令
----------------

+------------------------------------------------------------------------+--------------------+
| emqx_ctl resources create <name> <type> [-c [<config>]] [-d [<descr>]] | Create a resource  |
+------------------------------------------------------------------------+--------------------+
| resources list [-t <ResourceType>]                                     | List all resources |
+------------------------------------------------------------------------+--------------------+
| resources show <ResourceId>                                            | Show a resource    |
+------------------------------------------------------------------------+--------------------+
| resources delete <ResourceId>                                          | Delete a resource  |
+------------------------------------------------------------------------+--------------------+

resources create
----------------
创建一个新的资源::

    $ ./bin/emqx_ctl resources create 'webhook1' 'web_hook' -c '{"url": "http://host-name/chats"}' -d 'forward msgs to host-name/chats'

    Resource web_hook:webhook1 created

resources list
--------------

列出当前所有的资源::

    $ ./bin/emqx_ctl resources list

    resource(id='web_hook:webhook1', name='webhook1', type='web_hook', config=#{<<"url">> => <<"http://host-name/chats">>}, attrs=undefined, description='forward msgs to host-name/chats')

resources list by type
----------------------

列出当前所有的资源::

    $ ./bin/emqx_ctl resources list --type 'debug_resource_type'

    resource(id='web_hook:webhook1', name='webhook1', type='web_hook', config=#{<<"url">> => <<"http://host-name/chats">>}, attrs=undefined, description='forward msgs to host-name/chats')

resources show
--------------

查询资源::

    $ ./bin/emqx_ctl resources show 'web_hook:webhook1'

    resource(id='web_hook:webhook1', name='webhook1', type='web_hook', config=#{<<"url">> => <<"http://host-name/chats">>}, attrs=undefined, description='forward msgs to host-name/chats')

resources delete
----------------

删除资源::

    $ ./bin/emqx_ctl resources delete 'web_hook:webhook1'

    ok

-------------------
resource-types 命令
-------------------

+----------------------------+-------------------------+
| resource-types list        | List all resource-types |
+----------------------------+-------------------------+
| resource-types show <Type> | Show a resource-type    |
+----------------------------+-------------------------+

.. note:: 资源类型可以由 emqx 内置(称为系统内置资源类型)，或者由 emqx 插件编写，但不能通过 CLI/API 添加或删除。

resource-types list
-------------------

列出当前所有的资源类型::

    ./bin/emqx_ctl resource-types list

    resource_type(name='built_in', provider='emqx_rule_engine', params=#{}, on_create={emqx_rule_actions,on_resource_create}, description='The built in resource type for debug purpose')
    resource_type(name='web_hook', provider='emqx_web_hook', params=#{headers => #{default => #{},description => <<"Request Header">>,schema => #{},title => <<"Request Header">>,type => object},method => #{default => <<"POST">>,description => <<"Request Method">>,enum => [<<"PUT">>,<<"POST">>],title => <<"Request Method">>,type => string},url => #{description => <<"Request URL">>,format => url,required => true,title => <<"Request URL">>,type => string}}, on_create={emqx_web_hook_actions,on_resource_create}, description='WebHook Resource')

resource-types show
-------------------

查询资源类型::

    $ ./bin/emqx_ctl resource-types show built_in

    resource_type(name='built_in', provider='emqx_rule_engine', params=#{}, on_create={emqx_rule_actions,on_resource_create}, description='The built in resource type for debug purpose')

----------
recon 命令
----------

+-----------------------+--------------------------------------------------+
| recon memory          | recon_alloc:memory/2                             |
+-----------------------+--------------------------------------------------+
| recon allocated       | recon_alloc:memory(allocated_types, current/max) |
+-----------------------+--------------------------------------------------+
| recon bin_leak        | recon:bin_leak(100)                              |
+-----------------------+--------------------------------------------------+
| recon node_stats      | recon:node_stats(10, 1000)                       |
+-----------------------+--------------------------------------------------+
| recon remote_load Mod | recon:remote_load(Mod)                           |
+-----------------------+--------------------------------------------------+

访问 `Documentation for recon <http://ferd.github.io/recon/>`_ 以获取详细信息。

recon memory
------------

recon_alloc:memory/2::

    $ ./bin/emqx_ctl recon memory

    usage/current       : 0.810331960305788
    usage/max           : 0.7992495929358717
    used/current        : 84922296
    used/max            : 122519208
    allocated/current   : 104345600
    allocated/max       : 153292800
    unused/current      : 19631520
    unused/max          : 30773592

recon allocated
---------------

recon_alloc:memory(allocated_types, current/max)::

    $ ./bin/emqx_ctl recon allocated

    binary_alloc/current: 425984
    driver_alloc/current: 425984
    eheap_alloc/current : 4063232
    ets_alloc/current   : 3833856
    fix_alloc/current   : 1474560
    ll_alloc/current    : 90439680
    sl_alloc/current    : 163840
    std_alloc/current   : 2260992
    temp_alloc/current  : 655360
    binary_alloc/max    : 4907008
    driver_alloc/max    : 425984
    eheap_alloc/max     : 25538560
    ets_alloc/max       : 5931008
    fix_alloc/max       : 1736704
    ll_alloc/max        : 90439680
    sl_alloc/max        : 20348928
    std_alloc/max       : 2260992
    temp_alloc/max      : 1703936

recon bin_leak
--------------

recon:bin_leak(100)::

    $ ./bin/emqx_ctl recon bin_leak

    {<10623.1352.0>,-3,
     [cowboy_clock,
      {current_function,{gen_server,loop,7}},
      {initial_call,{proc_lib,init_p,5}}]}
    {<10623.3865.0>,0,
     [{current_function,{recon_lib,proc_attrs,2}},
      {initial_call,{erlang,apply,2}}]}
    {<10623.3863.0>,0,
     [{current_function,{dist_util,con_loop,2}},
      {initial_call,{inet_tcp_dist,do_accept,7}}]}
      ...

recon node_stats
----------------

recon:node_stats(10, 1000)::

    $ ./bin/emqx_ctl recon node_stats

    {[{process_count,302},
      {run_queue,0},
      {memory_total,88925536},
      {memory_procs,27999296},
      {memory_atoms,1182843},
      {memory_bin,24536},
      {memory_ets,7163216}],
     [{bytes_in,62},
      {bytes_out,458},
      {gc_count,4},
      {gc_words_reclaimed,3803},
      {reductions,3036},
      {scheduler_usage,[{1,9.473889959272245e-4},
                        {2,5.085983030767205e-5},
                        {3,5.3851477624711046e-5},
                        {4,7.579021269127057e-5},
                        {5,0.0},
                        {6,0.0},
                        {7,0.0},
                        {8,0.0}]}]}
    ...

recon remote_load Mod
---------------------

recon:remote_load(Mod)::

    $ ./bin/emqx_ctl recon remote_load

--------------
retainer 命令
--------------

+-----------------+------------------------+
| retainer info   | 显示保留消息的数量     |
+-----------------+------------------------+
| retainer topics | 显示保留消息的所有主题 |
+-----------------+------------------------+
| retainer clean  | 清除所有保留的消息     |
+-----------------+------------------------+

retainer info
-------------

显示保留消息的数量::

    $ ./bin/emqx_ctl retainer info

    retained/total: 3

retainer topics
---------------

显示保留消息的所有主题::

    $ ./bin/emqx_ctl retainer topics

    $SYS/brokers/emqx@127.0.0.1/version
    $SYS/brokers/emqx@127.0.0.1/sysdescr
    $SYS/brokers

retainer clean
--------------

清除所有保留的消息::

    $ ./bin/emqx_ctl retainer clean

    Cleaned 3 retained messages

-----------
admins 命令
-----------

Dashboard 插件会自动注册 admins 命令，用于创建、删除管理员账号，重置管理员密码。

+------------------------------------------+-----------------------------+
| admins add <Username> <Password> <Tags>  | 创建 admin 账号             |
+------------------------------------------+-----------------------------+
| admins passwd <Username> <Password>      | 重置 admin 密码             |
+------------------------------------------+-----------------------------+
| admins del <Username>                    | 删除 admin 账号             |
+------------------------------------------+-----------------------------+

admins add <Username> <Password> <Tags>
---------------------------------------

创建 admin 账户::

    $ ./bin/emqx_ctl admins add root public test

    ok

admins passwd <Username> <Password>
------------------------------------

重置 admin 账户密码::

    $ ./bin/emqx_ctl admins passwd root private

    ok

admins del <Username>
---------------------

删除 admin 账户::

    $ ./bin/emqx_ctl admins del root

    ok

------------
luahook 命令
------------

+--------------------------+------------------------------------------------------------------------------+
| luahook load <Script>    | 加载 lua 脚本                                                                |
+--------------------------+------------------------------------------------------------------------------+
| luahook unload <Script>  | 卸载 lua 脚本                                                                |
+--------------------------+------------------------------------------------------------------------------+
| luahook reload <Script>  | 重新加载 lua 脚本                                                            |
+--------------------------+------------------------------------------------------------------------------+
| luahook enable <Script>  | 将名为 <Script>.x 的 lua 脚本重命名为 <Script> 并加载                        |
+--------------------------+------------------------------------------------------------------------------+
| luahook disable <Script> | 卸载名为 <Script> 的 lua 脚本并重命名为 <Script>.x，以避免下次启动时自动加载 |
+--------------------------+------------------------------------------------------------------------------+

luahook load <Script>
----------------------

加载 lua 脚本::

    $ ./bin/emqx_ctl luahook load test.lua

    Load "test.lua" successfully

luahook unload <Script>
------------------------

卸载 lua 脚本::

    $ ./bin/emqx_ctl luahook unload test.lua

    Unload "test.lua" successfully

luahook reload <Script>
------------------------

重新加载 lua 脚本::

    $ ./bin/emqx_ctl luahook reload test.lua

    Reload "test.lua" successfully

luahook enable <Script>
------------------------

将名为 <Script>.x 的 lua 脚本重命名为 <Script> 并加载::

    $ ./bin/emqx_ctl luahook enable test.lua

    Enable "test.lua" successfully

luahook disable <Script>
------------------------

卸载名为 <Script> 的 lua 脚本并重命名为 <Script>.x，以避免下次启动时自动加载::

    $ ./bin/emqx_ctl luahook disable test.lua

    Disable "test.lua" successfully
