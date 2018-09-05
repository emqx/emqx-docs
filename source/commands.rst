
.. _commands:

===================
管理命令 (Commands)
===================

EMQ 消息服务器提供了 './bin/emqx_ctl' 的管理命令行。

-----------
status 命令
-----------

查询 EMQ 消息服务器运行状态::

    $ ./bin/emqx_ctl status

    Node 'emqx@127.0.0.1' is started
    emqx 2.0 is running

---------
mgmt 命令
---------

mgmt 命令查询应用程序。

+------------------------------+----------------------------+
| mgmt list                    | 列出应用程序列表           |
+------------------------------+----------------------------+
| mgmt insert <AppId> <Name>   | 添加REST API的应用程序     |
+------------------------------+----------------------------+
| mgmt update <AppId> <status> | 更新REST API的应用程序     |
+------------------------------+----------------------------+
| mgmt lookup <AppId>          | 获取REST API的应用程序详情 |
+------------------------------+----------------------------+
| mgmt delete <AppId>          | 删除REST API的应用程序     |
+------------------------------+----------------------------+

mgmt list  
---------

列出应用程序列表::

    $ ./bin/emqx_ctl mgmt list
    app_id: 901abdba8eb8c, secret: MjgzMzQ5MjM1MzUzMTc4MjgyMjE3NzU4ODcwMDg0NjQ4OTG, name: hello, desc: , status: true, expired: undefined

mgmt insert <AppId> <Name>
------------

添加REST API的应用程序::
    
    $ ./bin/emqx_ctl mgmt insert dbcb6e023370b world
    AppSecret: MjgzMzQ5MjYyMTY3ODk4MjA5NzMwODExODMxMDM1NDk0NDA

mgmt update <AppId> <status>
------------

更新REST API的应用程序::
    
    $ ./bin/emqx_ctl mgmt update dbcb6e023370b stop
    update successfully.

mgmt lookup <AppId>
------------

获取REST API的应用程序详情::
    
    $ ./bin/emqx_ctl mgmt lookup dbcb6e023370b
    app_id: dbcb6e023370b
    secret: MjgzMzQ5MjYyMTY3ODk4MjA5NzMwODExODMxMDM1NDk0NDA
    name: world
    desc: Application user
    status: stop
    expired: undefined

mgmt delete <AppId>
------------

删除REST API的应用程序::
    
    $ ./bin/emqx_ctl mgmt delete dbcb6e023370b
    ok


-----------
broker 命令
-----------

broker 命令查询服务器基本信息，启动时间，统计数据与性能数据。

+----------------+-----------------------------------------------+
| broker         | 查询 EMQ 消息服务器描述、版本、启动时间       |
+----------------+-----------------------------------------------+
| broker pubsub  | 查询核心的 Erlang PubSub 进程状态(调试)       |
+----------------+-----------------------------------------------+
| broker stats   | 查询连接(Client)、会话(Session)、主题(Topic)、|
|                | 订阅(Subscription)、路由(Route)统计信息       |
+----------------+-----------------------------------------------+
| broker metrics | 查询 MQTT 报文(Packet)、消息(Message)收发统计 |
+----------------+-----------------------------------------------+

查询 EMQ 消息服务器基本信息包括版本、启动时间等::

    $ ./bin/emqx_ctl broker

    sysdescr  : Erlang MQTT Broker
    version   : 2.0
    uptime    : 25 seconds
    datetime  : 2016-10-18 10:42:10

broker stats
------------

查询服务器客户端连接(Client)、会话(Session)、主题(Topic)、订阅(Subscription)、路由(Route)统计::

    $ ./bin/emqx_ctl broker stats

    clients/count       : 1
    clients/max         : 1
    queues/count        : 0
    queues/max          : 0
    retained/count      : 2
    retained/max        : 2
    routes/count        : 2
    routes/reverse      : 2
    sessions/count      : 0
    sessions/max        : 0
    subscriptions/count : 1
    subscriptions/max   : 1
    topics/count        : 54
    topics/max          : 54

broker metrics
--------------

查询服务器流量(Bytes)、MQTT报文(Packets)、消息(Messages)收发统计::

    $ ./bin/emqx_ctl broker metrics

    bytes/received          : 297
    bytes/sent              : 40
    messages/dropped        : 348
    messages/qos0/received  : 0
    messages/qos0/sent      : 0
    messages/qos1/received  : 0
    messages/qos1/sent      : 0
    messages/qos2/received  : 0
    messages/qos2/sent      : 0
    messages/received       : 0
    messages/retained       : 2
    messages/sent           : 0
    packets/connack         : 5
    packets/connect         : 5
    packets/disconnect      : 0
    packets/pingreq         : 0
    packets/pingresp        : 0
    packets/puback/received : 0
    packets/puback/sent     : 0
    packets/pubcomp/received: 0
    packets/pubcomp/sent    : 0
    packets/publish/received: 0
    packets/publish/sent    : 0
    packets/pubrec/received : 0
    packets/pubrec/sent     : 0
    packets/pubrel/received : 0
    packets/pubrel/sent     : 0
    packets/received        : 9
    packets/sent            : 9
    packets/suback          : 4
    packets/subscribe       : 4
    packets/unsuback        : 0
    packets/unsubscribe     : 0

------------
cluster 命令
------------

cluster 命令集群多个 EMQ 消息服务器节点(进程):

+-----------------------+---------------------+
| cluster join <Node>   | 加入集群            |
+-----------------------+---------------------+
| cluster leave         | 离开集群            |
+-----------------------+---------------------+
| cluster force-leave <Node> | 从集群删除节点      |
+-----------------------+---------------------+
| cluster status        | 查询集群状态        |
+-----------------------+---------------------+

cluster 命令集群本机两个 EMQ 节点示例:

+-----------+---------------------+-------------+
| 目录      | 节点名              | MQTT 端口   |
+-----------+---------------------+-------------+
| emqx1   | emqx1@127.0.0.1   | 1883        |
+-----------+---------------------+-------------+
| emqx2   | emqx2@127.0.0.1   | 2883        |
+-----------+---------------------+-------------+

启动 emqx1 ::

    cd emqx1 && ./bin/emqx start

启动 emqx2 ::

    cd emqx2 && ./bin/emqx start

emqx2 节点与 emqx1 集群，emqx2 目录下::

    $ ./bin/emqx_ctl cluster join emqx1@127.0.0.1

    Join the cluster successfully.
    Cluster status: [{running_nodes,['emqx1@127.0.0.1','emqx2@127.0.0.1']}]

任意节点目录下查询集群状态::

    $ ./bin/emqx_ctl cluster status

    Cluster status: [{running_nodes,['emqx2@127.0.0.1','emqx1@127.0.0.1']}]

集群消息路由测试::

    # emqx1节点上订阅x
    mosquitto_sub -t x -q 1 -p 1883

    # emqx2节点上向x发布消息
    mosquitto_pub -t x -q 1 -p 2883 -m hello

emqx2 节点离开集群::

    cd emqx2 && ./bin/emqx_ctl cluster leave

emqx1 节点下删除 emqx2::

    cd emqx1 && ./bin/emqx_ctl cluster force-leave emqx2@127.0.0.1

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

    Client(mosqsub/43832-airlee.lo, clean_sess=true, username=test, peername=127.0.0.1:64896, connected_at=1452929113)
    Client(mosqsub/44011-airlee.lo, clean_sess=true, username=test, peername=127.0.0.1:64961, connected_at=1452929275)
    ...

返回 Client 对象的属性:

+--------------+-----------------------------+
| clean_sess   | 清除会话标记                |
+--------------+-----------------------------+
| username     | 用户名                      |
+--------------+-----------------------------+
| peername     | 对端 TCP 地址               |
+--------------+-----------------------------+
| connected_at | 客户端连接时间              |
+--------------+-----------------------------+

clients show <ClientId>
-----------------------

根据 ClientId 查询客户端::

    ./bin/emqx_ctl clients show "mosqsub/43832-airlee.lo"

    Client(mosqsub/43832-airlee.lo, clean_sess=true, username=test, peername=127.0.0.1:64896, connected_at=1452929113)

clients kick <ClientId>
-----------------------

根据 ClientId 踢出客户端::

    ./bin/emqx_ctl clients kick "clientid"

.. _command_sessions::

-------------
sessions 命令
-------------

sessions 命令查询 MQTT 连接会话。EMQ 消息服务器会为每个连接创建会话，clean_session 标记 true，创建临时(transient)会话；clean_session 标记为 false，创建持久会话(persistent)。

+--------------------------+-----------------------------+
| sessions list            | 查询全部会话                |
+--------------------------+-----------------------------+
| sessions list persistent | 查询全部持久会话            |
+--------------------------+-----------------------------+
| sessions list transient  | 查询全部临时会话            |
+--------------------------+-----------------------------+
| sessions show <ClientId> | 根据 ClientID 查询会话      |
+--------------------------+-----------------------------+

sessions list
-------------

查询全部会话::

    $ ./bin/emqx_ctl sessions list

    Session(clientid, clean_sess=false, max_inflight=100, inflight_queue=0, message_queue=0, message_dropped=0, awaiting_rel=0, awaiting_ack=0, awaiting_comp=0, created_at=1452935508)
    Session(mosqsub/44101-airlee.lo, clean_sess=true, max_inflight=100, inflight_queue=0, message_queue=0, message_dropped=0, awaiting_rel=0, awaiting_ack=0, awaiting_comp=0, created_at=1452935401)

返回 Session 对象属性:

+-------------------+----------------------------------------+
| clean_sess        | false: 持久会话，true: 临时会话        |
+-------------------+----------------------------------------+
| max_inflight      | 飞行窗口(最大允许同时下发消息数)       |
+-------------------+----------------------------------------+
| inflight_queue    | 当前正在下发的消息数                   |
+-------------------+----------------------------------------+
| message_queue     | 当前缓存消息数                         |
+-------------------+----------------------------------------+
| message_dropped   | 会话丢掉的消息数                       |
+-------------------+----------------------------------------+
| awaiting_rel      | 等待客户端发送 PUBREL 的 QoS2 消息数   |
+-------------------+----------------------------------------+
| awaiting_ack      | 等待客户端响应 PUBACK 的 QoS1/2 消息数 |
+-------------------+----------------------------------------+
| awaiting_comp     | 等待客户端响应 PUBCOMP 的 QoS2 消息数  |
+-------------------+----------------------------------------+
| created_at        | 会话创建时间戳                         |
+-------------------+----------------------------------------+

sessions list persistent
------------------------

查询全部持久会话::

    $ ./bin/emqx_ctl sessions list persistent

    Session(clientid, clean_sess=false, max_inflight=100, inflight_queue=0, message_queue=0, message_dropped=0, awaiting_rel=0, awaiting_ack=0, awaiting_comp=0, created_at=1452935508)

sessions list transient
-----------------------

查询全部临时会话::

    $ ./bin/emqx_ctl sessions list transient

    Session(mosqsub/44101-airlee.lo, clean_sess=true, max_inflight=100, inflight_queue=0, message_queue=0, message_dropped=0, awaiting_rel=0, awaiting_ack=0, awaiting_comp=0, created_at=1452935401)

sessions show <ClientId>
------------------------

根据 ClientId 查询会话::

    $ ./bin/emqx_ctl sessions show clientid

    Session(clientid, clean_sess=false, max_inflight=100, inflight_queue=0, message_queue=0, message_dropped=0, awaiting_rel=0, awaiting_ack=0, awaiting_comp=0, created_at=1452935508)

-----------
routes 命令
-----------

routes 命令查询路由表。

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
| subscriptions add <ClientId> <Topic> <QoS> | 手动添加静态订阅          |
+--------------------------------------------+--------------------------+
| subscriptions del <ClientId> <Topic>       | 手动删除静态订阅          |
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
-----------------------------

手动添加静态订阅::

    $ ./bin/emqx_ctl subscriptions add 'mosqsub/90475-airlee.lo' 't/y:1' 0

subscriptions del <ClientId> <Topic> 
-----------------------------

手动删除静态订阅::

    $ ./bin/emqx_ctl subscriptions del 'mosqsub/90475-airlee.lo' 't/+/x:2'

    ok


------------
plugins 命令
------------

plugins 命令用于加载、卸载、查询插件应用。EMQ 消息服务器通过插件扩展认证、定制功能，插件置于 plugins/ 目录下。

+---------------------------+-------------------------+
| plugins list              | 列出全部插件(Plugin)    |
+---------------------------+-------------------------+
| plugins load <Plugin>     | 加载插件(Plugin)        |
+---------------------------+-------------------------+
| plugins unload <Plugin>   | 卸载插件(Plugin)        |
+---------------------------+-------------------------+

plugins list
------------

列出全部插件::

    $ ./bin/emqx_ctl plugins list

    Plugin(emqx_dashboard, version=0.16.0, description=emqx web dashboard, active=true)
    Plugin(emqx_plugin_mysql, version=0.16.0, description=emqx Authentication/ACL with MySQL, active=false)
    Plugin(emqx_plugin_pgsql, version=0.16.0, description=emqx PostgreSQL Plugin, active=false)
    Plugin(emqx_plugin_redis, version=0.16.0, description=emqx Redis Plugin, active=false)
    Plugin(emqx_plugin_template, version=0.16.0, description=emqx plugin template, active=false)
    Plugin(emqx_recon, version=0.16.0, description=emqx recon plugin, active=false)
    Plugin(emqx_stomp, version=0.16.0, description=Stomp Protocol Plugin for emqx broker, active=false)

插件属性:

+-------------+-----------------+
| version     | 插件版本        |
+-------------+-----------------+
| description | 插件描述        |
+-------------+-----------------+
| active      | 是否已加载      |
+-------------+-----------------+

plugins load <Plugin>
-------------

加载插件::

    $ ./bin/emqx_ctl plugins load emq_recon

    Start apps: [recon,emq_recon]
    Plugin emqx_recon loaded successfully.

plugins unload <Plugin>
---------------

卸载插件::

    $ ./bin/emqx_ctl plugins unload emq_recon

    Plugin emq_recon unloaded successfully.

------------
bridges 命令
------------

bridges 命令用于在多台 EMQ 服务器节点间创建桥接::

                  ---------                     ---------
    Publisher --> | node1 | --Bridge Forward--> | node2 | --> Subscriber
                  ---------                     ---------

+--------------------------+---------------------+
| bridges list             | 查询全部桥接        |
+--------------------------+---------------------+
| bridges start <Name>     | 开启一个桥接        |
+--------------------------+---------------------+
| bridges stop <Name>      | 停止一个桥接        |
+--------------------------+---------------------+

关于 bridges 的配置项在 emqx/emqx.config文件内。

bridges list
-------------

查询全部桥接::

    $ ./bin/emqx_ctl bridges list
    name: edge     status: Stopped
    name: cloud     status: Stopped

bridges start <Name> 
---------------------------

开启一个桥接::

    $ ./bin/emqx_ctl bridges start edge
    start bridge successfully.

bridges stop <Name> 
---------------------------

停止一个桥接::

    $ ./bin/emqx_ctl bridges stop edge
    stop bridge successfully.

-------
vm 命令
-------

vm 命令用于查询 Erlang 虚拟机负载、内存、进程、IO 信息。

+-------------+-------------------------+
| vm all      | 查询 VM 全部信息        |
+-------------+-------------------------+
| vm load     | 查询 VM 负载            |
+-------------+-------------------------+
| vm memory   | 查询 VM 内存            |
+-------------+-------------------------+
| vm process  | 查询 VM Erlang 进程数量 |
+-------------+-------------------------+
| vm io       | 查询 VM io 最大文件句柄 |
+-------------+-------------------------+
| vm ports       | 查询 VM 的端口 |
+-------------+-------------------------+

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

    process/limit           : 8192
    process/count           : 221

vm io
-----

查询 IO 最大句柄数::

    $ ./bin/emqx_ctl vm io

    io/max_fds              : 2560
    io/active_fds           : 1

vm ports
-----

查询 VM 的端口::

    $ ./bin/emqx_ctl vm ports

    ports/count           : 19
    ports/limit           : 262144

----------
trace 命令
----------

trace 命令用于追踪某个客户端或 Topic，打印日志信息到文件。

+-----------------------------------+-----------------------------------+
| trace list                        | 查询全部开启的追踪                |
+-----------------------------------+-----------------------------------+
| trace client <ClientId> <LogFile> | 开启 Client 追踪，日志到文件      |
+-----------------------------------+-----------------------------------+
| trace client <ClientId> off       | 关闭 Client 追踪                  |
+-----------------------------------+-----------------------------------+
| trace topic <Topic> <LogFile>     | 开启 Topic 追踪，日志到文件       |
+-----------------------------------+-----------------------------------+
| trace topic <Topic> off           | 关闭 Topic 追踪                   |
+-----------------------------------+-----------------------------------+

trace client <ClientId> <LogFile>
---------------------------------

开启 Client 追踪::

    $ ./bin/emqx_ctl trace client clientid log/clientid_trace.log

    trace client clientid successfully.


trace client <ClientId> off
---------------------------

关闭 Client 追踪::

    $ ./bin/emqx_ctl trace client clientid off

    stop to trace client clientid successfully.

trace topic <Topic> <LogFile>
-----------------------------

开启 Topic 追踪::

    $ ./bin/emqx_ctl trace topic topic log/topic_trace.log

    trace topic topic successfully.

trace topic <Topic> off
-----------------------

关闭 Topic 追踪::

    $ ./bin/emqx_ctl trace topic topic off

    stop to trace topic topic successfully.

trace list
----------

查询全部开启的追踪::

    $ ./bin/emqx_ctl trace list

    trace client clientid -> log/clientid_trace.log
    trace topic topic -> log/topic_trace.log

---------
listeners
---------

listeners 命令用于查询开启的 TCP 服务监听器

+-----------------------------------+-----------------------------------+
| listeners                         | 查询开启的 TCP 服务监听器            |
+-----------------------------------+-----------------------------------+
| listeners stop <Proto> <Port>     | 停止监听端口                        |
+-----------------------------------+-----------------------------------+

listeners list  
----------

查询开启的 TCP 服务监听器::

    $ ./bin/emqx_ctl listeners

    listener on mqtt:api:127.0.0.1:8080
      acceptors       : 4
      max_clients     : 64
      current_clients : 0
      shutdown_count  : []
    listener on mqtt:wss:8084
      acceptors       : 4
      max_clients     : 64
      current_clients : 0
      shutdown_count  : []
    listener on mqtt:ssl:8883
      acceptors       : 16
      max_clients     : 1024
      current_clients : 0
      shutdown_count  : []
    listener on mqtt:ws:8083
      acceptors       : 4
      max_clients     : 64
      current_clients : 0
      shutdown_count  : []
    listener on mqtt:tcp:0.0.0.0:1883
      acceptors       : 16
      max_clients     : 102400
      current_clients : 0
      shutdown_count  : []
    listener on mqtt:tcp:127.0.0.1:11883
      acceptors       : 16
      max_clients     : 102400
      current_clients : 0
      shutdown_count  : []
    listener on dashboard:http:18083
      acceptors       : 2
      max_clients     : 512
      current_clients : 0
      shutdown_count  : []

listener 参数说明:

+-----------------+-----------------------------------+
| acceptors       | TCP Acceptor 池                   |
+-----------------+-----------------------------------+
| max_clients     | 最大允许连接数                    |
+-----------------+-----------------------------------+
| current_clients | 当前连接数                        |
+-----------------+-----------------------------------+
| shutdown_count  | Socket 关闭原因统计               |
+-----------------+-----------------------------------+



listeners stop <Proto> <Port>
-----------

停止监听端口::

    $ ./bin/emqx_ctl listeners stop mqtt:tcp 0.0.0.0:1883
    Stop mqtt:tcp listener on 0.0.0.0:1883 successfully.

-----------
mnesia 命令
-----------

查询 mnesia 数据库系统状态。

-----------
retainer 命令
-----------

+------------------------------------+-----------------------------+
| retainer info                       | 显示保留消息的数量            |
+------------------------------------+-----------------------------+
| retainer topics                     | 显示保留消息的所有主题         |
+------------------------------------+-----------------------------+
| retainer clean                      | 清除所有保留的消息            |
+------------------------------------+-----------------------------+

retainer info 
----------

显示保留消息的数量：：

    $ ./bin/emqx_ctl retainer info
    retained/total: 3

retainer topics
-------------

显示保留消息的所有主题::

    $ ./bin/emqx_ctl retainer topics
    $SYS/brokers/emqx@127.0.0.1/version
    $SYS/brokers/emqx@127.0.0.1/sysdescr
    $SYS/brokers

retainer clean
----------

清除所有保留的消息::

    $ ./bin/emqx_ctl retainer clean
    Cleaned 3 retained messages



-----------
admins 命令
-----------

Dashboard 插件会自动注册 admins 命令，用于创建、删除管理员账号，重置管理员密码。

+------------------------------------+-----------------------------+
| admins add <Username> <Password> <Tags>  | 创建 admin 账号             |
+------------------------------------+-----------------------------+
| admins passwd <Username> <Password>| 重置 admin 密码             |
+------------------------------------+-----------------------------+
| admins del <Username>              | 删除 admin 账号             |
+------------------------------------+-----------------------------+

admins add <Username> <Password> <Tags> 
----------

创建 admin 账户::

    $ ./bin/emqx_ctl admins add root public test
    ok

admins passwd <Username> <Password>
-------------

重置 admin 账户密码::

    $ ./bin/emqx_ctl admins passwd root private
    ok

admins del <Username>
----------

删除 admin 账户::

    $ ./bin/emqx_ctl admins del root
    ok

