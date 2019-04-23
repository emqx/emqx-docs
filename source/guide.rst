
.. _guide:


用户指南 (User Guide)
^^^^^^^^^^^^^^^^^^^^^^

.. _authentication:


MQTT 认证/访问控制
------------------

**EMQ X** 消息服务器 *连接认证* 和 *访问控制* 由一系列的认证插件(Plugins)提供，他们的命名都符合 ``emqx_auth_<name>`` 的规则。

在 EMQ X 中，这俩个功能分别是指：

1. **连接认证**: *EMQ X* 会校验每个连接上的客户端是否具有接入系统的权限，若没有则会断开该连接
2. **访问控制**: *EMQ X* 会校验客户端每个 *发布/订阅(PUBLISH/SUBSCRIBE)* 的权限，以 *拒绝/允许* 此处操作


认证(Authentication)
>>>>>>>>>>>>>>>>>>>>>

*EMQ X* 消息服务器认证由一系列认证插件(Plugin)提供，系统支持按用户名密码、ClientID 或匿名认证。


系统默认开启匿名认证(anonymous)，通过加载认证插件可开启的多个认证模块组成认证链::

               ----------------           ----------------           ------------
    Client --> | Username认证 | -ignore-> | ClientID认证 | -ignore-> | 匿名认证 |
               ----------------           ----------------           ------------
                      |                         |                         |
                     \|/                       \|/                       \|/
                allow | deny              allow | deny              allow | deny


**开启匿名认证**

etc/emqx.conf 配置启用匿名认证:

.. code:: properties

    ## Allow anonymous authentication by default if no auth plugins loaded.
    ## Notice: Disable the option in production deployment!
    ##
    ## Value: true | false
    allow_anonymous = true


.. _acl:

访问控制(ACL)
>>>>>>>>>>>>>

*EMQ X* 消息服务器通过 ACL(Access Control List) 实现 MQTT 客户端访问控制。

ACL 访问控制规则定义::

    允许(Allow)|拒绝(Deny) 谁(Who) 订阅(Subscribe)|发布(Publish) 主题列表(Topics)

MQTT 客户端发起订阅/发布请求时，EMQ X 消息服务器的访问控制模块，会逐条匹配 ACL 规则，直到匹配成功为止::

              ---------              ---------              ---------
    Client -> | Rule1 | --nomatch--> | Rule2 | --nomatch--> | Rule3 | --> Default
              ---------              ---------              ---------
                  |                      |                      |
                match                  match                  match
                 \|/                    \|/                    \|/
            allow | deny           allow | deny           allow | deny


**默认访问控制设置**


*EMQ X* 消息服务器默认访问控制，在 etc/emqx.conf 中设置:

.. code:: properties

    ## Allow or deny if no ACL rules matched.
    ##
    ## Value: allow | deny
    acl_nomatch = allow

    ## Default ACL File.
    ##
    ## Value: File Name
    acl_file = etc/acl.conf

ACL 规则定义在 etc/acl.conf，EMQ X 启动时加载到内存:

.. code:: erlang

    %% Allow 'dashboard' to subscribe '$SYS/#'
    {allow, {user, "dashboard"}, subscribe, ["$SYS/#"]}.

    %% Allow clients from localhost to subscribe any topics
    {allow, {ipaddr, "127.0.0.1"}, pubsub, ["$SYS/#", "#"]}.

    %% Deny clients to subscribe '$SYS#' and '#'
    {deny, all, subscribe, ["$SYS/#", {eq, "#"}]}.

    %% Allow all by default
    {allow, all}.


EMQ X 3.1 版本提供的认证插件包括:

+----------------------------+---------------------------+
| 插件                       | 说明                      |
+============================+===========================+
| `emqx_auth_clientid`_      | ClientId 认证/鉴权插件    |
+----------------------------+---------------------------+
| `emqx_auth_username`_      | 用户名密码认证/鉴权插件   |
+----------------------------+---------------------------+
| `emqx_auth_jwt`_           | JWT 认证/鉴权插件         |
+----------------------------+---------------------------+
| `emqx_auth_ldap`_          | LDAP 认证/鉴权插件        |
+----------------------------+---------------------------+
| `emqx_auth_http`_          | HTTP 认证/鉴权插件        |
+----------------------------+---------------------------+
| `emqx_auth_mysql`_         | MySQ L认证/鉴权插件       |
+----------------------------+---------------------------+
| `emqx_auth_pgsql`_         | Postgre 认证/鉴权插件     |
+----------------------------+---------------------------+
| `emqx_auth_redis`_         | Redis 认证/鉴权插件       |
+----------------------------+---------------------------+
| `emqx_auth_mongo`_         | MongoDB 认证/鉴权插件     |
+----------------------------+---------------------------+

其中，关于每个认证插件的配置及用法，可参考 `扩展插件 (Plugins) <https://developer.emqx.io/docs/emq/v3/cn/plugins.html>`_ 关于认证部分。


.. note:: auth 插件可以同时启动多个。每次检查的时候，按照优先级从高到低依次检查，同一优先级的，先启动的插件先检查。(内置默认的 acl.conf 优先级为-1，各个插件默认为0)

此外 *EMQ X* 还支持使用 **PSK (Pre-shared Key)** 的方式来控制客户端的接入，但它并不是使用的上述的 *连接认证* 链的方式，而是在 SSL 握手期间进行验证。详情参考 `Pre-shared Key <https://en.wikipedia.org/wiki/Pre-shared_key>`_ 和 `emqx_psk_file`_


MQTT 发布订阅
-------------

MQTT 是为移动互联网、物联网设计的轻量发布订阅模式的消息服务器，目前支持 MQTT `v3.1.1 <http://docs.oasis-open.org/mqtt/mqtt/v3.1.1/mqtt-v3.1.1.html>`_ 和 `v5.0 <http://docs.oasis-open.org/mqtt/mqtt/v5.0/mqtt-v5.0.html>`_:

.. image:: ./_static/images/pubsub_concept.png

*EMQ X* 启动成功后，任何设备或终端的 MQTT 客户端，可通过 MQTT 协议连接到服务器，通过 PUBLISH/SUBSCRIBE 进行交换消息。

MQTT 协议客户端库: https://github.com/mqtt/mqtt.github.io/wiki/libraries

例如，mosquitto_sub/pub 命令行发布订阅消息::

    mosquitto_sub -t topic -q 2
    mosquitto_pub -t topic -q 1 -m "Hello, MQTT!"

*EMQ X* 对于 MQTT 协议服务所监听的端口等配置，都可在 etc/emqx.conf 文件中设置:

.. code:: properties

    ## TCP Listener: 1883, 127.0.0.1:1883, ::1:1883
    listener.tcp.external = 0.0.0.0:1883

    ## Size of acceptor pool
    listener.tcp.external.acceptors = 8

    ## Maximum number of concurrent clients
    listener.tcp.external.max_connections = 1024000

    ## Maximum external connections per second.
    ##
    ## Value: Number
    listener.tcp.external.max_conn_rate = 1000

MQTT/SSL 监听器，缺省端口8883:

.. code:: properties

    ## SSL Listener: 8883, 127.0.0.1:8883, ::1:8883
    listener.ssl.external = 8883

    ## Size of acceptor pool
    listener.ssl.external.acceptors = 16

    ## Maximum number of concurrent clients
    listener.ssl.external.max_connections = 102400

    ## Maximum MQTT/SSL connections per second.
    ##
    ## Value: Number
    listener.ssl.external.max_conn_rate = 500

.. _http_publish:


HTTP 发布接口
-------------

*EMQ X* 消息服务器提供了一个 HTTP 发布接口，应用服务器或 Web 服务器可通过该接口发布 MQTT 消息::

    HTTP POST http://host:8080/api/v3/mqtt/publish

Web 服务器例如 PHP/Java/Python/NodeJS 或 Ruby on Rails，可通过 HTTP POST 请求发布 MQTT 消息:

.. code:: bash

    curl -v --basic -u user:passwd -H "Content-Type: application/json" -d '{"qos":1, "retain": false, "topic":"world", "payload":"test" , "client_id": "C_1492145414740"}'  -k http://localhost:8080/api/v3/mqtt/publish

HTTP 接口参数:

+----------+----------------------+
| 参数     | 说明                 |
+==========+======================+
| client_id| MQTT 客户端 ID       |
+----------+----------------------+
| qos      | QoS: 0 | 1 | 2       |
+----------+----------------------+
| retain   | Retain: true | false |
+----------+----------------------+
| topic    | 主题(Topic)          |
+----------+----------------------+
| payload  | 消息载荷             |
+----------+----------------------+

.. NOTE::

    HTTP 发布接口采用 `Basic <https://en.wikipedia.org/wiki/Basic_access_authentication>`_ 认证。上例中的 ``user`` 和 ``passwd`` 是来自于 Dashboard 下的 Applications 内的 AppId 和 其密码


MQTT WebSocket 连接
-------------------

*EMQ X* 还支持 WebSocket 连接，Web 浏览器可直接通过 WebSocket 连接至服务器:

+-------------------------+----------------------------+
| WebSocket URI:          | ws(s)://host:8083/mqtt     |
+-------------------------+----------------------------+
| Sec-WebSocket-Protocol: | 'mqttv3.1' or 'mqttv3.1.1' |
+-------------------------+----------------------------+

Dashboard 插件提供了一个 MQTT WebSocket 连接的测试页面::

    http://127.0.0.1:18083/#/websocket

*EMQ X* 通过内嵌的 HTTP 服务器，实现 MQTT/WebSocket，etc/emqx.conf 设置:

.. code:: properties

    ## MQTT/WebSocket Listener
    listener.ws.external = 8083
    listener.ws.external.acceptors = 4
    ## Maximum number of concurrent MQTT/WebSocket connections.
    ##
    ## Value: Number
    listener.ws.external.max_connections = 102400

    ## Maximum MQTT/WebSocket connections per second.
    ##
    ## Value: Number
    listener.ws.external.max_conn_rate = 1000



.. _shared_sub:

共享订阅 (Shared Subscription)
-------------------------------

*EMQ X* R3.1 版本支持集群级别的共享订阅功能。 共享订阅(Shared Subscription)支持在多订阅者间采用多种常用的负载策略派发消息::

                                ---------
                                |       | --Msg1--> Subscriber1
    Publisher--Msg1,Msg2,Msg3-->| EMQ X | --Msg2--> Subscriber2
                                |       | --Msg3--> Subscriber3
                                ---------

共享订阅支持两种使用方式:

+-----------------+-------------------------------------------+
|  订阅前缀       | 使用示例                                  |
+-----------------+-------------------------------------------+
| $queue/         | mosquitto_sub -t '$queue/topic'           |
+-----------------+-------------------------------------------+
| $share/<group>/ | mosquitto_sub -t '$share/group/topic'     |
+-----------------+-------------------------------------------+

示例::

    mosquitto_sub -t '$share/group/topic'

    mosquitto_pub -t 'topic' -m msg -q 2


目前在 *EMQ X* R3.1 的版本中支持按以下几种策略进行负载共享的消息：

+---------------------------+-------------------------+
| 策略                      | 说明                    |
+===========================+=========================+
| random                    | 在所有共享订阅者中随机  |
+---------------------------+-------------------------+
| round_robin               | 按订阅顺序              |
+---------------------------+-------------------------+
| sticky                    | 使用上次派发的订阅者    |
+---------------------------+-------------------------+
| hash                      | 根据发送者的 ClientId   |
+---------------------------+-------------------------+

.. note:: 当所有的订阅者都不在线时，仍会挑选一个订阅者，并存至其 Session 的消息队列中


.. _sys_topic:

$SYS-系统主题
-------------

*EMQ X* 消息服务器周期性发布自身运行状态、消息统计、客户端上下线事件到 以 ``$SYS/`` 开头系统主题。

$SYS 主题路径以 ``$SYS/brokers/{node}/`` 开头。 ``{node}`` 是指产生该 事件/消息 所在的节点名称，例如::

    $SYS/brokers/emqx@127.0.0.1/version

    $SYS/brokers/emqx@127.0.0.1/uptime

.. NOTE:: 默认只允许 localhost 的 MQTT 客户端订阅 $SYS 主题，可通过 etc/acl.config 修改访问控制规则。

$SYS 系统消息发布周期，通过 etc/emqx.conf 配置:

.. code:: properties

    ## System interval of publishing $SYS messages.
    ##
    ## Value: Duration
    ## Default: 1m, 1 minute
    broker.sys_interval = 1m

.. _sys_brokers:

集群状态信息
>>>>>>>>>>>>

+--------------------------------+-----------------------+
| 主题                           | 说明                  |
+================================+=======================+
| $SYS/brokers                   | 集群节点列表          |
+--------------------------------+-----------------------+
| $SYS/brokers/${node}/version   | EMQ 服务器版本        |
+--------------------------------+-----------------------+
| $SYS/brokers/${node}/uptime    | EMQ 服务器启动时间    |
+--------------------------------+-----------------------+
| $SYS/brokers/${node}/datetime  | EMQ 服务器时间        |
+--------------------------------+-----------------------+
| $SYS/brokers/${node}/sysdescr  | EMQ 服务器描述        |
+--------------------------------+-----------------------+

.. _sys_clients:

客户端上下线事件
>>>>>>>>>>>>>>>>

$SYS 主题前缀: $SYS/brokers/${node}/clients/

+--------------------------+------------------------------------+
| 主题(Topic)              | 说明                               |
+==========================+====================================+
| ${clientid}/connected    | 上线事件。当某客户端上线时，会发布 |
|                          | 该消息                             |
+--------------------------+------------------------------------+
| ${clientid}/disconnected | 下线事件。当某客户端离线时，会发布 |
|                          | 该消息                             |
+--------------------------+------------------------------------+

'connected' 事件消息的 Payload 可解析成 JSON 格式:

.. code:: json

    {
        "clientid":"id1",
        "username":"u",
        "ipaddress":"127.0.0.1",
        "connack":0,
        "ts":1554047291,
        "proto_ver":3,
        "proto_name":"MQIsdp",
        "clean_start":true,
        "keepalive":60
    }


'disconnected' 事件消息的 Payload 可解析成 JSON 格式:

.. code:: json
    
    {
        "clientid":"id1",
        "username":"u",
        "reason":"normal",
        "ts":1554047291
    }


.. _sys_stats:

系统统计(Statistics)
>>>>>>>>>>>>>>>>>>>>

系统主题前缀: $SYS/brokers/${node}/stats/


客户端统计
::::::::::

+---------------------+---------------------------------------------+
| 主题(Topic)         | 说明                                        |
+---------------------+---------------------------------------------+
| connections/count   | 当前客户端总数                              |
+---------------------+---------------------------------------------+
| connections/max     | 最大客户端数量                              |
+---------------------+---------------------------------------------+


会话统计
::::::::

+-----------------------------+---------------------------------------------+
| 主题(Topic)                 | 说明                                        |
+-----------------------------+---------------------------------------------+
| sessions/count              | 当前会话总数                                |
+-----------------------------+---------------------------------------------+
| sessions/max                | 最大会话数量                                |
+-----------------------------+---------------------------------------------+
| sessions/persistent/count   | 当前持久会话总数                            |
+-----------------------------+---------------------------------------------+
| sessions/persistent/max     | 最大持久会话数量                            |
+-----------------------------+---------------------------------------------+


订阅统计
::::::::

+---------------------------------+---------------------------------------------+
| 主题(Topic)                     | 说明                                        |
+---------------------------------+---------------------------------------------+
| suboptions/count                | 当前订阅选项个数                            |
+---------------------------------+---------------------------------------------+
| suboptions/max                  | 最大订阅选项总数                            |
+---------------------------------+---------------------------------------------+
| subscribers/max                 | 最大订阅者总数                              |
+---------------------------------+---------------------------------------------+
| subscribers/count               | 当前订阅者数量                              |
+---------------------------------+---------------------------------------------+
| subscriptions/max               | 最大订阅数量                                |
+---------------------------------+---------------------------------------------+
| subscriptions/count             | 当前订阅总数                                |
+---------------------------------+---------------------------------------------+
| subscriptions/shared/count      | 当前共享订阅个数                            |
+---------------------------------+---------------------------------------------+
| subscriptions/shared/max        | 当前共享订阅总数                            |
+---------------------------------+---------------------------------------------+


主题统计
::::::::

+---------------------+---------------------------------------------+
| 主题(Topic)         | 说明                                        |
+---------------------+---------------------------------------------+
| topics/count        | 当前 Topic 总数                             |
+---------------------+---------------------------------------------+
| topics/max          | 最大 Topic 数量                             |
+---------------------+---------------------------------------------+


路由统计
::::::::

+---------------------+---------------------------------------------+
| 主题(Topic)         | 说明                                        |
+---------------------+---------------------------------------------+
| routes/count        | 当前 Routes 总数                            |
+---------------------+---------------------------------------------+
| routes/max          | 最大 Routes 数量                            |
+---------------------+---------------------------------------------+

.. note:: ``topics/count`` 和 ``topics/max`` 与 ``routes/count`` 和 ``routes/max`` 数值上是想等的


收发流量/报文/消息统计
>>>>>>>>>>>>>>>>>>>>>>

系统主题(Topic)前缀: $SYS/brokers/${node}/metrics/

收发流量统计
::::::::::::

+---------------------+---------------------------------------------+
| 主题(Topic)         | 说明                                        |
+---------------------+---------------------------------------------+
| bytes/received      | 累计接收流量                                |
+---------------------+---------------------------------------------+
| bytes/sent          | 累计发送流量                                |
+---------------------+---------------------------------------------+

MQTT报文收发统计
::::::::::::::::

+-----------------------------+---------------------------------------------+
| 主题(Topic)                 | 说明                                        |
+-----------------------------+---------------------------------------------+
| packets/received            | 累计接收 MQTT 报文                          |
+-----------------------------+---------------------------------------------+
| packets/sent                | 累计发送 MQTT 报文                          |
+-----------------------------+---------------------------------------------+
| packets/connect             | 累计接收 MQTT CONNECT 报文                  |
+-----------------------------+---------------------------------------------+
| packets/connack             | 累计发送 MQTT CONNACK 报文                  |
+-----------------------------+---------------------------------------------+
| packets/publish/received    | 累计接收 MQTT PUBLISH 报文                  |
+-----------------------------+---------------------------------------------+
| packets/publish/sent        | 累计发送 MQTT PUBLISH 报文                  |
+-----------------------------+---------------------------------------------+
| packets/puback/received     | 累计接收 MQTT PUBACK 报文                   |
+-----------------------------+---------------------------------------------+
| packets/puback/sent         | 累计发送 MQTT PUBACK 报文                   |
+-----------------------------+---------------------------------------------+
| packets/puback/missed       | 累计丢失 MQTT PUBACK 报文                   |
+-----------------------------+---------------------------------------------+
| packets/pubrec/received     | 累计接收 MQTT PUBREC 报文                   |
+-----------------------------+---------------------------------------------+
| packets/pubrec/sent         | 累计发送 MQTT PUBREC 报文                   |
+-----------------------------+---------------------------------------------+
| packets/pubrec/missed       | 累计丢失 MQTT PUBREC 报文                   |
+-----------------------------+---------------------------------------------+
| packets/pubrel/received     | 累计接收 MQTT PUBREL 报文                   |
+-----------------------------+---------------------------------------------+
| packets/pubrel/sent         | 累计发送 MQTT PUBREL 报文                   |
+-----------------------------+---------------------------------------------+
| packets/pubrel/missed       | 累计丢失 MQTT PUBREL 报文                   |
+-----------------------------+---------------------------------------------+
| packets/pubcomp/received    | 累计接收 MQTT PUBCOMP 报文                  |
+-----------------------------+---------------------------------------------+
| packets/pubcomp/sent        | 累计发送 MQTT PUBCOMP 报文                  |
+-----------------------------+---------------------------------------------+
| packets/pubcomp/missed      | 累计丢失 MQTT PUBCOMP 报文                  |
+-----------------------------+---------------------------------------------+
| packets/subscribe           | 累计接收 MQTT SUBSCRIBE 报文                |
+-----------------------------+---------------------------------------------+
| packets/suback              | 累计发送 MQTT SUBACK 报文                   |
+-----------------------------+---------------------------------------------+
| packets/unsubscribe         | 累计接收 MQTT UNSUBSCRIBE 报文              |
+-----------------------------+---------------------------------------------+
| packets/unsuback            | 累计发送 MQTT UNSUBACK 报文                 |
+-----------------------------+---------------------------------------------+
| packets/pingreq             | 累计接收 MQTT PINGREQ 报文                  |
+-----------------------------+---------------------------------------------+
| packets/pingresp            | 累计发送 MQTT PINGRESP 报文                 |
+-----------------------------+---------------------------------------------+
| packets/disconnect/received | 累计接收 MQTT DISCONNECT 报文               |
+-----------------------------+---------------------------------------------+
| packets/disconnect/sent     | 累计接收 MQTT DISCONNECT 报文               |
+-----------------------------+---------------------------------------------+
| packets/auth                | 累计接收Auth 报文                           |
+-----------------------------+---------------------------------------------+


MQTT 消息收发统计
:::::::::::::::::

+--------------------------+---------------------------------------------+
| 主题(Topic)              | 说明                                        |
+--------------------------+---------------------------------------------+
| messages/received        | 累计接收消息                                |
+--------------------------+---------------------------------------------+
| messages/sent            | 累计发送消息                                |
+--------------------------+---------------------------------------------+
| messages/expired         | 累计发送消息                                |
+--------------------------+---------------------------------------------+
| messages/retained        | Retained 消息总数                           |
+--------------------------+---------------------------------------------+
| messages/dropped         | 丢弃消息总数                                |
+--------------------------+---------------------------------------------+
| messages/forward         | 节点转发消息总数                            |
+--------------------------+---------------------------------------------+
| messages/qos0/received   | 累计接受QoS0消息                            |
+--------------------------+---------------------------------------------+
| messages/qos0/sent       | 累计发送QoS0消息                            |
+--------------------------+---------------------------------------------+
| messages/qos1/received   | 累计接受QoS1消息                            |
+--------------------------+---------------------------------------------+
| messages/qos1/sent       | 累计发送QoS1消息                            |
+--------------------------+---------------------------------------------+
| messages/qos2/received   | 累计接受QoS2消息                            |
+--------------------------+---------------------------------------------+
| messages/qos2/sent       | 累计发送QoS2消息                            |
+--------------------------+---------------------------------------------+
| messages/qos2/expired    | QoS2过期消息总数                            |
+--------------------------+---------------------------------------------+
| messages/qos2/dropped    | QoS2丢弃消息总数                            |
+--------------------------+---------------------------------------------+

.. _sys_alarms:

Alarms - 系统告警
>>>>>>>>>>>>>>>>>

系统主题(Topic)前缀: $SYS/brokers/${node}/alarms/

+------------------+------------------+
| 主题(Topic)      | 说明             |
+------------------+------------------+
| ${alarmId}/alert | 新产生告警       |
+------------------+------------------+
| ${alarmId}/clear | 清除告警         |
+------------------+------------------+

.. _sys_sysmon:

Sysmon - 系统监控
>>>>>>>>>>>>>>>>>

系统主题(Topic)前缀: $SYS/brokers/${node}/sysmon/

+------------------+--------------------+
| 主题(Topic)      | 说明               |
+------------------+--------------------+
| long_gc          | GC 时间过长警告    |
+------------------+--------------------+
| long_schedule    | 调度时间过长警告   |
+------------------+--------------------+
| large_heap       | Heap 内存占用警告  |
+------------------+--------------------+
| busy_port        | Port 忙警告        |
+------------------+--------------------+
| busy_dist_port   | Dist Port 忙警告   |
+------------------+--------------------+

.. _trace:


追踪
----

EMQ X 消息服务器支持追踪来自某个客户端(Client)的全部报文，或者发布到某个主题(Topic)的全部消息。

追踪客户端(Client):

.. code:: bash

    ./bin/emqx_ctl trace client "clientid" "trace_clientid.log" debug

追踪主题(Topic):

.. code:: bash

    ./bin/emqx_ctl trace topic "topic" "trace_topic.log" debug

查询追踪:

.. code:: bash

    ./bin/emqx_ctl trace list

停止追踪:

.. code:: bash

    ./bin/emqx_ctl trace stop client "clientid"

    ./bin/emqx_ctl trace stop topic "topic"

.. _emqx_auth_clientid: https://github.com/emqx/emqx-auth-clientid
.. _emqx_auth_username: https://github.com/emqx/emqx-auth-username
.. _emqx_auth_ldap:     https://github.com/emqx/emqx-auth-ldap
.. _emqx_auth_http:     https://github.com/emqx/emqx-auth-http
.. _emqx_auth_mysql:    https://github.com/emqx/emqx-auth-mysql
.. _emqx_auth_pgsql:    https://github.com/emqx/emqx-auth-pgsql
.. _emqx_auth_redis:    https://github.com/emqx/emqx-auth-redis
.. _emqx_auth_mongo:    https://github.com/emqx/emqx-auth-mongo
.. _emqx_auth_jwt:      https://github.com/emqx/emqx-auth-jwt
.. _emqx_psk_file:      https://github.com/emqx/emqx-psk-file

