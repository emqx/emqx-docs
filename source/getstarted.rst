
.. _getstarted:

========
开始使用
========

.. _intro:

------------------------
*EMQ* 2.0 消息服务器简介
------------------------

*EMQ* (Erlang/Enterprise/Elastic MQTT Broker)是基于Erlang/OTP平台开发的开源物联网MQTT消息服务器。Erlang/OTP是出色的软实时(Soft-Realtime)、低延时(Low-Latency)、分布式(Distributed)的语言平台。MQTT是轻量的(Lightweight)、发布订阅模式(PubSub)的物联网消息协议。

*EMQ* 项目设计目标是承载移动终端或物联网终端海量的MQTT连接，并实现在海量物联网设备间快速低延时(Low-Latency)消息路由:

1. 稳定承载大规模的MQTT客户端连接，单服务器节点支持50万到100万连接。

2. 分布式节点集群，快速低延时的消息路由，单集群支持1000万规模的路由。

3. 消息服务器内扩展，支持定制多种认证方式、高效存储消息到后端数据库。

4. 完整物联网协议支持，MQTT、MQTT-SN、CoAP、WebSocket或私有协议支持。

.. _mqtt_pubsub:

--------------------
MQTT发布订阅模式简述
--------------------

MQTT是发布订阅(Publish/Subscribe)模式的消息协议，与HTTP协议请求响应(Request/Response)模式不同。

MQTT发布者与订阅者之间通过"主题"(Topic)进行消息路由，主题(Topic)格式类似Unix文件路径，例如::

    sensor/1/temperature

    chat/room/subject

    presence/user/feng

    sensor/1/#

    sensor/+/temperature

    uber/drivers/joe/inbox

MQTT主题(Topic)支持'+', '#'的通配符，'+'通配一个层级，'#'通配多个层级(必须在末尾)。

MQTT消息发布者(Publisher)只能向特定'名称主题'(不支持通配符)发布消息，订阅者(Subscriber)通过订阅'过滤主题'(支持通配符)来匹配消息。

.. NOTE::

    初接触MQTT协议的用户，通常会向通配符的'过滤主题'发布广播消息，MQTT协议不支持这种模式，需从订阅侧设计广播主题(Topic)。
    例如Android推送，向所有广州用户，推送某类本地消息，客户端获得GIS位置后，可订阅'news/city/guangzhou'主题。

.. _quick_start:

-----------------
五分钟下载启动EMQ
-----------------

*EMQ* 2.0消息服务器每个版本，会发布Ubuntu、CentOS、FreeBSD、Mac OS X、Windows平台程序包与Docker镜像。

下载地址: http://emqtt.com/downloads

程序包下载后，可直接解压启动运行，例如Mac平台:

.. code-block:: bash

    unzip emqttd-macosx-v2.0.zip && cd emqttd

    # 启动emqttd
    ./bin/emqttd start

    # 检查运行状态
    ./bin/emqttd_ctl status

    # 停止emqttd
    ./bin/emqttd stop

*EMQ* 消息服务默认允许匿名认证，启动后MQTT客户端可连接1883端口，启动运行日志输出在log/目录。

.. _compile:

----------------
源码编译EMQ 2.0
----------------

.. code-block:: bash

    git clone https://github.com/emqtt/emq-relx.git

    cd emq-relx && make

    cd _rel/emqttd && ./bin/emqttd console

.. _dashboard:

------------------------
Web管理控制台(Dashboard)
------------------------

*EMQ* 消息服务器启动后，会默认加载Dashboard插件，启动Web管理控制台。用户可通过Web控制台，查看服务器运行状态、统计数据、客户端(Client)、会话(Session)、主题(Topic)、订阅(Subscription)、插件(Plugin)。

控制台地址: http://127.0.0.1:18083，默认用户: admin，密码：public

.. image:: ./_static/images/dashboard.png

.. _features:

---------------------------
*EMQ* 2.0消息服务器功能列表
---------------------------

* 完整的MQTT V3.1/V3.1.1协议规范支持
* QoS0, QoS1, QoS2消息支持
* 持久会话与离线消息支持
* Retained消息支持
* Last Will消息支持
* TCP/SSL连接支持
* MQTT/WebSocket(SSL)支持
* HTTP消息发布接口支持
* $SYS/#系统主题支持
* 客户端在线状态查询与订阅支持
* 客户端ID或IP地址认证支持
* 用户名密码认证支持
* LDAP认证
* Redis、MySQL、PostgreSQL、MongoDB、HTTP认证集成
* 浏览器Cookie认证
* 基于客户端ID、IP地址、用户名的访问控制(ACL)
* 多服务器节点集群(Cluster)
* 多服务器节点桥接(Bridge)
* mosquitto桥接支持
* Stomp协议支持
* MQTT-SN协议支持
* CoAP协议支持
* Stomp/SockJS支持
* 通过Paho兼容性测试
* 2.0新功能: 本地订阅($local/topic)
* 2.0新功能: 共享订阅($share/<group>/topic)
* 2.0新功能: sysctl类似k = v格式配置文件

.. _plugins:

---------------------
*EMQ* 2.0扩展插件列表
---------------------

*EMQ* 2.0支持丰富的扩展插件，包括控制台、扩展模块、多种认证方式、多种接入协议等:

+----------------------------+-----------------------------------+
| `emq_plugin_template`_     | 插件模版与演示代码                |
+----------------------------+-----------------------------------+
| `emq_mod_retainer`_        | Retain消息存储模块                |
+----------------------------+-----------------------------------+
| `emq_mod_presence`_        | 客户端上下线状态消息发布          |
+----------------------------+-----------------------------------+
| `emq_mod_subscription`_    | 客户端上线自动主题订阅            |
+----------------------------+-----------------------------------+
| `emq_dashboard`_           | Web管理控制台，默认加载           |
+----------------------------+-----------------------------------+
| `emq_mod_rewrite`_         | 重写发布订阅主题(Topic)插件       |
+----------------------------+-----------------------------------+
| `emq_auth_clientid`_       | ClientId、密码认证插件            |
+----------------------------+-----------------------------------+
| `emq_auth_username`_       | 用户名、密码认证插件              |
+----------------------------+-----------------------------------+
| `emq_auth_ldap`_           | LDAP认证插件                      |
+----------------------------+-----------------------------------+
| `emq_auth_http`_           | HTTP认证插件                      |
+----------------------------+-----------------------------------+
| `emq_auth_mysql`_          | MySQL认证插件                     |
+----------------------------+-----------------------------------+
| `emq_auth_pgsql`_          | PostgreSQL认证插件                |
+----------------------------+-----------------------------------+
| `emq_auth_redis`_          | Redis认证插件                     |
+----------------------------+-----------------------------------+
| `emq_auth_mongo`_          | MongoDB认证插件                   |
+----------------------------+-----------------------------------+
| `emq_sn`_                  | MQTT-SN协议插件                   |
+----------------------------+-----------------------------------+
| `emq_coap`_                | CoAP协议插件                      |
+----------------------------+-----------------------------------+
| `emq_stomp`_               | Stomp协议插件                     |
+----------------------------+-----------------------------------+
| `emq_recon`_               | Recon优化调测插件                 |
+----------------------------+-----------------------------------+
| `emq_reloader`_            | 热升级插件(开发调试)              |
+----------------------------+-----------------------------------+
| `emq_sockjs`_              | SockJS插件()                      |
+----------------------------+-----------------------------------+

扩展插件通过'bin/emqttd_ctl'管理命令行，或Dashboard控制台加载启用。例如启用PostgreSQL认证插件::

    ./bin/emqttd_ctl plugins load emq_auth_pgsql

.. _c1000k:

-------------------
100万线连接测试说明
-------------------

.. NOTE::

    *EMQ* 2.0消息服务器默认设置，允许最大客户端连接是512，因为大部分操作系统'ulimit -n'限制为1024。

*EMQ* 消息服务器1.1.3版本，连接压力测试到130万线，8核心/32G内存的CentOS云服务器。

操作系统内核参数、TCP协议栈参数、Erlang虚拟机参数、EMQ最大允许连接数设置简述如下：

Linux操作系统参数
-----------------

# 2M - 系统所有进程可打开的文件数量::

    sysctl -w fs.file-max=2097152
    sysctl -w fs.nr_open=2097152

# 1M - 系统允许当前进程打开的文件数量::

    ulimit -n 1048576

TCP协议栈参数
-------------

# backlog - Socket监听队列长度::

    sysctl -w net.core.somaxconn=65536

Erlang虚拟机参数
----------------

emqttd/etc/emq.conf:

.. code-block:: properties

    ## Erlang Process Limit
    node.process_limit = 2097152

    ## Sets the maximum number of simultaneously existing ports for this system
    node.max_ports = 1048576

EMQ 最大允许连接数
------------------

emqttd/etc/emq.conf 'listeners'段落::

    ## Size of acceptor pool
    mqtt.listener.tcp.acceptors = 64

    ## Maximum number of concurrent clients
    mqtt.listener.tcp.max_clients = 1000000

测试客户端设置
--------------

测试客户端在一个接口上，最多只能创建65000连接::

    sysctl -w net.ipv4.ip_local_port_range="500 65535"

    echo 1000000 > /proc/sys/fs/nr_open

按应用场景测试
--------------

MQTT是一个设计得非常出色的传输层协议，在移动消息、物联网、车联网、智能硬件甚至能源勘探等领域有着广泛的应用。1个字节报头、2个字节心跳、消息QoS支持等设计，非常适合在低带宽、不可靠网络、嵌入式设备上应用。

不同的应用有不同的系统要求，用户使用emqttd消息服务器前，可以按自己的应用场景进行测试，而不是简单的连接压力测试:

1. Android消息推送: 推送消息广播测试。

2. 移动即时消息应用: 消息收发确认测试。

3. 智能硬件应用: 消息的往返时延测试。

4. 物联网数据采集: 并发连接与吞吐测试。

.. _mqtt_clients:

------------------
开源MQTT客户端项目
------------------

GitHub: https://github.com/emqtt

+--------------------+----------------------+
| `emqttc`_          | Erlang MQTT客户端库  |
+--------------------+----------------------+
| `emqtt_benchmark`_ | MQTT连接测试工具     |
+--------------------+----------------------+
| `CocoaMQTT`_       | Swift语言MQTT客户端库|
+--------------------+----------------------+
| `QMQTT`_           | QT框架MQTT客户端库   |
+--------------------+----------------------+

Eclipse Paho: https://www.eclipse.org/paho/

MQTT.org: https://github.com/mqtt/mqtt.github.io/wiki/libraries

.. _emqttc:          https://github.com/emqtt/emqttc
.. _emqtt_benchmark: https://github.com/emqtt/emqtt_benchmark
.. _CocoaMQTT:       https://github.com/emqtt/CocoaMQTT
.. _QMQTT:           https://github.com/emqtt/qmqtt

.. _emq_plugin_template:  https://github.com/emqtt/emq_plugin_template
.. _emq_mod_retainer:     https://github.com/emqtt/emq_mod_retainer
.. _emq_mod_presence:     https://github.com/emqtt/emq_mod_presence
.. _emq_mod_subscription: https://github.com/emqtt/emq_mod_subscription
.. _emq_dashboard:        https://github.com/emqtt/emq_dashboard
.. _emq_mod_rewrite:      https://github.com/emqtt/emq_mod_rewrite
.. _emq_auth_clientid:    https://github.com/emqtt/emq_auth_clientid
.. _emq_auth_username:    https://github.com/emqtt/emq_auth_username
.. _emq_auth_ldap:        https://github.com/emqtt/emq_auth_ldap
.. _emq_auth_http:        https://github.com/emqtt/emq_auth_http
.. _emq_auth_mysql:       https://github.com/emqtt/emq_auth_mysql
.. _emq_auth_pgsql:       https://github.com/emqtt/emq_auth_pgsql
.. _emq_auth_redis:       https://github.com/emqtt/emq_auth_redis
.. _emq_auth_mongo:       https://github.com/emqtt/emq_auth_mongo
.. _emq_reloader:         https://github.com/emqtt/emq_reloader
.. _emq_stomp:            https://github.com/emqtt/emq_stomp
.. _emq_sockjs:           https://github.com/emqtt/emq_sockjs
.. _emq_recon:            https://github.com/emqtt/emq_recon
.. _emq_sn:               https://github.com/emqtt/emq_sn
.. _emq_coap:             https://github.com/emqtt/emq_coap

