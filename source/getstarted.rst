
.. _getstarted:

======================
开始使用 (Get Started)
======================

.. _intro:

------------------------
*EMQ X* 3.0 消息服务器简介
------------------------

*EMQ X* (Erlang/Enterprise/Elastic MQTT Broker) 是基于 Erlang/OTP 平台开发的开源物联网 MQTT 消息服务器。Erlang/OTP 是出色的软实时(Soft-Realtime)、低延时(Low-Latency)、分布式(Distributed) 的语言平台。MQTT 是轻量的(Lightweight)、发布订阅模式(PubSub) 的物联网消息协议。

*EMQ X* 项目设计目标是承载移动终端或物联网终端海量 MQTT 连接，并实现在海量物联网设备间快速低延时消息路由:

1. 稳定承载大规模的 MQTT 客户端连接，单服务器节点支持50万到100万连接。

2. 分布式节点集群，快速低延时的消息路由，单集群支持1000万规模的路由。

3. 消息服务器内扩展，支持定制多种认证方式、高效存储消息到后端数据库。

4. 完整物联网协议支持，MQTT、MQTT-SN、CoAP、WebSocket 或私有协议支持。

.. _mqtt_pubsub:

---------------------
MQTT 发布订阅模式简述
---------------------

MQTT 是发布订阅(Publish/Subscribe) 模式的消息协议，与 HTTP 协议请求响应(Request/Response) 模式不同。

MQTT 发布者与订阅者之间通过"主题"(Topic) 进行消息路由，主题(Topic) 格式类似 Unix 文件路径，例如::

    sensor/1/temperature

    chat/room/subject

    presence/user/feng

    sensor/1/#

    sensor/+/temperature

    uber/drivers/joe/inbox

MQTT 主题(Topic) 支持'+', '#'的通配符，'+'通配一个层级，'#'通配多个层级(必须在末尾)。

MQTT 消息发布者(Publisher) 只能向特定'名称主题'(不支持通配符)发布消息，订阅者(Subscriber)通过订阅'过滤主题'(支持通配符)来匹配消息。

.. NOTE::

    初接触MQTT协议的用户，通常会向通配符的'过滤主题'发布广播消息，MQTT 协议不支持这种模式，需从订阅侧设计广播主题(Topic)。
    例如 Android 推送，向所有广州用户，推送某类本地消息，客户端获得 GIS 位置后，可订阅 'news/city/guangzhou' 主题。

.. _quick_start:

------------------
五分钟下载启动 EMQ
------------------

*EMQ X* 3.0 消息服务器每个版本，会发布 Ubuntu、CentOS、FreeBSD、Mac OS X、Windows 平台程序包与 Docker 镜像。

下载地址: http://emqtt.com/downloads

程序包下载后，可直接解压启动运行，例如 Mac 平台:

.. code-block:: bash

    unzip emqx-macosx-v3.0.zip && cd emqx

    # 启动emqx
    ./bin/emqx start

    # 检查运行状态
    ./bin/emqx_ctl status

    # 停止emqx
    ./bin/emqx stop

*EMQ X* 消息服务默认允许匿名认证，启动后 MQTT 客户端可连接1883端口，启动运行日志输出在 log/ 目录。

.. _compile:

----------------
源码编译EMQ X 3.0
----------------

.. code-block:: bash

    git clone https://github.com/emqx/emqx-rel.git

    cd emqx-rel && make

    cd _rel/emqx && ./bin/emqx console

.. _dashboard:

-------------------------
Web 管理控制台(Dashboard)
-------------------------

*EMQ* 消息服务器启动后，会默认加载 Dashboard 插件，启动 Web 管理控制台。用户可通过 Web 控制台，查看服务器运行状态、统计数据、客户端(Client)、会话(Session)、主题(Topic)、订阅(Subscription)、插件(Plugin)。

控制台地址: http://127.0.0.1:18083，默认用户: admin，密码：public

.. image:: ./_static/images/dashboard.png

.. _features:

----------------------------
*EMQ X* 3.0 消息服务器功能列表
----------------------------

* 完整的 MQTT V3.1/V3.1.1 及V5.0协议规范支持
* QoS0, QoS1, QoS2 消息支持
* 持久会话与离线消息支持
* Retained 消息支持
* Last Will 消息支持
* TCP/SSL 连接支持
* MQTT/WebSocket/SSL 支持
* HTTP消息发布接口支持
* $SYS/# 系统主题支持
* 客户端在线状态查询与订阅支持
* 客户端 ID 或 IP 地址认证支持
* 用户名密码认证支持
* LDAP 认证
* Redis、MySQL、PostgreSQL、MongoDB、HTTP 认证集成
* 浏览器 Cookie 认证
* 基于客户端 ID、IP 地址、用户名的访问控制(ACL)
* 多服务器节点集群(Cluster)
* 自动集群
* 集群分片自动愈合
* 消息速率限制
* 连接速率限制
* 按分区配置节点
* 多服务器节点桥接(Bridge)
* mosquitto 桥接支持
* Stomp 协议支持
* MQTT-SN 协议支持
* CoAP 协议支持
* Stomp/SockJS 支持
* 通过 Paho 兼容性测试
* 本地订阅($local/topic)
* 共享订阅($share/<group>/topic)
* sysctl 类似 k = v 格式配置文件

.. _plugins:

----------------------
*EMQ X* 3.0 扩展插件列表
----------------------

*EMQ X* 3.0 支持丰富的扩展插件，包括控制台、扩展模块、多种认证方式、多种接入协议等:

+----------------------------+-------------------------------------+
| `emqx_plugin_template`_    | 插件模版与演示代码                  |
+----------------------------+-------------------------------------+
| `emqx_retainer`_           | Retain 消息存储插件                 |
+----------------------------+-------------------------------------+
| `emqx_dashboard`_          | Web 管理控制台，默认加载            |
+----------------------------+-------------------------------------+
| `emqx_auth_clientid`_      | ClientId、密码认证插件              |
+----------------------------+-------------------------------------+
| `emqx_auth_username`_      | 用户名、密码认证插件                |
+----------------------------+-------------------------------------+
| `emqx_auth_ldap`_          | LDAP 认证插件                       |
+----------------------------+-------------------------------------+
| `emqx_auth_http`_          | HTTP 认证插件                       |
+----------------------------+-------------------------------------+
| `emqx_auth_mysql`_         | MySQL 认证插件                      |
+----------------------------+-------------------------------------+
| `emqx_auth_pgsql`_         | PostgreSQL 认证插件                 |
+----------------------------+-------------------------------------+
| `emqx_auth_redis`_         | Redis 认证插件                      |
+----------------------------+-------------------------------------+
| `emqx_auth_mongo`_         | MongoDB 认证插件                    |
+----------------------------+-------------------------------------+
| `emqx_sn`_                 | MQTT-SN 协议插件                    |
+----------------------------+-------------------------------------+
| `emqx_coap`_               | CoAP 协议插件                       |
+----------------------------+-------------------------------------+
| `emqx_stomp`_              | Stomp 协议插件                      |
+----------------------------+-------------------------------------+
| `emqx_recon`_              | Recon 优化调测插件                  |
+----------------------------+-------------------------------------+
| `emqx_reloader`_           | 热升级插件(开发调试)                |
+----------------------------+-------------------------------------+
|`emqx_delayed_publish`_     | 延时发布消息                        |
+----------------------------+-------------------------------------+

扩展插件通过 'bin/emqx_ctl' 管理命令行，或 Dashboard 控制台加载启用。例如启用 PostgreSQL 认证插件::

    ./bin/emqx_ctl plugins load emqx_auth_pgsql

.. _c1000k:

-------------------
100万线连接测试说明
-------------------

.. NOTE::

    *EMQ X* 3.0 消息服务器默认设置，允许最大客户端连接是512，因为大部分操作系统 'ulimit -n' 限制为1024。

*EMQ X* 消息服务器1.1.3版本，连接压力测试到130万线，8核心/32G内存的 CentOS 云服务器。

操作系统内核参数、TCP 协议栈参数、Erlang 虚拟机参数、EMQ 最大允许连接数设置简述如下：

Linux 操作系统参数
------------------

# 2M - 系统所有进程可打开的文件数量::

    sysctl -w fs.file-max=2097152
    sysctl -w fs.nr_open=2097152

# 1M - 系统允许当前进程打开的文件数量::

    ulimit -n 1048576

TCP 协议栈参数
--------------

# backlog - Socket 监听队列长度::

    sysctl -w net.core.somaxconn=65536

Erlang 虚拟机参数
-----------------

emqttd/etc/emq.conf:

.. code-block:: properties

    ## Erlang Process Limit
    node.process_limit = 2097152

    ## Sets the maximum number of simultaneously existing ports for this system
    node.max_ports = 1048576

EMQ 最大允许连接数
------------------

emqx/etc/emqx.conf 'listeners'段落::

    ## Size of acceptor pool
    listener.tcp.external.acceptors = 64

    ## Maximum number of concurrent clients
    listener.tcp.external.max_clients = 1000000

测试客户端设置
--------------

测试客户端在一个接口上，最多只能创建65000连接::

    sysctl -w net.ipv4.ip_local_port_range="500 65535"

    echo 1000000 > /proc/sys/fs/nr_open

按应用场景测试
--------------

MQTT 是一个设计得非常出色的传输层协议，在移动消息、物联网、车联网、智能硬件甚至能源勘探等领域有着广泛的应用。1个字节报头、2个字节心跳、消息 QoS 支持等设计，非常适合在低带宽、不可靠网络、嵌入式设备上应用。

不同的应用有不同的系统要求，用户使用emqttd消息服务器前，可以按自己的应用场景进行测试，而不是简单的连接压力测试:

1. Android 消息推送: 推送消息广播测试。

2. 移动即时消息应用: 消息收发确认测试。

3. 智能硬件应用: 消息的往返时延测试。

4. 物联网数据采集: 并发连接与吞吐测试。

.. _mqtt_clients:

--------------------
开源 MQTT 客户端项目
--------------------

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

.. _emq_plugin_template:  https://github.com/emqx/emqx_plugin_template
.. _emq_retainer:         https://github.com/emqx/emqx_retainer
.. _emq_dashboard:        https://github.com/emqx/emqx_dashboard
.. _emq_auth_clientid:    https://github.com/emqx/emqx_auth_clientid
.. _emq_auth_username:    https://github.com/emqx/emqx_auth_username
.. _emq_auth_ldap:        https://github.com/emqx/emqx_auth_ldap
.. _emq_auth_http:        https://github.com/emqx/emqx_auth_http
.. _emq_auth_mysql:       https://github.com/emqx/emqx_auth_mysql
.. _emq_auth_pgsql:       https://github.com/emqx/emqx_auth_pgsql
.. _emq_auth_redis:       https://github.com/emqx/emqx_auth_redis
.. _emq_auth_mongo:       https://github.com/emqx/emqx_auth_mongo
.. _emq_reloader:         https://github.com/emqx/emqx_reloader
.. _emq_stomp:            https://github.com/emqx/emqx_stomp
.. _emq_recon:            https://github.com/emqx/emqx_recon
.. _emq_sn:               https://github.com/emqx/emqx_sn
.. _emq_coap:             https://github.com/emqx/emqx_coap
.. _emq_delayed_publish:  https://github.com/emqx/emqx_delayed_publish
