
.. _getstarted:

======================
开始使用 (Get Started)
======================

.. _intro:

---------------------------
*EMQ X* R3.1 消息服务器简介
---------------------------

*EMQ X* (Erlang/Enterprise/Elastic MQTT Broker) 是基于 Erlang/OTP 平台开发的开源物联网 MQTT 消息服务器。Erlang/OTP 是出色的软实时(Soft-Realtime)、低延时(Low-Latency)、分布式(Distributed) 的语言平台。MQTT 是轻量的(Lightweight)、发布订阅模式(PubSub) 的物联网消息协议。

*EMQ X* 旨在承载海量的 移动/物联网/车载 等终端接入，并在海量连接间的快速、低延时的消息路由:

1. 稳定承载大规模的 MQTT 客户端连接，单服务器节点支持50万到100万连接。

2. 分布式节点集群，快速低延时的消息路由，单集群支持1000万规模的路由。

3. 消息服务器内扩展，支持定制多种认证方式、高效存储消息到后端数据库。

4. 完整物联网协议支持，MQTT、MQTT-SN、CoAP、LwM2M、私有 TCP/UDP 协议 支持。

.. _mqtt_pubsub:

---------------------
MQTT 发布订阅模式简述
---------------------

MQTT 是基于 发布(Publish)/订阅(Subscribe) 模式来进行通信及数据交换的，与 HTTP 此类的 请求(Request)/应答(Response) 的模式有本质的不同。

**订阅者(Subscriber)** 会向 **消息服务器(Broker)** 订阅一个 **主题(Topic)** 。 成功订阅后，消息服务器会将后续到达消息的主题与其匹配，成功匹配则将该消息发送给对应的所有订阅者。

主题(Topic) 格式类似 Unix 文件路径，'/' 进行为分隔符以代表不同的层级。包含通配符 '+' 或 '#' 的主题又称为 **主题过滤器(Topic Filters)**; 不含通配符的称为 **主题名(Topic Names)** 例如::

    sensor/1/temperature

    chat/room/subject

    presence/user/feng

    sensor/1/#

    sensor/+/temperature

    uber/drivers/joe/inbox


.. NOTE:: 注: '+'通配一个层级，'#'通配多个层级(必须在末尾)。
.. NOTE:: 注: 发布者(Publisher) 只能向特定 '主题名称'(不支持通配符)发布消息，订阅者(Subscriber) 则可以通过订阅 '主题过滤器'(支持通配符) 来通配多个主题名称。

.. _features:

-------------------------------
*EMQ X* R3.1 消息服务器功能列表
-------------------------------

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
* 支持manual、mcast、dns、etcd、k8s等多种集群发现方式
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
* TLS/PSK 支持
* 规则引擎支持

.. _quick_start:

------------------
五分钟下载启动 EMQ
------------------

*EMQ X* 的每个版本都会发布 Ubuntu、CentOS、FreeBSD、Mac OS X、Windows 、openSUSE 平台程序包与 Docker 镜像。

下载地址: http://emqtt.com/downloads

程序包下载后，可直接解压启动运行，例如 Mac 平台:

.. code-block:: bash

    unzip emqx-macosx-v3.1.0.zip && cd emqx

    # 启动emqx
    ./bin/emqx start

    # 检查运行状态
    ./bin/emqx_ctl status

    # 停止emqx
    ./bin/emqx stop

*EMQ X* 成功启动后，MQTT 客户端可直接连接 1883 端口接入系统。启动运行日志输出在 log/ 目录。

此外， *EMQ X* 会默认加载 Dashboard 插件，启动 Web 管理控制台。用户可通过 Web 控制台，查看服务器运行状态、统计数据、连接(Connections)、会话(Session)、主题(Topic)、订阅(Subscription)、插件(Plugin)等。

控制台地址: http://127.0.0.1:18083，默认用户名: admin，密码：public

.. image:: ./_static/images/dashboard.png

.. _mqtt_clients:

--------------------
开源 MQTT 客户端项目
--------------------

GitHub: https://github.com/emqtt

+--------------------+----------------------+
| `emqttc`_          | Erlang MQTT客户端库  |
+--------------------+----------------------+
| `CocoaMQTT`_       | Swift语言MQTT客户端库|
+--------------------+----------------------+
| `QMQTT`_           | QT框架MQTT客户端库   |
+--------------------+----------------------+
| `emqtt_benchmark`_ | MQTT连接测试工具     |
+--------------------+----------------------+

Eclipse Paho: https://www.eclipse.org/paho/

MQTT.org: https://github.com/mqtt/mqtt.github.io/wiki/libraries

.. _emqttc:          https://github.com/emqtt/emqttc
.. _emqtt_benchmark: https://github.com/emqtt/emqtt_benchmark
.. _CocoaMQTT:       https://github.com/emqtt/CocoaMQTT
.. _QMQTT:           https://github.com/emqtt/qmqtt
