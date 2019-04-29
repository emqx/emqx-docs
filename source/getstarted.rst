
.. _getstarted:

======================
开始使用 (Get Started)
======================

.. _intro:

---------------------------
*EMQ X* R3.1 消息服务器简介
---------------------------

*EMQ X* (Erlang/Enterprise/Elastic MQTT Broker) 是基于 Erlang/OTP 平台开发的开源物联网 MQTT 消息服务器。Erlang/OTP 是出色的软实时(Soft-Realtime)、低延时(Low-Latency)、分布式(Distributed) 的语言平台。MQTT 是轻量的(Lightweight)、发布订阅模式(PubSub) 的物联网消息协议。

*EMQ X* 项目设计目标是承载移动终端或物联网终端海量 MQTT 连接，并实现在海量物联网设备间快速低延时消息路由:

1. 稳定承载大规模的 MQTT 客户端连接，单服务器节点支持50万到100万连接。

2. 分布式节点集群，快速低延时的消息路由，单集群支持1000万规模的路由。

3. 消息服务器内扩展，支持定制多种认证方式、高效存储消息到后端数据库。

4. 完整物联网协议支持，MQTT、MQTT-SN、CoAP、LwM2M、WebSocket 或私有协议支持。

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
* TLS/PSK 支持
* 规则引擎支持

.. _quick_start:

------------------
五分钟下载启动 EMQ
------------------

*EMQ X* R3.1 消息服务器每个版本，会发布 Ubuntu、CentOS、FreeBSD、Mac OS X、Windows 、openSUSE 平台程序包与 Docker 镜像。

下载地址: https://www.emqx.io/downloads/broker?osType=Linux

程序包下载后，可直接解压启动运行，例如 Mac 平台:

.. code-block:: bash

    unzip emqx-macosx-v3.1.0.zip && cd emqx

    # 启动emqx
    ./bin/emqx start

    # 检查运行状态
    ./bin/emqx_ctl status

    # 停止emqx
    ./bin/emqx stop

*EMQ X* 消息服务默认允许匿名认证，启动后 MQTT 客户端可连接1883端口，启动运行日志输出在 log/ 目录。

*EMQ X* 消息服务器启动后，会默认加载 Dashboard 插件，启动 Web 管理控制台。用户可通过 Web 控制台，查看服务器运行状态、统计数据、客户端(Client)、会话(Session)、主题(Topic)、订阅(Subscription)、插件(Plugin)。

控制台地址: http://127.0.0.1:18083，默认用户: admin，密码：public

.. image:: ./_static/images/dashboard.png

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
