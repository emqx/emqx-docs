
=====================
快速开始(Get Started)
=====================

--------------------
emqttd消息服务器简介
--------------------

emqttd(Erlang MQTT Broker)是采用Erlang语言开发的开源MQTT消息服务器。Erlang/OTP是非常出色的软实时(Soft-Realtime)、低延时(Low-Latency)、分布式(Distributed)的语言平台。MQTT是非常轻量的(Lightweight)、发布订阅模式(PubSub)的物联网移动互联网消息协议。

emqttd设计目标是承载移动终端或物联网终端大量的MQTT连接，并实现在大量终端间快速低延时(Low-Latency)的消息路由。emqttd设计不同于传统的企业消息服务器(例如JMS, AMQP)，企业消息服务器主要处理少量连接下高吞吐(Throughput)的消息。

emqttd消息服务器设计目标与应用场景：

1. 稳定承载大量的客户端连接。单服务器节点支持50万到100万的MQTT连接。

2. 分布集群的多节点间，快速低延时的消息路由，单集群支持1000万规模的路由。

3. 消息服务器内插件扩展，可以扩展定制多种认证方式、高效存储消息到后端数据库。

4. 多协议支持，除完整支持MQTT V3.1.1协议，扩展支持WebSocket、Stomp、CoAP或私有TCP协议。


--------------------------

--------------------------

--------------------------
五分钟下载启动emqttd
--------------------------


--------------------------
Dashboard
--------------------------


------------------------
emqttd消息服务器功能列表
------------------------


完整的MQTT V3.1/V3.1.1 协议规范支持

保持简单架构，专注接入层与消息路由

Scalable, Scalable, Massively Scalable…

支持插件方式扩展认证与ACL，定制Push、移动IM、物联网等应用

MQTT, HTTP Publish, WebSocket, Stomp, SockJS，CoAP多协议接口

Full MQTT V3.1/V3.1.1 protocol specification support
QoS0, QoS1, QoS2 Publish and Subscribe
Session Management and Offline Messages
Retained Messages Support
Last Will Message Support
TCP/SSL Connection Support
MQTT Over Websocket(SSL) Support
HTTP Publish API Support
$SYS/brokers/# Support
Client Authentication with clientId, ipaddress
Client Authentication with username, password.
Client ACL control with ipaddress, clientid, username.
Cluster brokers on several servers.
Bridge brokers locally or remotely
500K+ concurrent clients connections per server
Extensible architecture with Hooks, Modules and Plugins
Passed eclipse paho interoperability tests

------------------------
emqttd项目扩展模块与插件
------------------------

emqttd_auth_clientid - Authentication with ClientIds
emqttd_auth_username - Authentication with Username and Password
emqttd_auth_ldap - Authentication with LDAP
emqttd_mod_presence - Publish presence message to $SYS topics when client connected or disconnected
emqttd_mod_autosub - Subscribe topics when client connected
emqttd_mod_rewrite - Topics rewrite like HTTP rewrite module

emqttd_plugin_template - Plugin template and demo
emqttd_dashboard - Web Dashboard
emqttd_plugin_mysql - Authentication with MySQL
emqttd_plugin_pgsql - Authentication with PostgreSQL
emqttd_plugin_kafka - Publish MQTT Messages to Kafka
emqttd_plugin_redis - Redis Plugin
emqttd_plugin_mongo - MongoDB Plugin
emqttd_stomp - Stomp Protocol Plugin
emqttd_sockjs - SockJS(Stomp) Plugin
emqttd_recon - Recon Plugin


-----------------
100万连接测试说明
-----------------


--------------------
emqttd项目的开发历史
--------------------


--------------------
emqtt其他开源库
--------------------

eSockd：通用的Erlang TCP服务端框架

emqttc：Erlang MQTT客户端库

emqtt_benchmark：MQTT连接测试工具

CocoaMQTT：Swift语言MQTT客户端库

QMQTT：QT框架MQTT客户端库

