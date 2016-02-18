
=====================
快速开始(Get Started)
=====================

--------------------
emqttd消息服务器简介
--------------------

emqttd是采用Erlang语言开发的MQTT消息服务器。Erlang/OTP是非常出色的软实时(Soft-Realtime)、低延时(Low-Latency)、分布式(Distributed)的语言平台。MQTT是非常轻量的(Lightweight)、发布订阅模式(PubSub)的物联网移动互联网消息协议。

emqttd


emqttd is a massively scalable and clusterable MQTT V3.1/V3.1.1 broker written in Erlang/OTP. emqttd support both MQTT V3.1/V3.1.1 protocol specification with extended features.

emqttd could connect Sensor, Mobile, Web Browser and Application Server with asynchronous PUB/SUB MQTT messsages.




emqttd is aimed to provide a solid, enterprise grade, extensible open-source MQTT broker for IoT, M2M and Mobile applications that need to support ten millions of concurrent MQTT clients.

Easy to install
Massively scalable
Easy to extend
Solid stable




eMQTT是采用Erlang/OTP开发，基于MQTT协议的,发布订阅模式(Publish/Subscribe)模式消息服务器。

TODO: 发布订阅图。转移到index.rst???

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

