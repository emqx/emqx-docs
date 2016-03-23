
.. _mqtt:

========
MQTT协议
========

------------------------
MQTT轻量发布订阅消息协议
------------------------

MQTT是轻量发布订阅模式消息协议

MQTT V3.1/V3.1.1协议规范(IBM)

发布订阅模式(Publish/Subscribe)

基于Topic消息路由(Topic based Subscription Model)

MQTT PubSub模式介绍

---------------------------
MQTT基于主题(Topic)消息路由
---------------------------

基于主题(Topic)进行消息路由。

publish a/b/c

subscribe a/b/#

--------------------------------
MQTT Topic Name, Filter
--------------------------------

基于Topic的PubSub消息路由

Publish to Topic Name:
“chat/room/1”
“sensor/10/temperature”
“$SYS/broker/metrics/packets/received”

Subscribe Topic Filter:
“chat/room/1”
“sensor/+/temperature”
“$SYS/broker/metrics/#”

------------------------
MQTT V3.1.1协议报文格式
------------------------


CONNECT 发起连接    CONNACK 连接回执

PUBLISH 发布消息    PUBACK  发布回执

PUBREC  QoS2消息回执    PUBREL  QoS消息释放

PUBCOMP QoS2消息完成    DISCONNECT  断开连接

SUBSCRIBE   订阅Topic   SUBACK  订阅回执

UNSUBSCRIBE 取消订阅    UNSUBACK    取消订阅回执

PINGREQ PING请求    PINGRESP    PING响应


--------------------------------
MQTT消息QoS
--------------------------------

Qos0

Qos1, 

Qos2


QoS 0,1,2 Messages

--------------------------------
MQTT连接会话(Session)
--------------------------------

Clean Session Flag

Transient Session

Persistent Session

Offline Message


Transient, Persistent Sessions

--------------------------------
MQTT连接保活心跳
--------------------------------

CONNECT报文KeepAlive参数
PINGREQ 2字节心跳报文
XMPP KeepAlive???
KeepAlive and Two Bytes Heartbeat

--------------------------------
MQTT协议-Last Will Message
--------------------------------

Last Will

-------------------------
MQTT协议-Retained Message
-------------------------
Retained Message

--------------------------------
MQTT协议-WebSocket连接
--------------------------------

Binary mode frame over WebSocket

PubSub on Web Browser

Firefox, Safari, Chrome, Opera…

IE Sucks?

Better than Socket.IO?

MQTT Over WebSocket

----------------
MQTT协议客户端库
----------------

TODO: 客户端库table...

CocoaMQTT：Swift语言MQTT客户端库

QMQTT：QT框架MQTT客户端库

按功能介绍

--------------------------------
MQTT与XMPP协议对比
--------------------------------

轻量、简单

路由方式灵活，比如群组聊天

Throughput capacity: less overhead, more lightweight

Binary vs plain text

QoS in place (Fire-and-forget, At-least-once and Exactly-once)

Pub/Sub in place (XMPP requires extension XEP- 0060)

No need for an XML parser

--------------------------------
MQTT应用-Mobile, IoT, M2M…
--------------------------------

Android Push

Mobile Chat(Facebook Messenger)

物联网(IoT, M2M)、智能硬件、车联网...

行业市场(电力、石油、能源…)

