
=============
MQTT协议详解
==============


.. todo:: MQTT 介绍文档10篇

.. toctree::
    :maxdepth: 2

    overview

---------------------------------
MQTT协议 - 轻量PubSub消息服务协议
---------------------------------

MQTT V3.1/V3.1.1协议规范(IBM)

发布订阅模式(Publish/Subscribe)

基于Topic消息路由(Topic based Subscription Model)

QoS 0,1,2 Messages

Transient, Persistent Sessions

Last Will, Retained Message

KeepAlive and Two Bytes Heartbeat

MQTT Over WebSocket


MQTT PubSub模式介绍

publish a/b/c

subscribe a/b/#


--------------------------------
MQTT协议报文格式
--------------------------------


CONNECT 发起连接    CONNACK 连接回执

PUBLISH 发布消息    PUBACK  发布回执

PUBREC  QoS2消息回执    PUBREL  QoS消息释放

PUBCOMP QoS2消息完成    DISCONNECT  断开连接

SUBSCRIBE   订阅Topic   SUBACK  订阅回执

UNSUBSCRIBE 取消订阅    UNSUBACK    取消订阅回执

PINGREQ PING请求    PINGRESP    PING响应

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

--------------------------------
MQTT消息QoS
--------------------------------

Qos0

Qos1, 

Qos2


--------------------------------
MQTT连接会话(Session)
--------------------------------

Clean Session Flag

Transient Session

Persistent Session

Offline Message

--------------------------------
MQTT连接保活心跳
--------------------------------

CONNECT报文KeepAlive参数
PINGREQ 2字节心跳报文
XMPP KeepAlive???

--------------------------------
MQTT协议-Last Will, Retained Message
--------------------------------

Last Will
Retained Message


--------------------------------
MQTT协议-WebSocket连接
--------------------------------

Binary mode frame over WebSocket

PubSub on Web Browser

Firefox, Safari, Chrome, Opera…

IE Sucks?

Better than Socket.IO?

--------------------------------
MQTT应用-Mobile, IoT, M2M…
--------------------------------

Android Push

Mobile Chat(Facebook Messenger)

物联网(IoT, M2M)、智能硬件、车联网...

行业市场(电力、石油、能源…)


--------------------------------
MQTT与XMPP协议对比
--------------------------------

轻量、简单

路由方式灵活，比如群组聊天


----------------
MQTT协议客户端库
----------------

TODO: 客户端库table...

CocoaMQTT：Swift语言MQTT客户端库

QMQTT：QT框架MQTT客户端库

按功能介绍

