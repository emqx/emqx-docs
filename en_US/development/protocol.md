---
# 编写日期
date: 2020-02-07 17:15:26
# 作者 Github 名称
author: wivwiv
# 关键字
keywords:
# 描述
description:
# 分类
category: 
# 引用
ref: 
---

# Publish/Subscribe

## MQTT协议

### 概览

MQTT是一个轻量的发布订阅模式消息传输协议，专门针对低带宽和不稳定网络环境的物联网应用设计。

MQTT官网: <http://mqtt.org>

MQTT v3.1.1 协议规范:
<http://docs.oasis-open.org/mqtt/mqtt/v3.1.1/os/mqtt-v3.1.1-os.html>

MQTT v5.0 协议规范:
<https://docs.oasis-open.org/mqtt/mqtt/v5.0/mqtt-v5.0.html>

### 特点

1.  开放消息协议，简单易实现
2.  发布订阅模式，一对多消息发布
3.  基于TCP/IP网络连接
4.  1字节固定报头，2字节心跳报文，报文结构紧凑
5.  消息QoS支持，可靠传输保证

### 应用

MQTT协议广泛应用于物联网、移动互联网、智能硬件、车联网、电力能源等领域。

1.  物联网M2M通信，物联网大数据采集
2.  Android消息推送，WEB消息推送
3.  移动即时消息，例如Facebook Messenger
4.  智能硬件、智能家具、智能电器
5.  车联网通信，电动车站桩采集
6.  智慧城市、远程医疗、远程教育
7.  电力、石油与能源等行业市场

### MQTT基于主题(Topic)消息路由

MQTT协议基于主题(Topic)进行消息路由，主题(Topic)类似URL路径，例如:

```bash
chat/room/1

sensor/10/temperature

sensor/+/temperature

$SYS/broker/metrics/packets/received

$SYS/broker/metrics/#
```

主题(Topic)通过'/'分割层级，支持'+', '\#'通配符:
```
'+': 表示通配一个层级，例如a/+，匹配a/x, a/y
    
'#': 表示通配多个层级，例如a/#，匹配a/x, a/b/c/d
```
订阅者与发布者之间通过主题路由消息进行通信，例如采用mosquitto命令行发布订阅消息:

```bash
mosquitto_sub -t a/b/+ -q 1

mosquitto_pub -t a/b/c -m hello -q 1
```


订阅者可以订阅含通配符主题，但发布者不允许向含通配符主题发布消息。

### MQTT WebSocket连接

MQTT协议除支持TCP传输层外，还支持WebSocket作为传输层。通过WebSocket浏览器可以直连MQTT消息服务器，发布订阅模式与其他MQTT客户端通信。

MQTT协议的WebSocket连接，必须采用binary模式，并携带子协议Header:
```
Sec-WebSocket-Protocol: mqttv3.1 或 mqttv3.1.1
```
### MQTT 与 XMPP 协议对比

MQTT协议设计简单轻量、路由灵活，将在移动互联网物联网消息领域，全面取代PC时代的XMPP协议:

1.  MQTT协议一个字节固定报头，两个字节心跳报文，报文体积小编解码容易。XMPP协议基于繁重的XML，报文体积大且交互繁琐。
2.  MQTT协议基于主题(Topic)发布订阅模式消息路由，相比XMPP基于JID的点对点消息路由更为灵活。
3.  MQTT协议未定义报文内容格式，可以承载JSON、二进制等不同类型报文。XMPP协议采用XML承载报文，二进制必须Base64编码等处理。
4.  MQTT协议支持消息收发确认和QoS保证，XMPP主协议并未定义类似机制。MQTT协议有更好的消息可靠性保证。
