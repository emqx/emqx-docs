---
# 标题
title: MQTT 客户端库
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
ref: undefined
---

# MQTT Client library

This article selects popular MQTT client libraries from various programming languages for introduction and description, and provides basic function code examples for connecting, publishing, subscribing, and unsubscribing.

- [MQTT C Client library](./c.md)
- [MQTT Java Client library](./java.md)
- [MQTT Go Client library](./go.md)
- [MQTT Erlang Client library](./erlang.md)
- [MQTT JavaScript Client library](./javascript.md)
- [MQTT Python Client library](./python.md)

The MQTT community contains a complete list of MQTT client libraries. This chapter provides connection examples and support analysis for each popular library. You can [click here](https://github.com/mqtt/mqtt.github.io/wiki/libraries) to read it.


## MQTT Client life cycle

The behavior of the MQTT client throughout the life cycle can be summarized as establishing a connection, subscribing to a topic, receiving and processing messages, publishing messages to a specified topic, unsubscribing, and disconnecting.

The standard client library shows the corresponding method in each link. The meaning of the method parameters required by different libraries in the same link is roughly the same. In terms of which parameters to choose and which features to enable, it requires users to understand the MQTT protocol features in depth and combine them with actual application scenarios.

Taking a client to connect, publish, and process messages as an example, the steps generally required for each link are:

- **Create a connection:**
  - Specify MQTT Broker basic information access address and port
  - Specify whether the transmission type is TCP or MQTT over WebSocket
  - If TLS is enabled, you need to select the protocol version and carry the corresponding certificate
  - If Broker enables authentication, the client needs to carry the corresponding MQTT Username Password information
  - Configure client parameters such as keepalive duration, clean session callback retain flag, MQTT protocol version, will message (LWT), etc.
  
- **Subscribe to a topic**: You can subscribe to the topic after the connection is established successfully, and you need to specify the topic information
  - Specify topic filter, support topic wildcards `+` and `#`  when subscribing
  - Specify QoS, and the option of Qos 0 1 2 can be selected according to the client library and broker implementation. Please note that some brokers and cloud service providers do not support some QoS levels. For example, AWS IoT, Alibaba Cloud IoT Suite, and Azure IoT Hub do not support QoS 2 level 
  - Topic subscription may fail due to network problems, Broker side ACL rules restrictions

- **Receive messages and process:**
- Generally, the processing function is specified at the time of connection. This processing method is slightly different according to the network programming model of the client library and the platform.
  
- **Publish a message:** Publish a message to a specified topic
  - Specify the target topic. Note that the topic cannot contain wildcards `+` or `#`. If the topic contains wildcards, it may result in failure of message publishing and client disconnection (depending on the implementation of Broker and client library)
  - Specify the message QoS level. There are also different QoS levels supported by the Broker and the platform. For example, if the Azure IoT Hub releases a QoS 2 message, the client connection will be disconnected.
  - Specify the message payload, which cannot exceed the maximum message size set by Broker
  - Specify the retained message flag

- **Unsubscribe**
- Just specify the target topic
  
- **Disconnect：**
- The client actively disconnects, and the server publishes a will message (LWT)

