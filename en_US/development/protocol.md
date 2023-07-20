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

As a world-class MQTT broker, EMQX supports the [publish/subscribe messaging pattern](../advanced/mqtt-concepts.md#publish-subscribe-pattern), which is a key feature of the MQTT protocol. EMQX's publish/subscribe functionality offers a variety of features that make it well-suited for complex and high-performance messaging applications. These features include support for wildcard topics, topic-based message filtering, message persistence, and Quality of Service (QoS) settings. 

The publish function allows devices that are connected to the EMQX broker to send messages to a particular topic. The message can contain any type of data, such as sensor readings, status updates, or commands. When a device publishes a message to a topic, EMQX receives the message and forwards it to all devices that have subscribed to that topic.

The subscribe function in EMQX allows devices to receive messages from a particular topic. Devices can subscribe to one or more topics, and they will receive all messages that are published on those topics. This allows devices to monitor specific events or data streams in real-time, without having to constantly poll for updates.

<img src="./_assets/pub-sub-pattern.png" alt="pub-sub-pattern" style="zoom:50%;" />

