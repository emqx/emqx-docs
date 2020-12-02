---
# 标题
title: 飞行队列与消息队列
# 编写日期
date: 2020-02-25 17:15:26
# 作者 Github 名称
author: tigercl
# 关键字
keywords:
# 描述
description:
# 分类
category: 
# 引用
ref: undefined
---

# Inflight and Queue

## Introduction

To improve message throughput efficiency and reduce the impact of network fluctuations, EMQ X Broker allows multiple unacknowledged QoS 1 and QoS 2 packets to exist on the network link at the same time. These sent but unconfirmed packets will be stored in the Inflight Window until acknowledgment is complete.

When the number of concurrently existing packets in the network exceeds the limit, that is, the length limit of Inflight Window is reached(see `max_inflight`), EMQ X Broker will no longer send subsequent messages, but will store these packets in the Message Queue. Once a message is acknowledged in the Inflight Window, the message in the Message Queue will be sent in first-in, first-out order and stored in the Inflight Window.

When the client is offline, Message Queue is also used to store QoS 0 messages, which will be sent for the next time when the client is online. This feature is enabled by default, but you can also disable it manually. You can see `mqueue_store_qos0` for details.

It should be noted that if the length limit of Message Queue is also reached, subsequent packets will still be buffered to the Message Queue, but the first buffered message in the corresponding message queue will be discarded. If there are QoS 0 messages in the queue, the QoS 0 messages will be discarded first. Therefore, it is very important to configure a suitable Message Queue length limit (see `max_mqueue_len`) according to your actual situation.

## Inflight Queue and Receive Maximum

The MQTT v5.0 protocol adds a `Receive Maximum`  attribute to CONNECT packets, and the official explanation for it is:

The client uses this value to limit the maximum number of published messages with a QoS of1 and a QoS of 2 that the client is willing to process simultaneously. There is no mechanism to limit the published messages with a QoS of 0 that the server is trying to send.

That is, the server can send subsequent PUBLISH packets to the client with different message identifiers while waiting for acknowledgment, until the number of unacknowledged messages reaches the `Receive Maximum` limit.

It is not difficult to see that `Receive Maximum` is actually the same as the Inflight Window mechanism in EMQ X Broker. However, EMQ X already provided this function to the accessed MQTT client before the MQTT v5.0 protocol was released. Now, the clients using the MQTT v5.0 protocol will set the maximum length of the Inflight Window according to the specification of the Receive Maximum, while clients with earlier versions of the MQTT protocol will still set it according to the configuration.

## Configuration items

| Configuration items | Type    | Optional value  | Default value                              | Description                                                  |
| ------------------- | ------- | --------------- | ------------------------------------------ | ------------------------------------------------------------ |
| max_inflight        | integer | >= 0            | 32 *(external)*,<br /> 128 *(internal)*    | Inflight Window length limit, 0 means no limit               |
| max_mqueue_len      | integer | >= 0            | 1000 *(external)*,<br />10000 *(internal)* | Message Queue length limit, 0 means no limit                 |
| mqueue_store_qos0   | enum    | `true`, `false` | true                                       | Whether the EMQ X Broker store QoS 0 messages to the Message Queue when the client is offline |







