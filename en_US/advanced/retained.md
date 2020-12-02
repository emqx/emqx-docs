---
# 标题
title: Retained
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

# Retained

## Introduction

When the server receives a PUBLISH packet with a Retain flag of 1, it will treat the message as a retained message. In addition to being normally forwarded, the retained message will be stored on the server. There can only be one retained message under each topic. Therefore, if there is already a retained message of the same topic, the retained message is replaced.

When a client establishes a subscription, and there are retained messages with matching topics on the server, these retained messages will be sent to the client immediately. With retained messages, new subscribers can immediately get the latest status without waiting for unexpected time, which is very important in many scenarios.

EMQ X Broker enables the function of retaining messages by default. You can modify `mqtt.retain_available` to `false` in  `etc/emqx.conf` to disable the function of retaining messages. If the EMQ X Broker still receives the retained message when the function is disabled, it will return the DISCONNECT packet with a reason code of 0x9A (retained message is not supported).

## Configuration

EMQ X Broker's retained message function is implemented by the `emqx_retainer` plugin, which is enabled by default. By modifying the configuration of the ` emqx_retainer` plugin, you can adjust the storage location of the retained messages in EMQ X Broker, limit the number of received retained messages and the maximum length of Payload, and adjust the expiration time of retained messages. For more information about the EMQ X Broker plug-in, see  [plugin](advanced/plugins.md).

The `emqx_retainer` plugin is enabled by default, and the configuration path of the plugin is `etc/plugins/emqx_retainer.conf`.

| Configuration item       | Type  | Optional value      | Default value | Description                                               |
| ------------------------------ | -------- | ------------------------ | ------ | ------------------------------------------------------------ |
| retainer.storage_type          | enum     | `ram`, `disc`, `disc_only` | ram |ram: only stored in memory; <br /> disc: stored in memory and hard disk; <br /> disc_only: only stored in hard disk|
| retainer.max_retained_messages | integer  | \>= 0                    | 0      | The maximum number of retained messages, and 0 means no limit. After the number of retained messages exceeds the maximum limit, you can replace the existing retained messages, but cannot store retained messages for new topics. |
| retainer.max_payload_size      | bytesize |                          | 1MB    | Retain the maximum Payload value of the message. After the Payload value exceeds the maximum value, the EMQ X broker will treat the retained reserved message as a normal message. |
| retainer.expiry_interval       | duration |                          | ０     | The expiration time of retaining message, and 0 means never expire. If the message expiration interval is set in the PUBLISH packet, the message expiration interval in the PUBLISH packet shall prevail. |

::: tip
EMQ X Enterprise can store retained messages to various external databases
:::