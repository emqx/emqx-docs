---
# 标题
title: $SYS 系统主题
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

# $SYS 系统主题

EMQ X Broker 周期性发布自身运行状态、消息统计、客户端上下线事件到 以 $SYS/ 开头系统主题。

$SYS 主题路径以 $SYS/brokers/{node}/ 开头。 {node} 是指产生该 事件 / 消息 所在的节点名称，例如:

```
$SYS/brokers/emqx@127.0.0.1/version
$SYS/brokers/emqx@127.0.0.1/uptime
```
默认只允许 localhost 的 MQTT 客户端订阅 $SYS 主题，可通过 etc/acl.config 修改访问控制规则。

$SYS 系统消息发布周期，通过 etc/emqx.conf 配置:

```
## System interval of publishing $SYS messages.
##
## Value: Duration
## Default: 1m, 1 minute
broker.sys_interval = 1m
```

### 集群状态信息

| 主题                          | 说明                 |
| ----------------------------- | -------------------- |
| $SYS/brokers                  | 集群节点列表         |
| $SYS/brokers/\${node}/version  | EMQ X Broker 版本     |
| $SYS/brokers/\${node}/uptime   | EMQ X Broker 运行时间 |
| $SYS/brokers/\${node}/datetime | EMQ X Broker 系统时间     |
| $SYS/brokers/\${node}/sysdescr | EMQ X Broker 描述     |

### 客户端上下线事件

\$SYS 主题前缀: \$SYS/brokers/\${node}/clients/

| 主题 (Topic)              | 说明                                     |
| ------------------------ | ---------------------------------------- |
| ${clientid}/connected    | 上线事件。当某客户端上线时，会发布该消息 |
| ${clientid}/disconnected | 下线事件。当某客户端离线时，会发布该消息 |

‘connected’ 事件消息的 Payload 可解析成 JSON 格式:

```
{
    "clientid":"id1",
    "username":"u",
    "ipaddress":"127.0.0.1",
    "connack":0,
    "ts":1554047291,
    "proto_ver":3,
    "proto_name":"MQIsdp",
    "clean_start":true,
    "keepalive":60
}
```

‘disconnected’ 事件消息的 Payload 可解析成 JSON 格式:

```
{
    "clientid":"id1",
    "username":"u",
    "reason":"normal",
    "ts":1554047291
}
```

### 系统统计 (Statistics)

系统主题前缀: \$SYS/brokers/\${node}/stats/

#### 客户端统计

| 主题 (Topic)       | 说明           |
| ----------------- | -------------- |
| connections/count | 当前客户端总数 |
| connections/max   | 最大客户端数量 |

#### 会话统计

| 主题 (Topic)               | 说明             |
| ------------------------- | ---------------- |
| sessions/count            | 当前会话总数     |
| sessions/max              | 最大会话数量     |
| sessions/persistent/count | 当前持久会话总数 |
| sessions/persistent/max   | 最大持久会话数量 |

#### 订阅统计

| 主题 (Topic)                | 说明             |
| -------------------------- | ---------------- |
| suboptions/count           | 当前订阅选项个数 |
| suboptions/max             | 最大订阅选项总数 |
| subscribers/max            | 最大订阅者总数   |
| subscribers/count          | 当前订阅者数量   |
| subscriptions/max          | 最大订阅数量     |
| subscriptions/count        | 当前订阅总数     |
| subscriptions/shared/count | 当前共享订阅个数 |
| subscriptions/shared/max   | 当前共享订阅总数 |

#### 主题统计

| 主题 (Topic)  | 说明            |
| ------------ | --------------- |
| topics/count | 当前 Topic 总数 |
| topics/max   | 最大 Topic 数量 |

#### 路由统计

| 主题 (Topic)  | 说明             |
| ------------ | ---------------- |
| routes/count | 当前 Routes 总数 |
| routes/max   | 最大 Routes 数量 |

`topics/count` 和 `topics/max` 与 `routes/count` 和 `routes/max` 数值上是相等的。

### 收发流量 / 报文 / 消息统计

系统主题 (Topic) 前缀: \$SYS/brokers/\${node}/metrics/

#### 收发流量统计

| 主题 (Topic)    | 说明         |
| -------------- | ------------ |
| bytes/received | 累计接收流量 |
| bytes/sent     | 累计发送流量 |

#### MQTT 报文收发统计

| 主题 (Topic)                 | 说明                           |
| --------------------------- | ------------------------------ |
| packets/received            | 累计接收 MQTT 报文             |
| packets/sent                | 累计发送 MQTT 报文             |
| packets/connect             | 累计接收 MQTT CONNECT 报文     |
| packets/connack             | 累计发送 MQTT CONNACK 报文     |
| packets/publish/received    | 累计接收 MQTT PUBLISH 报文     |
| packets/publish/sent        | 累计发送 MQTT PUBLISH 报文     |
| packets/puback/received     | 累计接收 MQTT PUBACK 报文      |
| packets/puback/sent         | 累计发送 MQTT PUBACK 报文      |
| packets/puback/missed       | 累计丢失 MQTT PUBACK 报文      |
| packets/pubrec/received     | 累计接收 MQTT PUBREC 报文      |
| packets/pubrec/sent         | 累计发送 MQTT PUBREC 报文      |
| packets/pubrec/missed       | 累计丢失 MQTT PUBREC 报文      |
| packets/pubrel/received     | 累计接收 MQTT PUBREL 报文      |
| packets/pubrel/sent         | 累计发送 MQTT PUBREL 报文      |
| packets/pubrel/missed       | 累计丢失 MQTT PUBREL 报文      |
| packets/pubcomp/received    | 累计接收 MQTT PUBCOMP 报文     |
| packets/pubcomp/sent        | 累计发送 MQTT PUBCOMP 报文     |
| packets/pubcomp/missed      | 累计丢失 MQTT PUBCOMP 报文     |
| packets/subscribe           | 累计接收 MQTT SUBSCRIBE 报文   |
| packets/suback              | 累计发送 MQTT SUBACK 报文      |
| packets/unsubscribe         | 累计接收 MQTT UNSUBSCRIBE 报文 |
| packets/unsuback            | 累计发送 MQTT UNSUBACK 报文    |
| packets/pingreq             | 累计接收 MQTT PINGREQ 报文     |
| packets/pingresp            | 累计发送 MQTT PINGRESP 报文    |
| packets/disconnect/received | 累计接收 MQTT DISCONNECT 报文  |
| packets/disconnect/sent     | 累计接收 MQTT DISCONNECT 报文  |
| packets/auth                | 累计接收 Auth 报文             |

#### MQTT 消息收发统计

| 主题 (Topic)            | 说明               |
| ---------------------- | ------------------ |
| messages/received      | 累计接收消息       |
| messages/sent          | 累计发送消息       |
| messages/expired       | 累计发送消息       |
| messages/retained      | Retained 消息总数  |
| messages/dropped       | 丢弃消息总数       |
| messages/forward       | 节点转发消息总数   |
| messages/qos0/received | 累计接受 QoS0 消息 |
| messages/qos0/sent     | 累计发送 QoS0 消息 |
| messages/qos1/received | 累计接受 QoS1 消息 |
| messages/qos1/sent     | 累计发送 QoS1 消息 |
| messages/qos2/received | 累计接受 QoS2 消息 |
| messages/qos2/sent     | 累计发送 QoS2 消息 |
| messages/qos2/expired  | QoS2 过期消息总数  |
| messages/qos2/dropped  | QoS2 丢弃消息总数  |

### Alarms - 系统告警

系统主题 (Topic) 前缀: \$SYS/brokers/\${node}/alarms/

| 主题 (Topic) | 说明         |
| ----------- | ------------ |
| alert       | 新产生的告警 |
| clear       | 被清除的告警 |

### Sysmon - 系统监控

系统主题 (Topic) 前缀: $SYS/brokers/\${node}/sysmon/

| 主题 (Topic)    | 说明              |
| -------------- | ----------------- |
| long_gc        | GC 时间过长警告   |
| long_schedule  | 调度时间过长警告  |
| large_heap     | Heap 内存占用警告 |
| busy_port      | Port 忙警告       |
| busy_dist_port | Dist Port 忙警告  |