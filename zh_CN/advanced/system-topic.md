# 系统主题

EMQX 周期性发布自身运行状态、消息统计、客户端上下线事件到以 `$SYS/` 开头系统主题。

$SYS 主题路径以 `$SYS/brokers/{node}/` 开头。`{node}` 是指产生该 `事件 / 消息` 所在的节点名称，例如:

```bash
$SYS/brokers/emqx@127.0.0.1/version
$SYS/brokers/emqx@127.0.0.1/uptime
```


$SYS 系统消息发布周期配置项：

```bash
broker.sys_interval = 1m
```

{% emqxce %}

::: tip
EMQX 默认**只允许**本机的 MQTT 客户端订阅 $SYS 主题，请参照 [ACL File](../security/authz/file.md) 修改发布订阅 ACL 规则。

{% endemqxce %}

EMQX 中 $SYS 主题中绝大部分数据都可以通过其他更耦合性更低的方式获取，
设备上下线状态可通过规则引擎获取。
:::

## 集群状态信息

| 主题                          | 说明                 |
| ----------------------------- | -------------------- |
| $SYS/brokers                  | 集群节点列表         |
| $SYS/brokers/\${node}/version  | EMQX 版本     |
| $SYS/brokers/\${node}/uptime   | EMQX 运行时间 |
| $SYS/brokers/\${node}/datetime | EMQX 系统时间     |
| $SYS/brokers/\${node}/sysdescr | EMQX 系统信息     |

## 客户端上下线事件

`$SYS` 主题前缀：`$SYS/brokers/${node}/clients/`

| 主题 (Topic)              | 说明                                     |
| ------------------------ | ---------------------------------------- |
| ${clientid}/connected    | 上线事件。当任意客户端上线时，EMQX 就会发布该主题的消息 |
| ${clientid}/disconnected | 下线事件。当任意客户端下线时，EMQX 就会发布该主题的消息 |

`connected` 事件消息的 Payload 解析成 JSON 格式如下:

```bash
{
    "username": "foo",
    "ts": 1625572213873,
    "sockport": 1883,
    "proto_ver": 4,
    "proto_name": "MQTT",
    "keepalive": 60,
    "ipaddress": "127.0.0.1",
    "expiry_interval": 0,
    "connected_at": 1625572213873,
    "connack": 0,
    "clientid": "emqtt-8348fe27a87976ad4db3",
    "clean_start": true
}
```

`disconnected` 事件消息的 Payload 解析成 JSON 格式如下:

```bash
{
    "username": "foo",
    "ts": 1625572213873,
    "sockport": 1883,
    "reason": "tcp_closed",
    "proto_ver": 4,
    "proto_name": "MQTT",
    "ipaddress": "127.0.0.1",
    "disconnected_at": 1625572213873,
    "clientid": "emqtt-8348fe27a87976ad4db3"
}
```

## 系统统计

系统主题前缀：`$SYS/brokers/${node}/stats/`

### 客户端统计

| 主题 (Topic)       | 说明           |
| ----------------- | -------------- |
| connections/count | 当前客户端总数 |
| connections/max   | 客户端数量历史最大值 |

### 订阅统计

| 主题                       | 说明             |
| -------------------------- | ---------------- |
| suboptions/count           | 当前订阅选项个数 |
| suboptions/max             | 订阅选项总数历史最大值 |
| subscribers/count          | 当前订阅者数量   |
| subscribers/max            | 订阅者总数历史最大值 |
| subscriptions/count        | 当前订阅总数     |
| subscriptions/max          | 订阅数量历史最大值 |
| subscriptions/shared/count | 当前共享订阅个数 |
| subscriptions/shared/max   | 当前共享订阅总数 |

### 主题统计

| 主题          | 说明            |
| ------------ | --------------- |
| topics/count | 当前 Topic 总数 |
| topics/max   | Topic 数量历史最大值 |

### 路由统计

| 主题          | 说明             |
| ------------ | ---------------- |
| routes/count | 当前 Routes 总数 |
| routes/max   | Routes 数量历史最大值 |

`topics/count` 和 `topics/max` 与 `routes/count` 和 `routes/max` 数值上是相等的。

## 收发流量、报文、消息统计

系统主题前缀：`$SYS/brokers/${node}/metrics/`

### 收发流量统计

| 主题            | 说明         |
| -------------- | ------------ |
| bytes/received | 累计接收流量 |
| bytes/sent     | 累计发送流量 |

### MQTT 报文收发统计

| 主题                         | 说明                           |
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
| packets/auth                | 累计接收 MQTT AUTH 报文             |

### MQTT 消息收发统计

| 主题                    | 说明               |
| ---------------------- | ------------------ |
| messages/received      | 累计接收消息       |
| messages/sent          | 累计发送消息       |
| messages/expired       | 累计过期消息       |
| messages/retained      | Retained 消息总数  |
| messages/dropped       | 丢弃消息总数       |
| messages/forward       | 节点转发消息总数   |
| messages/qos0/received | 累计接收 QoS 0 消息 |
| messages/qos0/sent     | 累计发送 QoS 0 消息 |
| messages/qos1/received | 累计接收 QoS 1 消息 |
| messages/qos1/sent     | 累计发送 QoS 1 消息 |
| messages/qos2/received | 累计接收 QoS 2 消息 |
| messages/qos2/sent     | 累计发送 QoS 2 消息 |
| messages/qos2/expired  | QoS 2 过期消息总数  |
| messages/qos2/dropped  | QoS 2 丢弃消息总数  |

## 系统告警

系统主题前缀：`$SYS/brokers/${node}/alarms/`

| 主题         | 说明         |
| ----------- | ------------ |
| alert       | 新产生的告警 |
| clear       | 被清除的告警 |

## 系统监控

系统主题前缀：`$SYS/brokers/${node}/sysmon/`

| 主题            | 说明              |
| -------------- | ----------------- |
| long_gc        | 垃圾回收耗时过长 |
| long_schedule  | 进程调度时间过长，占用调度器过多的时间片 |
| large_heap     | 进程内存占用过高 |
| busy_port      | 进程向某个繁忙的端口发送消息，进程被挂起 |
| busy_dist_port | 节点间通讯使用的分布式通讯端口繁忙，进程被挂起 |
