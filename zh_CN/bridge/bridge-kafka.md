# Kafka 桥接

EMQ X 桥接转发 MQTT 消息到 Kafka 集群，Apache Kafka是一个快速、高可扩展、高吞吐的分布式日志系统，配合kafka Stream，在流式数据处理中非常常用。

![image](./assets/bridge_kafka.png)

Kafka 桥接插件配置文件: etc/plugins/emqx_bridge_kafka.conf。

## 配置 Kafka 集群地址

```bash
## Kafka 服务器地址
## bridge.kafka.servers = 127.0.0.1:9092,127.0.0.2:9092,127.0.0.3:9092
bridge.kafka.servers = 127.0.0.1:9092

## Kafka 分区策略。可选值: per_partition | per_broker
bridge.kafka.connection_strategy = per_partition

bridge.kafka.min_metadata_refresh_interval = 5S

## Produce 写类型。可选值: sync | async
bridge.kafka.produce = sync

bridge.kafka.produce.sync_timeout = 3S

## 指定 replayq 在磁盘上存储消息的基本目录。
## 如果该配置项缺失活着设置为 undefined, replayq 将以使用内存的
## 的方式工作。也就是说，消息不在磁盘上排队 -- 在这种情况下，send
## 和 send_async API 的调用者负责处理在应用程序、网络或 kafka
## 干扰时可能丢失的消息。
## bridge.kafka.replayq_dir = /tmp/emqx_bridge_kafka/

## default=10MB, replayq 分段大小。
## bridge.kafka.producer.replayq_seg_bytes = 10MB

## producer required_acks. 可选值: all_isr | leader_only | none.
bridge.kafka.producer.required_acks = none

## default=10000. leader 在回复 producer 前等待副本的超时时间。
bridge.kafka.producer.ack_timeout = 10S

## 收集到一次 produce 请求中的最大字节数
bridge.kafka.producer.max_batch_bytes = 1024KB

## 收集到一次 produce 请求中的最少字节数
bridge.kafka.producer.min_batch_bytes = 0

## 在没有接收到上次请求的 ack 的情况下，可以提前发送的 batch 数。
## 如果消息必须严格按照顺序传递，则必须为0。
bridge.kafka.producer.max_send_ahead = 0

## 默认为无压缩
## bridge.kafka.producer.compression = no_compression

## 默认值为 base64, 可选值: base64 | plain
## bridge.kafka.encode_payload_type = base64

## bridge.kafka.sock.buffer = 32KB
## bridge.kafka.sock.recbuf = 32KB
bridge.kafka.sock.sndbuf = 1MB
## bridge.kafka.sock.read_packets = 20
```

## 配置 Kafka 桥接规则

```bash
## Bridge Kafka Hooks
## ${topic}: the kafka topics to which the messages will be published.
## ${filter}: the mqtt topic (may contain wildcard) on which the action will be performed.

## Client Connected Record Hook
bridge.kafka.hook.client.connected.1     = {"topic": "client_connected"}

## Client Disconnected Record Hook
bridge.kafka.hook.client.disconnected.1  = {"topic": "client_disconnected"}

## Session Subscribed Record Hook
bridge.kafka.hook.session.subscribed.1   = {"filter": "#",  "topic": "session_subscribed"}

## Session Unsubscribed Record Hook
bridge.kafka.hook.session.unsubscribed.1 = {"filter": "#",  "topic": "session_unsubscribed"}

## Message Publish Record Hook
bridge.kafka.hook.message.publish.1      = {"filter": "#",  "topic": "message_publish"}

## Message Delivered Record Hook
bridge.kafka.hook.message.delivered.1    = {"filter": "#",  "topic": "message_delivered"}

## Message Acked Record Hook
bridge.kafka.hook.message.acked.1        = {"filter": "#",  "topic": "message_acked"}

## More Configures
## partitioner strategy:
## Option:  random | roundrobin | first_key_dispatch
## Example: bridge.kafka.hook.message.publish.1 = {"filter":"#", "topic":"message_publish", "strategy":"random"}

## key:
## Option: ${clientid} | ${username}
## Example: bridge.kafka.hook.message.publish.1 = {"filter":"#", "topic":"message_publish", "key":"${clientid}"}

## format:
## Option: json | json
## Example: bridge.kafka.hook.message.publish.1 = {"filter":"#", "topic":"message_publish", "format":"json"}
```

## Kafka 桥接规则说明

| 事件                                       | 说明           |
| ---------------------------------------- | ------------ |
| bridge.kafka.hook.client.connected.1     | 客户端登录        |
| bridge.kafka.hook.client.disconnected.1  | 客户端退出        |
| bridge.kafka.hook.session.subscribed.1   | 订阅主题         |
| bridge.kafka.hook.session.unsubscribed.1 | 取消订阅主题       |
| bridge.kafka.hook.message.publish.1      | 发布消息         |
| bridge.kafka.hook.message.delivered.1    | delivered 消息 |
| bridge.kafka.hook.message.acked.1        | ACK 消息       |

## 客户端上下线事件转发 Kafka

设备上线 EMQ X 转发上线事件消息到 Kafka:

```bash
topic = "client_connected",
value = {
  "client_id": ${clientid},
  "username": ${username},
  "node": ${node},
  "ts": ${ts}
}
```

设备下线 EMQ X 转发下线事件消息到 Kafka:

```bash
topic = "client_disconnected",
value = {
  "client_id": ${clientid},
  "username": ${username},
  "reason": ${reason},
  "node": ${node},
  "ts": ${ts}
}
```

## 客户端订阅主题事件转发 Kafka

```bash
topic = session_subscribed

value = {
  "client_id": ${clientid},
  "topic": ${topic},
  "qos": ${qos},
  "node": ${node},
  "ts": ${timestamp}
}
```

## 客户端取消订阅主题事件转发 Kafka

```bash
topic = session_unsubscribed

value = {
  "client_id": ${clientid},
  "topic": ${topic},
  "qos": ${qos},
  "node": ${node},
  "ts": ${timestamp}
}
```

## MQTT 消息转发到 Kafka

```bash
topic = message_publish

value = {
  "client_id": ${clientid},
  "username": ${username},
  "topic": ${topic},
  "payload": ${payload},
  "qos": ${qos},
  "node": ${node},
  "ts": ${timestamp}
}
```

## MQTT 消息派发 (Deliver) 事件转发 Kafka

```bash
topic = message_delivered

value = {
  "client_id": ${clientid},
  "username": ${username},
  "from": ${fromClientId},
  "topic": ${topic},
  "payload": ${payload},
  "qos": ${qos},
  "node": ${node},
  "ts": ${timestamp}
}
```

## MQTT 消息确认 (Ack) 事件转发 Kafka

```bash
topic = message_acked

value = {
  "client_id": ${clientid},
  "username": ${username},
  "from": ${fromClientId},
  "topic": ${topic},
  "payload": ${payload},
  "qos": ${qos},
  "node": ${node},
  "ts": ${timestamp}
}
```

## Kafka 消费示例

Kafka 读取 MQTT 客户端上下线事件消息:

```bash
kafka-console-consumer.sh --bootstrap-server 127.0.0.1:9092 --topic client_connected --from-beginning
    
kafka-console-consumer.sh --bootstrap-server 127.0.0.1:9092 --topic client_disconnected --from-beginning
```

Kafka 读取 MQTT 主题订阅事件消息:

```bash
kafka-console-consumer.sh --bootstrap-server 127.0.0.1:9092 --topic session_subscribed --from-beginning

kafka-console-consumer.sh --bootstrap-server 127.0.0.1:9092 --topic session_unsubscribed --from-beginning
```

Kafka 读取 MQTT 发布消息:

```bash
kafka-console-consumer.sh --bootstrap-server 127.0.0.1:9092 --topic message_publish --from-beginning
```

Kafka 读取 MQTT 消息发布 (Deliver)、确认 (Ack)事件:

```bash
kafka-console-consumer.sh --bootstrap-server 127.0.0.1:9092 --topic message_delivered --from-beginning

kafka-console-consumer.sh --bootstrap-server 127.0.0.1:9092 --topic message_acked --from-beginning
```

::: tip
默认 payload 被 base64 编码，可通过修改配置 bridge.kafka.encode_payload_type 指定
payload 数据格式。
:::

## 启用 Kafka 桥接插件

```bash
./bin/emqx_ctl plugins load emqx_bridge_kafka
```
