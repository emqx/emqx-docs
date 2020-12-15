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


## MQTT 桥接

EMQ X 桥接转发 MQTT 消息到 MQTT Broker，支持桥接至常见 MQTT 云服务:

![image](./assets/bridge_mqtt.png)

mqtt bridge 桥接插件配置文件: etc/plugins/emqx_bridge_mqtt.conf。

### 配置 MQTT 桥接的 Broker 地址

```bash
## 桥接地址： 使用节点名则用于 rpc 桥接，使用 host:port 用于 mqtt 连接
bridge.mqtt.aws.address = 127.0.0.1:1883

## 桥接的协议版本
## 枚举值: mqttv3 | mqttv4 | mqttv5
bridge.mqtt.aws.proto_ver = mqttv4

## mqtt 连接是否启用桥接模式
bridge.mqtt.aws.bridge_mode = true

## mqtt 客户端的 client_id
bridge.mqtt.aws.client_id = bridge_aws

## mqtt 客户端的 clean_start 字段
## 注: 有些 MQTT Broker 需要将 clean_start 值设成 `true`
bridge.mqtt.aws.clean_start = true

## mqtt 客户端的 username 字段
bridge.mqtt.aws.username = user

## mqtt 客户端的 password 字段
bridge.mqtt.aws.password = passwd

## mqtt 客户端是否使用 ssl 来连接远程服务器
bridge.mqtt.aws.ssl = off

## 客户端 SSL 连接的 CA 证书 (PEM格式)
bridge.mqtt.aws.cacertfile = etc/certs/cacert.pem

## 客户端 SSL 连接的 SSL 证书
bridge.mqtt.aws.certfile = etc/certs/client-cert.pem

## 客户端 SSL 连接的密钥文件
bridge.mqtt.aws.keyfile = etc/certs/client-key.pem

## SSL 加密算法
bridge.mqtt.aws.ciphers = ECDHE-ECDSA-AES256-GCM-SHA384,ECDHE-RSA-AES256-GCM-SHA384

## TLS PSK 的加密算法
## 注意 'listener.ssl.external.ciphers' 和 'listener.ssl.external.psk_ciphers' 不能同时配置
##
## See 'https://tools.ietf.org/html/rfc4279#section-2'.
bridge.mqtt.aws.psk_ciphers = PSK-AES128-CBC-SHA,PSK-AES256-CBC-SHA,PSK-3DES-EDE-CBC-SHA,PSK-RC4-SHA

## 客户端的心跳间隔
bridge.mqtt.aws.keepalive = 60s

## 支持的 TLS 版本
bridge.mqtt.aws.tls_versions = tlsv1.2,tlsv1.1,tlsv1
```

### 配置 MQTT 桥接转发和订阅主题

```bash
## 桥接的 mountpoint(挂载点)
bridge.mqtt.aws.mountpoint = bridge/aws/${node}/

## 转发消息的主题
bridge.mqtt.aws.forwards = topic1/#,topic2/#

## 用于桥接的订阅主题
bridge.mqtt.aws.subscription.1.topic = cmd/topic1

## 用于桥接的订阅 qos
bridge.mqtt.aws.subscription.1.qos = 1

## 用于桥接的订阅主题
bridge.mqtt.aws.subscription.2.topic = cmd/topic2

## 用于桥接的订阅 qos
bridge.mqtt.aws.subscription.2.qos = 1
```

### MQTT 桥接转发和订阅主题说明

挂载点 Mountpoint: mountpoint 用于在转发消息时加上主题前缀，该配置选项须配合 forwards 使用，转发主题为
sensor1/hello 的消息, 到达远程节点时主题为 bridge/aws/emqx1@192.168.1.1/sensor1/hello。

转发主题 Forwards: 转发到本地 EMQX 指定 forwards 主题上的消息都会被转发到远程 MQTT Broker 上。

订阅主题 Subscription: 本地 EMQX 通过订阅远程 MQTT Broker 的主题来将远程 MQTT Broker
上的消息同步到本地。

### 启用 bridge_mqtt 桥接插件

```bash
./bin/emqx_ctl plugins load emqx_bridge_mqtt
```

### 桥接 CLI 命令

```bash
$ cd emqx1/ && ./bin/emqx_ctl bridges
bridges list                                    # List bridges
bridges start <Name>                            # Start a bridge
bridges stop <Name>                             # Stop a bridge
bridges forwards <Name>                         # Show a bridge forward topic
bridges add-forward <Name> <Topic>              # Add bridge forward topic
bridges del-forward <Name> <Topic>              # Delete bridge forward topic
bridges subscriptions <Name>                    # Show a bridge subscriptions topic
bridges add-subscription <Name> <Topic> <Qos>   # Add bridge subscriptions topic
```

### 列出全部 bridge 状态

```bash
$ ./bin/emqx_ctl bridges list
name: emqx     status: Stopped
```

### 启动指定 bridge

```bash
$ ./bin/emqx_ctl bridges start emqx
Start bridge successfully.
```

### 停止指定 bridge

```bash
$ ./bin/emqx_ctl bridges stop emqx
Stop bridge successfully.
```

### 列出指定 bridge 的转发主题

```bash
$ ./bin/emqx_ctl bridges forwards emqx
topic:   topic1/#
topic:   topic2/#
```

### 添加指定 bridge 的转发主题

```bash
$ ./bin/emqx_ctl bridges add-forwards emqx topic3/#
Add-forward topic successfully.
```

### 删除指定 bridge 的转发主题

```bash
$ ./bin/emqx_ctl bridges del-forwards emqx topic3/#
Del-forward topic successfully.
```

### 列出指定 bridge 的订阅

```bash
$ ./bin/emqx_ctl bridges subscriptions emqx
topic: cmd/topic1, qos: 1
topic: cmd/topic2, qos: 1
```

### 添加指定 bridge 的订阅主题

```bash
$ ./bin/emqx_ctl bridges add-subscription emqx cmd/topic3 1
Add-subscription topic successfully.
```

### 删除指定 bridge 的订阅主题

```bash
$ ./bin/emqx_ctl bridges del-subscription emqx cmd/topic3
Del-subscription topic successfully.
```

## RPC 桥接

EMQ X 桥接转发 MQTT 消息到远程 EMQ X:

![image](./assets/bridge_rpc.png)

rpc bridge 桥接插件配置文件: etc/plugins/emqx_bridge_mqtt.conf

### 配置 RPC 桥接的 Broker 地址

```bash
bridge.mqtt.emqx.address = emqx2@192.168.1.2
```

### 配置 MQTT 桥接转发和订阅主题

```bash
## 桥接的 mountpoint(挂载点)
bridge.mqtt.emqx.mountpoint = bridge/emqx1/${node}/

## 转发消息的主题
bridge.mqtt.emqx.forwards = topic1/#,topic2/#
```

### MQTT 桥接转发和订阅主题说明

挂载点 Mountpoint: mountpoint 用于在转发消息时加上主题前缀，该配置选项须配合 forwards 使用，转发主题为
sensor1/hello 的消息, 到达远程节点时主题为 bridge/aws/emqx1@192.168.1.1/sensor1/hello。

转发主题 Forwards: 转发到本地 EMQX 指定 forwards 主题上的消息都会被转发到远程 MQTT Broker 上。

### 桥接 CLI 命令

桥接 CLI 的使用方式与 mqtt bridge 相同。

## Kafka 桥接

EMQ X 桥接转发 MQTT 消息到 Kafka 集群，Apache Kafka是一个快速、高可扩展、高吞吐的分布式日志系统，配合kafka Stream，在流式数据处理中非常常用。

![image](./assets/bridge_kafka.png)

Kafka 桥接插件配置文件: etc/plugins/emqx_bridge_kafka.conf。

### 配置 Kafka 集群地址

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

### 配置 Kafka 桥接规则

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

### Kafka 桥接规则说明

| 事件                                       | 说明           |
| ---------------------------------------- | ------------ |
| bridge.kafka.hook.client.connected.1     | 客户端登录        |
| bridge.kafka.hook.client.disconnected.1  | 客户端退出        |
| bridge.kafka.hook.session.subscribed.1   | 订阅主题         |
| bridge.kafka.hook.session.unsubscribed.1 | 取消订阅主题       |
| bridge.kafka.hook.message.publish.1      | 发布消息         |
| bridge.kafka.hook.message.delivered.1    | delivered 消息 |
| bridge.kafka.hook.message.acked.1        | ACK 消息       |

### 客户端上下线事件转发 Kafka

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

### 客户端订阅主题事件转发 Kafka

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

### 客户端取消订阅主题事件转发 Kafka

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

### MQTT 消息转发到 Kafka

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

### MQTT 消息派发 (Deliver) 事件转发 Kafka

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

### MQTT 消息确认 (Ack) 事件转发 Kafka

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

### Kafka 消费示例

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

### 启用 Kafka 桥接插件

```bash
./bin/emqx_ctl plugins load emqx_bridge_kafka
```



## RabbitMQ 桥接

EMQ X 桥接转发 MQTT 消息到 RabbitMQ 集群:

![image](./assets/bridge_rabbit.png)

RabbitMQ 桥接插件配置文件: etc/plugins/emqx_bridge_rabbit.conf。

### 配置 RabbitMQ 桥接地址

```bash
## RabbitMQ 的服务器地址
bridge.rabbit.1.server = 127.0.0.1:5672

## RabbitMQ 的连接池大小
bridge.rabbit.1.pool_size = 4

## RabbitMQ 的用户名
bridge.rabbit.1.username = guest

## RabbitMQ 的密码
bridge.rabbit.1.password = guest

## RabbitMQ 的虚拟 Host
bridge.rabbit.1.virtual_host = /

## RabbitMQ 的心跳间隔
bridge.rabbit.1.heartbeat = 0

# bridge.rabbit.2.server = 127.0.0.1:5672

# bridge.rabbit.2.pool_size = 8

# bridge.rabbit.2.username = guest

# bridge.rabbit.2.password = guest

# bridge.rabbit.2.virtual_host = /

# bridge.rabbit.2.heartbeat = 0
```

### 配置 RabbitMQ 桥接规则

```bash
## Bridge Hooks
bridge.rabbit.hook.client.subscribe.1 = {"action": "on_client_subscribe", "rabbit": 1, "exchange": "direct:emq.subscription"}

bridge.rabbit.hook.client.unsubscribe.1 = {"action": "on_client_unsubscribe", "rabbit": 1, "exchange": "direct:emq.unsubscription"}

bridge.rabbit.hook.message.publish.1 = {"topic": "$SYS/#", "action": "on_message_publish", "rabbit": 1, "exchange": "topic:emq.$sys"}

bridge.rabbit.hook.message.publish.2 = {"topic": "#", "action": "on_message_publish", "rabbit": 1, "exchange": "topic:emq.pub"}

bridge.rabbit.hook.message.acked.1 = {"topic": "#", "action": "on_message_acked", "rabbit": 1, "exchange": "topic:emq.acked"}
```

### 客户端订阅主题事件转发 RabbitMQ

```bash
routing_key = subscribe
exchange = emq.subscription
headers = [{<<"x-emq-client-id">>, binary, ClientId}]
payload = jsx:encode([{Topic, proplists:get_value(qos, Opts)} || {Topic, Opts} <- TopicTable])
```

### 客户端取消订阅事件转发 RabbitMQ

```bash
routing_key = unsubscribe
exchange = emq.unsubscription
headers = [{<<"x-emq-client-id">>, binary, ClientId}]
payload = jsx:encode([Topic || {Topic, _Opts} <- TopicTable]),
```

### MQTT 消息转发 RabbitMQ

```bash
routing_key = binary:replace(binary:replace(Topic, <<"/">>, <<".">>, [global]),<<"+">>, <<"*">>, [global])
exchange = emq.$sys | emq.pub
headers = [{<<"x-emq-publish-qos">>, byte, Qos},
           {<<"x-emq-client-id">>, binary, pub_from(From)},
           {<<"x-emq-publish-msgid">>, binary, emqx_base62:encode(Id)},
           {<<"x-emqx-topic">>, binary, Topic}]
payload = Payload
```

### MQTT 消息确认 (Ack) 事件转发 RabbitMQ

```bash
routing_key = puback
exchange = emq.acked
headers = [{<<"x-emq-msg-acked">>, binary, ClientId}],
payload = emqx_base62:encode(Id)
```

### RabbitMQ 订阅消费 MQTT 消息示例

Python RabbitMQ消费者代码示例:

```python
#!/usr/bin/env python
import pika
import sys

connection = pika.BlockingConnection(pika.ConnectionParameters(host='localhost'))
channel = connection.channel()

channel.exchange_declare(exchange='direct:emq.subscription', exchange_type='direct')

result = channel.queue_declare(exclusive=True)
queue_name = result.method.queue

channel.queue_bind(exchange='direct:emq.subscription', queue=queue_name, routing_key= 'subscribe')

def callback(ch, method, properties, body):
    print(" [x] %r:%r" % (method.routing_key, body))

channel.basic_consume(callback, queue=queue_name, no_ack=True)

channel.start_consuming()
```

其他语言 RabbitMQ 客户端代码示例:

[https://github.com/rabbitmq/rabbitmq-tutorials](https://github.com/rabbitmq/rabbitmq-tutorials)

### 启用 RabbitMQ 桥接插件

```bash
./bin/emqx_ctl plugins load emqx_bridge_rabbit
```

## Pulsar 桥接

EMQ X 桥接转发 MQTT 消息到 Pulsar 集群:

![image](./assets/bridge_pulsar.png)

Pulsar 桥接插件配置文件: etc/plugins/emqx_bridge_pulsar.conf。

### 配置 Pulsar 集群地址

```bash
## Pulsar 服务器集群配置
## bridge.pulsar.servers = 127.0.0.1:6650,127.0.0.2:6650,127.0.0.3:6650
bridge.pulsar.servers = 127.0.0.1:6650

## 分区生产者是同步/异步模式选择
bridge.pulsar.produce = sync

## 生产者同步模式下的超时时间
## bridge.pulsar.produce.sync_timeout = 3s

## 生产者 batch 的消息数量
## bridge.pulsar.producer.batch_size = 1000

## 默认情况下不为生产者启用压缩选项
## bridge.pulsar.producer.compression = no_compression

## 采用 base64 编码或不编码
## bridge.pulsar.encode_payload_type = base64

## bridge.pulsar.sock.buffer = 32KB
## bridge.pulsar.sock.recbuf = 32KB
bridge.pulsar.sock.sndbuf = 1MB
## bridge.pulsar.sock.read_packets = 20
```

### 配置 Pulsar 桥接规则

```bash
## Bridge Pulsar Hooks
## ${topic}: the pulsar topics to which the messages will be published.
## ${filter}: the mqtt topic (may contain wildcard) on which the action will be performed .

## Client Connected Record Hook
bridge.pulsar.hook.client.connected.1     = {"topic": "client_connected"}

## Client Disconnected Record Hook
bridge.pulsar.hook.client.disconnected.1  = {"topic": "client_disconnected"}

## Session Subscribed Record Hook
bridge.pulsar.hook.session.subscribed.1   = {"filter": "#",  "topic": "session_subscribed"}

## Session Unsubscribed Record Hook
bridge.pulsar.hook.session.unsubscribed.1 = {"filter": "#",  "topic": "session_unsubscribed"}

## Message Publish Record Hook
bridge.pulsar.hook.message.publish.1      = {"filter": "#",  "topic": "message_publish"}

## Message Delivered Record Hook
bridge.pulsar.hook.message.delivered.1    = {"filter": "#",  "topic": "message_delivered"}

## Message Acked Record Hook
bridge.pulsar.hook.message.acked.1        = {"filter": "#",  "topic": "message_acked"}

## More Configures
## partitioner strategy:
## Option:  random | roundrobin | first_key_dispatch
## Example: bridge.pulsar.hook.message.publish.1 = {"filter":"#", "topic":"message_publish", "strategy":"random"}

## key:
## Option: ${clientid} | ${username}
## Example: bridge.pulsar.hook.message.publish.1 = {"filter":"#", "topic":"message_publish", "key":"${clientid}"}

## format:
## Option: json | json
## Example: bridge.pulsar.hook.message.publish.1 = {"filter":"#", "topic":"message_publish", "format":"json"}
```

### Pulsar 桥接规则说明

<table style="width:85%;">
<colgroup>
<col style="width: 58%" />
<col style="width: 26%" />
</colgroup>
<thead>
<tr class="header">
<th>事件</th>
<th>说明</th>
</tr>
</thead>
<tbody>
<tr class="odd">
<td>bridge.pulsar.hook.client.connected.1</td>
<td><div class="line-block">客户端登录</div></td>
</tr>
<tr class="even">
<td>bridge.pulsar.hook.client.disconnected.1</td>
<td><div class="line-block">客户端退出</div></td>
</tr>
<tr class="odd">
<td>bridge.pulsar.hook.session.subscribed.1</td>
<td><div class="line-block">订阅主题</div></td>
</tr>
<tr class="even">
<td>bridge.pulsar.hook.session.unsubscribed.1</td>
<td><div class="line-block">取消订阅主题    </div></td>
</tr>
<tr class="odd">
<td>bridge.pulsar.hook.message.publish.1</td>
<td><div class="line-block">发布消息</div></td>
</tr>
<tr class="even">
<td>bridge.pulsar.hook.message.delivered.1</td>
<td><div class="line-block">delivered 消息</div></td>
</tr>
<tr class="odd">
<td>bridge.pulsar.hook.message.acked.1</td>
<td><div class="line-block">ACK 消息</div></td>
</tr>
</tbody>
</table>

### 客户端上下线事件转发 Pulsar

设备上线 EMQ X 转发上线事件消息到 Pulsar:

```bash
topic = "client_connected",
value = {
         "client_id": ${clientid},
         "username": ${username},
         "node": ${node},
         "ts": ${ts}
        }
```

设备下线 EMQ X 转发下线事件消息到 Pulsar:

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

### 客户端订阅主题事件转发 Pulsar

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

### 客户端取消订阅主题事件转发 Pulsar

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

### MQTT 消息转发到 Pulsar

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

### MQTT 消息派发 (Deliver) 事件转发 Pulsar

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

### MQTT 消息确认 (Ack) 事件转发 Pulsar

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

### Pulsar 消费示例

Pulsar 读取 MQTT 客户端上下线事件消息:

```bash
pulsar-client consume client_connected  -s "client_connected" -n 1000

pulsar-client consume client_disconnected  -s "client_disconnected" -n 1000
```

Pulsar 读取 MQTT 主题订阅事件消息:

```bash
pulsar-client consume session_subscribed  -s "session_subscribed" -n 1000

pulsar-client consume session_unsubscribed  -s "session_unsubscribed" -n 1000
```

Pulsar 读取 MQTT 发布消息:

```bash
pulsar-client consume message_publish  -s "message_publish" -n 1000
```

Pulsar 读取 MQTT 消息发布 (Deliver)、确认 (Ack)事件:

```bash
pulsar-client consume message_delivered  -s "message_delivered" -n 1000

pulsar-client consume message_acked  -s "message_acked" -n 1000
```
::: tip
默认 payload 被 base64 编码，可通过修改配置 bridge.pulsar.encode_payload_type 指定 payload 数据格式。
:::

### 启用 Pulsar 桥接插件

```bash
./bin/emqx_ctl plugins load emqx_bridge_pulsar
```



## RocketMQ 桥接

EMQ X 桥接转发 MQTT 消息到 RocketMQ 集群:

![image](./assets/bridge_rocket.png)

RocketMQ 桥接插件配置文件: etc/plugins/emqx_bridge_rocket.conf。

### 配置 RocketMQ 集群地址

```bash
## RocketMQ 服务器集群配置
## bridge.rocket.servers = 127.0.0.1:9876,127.0.0.2:9876,127.0.0.3:9876
bridge.rocket.servers = 127.0.0.1:9876

bridge.rocket.refresh_topic_route_interval = 5S

## 分区生产者是同步/异步模式选择
bridge.rocket.produce = sync

## 生产者同步模式下的超时时间
## bridge.rocket.produce.sync_timeout = 3s

## 生产者 batch 的消息数量
## bridge.rocket.producer.batch_size = 100

## 采用 base64 编码或不编码
## bridge.rocket.encode_payload_type = base64

## bridge.rocket.sock.buffer = 32KB
## bridge.rocket.sock.recbuf = 32KB
bridge.rocket.sock.sndbuf = 1MB
## bridge.rocket.sock.read_packets = 20
```

### 配置 RocketMQ 桥接规则

```bash
## Bridge RocketMQ Hooks
## ${topic}: the RocketMQ topics to which the messages will be published.
## ${filter}: the mqtt topic (may contain wildcard) on which the action will be performed .

## Client Connected Record Hook
bridge.rocket.hook.client.connected.1     = {"topic": "ClientConnected"}

## Client Disconnected Record Hook
bridge.rocket.hook.client.disconnected.1  = {"topic": "ClientDisconnected"}

## Session Subscribed Record Hook
bridge.rocket.hook.session.subscribed.1   = {"filter": "#",  "topic": "SessionSubscribed"}

## Session Unsubscribed Record Hook
bridge.rocket.hook.session.unsubscribed.1 = {"filter": "#",  "topic": "SessionUnsubscribed"}

## Message Publish Record Hook
bridge.rocket.hook.message.publish.1      = {"filter": "#",  "topic": "MessagePublish"}

## Message Delivered Record Hook
bridge.rocket.hook.message.delivered.1    = {"filter": "#",  "topic": "MessageDeliver"}

## Message Acked Record Hook
bridge.rocket.hook.message.acked.1        = {"filter": "#",  "topic": "MessageAcked"}
```

### RocketMQ 桥接规则说明

<table style="width:85%;">
<colgroup>
<col style="width: 58%" />
<col style="width: 26%" />
</colgroup>
<thead>
<tr class="header">
<th>事件</th>
<th>说明</th>
</tr>
</thead>
<tbody>
<tr class="odd">
<td>bridge.rocket.hook.client.connected.1</td>
<td><div class="line-block">客户端登录</div></td>
</tr>
<tr class="even">
<td>bridge.rocket.hook.client.disconnected.1</td>
<td><div class="line-block">客户端退出</div></td>
</tr>
<tr class="odd">
<td>bridge.rocket.hook.session.subscribed.1</td>
<td><div class="line-block">订阅主题</div></td>
</tr>
<tr class="even">
<td>bridge.rocket.hook.session.unsubscribed.1</td>
<td><div class="line-block">取消订阅主题    </div></td>
</tr>
<tr class="odd">
<td>bridge.rocket.hook.message.publish.1</td>
<td><div class="line-block">发布消息</div></td>
</tr>
<tr class="even">
<td>bridge.rocket.hook.message.delivered.1</td>
<td><div class="line-block">delivered 消息</div></td>
</tr>
<tr class="odd">
<td>bridge.rocket.hook.message.acked.1</td>
<td><div class="line-block">ACK 消息</div></td>
</tr>
</tbody>
</table>

### 客户端上下线事件转发 RocketMQ

设备上线 EMQ X 转发上线事件消息到 RocketMQ:

```bash
topic = "ClientConnected",
value = {
         "client_id": ${clientid},
         "username": ${username},
         "node": ${node},
         "ts": ${ts}
        }
```

设备下线 EMQ X 转发下线事件消息到 RocketMQ:

```bash
topic = "ClientDisconnected",
value = {
         "client_id": ${clientid},
         "username": ${username},
         "reason": ${reason},
         "node": ${node},
         "ts": ${ts}
        }
```

### 客户端订阅主题事件转发 RocketMQ

```bash
topic = "SessionSubscribed"

value = {
         "client_id": ${clientid},
         "topic": ${topic},
         "qos": ${qos},
         "node": ${node},
         "ts": ${timestamp}
        }
```

### 客户端取消订阅主题事件转发 RocketMQ

```bash
topic = "SessionUnsubscribed"

value = {
         "client_id": ${clientid},
         "topic": ${topic},
         "qos": ${qos},
         "node": ${node},
         "ts": ${timestamp}
        }
```

### MQTT 消息转发到 RocketMQ

```bash
topic = "MessagePublish"

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

### MQTT 消息派发 (Deliver) 事件转发 RocketMQ

```bash
topic = "MessageDeliver"

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

### MQTT 消息确认 (Ack) 事件转发 RocketMQ

```bash
topic = "MessageAcked"

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

### RocketMQ 消费示例

RocketMQ 读取 MQTT 客户端上下线事件消息:

```bash
bin/tools.sh org.apache.rocketmq.example.quickstart.Consumer ClientConnected
  
bin/tools.sh org.apache.rocketmq.example.quickstart.Consumer ClientDisconnected
```

RocketMQ 读取 MQTT 主题订阅事件消息:

```bash
bin/tools.sh org.apache.rocketmq.example.quickstart.Consumer SessionSubscribed

bin/tools.sh org.apache.rocketmq.example.quickstart.Consumer SessionUnsubscribed
```

RocketMQ 读取 MQTT 发布消息:

```bash
bin/tools.sh org.apache.rocketmq.example.quickstart.Consumer MessagePublish
```

RocketMQ 读取 MQTT 消息发布 (Deliver)、确认 (Ack) 事件:

```bash
bin/tools.sh org.apache.rocketmq.example.quickstart.Consumer MessageDeliver

bin/tools.sh org.apache.rocketmq.example.quickstart.Consumer MessageAcked
```

::: tip
默认 payload 被 base64 编码，可通过修改配置 bridge.rocket.encode_payload_type 指定
payload 数据格式。
:::

### 启用 RocketMQ 桥接插件

```bash
./bin/emqx_ctl plugins load emqx_bridge_rocket
```
