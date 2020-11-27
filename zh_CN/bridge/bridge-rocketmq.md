# RocketMQ 桥接

EMQ X 桥接转发 MQTT 消息到 RocketMQ 集群:

![image](./assets/bridge_rocket.png)

RocketMQ 桥接插件配置文件: etc/plugins/emqx_bridge_rocket.conf。

## 配置 RocketMQ 集群地址

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

## 配置 RocketMQ 桥接规则

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

## RocketMQ 桥接规则说明

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

## 客户端上下线事件转发 RocketMQ

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

## 客户端订阅主题事件转发 RocketMQ

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

## 客户端取消订阅主题事件转发 RocketMQ

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

## MQTT 消息转发到 RocketMQ

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

## MQTT 消息派发 (Deliver) 事件转发 RocketMQ

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

## MQTT 消息确认 (Ack) 事件转发 RocketMQ

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

## RocketMQ 消费示例

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

## 启用 RocketMQ 桥接插件

```bash
./bin/emqx_ctl plugins load emqx_bridge_rocket
```
