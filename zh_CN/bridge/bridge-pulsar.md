# Pulsar 桥接

EMQ X 桥接转发 MQTT 消息到 Pulsar 集群:

![image](./assets/bridge_pulsar.png)

Pulsar 桥接插件配置文件: etc/plugins/emqx_bridge_pulsar.conf。

## 配置 Pulsar 集群地址

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

## 配置 Pulsar 桥接规则

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

## Pulsar 桥接规则说明

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

## 客户端上下线事件转发 Pulsar

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

## 客户端订阅主题事件转发 Pulsar

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

## 客户端取消订阅主题事件转发 Pulsar

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

## MQTT 消息转发到 Pulsar

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

## MQTT 消息派发 (Deliver) 事件转发 Pulsar

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

## MQTT 消息确认 (Ack) 事件转发 Pulsar

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

## Pulsar 消费示例

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

## 启用 Pulsar 桥接插件

```bash
./bin/emqx_ctl plugins load emqx_bridge_pulsar
```
