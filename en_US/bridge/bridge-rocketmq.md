# RocketMQ Bridge

::: tip

After EMQ X version 3.1, a powerful rule engine is introduced to replace plug-ins. It is recommended that you use it. See [Bridge data to RocketMQ](../rule/bridge_rocketmq.md) to setup RocketMQ bridges in rule engine.

:::

EMQ X bridges and forwards MQTT messages to RocketMQ cluster:

![image](./assets/bridge_rocket.png)

Config file of RocketMQ bridge plugin:
etc/plugins/emqx_bridge_rocket.conf.

## Configure RocketMQ Cluster

```bash
## RocketMQ Brokers Server
## bridge.rocket.servers = 127.0.0.1:9876,127.0.0.2:9876,127.0.0.3:9876
bridge.rocket.servers = 127.0.0.1:9876

bridge.rocket.refresh_topic_route_interval = 5S

## Pick a partition producer and sync/async
bridge.rocket.produce = sync

## bridge.rocket.produce.sync_timeout = 3s

## bridge.rocket.producer.batch_size = 100

## bridge.rocket.encode_payload_type = base64

## bridge.rocket.sock.buffer = 32KB
## bridge.rocket.sock.recbuf = 32KB
bridge.rocket.sock.sndbuf = 1MB
## bridge.rocket.sock.read_packets = 20
```

## Configure RocketMQ Bridge Hooks

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


## Description of Pulsar Bridge Hooks

| Event                                     | Description           |
| ----------------------------------------- | --------------------- |
| bridge.rocket.hook.client.connected.1     | Client connected      |
| bridge.rocket.hook.client.disconnected.1  | Client disconnected   |
| bridge.rocket.hook.session.subscribed.1   | Topics subscribed     |
| bridge.rocket.hook.session.unsubscribed.1 | Topics unsubscribed   |
| bridge.rocket.hook.message.publish.1      | Messages published    |
| bridge.rocket.hook.message.delivered.1    | Messages delivered    |
| bridge.rocket.hook.message.acked.1        | Messages acknowledged |


## Forward Client Connected / Disconnected Events to RocketMQ


```bash
topic = "ClientConnected",
value = {
         "client_id": ${clientid},
         "username": ${username},
         "node": ${node},
         "ts": ${ts}
        }
```

## Client goes offline, EMQ X forwards 'client\_disconnected' event message

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

## Forward Subscription Event to RocketMQ

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

## Forward Unsubscription Event to RocketMQ

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

## Forward MQTT Messages to RocketMQ

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

## Forwarding MQTT Message Deliver Event to RocketMQ

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

## Forwarding MQTT Message Ack Event to RocketMQ

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

## Examples of RocketMQ Message Consumption

RocketMQ consumes MQTT clients connected / disconnected event

```bash
bin/tools.sh org.apache.rocketmq.example.quickstart.Consumer ClientConnected

bin/tools.sh org.apache.rocketmq.example.quickstart.Consumer ClientDisconnected
```

RocketMQ consumes MQTT subscription

```bash
bin/tools.sh org.apache.rocketmq.example.quickstart.Consumer SessionSubscribed

bin/tools.sh org.apache.rocketmq.example.quickstart.Consumer SessionUnsubscribed
```

RocketMQ consumes MQTT published

```bash
bin/tools.sh org.apache.rocketmq.example.quickstart.Consumer MessagePublish
```

RocketMQ consumes MQTT message Deliver and Ack event

```bash
bin/tools.sh org.apache.rocketmq.example.quickstart.Consumer MessageDeliver

bin/tools.sh org.apache.rocketmq.example.quickstart.Consumer MessageAcked
```

::: tip
the payload is base64 encoded
:::

## Enable RocketMQ Bridge

```bash
./bin/emqx_ctl plugins load emqx_bridge_rocket
```
