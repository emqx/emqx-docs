# Kafka Bridge

::: tip

After EMQX version 3.1, a powerful rule engine is introduced to replace plug-ins. It is recommended that you use it. See [Bridge data to Kafka](../rule/bridge_kafka.md) to setup Kafka bridges in rule engine.

:::

EMQX bridges and forwards MQTT messages to Kafka cluster:

![image](./assets/bridges_1.png)

Config file for Kafka bridge plugin:
`etc/plugins/emqx_bridge_kafka.conf`

## Configure Kafka Cluster

```bash
## Kafka Server
## bridge.kafka.servers = 127.0.0.1:9092,127.0.0.2:9092,127.0.0.3:9092
bridge.kafka.servers = 127.0.0.1:9092

## Kafka Parition Strategy. option value: per_partition | per_broker
bridge.kafka.connection_strategy = per_partition

bridge.kafka.min_metadata_refresh_interval = 5S

## Produce writes type. option value: sync | async
bridge.kafka.produce = sync

bridge.kafka.produce.sync_timeout = 3S

## Base directory for replayq to store messages on disk.
## If this config entry if missing or set to undefined,
## replayq works in a mem-only manner.
## i.e. messages are not queued on disk -- in such case,
## the send or send_sync API callers are responsible for
## possible message loss in case of application,
## network or kafka disturbances. For instance,
## in the wolff:send API caller may trap_exit then
## react on parition-producer worker pid's 'EXIT'
## message to issue a retry after restarting the producer.
## bridge.kafka.replayq_dir = /tmp/emqx_bridge_kafka/

## default=10MB, replayq segment size.
## bridge.kafka.producer.replayq_seg_bytes = 10MB

## producer required_acks. option value all_isr | leader_only | none.
bridge.kafka.producer.required_acks = none

## default=10000. Timeout leader wait for replicas before reply to producer.
## bridge.kafka.producer.ack_timeout = 10S

## default number of message sets sent on wire before block waiting for acks
## bridge.kafka.producer.max_batch_bytes = 1024KB

## by default, send max 1 MB of data in one batch (message set)
## bridge.kafka.producer.min_batch_bytes = 0

## Number of batches to be sent ahead without receiving ack for the last request.
## Must be 0 if messages must be delivered in strict order.
## bridge.kafka.producer.max_send_ahead = 0

## by default, no compression
# bridge.kafka.producer.compression = no_compression

# bridge.kafka.encode_payload_type = base64

# bridge.kafka.sock.buffer = 32KB
# bridge.kafka.sock.recbuf = 32KB
bridge.kafka.sock.sndbuf = 1MB
# bridge.kafka.sock.read_packets = 20
```

## Configure Kafka Bridge Hooks

```bash
## Bridge Kafka Hooks
## ${topic}: the kafka topics to which the messages will be published.
## ${filter}: the mqtt topic (may contain wildcard) on which the action will be performed .

bridge.kafka.hook.client.connected.1     = {"topic": "client_connected"}
bridge.kafka.hook.client.disconnected.1  = {"topic": "client_disconnected"}
bridge.kafka.hook.session.subscribed.1   = {"filter": "#",  "topic": "session_subscribed"}
bridge.kafka.hook.session.unsubscribed.1 = {"filter": "#",  "topic": "session_unsubscribed"}
bridge.kafka.hook.message.publish.1      = {"filter": "#",  "topic": "message_publish"}
bridge.kafka.hook.message.delivered.1    = {"filter": "#",  "topic": "message_delivered"}
bridge.kafka.hook.message.acked.1        = {"filter": "#",  "topic": "message_acked"}
```

## Description of Kafka Bridge Hooks

| Event                                    | Description           |
| ---------------------------------------- | --------------------- |
| bridge.kafka.hook.client.connected.1     | Client connected      |
| bridge.kafka.hook.client.disconnected.1  | Client disconnected   |
| bridge.kafka.hook.session.subscribed.1   | Topics subscribed     |
| bridge.kafka.hook.session.unsubscribed.1 | Topics unsubscribed   |
| bridge.kafka.hook.message.publish.1      | Messages published    |
| bridge.kafka.hook.message.delivered.1    | Messages delivered    |
| bridge.kafka.hook.message.acked.1        | Messages acknowledged |

## Forward Client Connected / Disconnected Events to Kafka

Client goes online, EMQX forwards 'client\_connected' event message to
Kafka:

```python
topic = "client_connected",
value = {
         "client_id": ${clientid},
         "node": ${node},
         "ts": ${ts}
        }
```

Client goes offline, EMQX forwards 'client\_disconnected' event message
to Kafka:

```python
topic = "client_disconnected",
value = {
        "client_id": ${clientid},
        "reason": ${reason},
        "node": ${node},
        "ts": ${ts}
        }
```

## Forward Subscription Event to Kafka

```python
topic = session_subscribed

value = {
         "client_id": ${clientid},
         "topic": ${topic},
         "qos": ${qos},
         "node": ${node},
         "ts": ${timestamp}
        }
```

## Forward Unsubscription Event to Kafka

```python
topic = session_unsubscribed

value = {
         "client_id": ${clientid},
         "topic": ${topic},
         "qos": ${qos},
         "node": ${node},
         "ts": ${timestamp}
        }
```

## Forward MQTT Messages to Kafka

```python
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

## Forwarding MQTT Message Deliver Event to Kafka

```python
topic = message_delivered

value = {"client_id": ${clientid},
         "username": ${username},
         "from": ${fromClientId},
         "topic": ${topic},
         "payload": ${payload},
         "qos": ${qos},
         "node": ${node},
         "ts": ${timestamp}
        }
```

## Forwarding MQTT Message Ack Event to Kafka

```python
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

## Examples of Kafka Message Consumption

Kafka consumes MQTT clients connected / disconnected event
    messages:

```bash
sh kafka-console-consumer.sh --zookeeper localhost:2181 --topic client_connected --from-beginning

sh kafka-console-consumer.sh --zookeeper localhost:2181 --topic client_disconnected --from-beginning
```

Kafka consumes MQTT subscription
    messages:

```bash
sh kafka-console-consumer.sh --zookeeper localhost:2181 --topic session_subscribed --from-beginning

sh kafka-console-consumer.sh --zookeeper localhost:2181 --topic session_unsubscribed --from-beginning
```

Kafka consumes MQTT published
    messages:

```bash
sh kafka-console-consumer.sh --zookeeper localhost:2181 --topic message_publish --from-beginning
```

Kafka consumes MQTT message Deliver and Ack event
    messages:

```bash
sh kafka-console-consumer.sh --zookeeper localhost:2181 --topic message_delivered --from-beginning

sh kafka-console-consumer.sh --zookeeper localhost:2181 --topic message_acked --from-beginning
```

::: tip
the payload is base64 encoded
:::

## Enable Kafka Bridge

```bash
./bin/emqx_ctl plugins load emqx_bridge_kafka
```
