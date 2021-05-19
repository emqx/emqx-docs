# Pulsar Bridge

::: tip

After EMQ X version 3.1, a powerful rule engine is introduced to replace plug-ins. It is recommended that you use it. See [Bridge data to Pulsar](../rule/bridge_pulsar.md) to setup pulasr bridges in rule engine.

:::

EMQ X bridges and forwards MQTT messages to Pulsar cluster:

![image](./assets/bridge_pulsar.png)

Config file for Pulsar bridge plugin:
etc/plugins/emqx\_bridge\_pulsar.conf

## Configure Pulsar Cluster

```bash
## Pulsar Server
bridge.pulsar.servers = 127.0.0.1:6650

## Pick a partition producer and sync/async
bridge.pulsar.produce = sync

## bridge.pulsar.produce.sync_timeout = 3s

## bridge.pulsar.producer.batch_size = 1000

## by default, no compression
## bridge.pulsar.producer.compression = no_compression

## bridge.pulsar.encode_payload_type = base64

## bridge.pulsar.sock.buffer = 32KB
## bridge.pulsar.sock.recbuf = 32KB
bridge.pulsar.sock.sndbuf = 1MB
## bridge.pulsar.sock.read_packets = 20
```

## Configure Pulsar Bridge Hooks

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

## Description of Pulsar Bridge Hooks

| Event                                     | Description           |
| ----------------------------------------- | --------------------- |
| bridge.pulsar.hook.client.connected.1     | Client connected      |
| bridge.pulsar.hook.client.disconnected.1  | Client disconnected   |
| bridge.pulsar.hook.session.subscribed.1   | Topics subscribed     |
| bridge.pulsar.hook.session.unsubscribed.1 | Topics unsubscribed   |
| bridge.pulsar.hook.message.publish.1      | Messages published    |
| bridge.pulsar.hook.message.delivered.1    | Messages delivered    |
| bridge.pulsar.hook.message.acked.1        | Messages acknowledged |

## Forward Client Connected / Disconnected Events to Pulsar

Client goes online, EMQ X forwards 'client\_connected' event message to
Pulsar:

```python
topic = "client_connected",
value = {
         "client_id": ${clientid},
         "username": ${username},
         "node": ${node},
         "ts": ${ts}
        }
```

Client goes offline, EMQ X forwards 'client\_disconnected' event message
to Pulsar:

```python
topic = "client_disconnected",
value = {
         "client_id": ${clientid},
         "username": ${username},
         "reason": ${reason},
         "node": ${node},
         "ts": ${ts}
        }
```

## Forward Subscription Event to Pulsar

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

## Forward Unsubscription Event to Pulsar

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

## Forward MQTT Messages to Pulsar

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

## Forwarding MQTT Message Deliver Event to Pulsar

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

## Forwarding MQTT Message Ack Event to Pulsar

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

## Examples of Pulsar Message Consumption

Pulsar consumes MQTT clients connected / disconnected event
    messages:

```bash
sh pulsar-client consume client_connected  -s "client_connected" -n 1000

sh pulsar-client consume client_disconnected  -s "client_disconnected" -n 1000
```

Pulsar consumes MQTT subscription
    messages:

```bash
sh pulsar-client consume session_subscribed  -s "session_subscribed" -n 1000

sh pulsar-client consume session_unsubscribed  -s "session_unsubscribed" -n 1000
```

Pulsar consumes MQTT published
    messages:
```bash
sh pulsar-client consume message_publish  -s "message_publish" -n 1000
```

Pulsar consumes MQTT message Deliver and Ack event
    messages:

```bash
sh pulsar-client consume message_delivered  -s "message_delivered" -n 1000

sh pulsar-client consume message_acked  -s "message_acked" -n 1000
```

::: tip
The payload is base64 encoded default
:::

## Enable Pulsar Bridge

```bash
./bin/emqx_ctl plugins load emqx_bridge_pulsar
```
