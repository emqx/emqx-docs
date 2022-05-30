## MQTT Bridge

EMQX bridges and forwards MQTT messages to MQTT Broker:

![image](./assets/bridge_mqtt.png)

Config file for MQTT bridge plugin: etc/plugins/emqx\_bridge\_mqtt.conf

### Configure MQTT Bridge

```bash
## Bridge address: node name for local bridge, host:port for remote
bridge.mqtt.aws.address = 127.0.0.1:1883

## Protocol version of the bridge: mqttv3 | mqttv4 | mqttv5
bridge.mqtt.aws.proto_ver = mqttv4

## Whether to enable bridge mode for mqtt bridge
bridge.mqtt.aws.bridge_mode = true

## The ClientId of a remote bridge
bridge.mqtt.aws.clientid = bridge_aws

## The Clean start flag of a remote bridge
## NOTE: Some IoT platforms require clean_start must be set to 'true'
bridge.mqtt.aws.clean_start = true

## The username for a remote bridge
bridge.mqtt.aws.username = user

## The password for a remote bridge
bridge.mqtt.aws.password = passwd

## Bribge to remote server via SSL
bridge.mqtt.aws.ssl = off

## PEM-encoded CA certificates of the bridge
bridge.mqtt.aws.cacertfile = etc/certs/cacert.pem

## Client SSL Certfile of the bridge
bridge.mqtt.aws.certfile = etc/certs/client-cert.pem

## Client SSL Keyfile of the bridge
bridge.mqtt.aws.keyfile = etc/certs/client-key.pem

## SSL Ciphers used by the bridge
bridge.mqtt.aws.ciphers = ECDHE-ECDSA-AES256-GCM-SHA384,ECDHE-RSA-AES256-GCM-SHA384

## Ciphers for TLS PSK
## Note that 'bridge.${BridgeName}.ciphers' and 'bridge.${BridgeName}.psk_ciphers' cannot be configured at the same time.
##
## See 'https://tools.ietf.org/html/rfc4279#section-2'.
bridge.mqtt.aws.psk_ciphers = PSK-AES128-CBC-SHA,PSK-AES256-CBC-SHA,PSK-3DES-EDE-CBC-SHA,PSK-RC4-SHA

## Ping interval of a down bridge.
bridge.mqtt.aws.keepalive = 60s

## TLS versions used by the bridge.
bridge.mqtt.aws.tls_versions = tlsv1.2,tlsv1.1,tlsv1
```

### Configure Topics MQTT Bridge Forwards and Subscribes

```bash
## Mountpoint of the bridge
bridge.mqtt.aws.mountpoint = bridge/aws/${node}/

## Forward message topics
bridge.mqtt.aws.forwards = topic1/#,topic2/#

## Subscriptions of the bridge topic
bridge.mqtt.aws.subscription.1.topic = cmd/topic1

## Subscriptions of the bridge qos
bridge.mqtt.aws.subscription.1.qos = 1

## Subscriptions of the bridge topic
bridge.mqtt.aws.subscription.2.topic = cmd/topic2

## Subscriptions of the bridge qos
bridge.mqtt.aws.subscription.2.qos = 1
```

### Description of Topics MQTT Bridge Forwards and Subscribes

Mountpoint: Mountpoint is used to prefix of topic when forwarding a
message, this option must be used with `forwards`. Forwards the message
whose topic is "sensor1/hello", its topic will change to
"<bridge/aws/emqx1@192.168.1.1/sensor1/hello>" when it reaches the
remote node.

Forwards: Messages forwarded to `forwards` specified by local EMQX are
forwarded to the remote MQTT Broker.

Subscription: Local EMQX synchronizes messages from a remote MQTT
Broker to local by subscribing to the topic of the remote MQTT Broker.

### Enable MQTT Bridge

```bash
./bin/emqx_ctl plugins load emqx_bridge_mqtt
```

### Bridge CLI Command

```bash
$ cd emqx && ./bin/emqx_ctl bridges
bridges list                                    # List bridges
bridges start <Name>                            # Start a bridge
bridges stop <Name>                             # Stop a bridge
bridges forwards <Name>                         # Show a bridge forward topic
bridges add-forward <Name> <Topic>              # Add bridge forward topic
bridges del-forward <Name> <Topic>              # Delete bridge forward topic
bridges subscriptions <Name>                    # Show a bridge subscriptions topic
bridges add-subscription <Name> <Topic> <Qos>   # Add bridge subscriptions topic
```

### List Status of All Bridges

```bash
$ ./bin/emqx_ctl bridges list
name: emqx     status: Stopped
```

### Start Specified Bridge

```bash
$ ./bin/emqx_ctl bridges start emqx
Start bridge successfully.
```

### Stop Specified Bridge

```bash
$ ./bin/emqx_ctl bridges stop emqx
Stop bridge successfully.
```

### List Forwarded Topic of Specified Bridge

```bash
$ ./bin/emqx_ctl bridges forwards emqx
topic:   topic1/#
topic:   topic2/#
```

### Add Forwarded Topic for Specified Bridge

```bash
$ ./bin/emqx_ctl bridges add-forward emqx topic3/#
Add-forward topic successfully.
```

### Delete Forwarded Topic for Specified Bridge

```bash
$ ./bin/emqx_ctl bridges del-forward emqx topic3/#
Del-forward topic successfully.
```

### List Subscriptions of Specified Bridge

```bash
$ ./bin/emqx_ctl bridges subscriptions emqx
topic: cmd/topic1, qos: 1
topic: cmd/topic2, qos: 1
```

### Add Subscriptions for Specified Bridge

```bash
$ ./bin/emqx_ctl bridges add-subscription emqx cmd/topic3 1
Add-subscription topic successfully.
```

### Delete Subscriptions of Specified Bridge

```bash
$ ./bin/emqx_ctl bridges del-subscription emqx cmd/topic3
Del-subscription topic successfully.
```

## RPC Bridge

EMQX bridges and forwards MQTT messages to remote EMQX:

![image](./assets/bridge_rpc.png)

Config file for RPC bridge plugin: etc/plugins/emqx\_bridge\_mqtt.conf

### Configure Broker Address for RPC Bridge

```bash
bridge.mqtt.emqx.address = emqx2@192.168.1.2
```

### Configure Topics RPC Bridge Forwards and Subscribes

```bash
## Mountpoint of the bridge
bridge.mqtt.emqx.mountpoint = bridge/emqx1/${node}/

## Forward message topics
bridge.mqtt.emqx.forwards = topic1/#,topic2/#
```

Mountpoint: Mountpoint is used to prefix of topic when forwarding a
message, this option must be used with `forwards`. Forwards the message
whose topic is "sensor1/hello", its topic will change to
"<bridge/aws/emqx1@192.168.1.1/sensor1/hello>" when it reaches the
remote node.

Forwards: Messages forwarded to `forwards` specified by local EMQX are
forwarded to the remote EMQX.

### Bridge CLI Command

CLI of RPC bridge is used in the same way as the MQTT bridge.



## Kafka Bridge

EMQX bridges and forwards MQTT messages to Kafka cluster:

![image](./assets/bridges_1.png)

Config file for Kafka bridge plugin:
etc/plugins/emqx\_bridge\_kafka.conf

### Configure Kafka Cluster

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

### Configure Kafka Bridge Hooks

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

### Description of Kafka Bridge Hooks

| Event                                    | Description           |
| ---------------------------------------- | --------------------- |
| bridge.kafka.hook.client.connected.1     | Client connected      |
| bridge.kafka.hook.client.disconnected.1  | Client disconnected   |
| bridge.kafka.hook.session.subscribed.1   | Topics subscribed     |
| bridge.kafka.hook.session.unsubscribed.1 | Topics unsubscribed   |
| bridge.kafka.hook.message.publish.1      | Messages published    |
| bridge.kafka.hook.message.delivered.1    | Messages delivered    |
| bridge.kafka.hook.message.acked.1        | Messages acknowledged |

### Forward Client Connected / Disconnected Events to Kafka

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

### Forward Subscription Event to Kafka

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

### Forward Unsubscription Event to Kafka

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

### Forward MQTT Messages to Kafka

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

### Forwarding MQTT Message Deliver Event to Kafka

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

### Forwarding MQTT Message Ack Event to Kafka

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

### Examples of Kafka Message Consumption

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

### Enable Kafka Bridge

```bash
./bin/emqx_ctl plugins load emqx_bridge_kafka
```

## RabbitMQ Bridge

EMQX bridges and forwards MQTT messages to RabbitMQ cluster:

![image](./assets/bridges_2.png)

Config file of RabbitMQ bridge plugin:
etc/plugins/emqx\_bridge\_rabbit.conf

### Configure RabbitMQ Cluster

```bash
## Rabbit Brokers Server
bridge.rabbit.1.server = 127.0.0.1:5672

## Rabbit Brokers pool_size
bridge.rabbit.1.pool_size = 4

## Rabbit Brokers username
bridge.rabbit.1.username = guest

## Rabbit Brokers password
bridge.rabbit.1.password = guest

## Rabbit Brokers virtual_host
bridge.rabbit.1.virtual_host = /

## Rabbit Brokers heartbeat
bridge.rabbit.1.heartbeat = 30

# bridge.rabbit.2.server = 127.0.0.1:5672

# bridge.rabbit.2.pool_size = 8

# bridge.rabbit.2.username = guest

# bridge.rabbit.2.password = guest

# bridge.rabbit.2.virtual_host = /

# bridge.rabbit.2.heartbeat = 30
```

### Configure RabbitMQ Bridge Hooks

```bash
## Bridge Hooks
bridge.rabbit.hook.client.subscribe.1 = {"action": "on_client_subscribe", "rabbit": 1, "exchange": "direct:emq.subscription"}

bridge.rabbit.hook.client.unsubscribe.1 = {"action": "on_client_unsubscribe", "rabbit": 1, "exchange": "direct:emq.unsubscription"}

bridge.rabbit.hook.message.publish.1 = {"topic": "$SYS/#", "action": "on_message_publish", "rabbit": 1, "exchange": "topic:emq.$sys"}

bridge.rabbit.hook.message.publish.2 = {"topic": "#", "action": "on_message_publish", "rabbit": 1, "exchange": "topic:emq.pub"}

bridge.rabbit.hook.message.acked.1 = {"topic": "#", "action": "on_message_acked", "rabbit": 1, "exchange": "topic:emq.acked"}
```

### Forward Subscription Event to RabbitMQ

```python
routing_key = subscribe
exchange = emq.subscription
headers = [{<<"x-emq-client-id">>, binary, ClientId}]
payload = jsx:encode([{Topic, proplists:get_value(qos, Opts)} || {Topic, Opts} <- TopicTable])
```

### Forward Unsubscription Event to RabbitMQ

```python
routing_key = unsubscribe
exchange = emq.unsubscription
headers = [{<<"x-emq-client-id">>, binary, ClientId}]
payload = jsx:encode([Topic || {Topic, _Opts} <- TopicTable]),
```

### Forward MQTT Messages to RabbitMQ

```python
routing_key = binary:replace(binary:replace(Topic, <<"/">>, <<".">>, [global]),<<"+">>, <<"*">>, [global])
exchange = emq.$sys | emq.pub
headers = [{<<"x-emq-publish-qos">>, byte, Qos},
           {<<"x-emq-client-id">>, binary, pub_from(From)},
           {<<"x-emq-publish-msgid">>, binary, emqx_base62:encode(Id)}]
payload = Payload
```

### Forward MQTT Message Ack Event to RabbitMQ

```python
routing_key = puback
exchange = emq.acked
headers = [{<<"x-emq-msg-acked">>, binary, ClientId}],
payload = emqx_base62:encode(Id)
```

### Example of RabbitMQ Subscription Message Consumption

Sample code of Rabbit message Consumption in Python:

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

Sample of RabbitMQ client coding in other programming languages:

[https://github.com/rabbitmq/rabbitmq-tutorials](https://github.com/rabbitmq/rabbitmq-tutorials)

### Enable RabbitMQ Bridge

```bash
./bin/emqx_ctl plugins load emqx_bridge_rabbit
```

## Pulsar Bridge

EMQX bridges and forwards MQTT messages to Pulsar cluster:

![image](./assets/bridge_pulsar.png)

Config file for Pulsar bridge plugin:
etc/plugins/emqx\_bridge\_pulsar.conf

### Configure Pulsar Cluster

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

### Configure Pulsar Bridge Hooks

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

### Description of Pulsar Bridge Hooks

| Event                                     | Description           |
| ----------------------------------------- | --------------------- |
| bridge.pulsar.hook.client.connected.1     | Client connected      |
| bridge.pulsar.hook.client.disconnected.1  | Client disconnected   |
| bridge.pulsar.hook.session.subscribed.1   | Topics subscribed     |
| bridge.pulsar.hook.session.unsubscribed.1 | Topics unsubscribed   |
| bridge.pulsar.hook.message.publish.1      | Messages published    |
| bridge.pulsar.hook.message.delivered.1    | Messages delivered    |
| bridge.pulsar.hook.message.acked.1        | Messages acknowledged |

### Forward Client Connected / Disconnected Events to Pulsar

Client goes online, EMQX forwards 'client\_connected' event message to
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

Client goes offline, EMQX forwards 'client\_disconnected' event message
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

### Forward Subscription Event to Pulsar

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

### Forward Unsubscription Event to Pulsar

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

### Forward MQTT Messages to Pulsar

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

### Forwarding MQTT Message Deliver Event to Pulsar

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

### Forwarding MQTT Message Ack Event to Pulsar

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

### Examples of Pulsar Message Consumption

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

### Enable Pulsar Bridge

```bash
./bin/emqx_ctl plugins load emqx_bridge_pulsar
```
