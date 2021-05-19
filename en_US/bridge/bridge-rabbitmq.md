# RabbitMQ Bridge

::: tip

After EMQ X version 3.1, a powerful rule engine is introduced to replace plug-ins. It is recommended that you use it. See [Bridge data to RabbitMQ](../rule/bridge_rabbitmq.md) to setup RabbitMQ bridges in rule engine.

:::

EMQ X bridges and forwards MQTT messages to RabbitMQ cluster:

![image](./assets/bridges_2.png)

Config file of RabbitMQ bridge plugin:
etc/plugins/emqx\_bridge\_rabbit.conf

## Configure RabbitMQ Cluster

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

## Configure RabbitMQ Bridge Hooks

```bash
## Bridge Hooks
bridge.rabbit.hook.client.subscribe.1 = {"action": "on_client_subscribe", "rabbit": 1, "exchange": "direct:emq.subscription"}

bridge.rabbit.hook.client.unsubscribe.1 = {"action": "on_client_unsubscribe", "rabbit": 1, "exchange": "direct:emq.unsubscription"}

bridge.rabbit.hook.message.publish.1 = {"topic": "$SYS/#", "action": "on_message_publish", "rabbit": 1, "exchange": "topic:emq.$sys"}

bridge.rabbit.hook.message.publish.2 = {"topic": "#", "action": "on_message_publish", "rabbit": 1, "exchange": "topic:emq.pub"}

bridge.rabbit.hook.message.acked.1 = {"topic": "#", "action": "on_message_acked", "rabbit": 1, "exchange": "topic:emq.acked"}
```

## Forward Subscription Event to RabbitMQ

```python
routing_key = subscribe
exchange = emq.subscription
headers = [{<<"x-emq-client-id">>, binary, ClientId}]
payload = jsx:encode([{Topic, proplists:get_value(qos, Opts)} || {Topic, Opts} <- TopicTable])
```

## Forward Unsubscription Event to RabbitMQ

```python
routing_key = unsubscribe
exchange = emq.unsubscription
headers = [{<<"x-emq-client-id">>, binary, ClientId}]
payload = jsx:encode([Topic || {Topic, _Opts} <- TopicTable]),
```

## Forward MQTT Messages to RabbitMQ

```python
routing_key = binary:replace(binary:replace(Topic, <<"/">>, <<".">>, [global]),<<"+">>, <<"*">>, [global])
exchange = emq.$sys | emq.pub
headers = [{<<"x-emq-publish-qos">>, byte, Qos},
           {<<"x-emq-client-id">>, binary, pub_from(From)},
           {<<"x-emq-publish-msgid">>, binary, emqx_base62:encode(Id)}]
payload = Payload
```

## Forward MQTT Message Ack Event to RabbitMQ

```python
routing_key = puback
exchange = emq.acked
headers = [{<<"x-emq-msg-acked">>, binary, ClientId}],
payload = emqx_base62:encode(Id)
```

## Example of RabbitMQ Subscription Message Consumption

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

## Enable RabbitMQ Bridge

```bash
./bin/emqx_ctl plugins load emqx_bridge_rabbit
```