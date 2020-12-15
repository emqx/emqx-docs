# RabbitMQ 桥接

EMQ X 桥接转发 MQTT 消息到 RabbitMQ 集群:

![image](./assets/bridge_rabbit.png)

RabbitMQ 桥接插件配置文件: etc/plugins/emqx_bridge_rabbit.conf。

## 配置 RabbitMQ 桥接地址

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

## 配置 RabbitMQ 桥接规则

```bash
## Bridge Hooks
bridge.rabbit.hook.client.subscribe.1 = {"action": "on_client_subscribe", "rabbit": 1, "exchange": "direct:emq.subscription"}

bridge.rabbit.hook.client.unsubscribe.1 = {"action": "on_client_unsubscribe", "rabbit": 1, "exchange": "direct:emq.unsubscription"}

bridge.rabbit.hook.message.publish.1 = {"topic": "$SYS/#", "action": "on_message_publish", "rabbit": 1, "exchange": "topic:emq.$sys"}

bridge.rabbit.hook.message.publish.2 = {"topic": "#", "action": "on_message_publish", "rabbit": 1, "exchange": "topic:emq.pub"}

bridge.rabbit.hook.message.acked.1 = {"topic": "#", "action": "on_message_acked", "rabbit": 1, "exchange": "topic:emq.acked"}
```

## 客户端订阅主题事件转发 RabbitMQ

```bash
routing_key = subscribe
exchange = emq.subscription
headers = [{<<"x-emq-client-id">>, binary, ClientId}]
payload = jsx:encode([{Topic, proplists:get_value(qos, Opts)} || {Topic, Opts} <- TopicTable])
```

## 客户端取消订阅事件转发 RabbitMQ

```bash
routing_key = unsubscribe
exchange = emq.unsubscription
headers = [{<<"x-emq-client-id">>, binary, ClientId}]
payload = jsx:encode([Topic || {Topic, _Opts} <- TopicTable]),
```

## MQTT 消息转发 RabbitMQ

```bash
routing_key = binary:replace(binary:replace(Topic, <<"/">>, <<".">>, [global]),<<"+">>, <<"*">>, [global])
exchange = emq.$sys | emq.pub
headers = [{<<"x-emq-publish-qos">>, byte, Qos},
           {<<"x-emq-client-id">>, binary, pub_from(From)},
           {<<"x-emq-publish-msgid">>, binary, emqx_base62:encode(Id)},
           {<<"x-emqx-topic">>, binary, Topic}]
payload = Payload
```

## MQTT 消息确认 (Ack) 事件转发 RabbitMQ

```bash
routing_key = puback
exchange = emq.acked
headers = [{<<"x-emq-msg-acked">>, binary, ClientId}],
payload = emqx_base62:encode(Id)
```

## RabbitMQ 订阅消费 MQTT 消息示例

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

## 启用 RabbitMQ 桥接插件

```bash
./bin/emqx_ctl plugins load emqx_bridge_rabbit
```
