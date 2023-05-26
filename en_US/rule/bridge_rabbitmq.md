# Ingest Data into RabbitMQ

## Set up RabbitMQ

Set up a RabbitMQ, taking Mac OSX for instance:

```bash
$ brew install rabbitmq

# start rabbitmq
$ rabbitmq-server
```
## Create a Rule

Go to [EMQX Dashboard](http://127.0.0.1:18083/#/rules), select the "rule" tab on the menu to the left.

Select "message.publish", then type in the following SQL:

```sql
SELECT
    *
FROM
    "message.publish"
```

<img src="./assets/rule-engine/mysql_sql_1.png" alt="image" style="zoom:50%;" />

## Add an Action

Click on the "+ Add" button under "Action Handler", and then select
"Data bridge to RabbitMQ" in the pop-up dialog window.

<img src="./assets/rule-engine/rabbit_action_0.png" alt="image" style="zoom:50%;" />

Fill in the parameters required by the action:

Three parameters are required by action "Data bridge to RabbitMQ":

1). RabbitMQ Exchange. Here set it to "messages"

2). RabbitMQ Exchange Type. Here set it to "topic"

3). RabbitMQ Routing Key. Here set it to "test"

<img src="./assets/rule-engine/rabbit_action_1.png" alt="image" style="zoom:50%;" />

4). Bind a resource to the action. Since the dropdown list "Resource"
is empty for now, we create a new resource by clicking on the "New
Resource" to the top right, and then select "RabbitMQ":

<img src="./assets/rule-engine/rabbit_action_2.png" alt="image" style="zoom:50%;" />

### Configure the Resource

Set "RabbitMQ Server" to "127.0.0.1:5672", and keep all other configs
as default, and click on the "Testing Connection" button to make sure
the connection can be created successfully, and then click on the
"Create" button.

<img src="./assets/rule-engine/rabbit_resource_0.png" alt="image" style="zoom:50%;" />

Back to the "Actions" dialog, and then click on the "Confirm"
    button.

<img src="./assets/rule-engine/rabbit_action_3.png" alt="image" style="zoom:50%;" />

Back to the creating rule page, then click on "Create" button. The
    rule we created will be shown in the rule list:

<img src="./assets/rule-engine/rabbit_rule_overview_0.png" alt="image" style="zoom:50%;" />

## Test the Rule

We have finished creating the rule, test the rule by sending an MQTT message to EMQX:

```bash
Topic: "t/1"

QoS: 0

Retained: false

Payload: "Hello, World\!"
```

Write an AMQP Client to consume the messages, following is the one
written in Python:

```python
#!/usr/bin/env python
import pika

connection = pika.BlockingConnection(
    pika.ConnectionParameters(host='localhost'))
channel = connection.channel()

channel.exchange_declare(exchange='messages', exchange_type='topic')

result = channel.queue_declare(queue='', exclusive=True)
queue_name = result.method.queue

channel.queue_bind(exchange='messages', queue=queue_name, routing_key='test')

print('[*] Waiting for messages. To exit press CTRL+C')

def callback(ch, method, properties, body):
    print(" [x] %r" % body)

channel.basic_consume(
    queue=queue_name, on_message_callback=callback, auto_ack=True)

channel.start_consuming()
```

<img src="./assets/rule-engine/rabbit_result.png" alt="image" style="zoom:50%;" />

And from the rule list, verify that the "Matched" column has increased
to 1:

<img src="./assets/rule-engine/rabbit_rule_overview_1.png" alt="image" style="zoom:50%;" />

