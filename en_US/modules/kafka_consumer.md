# Kafka Consumer Group

The Kafka consumer group uses external Kafka as a message queue, which can convert consumer messages from Kafka into MQTT messages and publish them in emqx.

Set up the Kafka environment, taking MacOS X as an example:

```bash
wget https://archive.apache.org/dist/kafka/2.8.0/kafka_2.13-2.8.0.tgz

tar -xzf  kafka_2.13-2.8.0.tgz

cd kafka_2.13-2.8.0

# Start Zookeeper
$ ./bin/zookeeper-server-start.sh config/zookeeper.properties
# Start Kafka
$ ./bin/kafka-server-start.sh config/server.properties
```

::: tip

Kafka consumer groups do not support Kafka versions below 0.9

Before creating resources, you need to create Kafka topics in advance, otherwise an error will be prompted

:::

Create Kafka topic:

```bash
./bin/kafka-topics.sh --zookeeper localhost:2181 --replication-factor 1 --partitions 1 --topic testTopic --create
```

## Create module

Open [EMQX Dashboard](http://127.0.0.1:18083/#/modules), click the "Modules" tab on the left, and choose to add:

Select Kafka consumer group module:

![img](./assets/modules.png)

Click "Select", and then select "Kafka Consumer Group":

![img](./assets/kafka_consumer2.png)

Fill in the relevant parameters:

![img](./assets/kafka_consumer3.png)

- Kafka server address
- Kafka consumer connection pool size
- Kafka subscription topic
- MQTT message subject
- MQTT theme service quality
- MQTT Payload. Use Kafka message.value or entire message
- Binary Key encode mode, force UTF-8 or base64 encode. The encoding method of the key in the message, if the key value is a non-string or a value that may generate a character set encoding exception, base64 mode is recommended
- Binary Value encode mode, force UTF-8 or base64 encode. The encoding method of the value in the message, if the value is a non-string or a value that may generate a character set encoding exception, base64 mode is recommended
- Kafka Max Bytes (the maximum number of bytes of messages consumed each time from Kafka)
- Kafka Offset Reset Policy (reset Offset policy, reset_to_latest | reset_by_subdcriber)
- Is Kafka consumer reconnected?

After clicking Add, the module is added:

![img](./assets/kafka_consumer4.png)

The resource has been created, now use Dashboard's websocket tool to subscribe to the MQTT topic "TestTopic":

![img](./assets/kafka_consumer5.png)

Use the kafka command line to produce a message:

```bash
./bin/kafka-console-producer.sh --broker-list localhost:9092 --topic TestTopic
```

![img](./assets/kafka_consumer6.png)

The websocket tool of Dashboard received the message "hello-kafka" produced by Kafka:

![img](./assets/kafka_consumer7.png)
