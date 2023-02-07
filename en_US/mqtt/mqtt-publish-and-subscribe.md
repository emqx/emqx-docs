# Publish/Subscribe Model

The publish/subscribe model is different from the traditional client/server model. It separates the client (publisher) that sends the message from the client (subscriber) that receives the message, and there is no need to build a direct connection between the publisher and the subscriber.

The MQTT protocol adopts this publish/subscribe model  and routes messages based on topics rather than content. Each message contains a topic, and the agent does not need to parse user data. This provides the possibility to implement a general-purpose, business-independent MQTT agent.

MQTT topics (Topic) are similar to URLs, for example:

```bash
chat/room/1
sensor/10/temperature
sensor/+/temperature
$SYS/broker/metrics/packets/received
$SYS/broker/metrics/#
```

Topic (Topic) is separated by `/` and supports wildcards like `+` and `#`:

```bash
'+': Indicates a wildcard level, such as a/+, matches a/x, a/y
'#': Indicates multiple levels of wildcarding, such as a/#, matches a/x, a/b/c/d
```

Subscribers and publishers communicate to each other via topic routing messages, for example, when using the MQTTX CLI command line to publish subscription messages:

```bash
mqttx pub -t a/b/+ -q 1
mqttx sub -t a/b/c -m hello -q 1
```

Subscribers can subscribe to topics containing wildcards, but publishers are not allowed to publish messages to topics containing wildcards.

::: tip

For more information about MQTT publish-subscribe model and MQTT topics, please refer to:

- [*Introduction to MQTT publish-subscribe model*](https://www.emqx.com/en/blog/mqtt-5-introduction-to-publish-subscribe-model)
- [*Understanding MQTT Topics & Wildcards by Case*](https://www.emqx.com/en/blog/advanced-features-of-mqtt-topics)

:::
