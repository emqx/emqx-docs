# Delayed Publish

MQTT messages are usually sent to subscribers as soon as possible by the MQTT broker,
however, in some cases, clients may wish for some delay, or even in a more organized way: to schedule publishes.

The 'scheduling' is represented as the duration of delay from the PUBLISH packet is received by EMQX.
When a client publishes a message to EMQX with the special topic prefix `$delayed/{DelayInteval}`, the delay-publish is triggered.

The specific format of the delay-publish topic is as below:

```bash
$delayed/{DelayInterval}/{TopicName}
```

- `$delayed`: Messages prefixed with `$delay` will be treated as messages that need to be delayed. The delay interval is determined by the content of the next topic level.
- `{DelayInterval}`: Specify the time interval for delaying the publish of this MQTT message with the unit of second. The maximum allowed interval is 4294967 seconds. If `{DelayInterval}` cannot be parsed as an integer number, EMQX Broker will discard the message and the client will not receive any information.
- `{TopicName}`: The topic name of the MQTT message.

E.g:

- `$delayed/15/x/y`: Publish MQTT message to the topic `x/y` after 15 seconds
- `$delayed/60/a/b`: Publish MQTT message to the topic `a/b` after 1 minute
- `$delayed/3600/$SYS/topic`: Publish MQTT message to the topic  `$SYS/topic` after 1 hour

## Config

```bash
delayed {
    enable = true
    max_delayed_messages = 12435
}
```

`enable`: enable or disable delayed publish.

`max_delayed_messages`: the max number of delayed messages.
