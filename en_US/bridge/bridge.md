# Bridge

EMQX can bridge and forward messages to Kafka, RabbitMQ or other EMQX nodes. Meanwhile, mosquitto and rsm can be bridged to EMQX using common MQTT connection.

## List of Bridge Plugins

| Bridge Plugin        | Config File               | Description        |
| -------------------- | ------------------------- | ------------------ |
| emqx\_bridge\_kafka  | emqx\_bridge\_kafka.conf  | Kafka Bridge       |
| emqx\_bridge\_rabbit | emqx\_bridge\_rabbit.conf | RabbitMQ Bridge    |
| emqx\_bridge\_pulsar | emqx\_bridge\_pulsar.conf | Pulsar Bridge      |
| emqx\_bridge\_mqtt   | emqx\_bridge\_mqtt.conf   | MQTT Broker Bridge |


{% emqxce %}
::: tip
Only the following functions are applicable in EMQX Broker：

- MQTT bridge
- RPC bridge

The rest are exclusive to EMQX Enterprise. It is recommended to use [rule engine] (../rule/rule-engine.md) to realize more flexible bridge function.
:::

{% endemqxce %}


{% emqxee %}

::: tip
It is recommended to use
[rule engine](../rule/rule-engine.md)
to realize more flexible bridge function.
:::

{% endemqxee %}
