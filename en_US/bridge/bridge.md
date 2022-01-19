# Bridge

EMQ X can bridge and forward messages to Kafka, RabbitMQ or other EMQ X nodes. Meanwhile, mosquitto and rsm can be bridged to EMQ X using common MQTT connection.

## List of Bridge Plugins

| Bridge Plugin        | Config File               | Description        |
| -------------------- | ------------------------- | ------------------ |
| emqx\_bridge\_kafka  | emqx\_bridge\_kafka.conf  | Kafka Bridge       |
| emqx\_bridge\_rabbit | emqx\_bridge\_rabbit.conf | RabbitMQ Bridge    |
| emqx\_bridge\_pulsar | emqx\_bridge\_pulsar.conf | Pulsar Bridge      |
| emqx\_bridge\_mqtt   | emqx\_bridge\_mqtt.conf   | MQTT Broker Bridge |


::: tip
Only the following functions are applicable in EMQ X Broker：

- MQTT bridge
- RPC bridge

The rest are exclusive to EMQ X Enterprise. It is recommended to use [rule engine] (../rule/rule-engine.md) to realize more flexible bridge function.
:::


