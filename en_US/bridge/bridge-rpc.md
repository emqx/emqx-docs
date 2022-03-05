# RPC Bridge

::: tip

After EMQX version 3.1, a powerful rule engine is introduced to replace plug-ins. It is recommended that you use it. See [Bridge data to EMQX](../rule/bridge_emqx.md) to setup EMQX bridges in rule engine.

:::

EMQX bridges and forwards MQTT messages to remote EMQX:

![image](./assets/bridge_rpc.png)

Config file for RPC bridge plugin: etc/plugins/emqx\_bridge\_mqtt.conf

## Configure Broker Address for RPC Bridge

```bash
bridge.mqtt.emqx.address = emqx2@192.168.1.2
```

## Configure Topics RPC Bridge Forwards and Subscribes

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

## Bridge CLI Command

CLI of RPC bridge is used in the same way as the MQTT bridge.

