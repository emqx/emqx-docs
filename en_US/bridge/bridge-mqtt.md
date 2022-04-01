# MQTT Bridge

::: tip

After EMQX version 3.1, a powerful rule engine is introduced to replace plug-ins. It is recommended that you use it. See [Bridge data to MQTT](../rule/bridge_mqtt.md) to setup mqtt bridges in rule engine.

:::

EMQX bridges and forwards MQTT messages to MQTT Broker:

![image](./assets/bridge_mqtt.png)

Config file for MQTT bridge plugin: etc/plugins/emqx\_bridge\_mqtt.conf

## Configure MQTT Bridge

```bash
## Bridge address: node name for local bridge, host:port for remote
bridge.mqtt.aws.address = 127.0.0.1:1883

## Protocol version of the bridge: mqttv3 | mqttv4 | mqttv5
bridge.mqtt.aws.proto_ver = mqttv4

## Whether to enable bridge mode for mqtt bridge
bridge.mqtt.aws.bridge_mode = true

## The ClientId of a remote bridge
bridge.mqtt.aws.client_id = bridge_aws

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

## Configure Topics MQTT Bridge Forwards and Subscribes

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

## Description of Topics MQTT Bridge Forwards and Subscribes

Mountpoint: Mountpoint is used to prefix of topic when forwarding a
message, this option must be used with `forwards`. Forwards the message
whose topic is "sensor1/hello", its topic will change to
"<bridge/aws/emqx1@192.168.1.1/sensor1/hello>" when it reaches the
remote node.

Forwards: Messages forwarded to `forwards` specified by local EMQX are
forwarded to the remote MQTT Broker.

Subscription: Local EMQX synchronizes messages from a remote MQTT
Broker to local by subscribing to the topic of the remote MQTT Broker.

## Enable MQTT Bridge

```bash
./bin/emqx_ctl plugins load emqx_bridge_mqtt
```

## Bridge CLI Command

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

## List Status of All Bridges

```bash
$ ./bin/emqx_ctl bridges list
name: emqx     status: Stopped
```

## Start Specified Bridge

```bash
$ ./bin/emqx_ctl bridges start emqx
Start bridge successfully.
```

## Stop Specified Bridge

```bash
$ ./bin/emqx_ctl bridges stop emqx
Stop bridge successfully.
```

## List Forwarded Topic of Specified Bridge

```bash
$ ./bin/emqx_ctl bridges forwards emqx
topic:   topic1/#
topic:   topic2/#
```

## Add Forwarded Topic for Specified Bridge

```bash
$ ./bin/emqx_ctl bridges add-forward emqx topic3/#
Add-forward topic successfully.
```

## Delete Forwarded Topic for Specified Bridge

```bash
$ ./bin/emqx_ctl bridges del-forward emqx topic3/#
Del-forward topic successfully.
```

## List Subscriptions of Specified Bridge

```bash
$ ./bin/emqx_ctl bridges subscriptions emqx
topic: cmd/topic1, qos: 1
topic: cmd/topic2, qos: 1
```

## Add Subscriptions for Specified Bridge

```bash
$ ./bin/emqx_ctl bridges add-subscription emqx cmd/topic3 1
Add-subscription topic successfully.
```

## Delete Subscriptions of Specified Bridge

```bash
$ ./bin/emqx_ctl bridges del-subscription emqx cmd/topic3
Del-subscription topic successfully.
```
