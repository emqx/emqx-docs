# MQTT Bridging

An MQTT bridge in a EMQX node is essentially an MQTT client
which can publish (forward) message to a remote MQTT broker,
or subscribe (pull) messages from a remote MQTT broker.

![image](../assets/bridge.png)

Publishers can publish messages to remote brokers via bridging:

![image](../assets/bridges_3.png)

EMQX Broker distinguishes different bridges based on different names.
Bridge can be added in `etc/emqx.conf` :

```bash
bridge.mqtt.aws.address = 211.182.34.1:1883

bridge.mqtt.azure.address = 54.33.120.8:1883
```

This configuration declares two bridges with the name of `aws` and `azure`, which respectively point to the responding service address using MQTT bridging method.

## MQTT bridging example
For MQTT bridging, it makes EMQX Broker connect as a MQTT client to a remote MQTT broker.

First you need to configure the MQTT client parameters:

Remote Broker Address:

```bash
bridge.mqtt.aws.address = 211.182.34.1:1883
```

MQTT protocol version, which can be one of  `mqttv3`, ` mqttv4`  or  `mqttv5`:

```bash
bridge.mqtt.aws.proto_ver = mqttv4
```

The clientid of the MQTT client:

```bash
bridge.mqtt.aws.clientid = bridge_emq
```

The username field of the MQTT client:

```bash
bridge.mqtt.aws.username = user
```

The password field of the MQTT client:

```bash
bridge.mqtt.aws.password = passwd
```

Keepalive configuration:

```bash
bridge.mqtt.aws.keepalive = 60s
```

The client's clean_start field. Some IoT Hubs require that the clean_start (or clean_session) field must be `true`:

```bash
bridge.mqtt.aws.clean_start = true
```

The reconnection interval can be set:

```bash
bridge.mqtt.aws.reconnect_interval = 30s
```

If TLS connection is used, you can set `bridge.mqtt.aws.ssl = on` and set the TLS certificate:

```bash
bridge.mqtt.aws.ssl = off
bridge.mqtt.aws.cacertfile = etc/certs/cacert.pem
bridge.mqtt.aws.certfile = etc/certs/client-cert.pem
bridge.mqtt.aws.keyfile = etc/certs/client-key.pem
bridge.mqtt.aws.ciphers = ECDHE-ECDSA-AES256-GCM-SHA384,ECDHE-RSA-AES256-GCM-SHA384
bridge.mqtt.aws.tls_versions = tlsv1.2,tlsv1.1,tlsv1
```

Next, we define the `forwards` rule, so that messages sent by this node to ` sensor1/# `and ` sensor2/# `will be forwarded to the remote broker:

```bash
bridge.mqtt.aws.forwards = sensor1/#,sensor2/#
```

You can also specify the retry interval for QoS1 and QoS2 messages and the number of packet sent in bulk:

```bash
bridge.mqtt.aws.retry_interval = 20s
bridge.mqtt.aws.max_inflight_batches = 32
```

If you want to add a specific prefix to the topic forwarding the message to `aws`,
you can set the mount point.

```bash
bridge.mqtt.aws.mountpoint = bridge/aws/${node}/
```

If you want your local broker to "pull" messages from remote brokers,
you can subscribe to certain topics from remote brokers:

```bash
bridge.mqtt.aws.subscription.1.topic = cmd/topic1
bridge.mqtt.aws.subscription.1.qos = 1
```

### Publishing bridge message buffer

EMQX Broker's Bridge has a message buffering mechanism.
When the Bridge is disconnected, the messages of the forwards topic are buffered.
The buffered messages are (re)sent to the remote broker when connection recovers.

Set the total cache queue size:

```bash
bridge.mqtt.aws.queue.max_total_size = 5GB
```

Cache messages to a certain path to the disk (Only cache to memory if not set):

```bash
bridge.mqtt.emqx2.queue.replayq_dir = data/emqx_emqx2_bridge/
```

Set the size of a single cache file. If it exceeds, a new file will be created
to store the message queue:

```bash
bridge.mqtt.emqx2.queue.replayq_seg_bytes = 10MB
```
