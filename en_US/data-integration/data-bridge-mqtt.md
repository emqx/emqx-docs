# Bridge Data into MQTT Broker

The MQTT data bridge is a channel for EMQX to communicate with other MQTT services, including EMQX clusters that use the MQTT protocol. This page introduces how the MQTT data bridge works in EMQX and provides a quick start tutorial on how to create an MQTT data bridge in EMQX Dashboard or using the configuration file.

## MQTT Data Bridge Modes

EMQX supports the MQTT data bridge that works in two primary modes: ingress and egress. The following sections explain how each mode works. Also in this section, it introduces the concept of connection pools used in both modes.

### Ingress Mode

In ingress mode, the local EMQX subscribes to the topics from the bridged remote MQTT brokers and distributes the received messages within the current cluster. Below is the message flow in **ingress** direction:

<img src="./assets/bridge_igress.png" alt="bridge_igress" style="zoom:50%;" />

The MQTT data bridge can be used either alone or in conjunction with rules for more powerful and flexible data processing capabilities. In **ingress** direction, the data bridge can be used as the data source of the rule. The message flow for MQTT data bridge working with rules is as follows:

<img src="./assets/bridge_igress_rule_link.png" alt="bridge_igress_rule_link" style="zoom:50%;" />

### Egress Mode

In egress mode, the local EMQX forwards messages from the current cluster to the bridged remote MQTT brokers following the rule settings. And this is the message flow in **egress** direction:

<img src="./assets/bridge_egerss.png" alt="bridge_egerss" style="zoom:50%;" />

Similar to the ingress mode, the MQTT data bridge can also be used in conjunction with rules. In **egress** direction, the data bridge can be used as the action of the rule:

<img src="./assets/bridge_egress_rule.png" alt="bridge_egress_rule" style="zoom: 50%;" />

### Connection Pool

EMQX allows more than one client to connect to the bridged MQTT broker at the same time. When creating the data bridge, you can set a pool of MQTT client connetions and configure the pool size indicating the number of the client connections in the pool. Enabling connection pool in MQTT data bridge allows for efficient utilization of server resources, leading to greater message throughput and improved concurrency performance. This brings benefits in scenarios with high loads and concurrent connections.

Because the MQTT protocol requires that a client connects to one broker must has it's own unique client ID, each client in the connection pool is assigned to a unique client ID. To make the client ID predictable, EMQX automatically generates the Client ID following the pattern below:

```bash
[${ClientIDPrefix}:]${BridgeName}:${Mode}:${NodeName}:${N}
```

| Fragment            | Description                                                  |
| ------------------- | ------------------------------------------------------------ |
| `${ClientIDPrefix}` | Prefix of the client ID as configured. If not set then whole first fragment is omitted. |
| `${BridgeName}`     | User-provided name of the bridge.                            |
| `${Mode}`           | Either `ingress` or `egress`.                                |
| `${NodeName}`       | [Name of the node](../configuration/cluster.md#node-names) on which the MQTT client is running. |
| `${N}`              | Number from `1` to the configured size of the connection pool. |

#### Use Connection Pool in Ingress Mode

Although the connection pool applies to both ingress and egress modes, using the connection pool in ingress mode has more requirements. When data bridges in ingress mode are created between MQTT brokers in different MQTT [clusters](../deploy/cluster/introduction.md), clients in the connection pool will receive duplicated messages from the remote broker if they all subscribe to the same topic. In that case, it will bring pressure to the brokers, so it is strongly advised to use [shared subscription](../messaging/mqtt-shared-subscription.md) as a kind of safety measure. For example, you can configure the topic of the remote MQTT Broker to  `$share/name1/topic1` or `$share/name2/topic2/#` if topic filter is used. 

## Quick Start Tutorial

The following section will use EMQX [public MQTT broker](https://www.emqx.com/en/mqtt/public-mqtt5-broker) as an example to illustrate how to configure a data bridge between EMQX and this public MQTT broker.

:::tip Prerequisites

- Knowledge about EMQX data integration [rules](./rules.md)
- Knowledge about [data bridges](./data-bridges.md)

:::

### Feature List

- [Async mode](./data-bridges.md#async-mode)
- [Buffer queue](./data-bridges.md#buffer-queue)

<!--  Configuration parameters TODO 链接到配置手册对应配置章节。 -->

### Create MQTT Data Bridge via Dashboard

1. Go to EMQX Dashboard, and click **Data Integration** -> **Data Bridge**.

2. Click **Create** in the top right corner of the page.

3. In the **Create Data Bridge** page, click to select **MQTT**, and then click **Next**.

4. Input a name for the data bridge. The name should be a combination of upper/lower case letters or numbers, for example, `my_mqtt_bridge`.

5. Input the connection information. Input `broker.emqx.io:1883` for **MQTT Broker**. As no authentication is required from the server side, you can leave the **Username** and **Password** blank. For the other fields in this section, you can keep the default value or set it as the actual condition.

6. Set the data bridge rules with the **Ingress** or **Egress** field.

   :::tip
   You can choose to configure either the **Ingress** or **Egress** field or both fields, but at least one field should be set up. Turn on the toggle switch of the corresponding field to start the configuration.
   :::

   - **Ingress** (optional): Set the rules to forward the messages from remote MQTT brokers to local ones. In this example, you forward the messages from `remote/topic/ingress` to `local/topic/ingress`, so you first need to subscribe to the remote topic and then specify the local topics to receive the messages:
     - **Remote MQTT Broker**: Subscribe to the remote topics.
       - **Topic**: In cluster mode, you can use the shared subscription to avoid repeated messages, therefore you can fill in `$share/g/remote/topic/ingress`.
       - **QoS**: Select `0`.
       
     - **Local MQTT Broker**: Forward the received messages to specific local topics or leave them blank, then these messages will first be processed by the configured rules and then forwarded with the [republish action](./rules.md).
       - **Topic**: Input `local/topic/ingress`.
       - **QoS**: Select `0` or `${qos}` (to use the QoS of the received messages).
       - **Retain**: Confirm whether the message will be published as a retained message.
       - **Payload**: Payload template for the messages to be forwarded, and supports reading data using `${field}` syntax.
     
     - **Connection Pool Size**: Specifies the size of the pool of MQTT client connections to the local broker. In this example, you can set `8`. This is safe as long as shared subscription is used for the remote topic.
     
   - **Egress** (optional): Set the rules to publish messages from specific local MQTT topics to remote MQTT brokers. In this example, you publish the messages from `local/topic/egress` to `remote/topic/egress`:
   - **Local MQTT Broker**: Specify the local message topics.
       - **Topic**: Input `local/topic/egress`.
     
   - **Remote MQTT Broker**: Specify the target topics on the remote broker.
       - **Topic**: Input `remote/topic/egress`.
       - **QoS**: Select `0` or `${qos}` (to use the QoS of the received messages).
       - **Retain**: Confirm whether the message will be published as a retained message.
       - **Payload**: Payload template for the messages to be forwarded, and Supports reading data using `${field}` syntax.
     
   - **MQTT Client Pool Size**: Specifies the size of the pool of MQTT client connections to the local broker. In this example, you can set `8`.
   
7. In **Query mode** field, you can configure whether to use sync/async mode and your buffer pool size as your business needs.

7. Before clicking **Create**, you can click **Test Connectivity** to test that the bridge can connect to the MQTT broker.

8. Click **Create** to finish the creation of the data bridge.

### Create MQTT Data Bridge via Configuration File

EMQX also supports to use configuration file to create an MQTT data bridge, and an example is as follows:

```bash
bridges.mqtt.my_mqtt_bridge {
  enable = true
  server = "broker.emqx.io:1883"
  username = "emqx_u"
  password = "public"
  proto_ver = "v4"
  clean_start = true
  keepalive = "60s"

  egress {
    local {topic = "local/topic/egress"}
    remote {
      payload = "${payload}"
      qos = 1
      retain = true
      topic = "remote/topic/egress"
    }
    pool_size = 8
  }
  ingress {
    local {
      topic = "$share/g/remote/topic/ingress"
      qos = 1
      payload = "${payload}"
    }
    remote {qos = 1, topic = "local/topic/ingress"}
    pool_size = 8
  }
}
```

