# Auto Subscribe

With Auto Subscription enabled, after a client is successfully connected to EMQX,
it may not need to send `SUBSCRIBE` requests to EMQX,
because EMQX will complete the pre-defined subscriptions for the client.

Prior to version 5, this feature is also known as "proxy subscription".

## Configuration

### Configuration Definition

| Field          | Definition                    | Range                                                       | Default |
| -------------- | ----------------------------- | ----------------------------------------------------------- | ------- |
| auto_subscribe | Auto subscribe configurations | topics                                                      | topics  |
| topics         | Subscription Options          | Subscription configurations list. See `Subscription Option` | []      |

#### Subscription Option

| Field | Definition                                                                                                   | Range                                                           | Default          |
| ----- | ------------------------------------------------------------------------------------------------------------ | --------------------------------------------------------------- | ---------------- |
| topic | Required. Topic                                                                                              | String, placeholders supported                                  | No default value |
| qos   | Not Required. Subscription QoS                                                                               | 0 or 1 or 2. Refer to the MQTT QoS definition                   | 0                |
| rh    | Not Required. MQTT version 5.0. Whether to send retain message when a subscription is created.               | 0: Not send the retain message </br>1: Send  the retain message | 0                |
| rap   | Not Required. MQTT version 5.0. When forwarding messages, Whether to send with retain flag                   | 0: Set retain 0</br>1: Keep retain flag                         | 0                |
| nl    | Not Required. MQTT version 5.0. Whether the message can be forwarded to the client when published by itself | 0: Forwarded to self</br>1: Not forwarded to self               | 0                |

#### Subscription Placeholders

| Placeholder | Definition                             |
| ----------- | -------------------------------------- |
| ${clientid} | Client ID                              |
| ${username} | Client Username                        |
| ${ip}       | Client TCP connection local IP address |
| ${port}     | Client TCP connection local Port       |

### Quick Start

Add the following configuration items to the configuration file

```
auto_subscribe {
    topics = [
        {
            topic = "c/${clientid}"
        },
        {
            topic = "client/${clientid}/username/${username}/host/${host}/port/${port}"
            qos   = 1
            rh    = 0
            rap   = 0
            nl    = 0
        }
    ]
}
```

```text
+---------------------------+             +----------------+
| clientid: demo_client1    |             |  EMQX Broker   |
| username: admin           |             |                |
| local host: 192.168.1.234 |<----------->|                |
| local port: 55678         |             |                |
+---------------------------+             +----------------+
```

When the client uses versions lower than 5, the following subscriptions are available after connection.

```text
topic: c/demo_client1
qos: 0
```

```text
topic: client/demo_client1/username/admin/host/192.168.1.234/port/55678
qos: 1
```

When the client uses version 5, the following subscriptions are available after connection.

```text
topic: c/demo_client1
qos: 0
rh: 0
rap: 0
nl: 0
```

```text
topic: client/demo_client1/username/admin/host/192.168.1.234/port/55678
qos: 1
rh: 0
rap: 0
nl: 0
```
