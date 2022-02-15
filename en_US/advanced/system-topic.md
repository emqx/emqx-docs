# $SYS - System Topic

The EMQ X Broker periodically publishes its running status, message statistics, client online and offline events to the system topic starting with `$SYS/`. 

 The `$SYS` topic path begins with `$SYS/brokers/{node}/`. `{node}` is the name of the node where the event/message is generated, for example: 

```bash
$SYS/brokers/emqx@127.0.0.1/version
$SYS/brokers/emqx@127.0.0.1/uptime
```


 $SYS system message publish interval is configured in `etc/emqx.conf`: 

```bash
broker.sys_interval = 1m
```
{% emqxce %}

::: danger
By default, only MQTT clients on localhost is allowed to subscribe to the $SYS topic. Please refer to [build-in ACL](./acl-file.md) to modify the ACL rules for publish and subscription.

{% endemqxce %}

Most of the data of the $SYS topic in EMQ X Broker can be obtained through other method with lower Couplings. The device online and offline status can be obtained through [Webhook](./webhook.md)), and the node and cluster status can be obtained through  [HTTP API - Statistics Metrics](./http-api.md#endpoint-metrics).

:::

## Cluster status information

| Topic                      | Description       |
| ----------------------------- | -------------------- |
| $SYS/brokers                  | cluster node list |
| $SYS/brokers/\${node}/version  | EMQ X Broker version |
| $SYS/brokers/\${node}/uptime   | EMQ X Broker startup time |
| $SYS/brokers/\${node}/datetime | EMQ X Broker time |
| $SYS/brokers/\${node}/sysdescr | EMQ X Broker description |

## Client Online and Offline Events

`$SYS` topic prefix: `$SYS/brokers/${node}/clients/`

| Topic              | Description                          |
| ------------------------ | ---------------------------------------- |
| ${clientid}/connected    | Online event. This message is published when a client goes online |
| ${clientid}/disconnected | Offline event. This message is published when a client is offline |

 The Payload of the ‘connected’ event message can be parsed into JSON format: 

```bash
{
    "username":"undefined",
    "ts":1582687922392,
    "sockport":1883,
    "proto_ver":5,
    "proto_name":"MQTT",
    "keepalive":300,
    "ipaddress":"127.0.0.1",
    "expiry_interval":0,
    "connected_at":1582687922,
    "connack":0,
    "clientid":"emqtt-8348fe27a87976ad4db3",
    "clean_start":true
}
```

 The Payload of the ‘disconnected’ event message can be parsed into JSON format: 

```bash
{
    "username":"undefined",
    "ts":1582688032203,
    "reason":"tcp_closed",
    "disconnected_at":1582688032,
    "clientid":"emqtt-8348fe27a87976ad4db3"
}
```

## Statistics

System topic prefix : `$SYS/brokers/${node}/stats/`

### Client statistics

| Topic       | Description |
| ----------------- | -------------- |
| connections/count | Total number of current clients |
| connections/max   | Maximum number of clients |

### Subscription statistics

| Topic                | Description  |
| -------------------------- | ---------------- |
| suboptions/count           | number of current subscription options |
| suboptions/max             | total number of maximum subscription options |
| subscribers/count          | number of current subscribers |
| subscribers/max            | maximum number of subscriptions |
| subscriptions/count        | total number of current subscription |
| subscriptions/max          | maximum number of subscriptions |
| subscriptions/shared/count | total number of current shared subscriptions |
| subscriptions/shared/max   | maximum number of shared subscriptions |

### Topic statistics

| Topic  | Description |
| ------------ | --------------- |
| topics/count | total number of current topics |
| topics/max   | maximum number of topics |

### Routes statistics

| Topic  | Description  |
| ------------ | ---------------- |
| routes/count | total number of current Routes |
| routes/max   | maximum number of Routes |

 The topics/count and topics/max are numerically equal to routes/count and routes/max.

### Throughput (bytes/packets/message) statistics

 System Topic Prefix : `$SYS/brokers/${node}/metrics/`

### sent and received bytes statistics

| Topic          | Description                |
| -------------- | ------------ |
| bytes/received | Accumulated received bytes |
| bytes/sent     | Accumulated sent bytes |

### sent and received MQTT packets statistics

| Topic                        | Description                                      |
| ---------------------------- | ------------------------------------------------ |
| packets/received             | Accumulative received MQTT packets               |
| packets/sent                 | Accumulative sent MQTT packets                   |
| packets/connect/received     | Accumulative received packets of CONNECT         |
| packets/connack/sent         | Accumulative sent packets of CONNACK             |
| packets/publish/received     | Accumulative received packets of PUBLISH         |
| packets/publish/sent         | Accumulative sent packets of PUBLISH             |
| packets/publish/error        | Accumulative handling packets of PUBLISH error   |
| packets/publish/auth_error   | Accumulative denied packets of PUBLISH           |
| packets/publish/dropped      | Accumulative dropped packets of PUBLISH          |
| packets/puback/received      | Accumulative received packets of PUBACK          |
| packets/puback/sent          | Accumulative sent packets of PUBACK              |
| packets/puback/inuse         | Accumulative dropped packets of PUBACK           |
| packets/puback/missed        | Accumulative missed packets of PUBACK            |
| packets/pubrec/received      | Accumulative received packets of PUBREC          |
| packets/pubrec/sent          | Accumulative sent packets of PUBREC              |
| packets/pubrec/inuse         | Accumulative dropped packets of PUBREC           |
| packets/pubrec/missed        | Accumulative missed packets of PUBREC            |
| packets/pubrel/received      | Accumulative received packets of PUBREL          |
| packets/pubrel/sent          | Accumulative sent packets of PUBREL              |
| packets/pubrel/missed        | Accumulative missed packets of PUBREL            |
| packets/pubcomp/received     | Accumulative received packets of PUBCOMP         |
| packets/pubcomp/sent         | Accumulative sent packets of PUBCOMP             |
| packets/pubcomp/inuse        | Accumulative dropped packets of PUBCOMP          |
| packets/pubcomp/missed       | Accumulative missed packets of PUBCOMP           |
| packets/subscribe/received   | Accumulative received packets of SUBSCRIBE       |
| packets/subscribe/error      | Accumulative handling packets of SUBSCRIBE error |
| packets/subscribe/auth_error | Accumulative denied packets of SUBSCRIBE         |
| packets/suback/sent          | Accumulative sent packets of SUBACK              |
| packets/unsubscribe/received | Accumulative received packets of UNSUBSCRIBE     |
| packets/unsuback/sent        | Accumulative sent packets of UNSUBACK            |
| packets/pingreq/received     | Accumulative received packets of PINGREQ         |
| packets/pingresp/sent        | Accumulative sent packets of PINGRESP            |
| packets/disconnect/received  | Accumulative received packets of DISCONNECT      |
| packets/disconnect/sent      | Accumulative sent packets of DISCONNECT          |
| packets/auth/received        | Accumulative received packets of AUTH            |
| packets/auth/sent            | Accumulative sent packets of AUTH                |

### MQTT sent and received messages statistics

| Topic                           | Description                                      |
| ------------------------------- | ------------------------------------------------ |
| messages/received               | Accumulative received messages                   |
| messages/sent                   | Accumulative sent messages                       |
| messages/qos0/received          | Accumulative received messages of QoS0           |
| messages/qos0/sent              | Accumulative sent messages of QoS0               |
| messages/qos1/received          | Accumulative received messages QoS1              |
| messages/qos1/sent              | Accumulative sent messages QoS1                  |
| messages/qos2/received          | Accumulative received messages of QoS2           |
| messages/qos2/sent              | Accumulative sent messages of QoS2               |
| messages/publish                | Accumulative PUBLISH messages                    |
| messages/dropped                | Total number of dropped messages                 |
| messages/dropped/expired        | Total number of dropped messages (Expired)       |
| messages/dropped/no_subscribers | Total number of dropped messages (No subscriber) |
| messages/forward                | Total number of messages forwarded by the node   |
| messages/retained               | Accumulative retained messages                   |
| messages/delayed                | Accumulative delayed messages                    |
| messages/delivered              | Accumulative delivered messages                  |
| messages/acked                  | Accumulative acked messages                      |

## Alarms - system alarms

System Topic Prefix: `$SYS/brokers/${node}/alarms/`

| Topic | Description           |
| ----------- | ------------ |
| alert       | newly generated alarm |
| clear       | cleared alarm |

## Sysmon - system monitoring

System Topic Prefix: `$SYS/brokers/${node}/sysmon/`

| Topic          | Description                         |
| -------------- | ----------------- |
| long_gc        | GC Overtime alarm |
| long_schedule  | Alarm for Excessive Scheduling Time |
| large_heap     | ALarm for Heap Memory Occupancy |
| busy_port      | Alarm for Port busy |
| busy_dist_port | Alarm for Dist Port busy |
