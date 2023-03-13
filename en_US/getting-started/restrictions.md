# Operating limitations

## Background

As well as following protocols, if there are no limitations on connection numbers or data transmission, users may experience reduced MQTT broker performance, such as a slow network connection or operation response, prolonged message latency, message accumulation, or even message discards, and other issues.

EMQX has therefore specified the following quotas and usage limits for some of our major features.

## Reference list

The operating limitations can be classified into:

- Broker resource limitation: Theoretically, there is no maximum limit, the actual limit varies with the server specification.
- Hard coding or protocol limitation: EMQX has set these limitations to comply with the protocols or to ensure stable performance. Note: In some projects, EMQX has modified the default setting (as specified in the protocol) to a more reasonable value. You can change the setting with our configuration file.

| **Description**                       | **Quota**                 |
| ------------------------------------- | ------------------------- |
| **Client Connection**                 |                           |
| Maximum concurrent connection devices | Unlimited                 |
| maximum device connection rate        | Unlimited                 |
| maximum MQTT client ID length         | 65535                     |
| **Subscription**                      |                           |
| Maximum subscriptions                 | Unlimited                 |
| Maximum subscription rate             | Unlimited                 |
| Per device subscriptions              | Unlimited                 |
| Per device subscription rate          | Unlimited                 |
| **Bandwidth**                         |                           |
| Maximum bandwidth                     | Unlimited                 |
| Per device bandwidth                  | Unlimited                 |
| **MQTT Message**                      |                           |
| Message Bytes                         | Default 1024KB, Max 256MB |
| Maximum QoS                           | 2                         |
| **MQTT Keepalive**                    |                           |
| Maximun Keepalive                     | 65535 seconds             |
| **MQTT Topic**                        |                           |
| Number of topics                      | Unlimited                 |
| Topic level                           | 65535                     |
| Topic length                          | Unlimited                 |
| Number of topic aliases               | 65535                     |
| **MQTT Retained Message**             |                           |
| Single Message Size                   | Default 1204KB, Max 256MB |
| Number of messages                    | Unlimited                 |
| Total messages size                   | Unlimited                 |
| **MQTT 5.0**                          |                           |
| Number of User Properity              | 65535                     |
| **MQTT Add-ons**                      |                           |
| Number of topic rewrite rules         | 30                        |
| Number of auto subscription rules     | 30                        |
| Number of delayed publish             | Unlimited                 |
| Delayed maximum duration              | 4294967 seconds           |
| **Rules**                             |                           |
| Number of rules                       | Unlimited                 |
| Rule execution timeout                | Unlimited                 |
| Number of single rule outputs         | Unlimited                 |
| **Data Bridge**                       |                           |
| Number of data bridges                | Unlimited                 |
| **REST API**                          |                           |
| Max page size                         | 10000                     |
| Number of API Keys                    | 1024                      |
| **Dashboard**                         |                           |
| Dashboard users                       | Unlimited                 |
