---
title: EMQX Operating Limitations
description: Without limits on connections or data transmission, MQTT broker performance can reduce. Therefore, EMQX has set usage limits on major features.
---

# EMQX Operating Limitations

## Background

If there are no limitations on connection numbers or data transmission, users may experience reduced MQTT broker performance, such as a slow network connection or operation response, prolonged message latency, message accumulation, or even message discards, and other issues. On the other hand, the protocol limitation also specify the usage limits.  

EMQX has therefore specified the following quotas and usage limits for some of the major features.

## Reference List

The operating limitations can be classified into:

- Broker resource limitation: Theoretically, there is no maximum limit. The actual limit varies with the server specification.
- Hard coding or protocol limitation: EMQX has set these limitations to comply with the protocols or to ensure stable performance. In some projects, EMQX has modified the default setting (as specified in the protocol) to a more reasonable value. You can change the setting with our configuration file.

| **Description**                       | **Quota**                 |
| ------------------------------------- | ------------------------- |
| **Client Connection**                 |                           |
| Maximum concurrent connection devices | Unlimited                 |
| Maximum device connection rate        | Unlimited                 |
| Maximum MQTT client ID length         | 65535                     |
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
| Number of API Keys                    | 100                       |
| **Dashboard**                         |                           |
| Dashboard users                       | Unlimited                 |
