# Introduction

*EMQ X* (Erlang/Enterprise/Elastic MQTT Broker) is an open source IoT MQTT message broker based on the Erlang/OTP platform.

Erlang/OTP is an excellent Soft-Realtime, Low-Latency and Distributed development platform.

MQTT is a lightweight message exchange protocol using publish-subscribe pattern.

*EMQ X* is designed for massive clients access and realizes fast and low-latency message routing between massive physical network devices:

1. Stable to host large-scale MQTT client connections, and a single server node supports 2 million connections.
2. Distributed cluster, fast and low-latency message routing, and single-cluster supports tens of thousands of routes.
3. Extensible, support customized plugins, such as authentication and other functions.
4. Comprehensive IoT protocol support, including MQTT, MQTT-SN, CoAP, LwM2M, and other TCP/UDP based proprietary protocol.

## Features List

- Full MQTT V3.1/V3.1.1 and V5.0 protocol specification support
  - QoS0, QoS1, QoS2 message support
  - Persistent conversation and offline message support
  - Retained message support
  - Last Will message support
- TCP/SSL connection support
- MQTT/WebSocket/SSL support
- HTTP message publishing interface support
- $SYS/# system theme support
- Client online status query and subscription support
- Client ID or IP address authentication support
- User name and password authentication support
- LDAP authentication
- Redis, MySQL, PostgreSQL, MongoDB, HTTP authentication integration
- Browser cookie authentication
- Access control (ACL) based on client ID, IP address, user name
- Multi-server node cluster (Cluster)
- Support manual, mcast, dns, etcd, k8s and other cluster discovery methods
- Automatic network partition healing
- Message rate limit
- Connection rate limit
- Configure nodes by partition
- Multi-server node bridge (Bridge)
- MQTT Broker bridge support
- Stomp protocol support
- MQTT-SN protocol support
- CoAP protocol support
- Stomp/SockJS support
- Delay Publish ($delay/topic)
- Flapping detection
- Blacklist support
- Shared subscription($share/<group\>/topic)
- TLS/PSK support
- Rule engine
  - No action (debug)
  - Message republish
  - Bridge data to MQTT Broker
  - Check (debug)
  - Send data to web service
