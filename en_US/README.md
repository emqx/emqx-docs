# Introduction (5.0 doc is under construction)

*EMQX* is an open source MQTT message broker based on the Erlang/OTP platform.

Erlang/OTP is an excellent Soft-Realtime, Low-Latency and Distributed development platform.

MQTT is a lightweight message exchange protocol using publish-subscribe pattern.

*EMQX* is designed for massive clients access and realizes fast and low-latency message routing between massive physical network devices:

1. Stable to host large-scale MQTT client connections. In our tests a single node can support up to 2 million connections.
1. Distributed cluster, fast and low-latency message routing, and single-cluster supports tens of thousands of topics.
1. Various authentidation and authorisation options, such as HTTP, PostgreSQL, and MongoDB.
{% emqxee %}
1. Rich data integration solutions, such as Kafka, Pulsar, Oracle, PostgreSQL, MySQL, Redis, MongoDB etc.
{% endemqxee %}
1. Comprehensive IoT protocol support, including MQTT, MQTT-SN, CoAP, LwM2M, and other TCP/UDP based proprietary protocol.
1. Extensible, support customized plugins, such as authentication and other functions.

## Features List

Below is a brief/incomplete highlighting a part of the features EMQX provides.

### Connectivity

- Full MQTT V3.1/V3.1.1 and V5.0 protocol specification support
  - QoS0, QoS1, QoS2 message support
  - Persistent conversation and offline message support
  - Retained message support
  - Last Will message support
- TCP/SSL
- MQTT/WebSocket/SSL
- HTTP message publishing interface support
- Gateways
  - Stomp
  - MQTT-SN
  - CoAP
  - Stomp/SockJS
  - LwM2M

### Advanced MQTT features

- Retained messages
- Shared subscription($share/<group\>/topic)
- Delayed Publish ($delay/topic)
- Auto subscription
- Topic rewrite
- `$SYS/` system events

### Security

- Redis, MySQL, PostgreSQL, MongoDB, HTTP authentication integration
- Browser cookie based authentication
- Access control (ACL) based on client ID, IP address, user name
- PSK authentication
- Client ID or IP address authentication support
- User name and password authentication support
- API to blacklisting clients

### Scalability

- Multi-server node cluster (Cluster)
- Support manual, mcast, dns, etcd, k8s and other cluster discovery methods
- Multi-server node bridge (Bridge)

{% emqxee %}
### Data Integration

- SQL syntax rule-engine to transform and transmit MQTT message or internal events to external
  data platform such as Oracle, PostgreSQL, MySQL, Redis, MongoDB etc.
  For more details, see [TODO]
{% endemqxee %}

### Reliability

- Overload protection
- Message rate limit
- Connection rate limit

### Observability

- Client online status query
- Automatic network partition healing
- Online log based tracing
- Erlang runtime tracing tools

### Extensibility

- Plugins
- Hooks
- gRPC hook extension
- gRPC protocol extension
