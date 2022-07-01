# Introduction

**EMQX** is an Open-source MQTT broker with a high-performance real-time message processing engine, powering event streaming for IoT devices at massive scale.

As the most scalable MQTT broker, EMQX can help you connect any device, at any scale. Move and process your IoT data anywhere.

## Benefits

- **Massive Scale**: Scale to 100 million concurrent MQTT connections with a single EMQX 5.0 cluster.
- **High Performance**: Move and process millions of MQTT messages per second in a single broker.
- **Low Latency**: Guarantee sub-millisecond latency in message delivery with the soft real-time runtime.
- **Fully MQTT 5.0**: 100% compliant with MQTT 5.0 and 3.x standard for better scalability, security, and reliability.
- **High Availability**: Achieve high availability and horizontal scalability through a masterless distributed architecture.
- **Cloud-Native & K8s**: Easy to deploy on-premises or in public clouds with Kubernetes Operator and Terraform.

## Features List

Below is a brief/incomplete highlighting a part of the features EMQX provides.

### Connectivity

- Full MQTT V3.1/V3.1.1 and V5.0 protocol specification support
  - QoS0, QoS1, QoS2 message support
  - Persistent conversation and offline message support
  - Retained message support
  - Last Will message support
  - Shared subscription support
  - `$SYS/` system topic support
- TCP/SSL
- MQTT/WebSocket/SSL
- MQTT Over QUIC(In Preview)
- HTTP message publishing interface support
- Gateways
  - CoAP
  - LwM2M
  - MQTT-SN
  - Stomp
  - GB/T 32960 (Enterprise edition) <!--cannot use 'emqxee' macro inside list-->
  - JT/T 808 (Enterprise edition)

Add more features to MQTT:

- Delayed Publish
- Auto subscription
- Topic rewrite

### Security

- Authentication based on user name/password supported using built-in database, Redis, MySQL, PostgreSQL, MongoDB, or HTTP as data sources
- Authentication based JWT supported using JWKs
- Enhanced Authentication for MQTT 5.0
- PSK authentication
- Access control (Authorization) based on client ID, IP address, and user name supported using built-in database, Redis, MySQL, PostgreSQL, MongoDB, or HTTP as data sources
- API to blacklisting clients

### Scalability

- Multi-server node cluster (Cluster)
- Support `manual`, `mcast`, `dns`, `etcd`, `k8s` cluster discovery methods
- Multi-server node bridge (Bridge)

### Data Integration

- SQL syntax data integrations to extract, filter, enrich, and transform MQTT message or internal events to formats expected by external data platforms
- Supports data bridging with other brokers or IoT platforms using MQTT(such as EMQX Cloud, AWS IoT Core, Azure IoT Hub)
- Supports integration with other apps using Webhook

### Reliability

- Overload protection
- Message rate limit
- Connection rate limit

### Observability

- Client online status query
- Cluster status and metrics query
- Integration with Prometheus/StatsD
- Automatic network partition healing
- Online log based tracing
- Erlang runtime tracing tools

### Extensibility

- Plugins
- Hooks
- gRPC hook extension
- gRPC protocol extension
