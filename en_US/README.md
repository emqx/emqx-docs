# Introduction

{% emqxce %}

[EMQX](https://www.emqx.com/en/products/emqx) is an Open-source [MQTT](https://mqtt.org/) broker with a high-performance real-time message processing engine, powering event streaming for IoT devices at massive scale.

As the most scalable MQTT broker, EMQX can help you connect any device, at any scale. Move and process your IoT data anywhere.

{% endemqxce %}

{% emqxee %}

EMQX Enterprise is the world’s most scalable and reliable MQTT messaging platform to connect, move and process your data in business-critical scenarios for the IoT era.

<img src="./assets/EMQX-enterprise.png" alt="EMQX-enterprise" style="zoom:50%;" />

{% endemqxee %}

## Benefits

{% emqxce %}

- **[Massive Scale](https://www.emqx.io)**: Scale to 100 million concurrent MQTT connections with a single EMQX 5.0 cluster.
- **High Performance**: Move and process millions of MQTT messages per second in a single broker.
- **Low Latency**: Guarantee sub-millisecond latency in message delivery with the soft real-time runtime.
- **[Fully MQTT 5.0](https://www.emqx.com/en/blog/introduction-to-mqtt-5)**: 100% compliant with MQTT 5.0 and 3.x standard for better scalability, security, and reliability.
- **[High Availability](./deploy/cluster/mria-introduction)**: Achieve high availability and horizontal scalability through a masterless distributed architecture.
- **[Cloud-Native & K8s](https://www.emqx.com/en/emqx-kubernetes-operator)**: Easy to deploy on-premises or in public clouds with Kubernetes Operator and Terraform.

{% endemqxce %}


{% emqxee %}

- **Massive Scale**: Scale horizontally to 20+ nodes in a single cluster for 100M MQTT connections.

- **Business-Critical Reliability**: Up to 99.99% SLA. Ensure no data loss with built-in RocksDB data persistence.

- **Data Security**: End-to-end data encryption and fine-grained access control to protect your data.

- **High Performance**: Ingest and process millions of MQTT messages efficiently per second per node.

- **Low Latency**: Guarantee sub-millisecond latency in message delivery with the soft real-time runtime.

- **Complete Observability**: Monitoring, alerting, and advanced end-to-end analysis with real-time MQTT tracing.

{% endemqxee %}

## Features List

Below is a brief/incomplete feature list, highlighting the features EMQX provides.

### Connectivity

- Full MQTT v3.1, v3.1.1 and v5.0 protocol specification support
  - [QoS 0, QoS 1, QoS 2 message support](./mqtt/mqtt-qos.md)
  - [Persistent conversation](./mqtt/mqtt-session-and-message-expiry.md#mqtt-会话d) and offline message support
  - [Retained message support](./mqtt/mqtt-retained-messages.md)
  - [Last Will message support](./mqtt/mqtt-last-will-and-testament.md)
  - [Shared subscription support](./mqtt/mqtt-shared-subscription.md)
  - [`$SYS/` system topic support](./mqtt/mqtt-system-topics.md)
- MQTT supports 4 transport protocols
  - TCP
  - [TLS](./network/emqx-mqtt-tls)
  - [WebSocket](./messaging/mqtt-publish-and-subscribe.md)
  - [QUIC (Experimental)](./mqtt-over-quic/introduction.md)
- HTTP message publishing interface support
- Gateways
  - [CoAP](./gateway/coap.md)
  - LwM2M
  - [MQTT-SN](./gateway/mqttsn.md)
  - [Stomp](./gateway/stomp.md)
  - GB/T 32960 (Enterprise edition) <!--cannot use 'emqxee' macro inside list-->
  - JT/T 808 (Enterprise edition)

Add more features to MQTT:

- [Delayed Publish](./mqtt/mqtt-delayed-publish.md)
- [Auto subscription](./mqtt/mqtt-auto-subscription)
- [Topic rewrite](./mqtt/mqtt-topic-rewrite.md)

### Security

- Authentication based on username/password supported using [built-in database](./access-control/authn/mnesia.md), [Redis](./access-control/authn/redis.md), [MySQL](./access-control/authn/mysql.md), [PostgreSQL](./access-control/authn/postgresql.md), and [MongoDB](./access-control/authn/mongodb.md) as data sources, and [HTTP Server](./access-control/authn/http.md) to provide authentication services
- Authentication based  [JWT](./access-control/authn/jwt.md)  supported using JWKs
- [Enhanced Authentication for MQTT 5.0](./access-control/authn/scram.md)
- PSK authentication
- Access control (Authorization) based on Client ID, IP address, and username supported using [built-in database](./access-control/authz/mnesia.md), [Redis](./access-control/authz/redis.md), [MySQL](./access-control/authz/mysql.md), [PostgreSQL](./access-control/authz/postgresql.md), and [MongoDB](./access-control/authz/mongodb.md) as data sources, and [HTTP Server](./access-control/authz/http.md) to provide authorization services
- [Client blacklist](./access-control/blacklist.md)

### Scalability

- [Multi-server node cluster (Cluster)](./deploy/cluster/introduction.md)
- Support manual cluster and auto cluster discovery(dns、etcd、k8s), see [sreate clster](./deploy/cluster/create-cluster.md)
- Multi-server node bridge (Bridge)

### Data Integration

- [SQL syntax](./data-integration/rules.md) data integrations to extract, filter, enrich, and transform MQTT messages or internal events to formats expected by users and send them to external data platforms
- Supports data bridging with other brokers or IoT platforms using MQTT(such as [EMQX Cloud](https://www.emqx.com/zh/cloud), AWS IoT Core, Azure IoT Hub)
- Supports integration with other apps using WebHook
- Integrate IoT data seamlessly with cloud services and enterprise systems, including Kafka, InfluxDB, MySQL, Redis, GCP PubSub and MongoDB. More integrations are coming soon (EMQX Enterprise only)

### Reliability

- [Overload protection](./deploy/cluster/lb.md)
- [Message rate limit](./rate-limit/rate-limit.md)
- [Connection rate limit](./rate-limit/rate-limit.md)

### Observability

- Client online status query
- [Cluster status and metrics query](./observability/metrics-and-stats.md)
- Integration with [Prometheus](./observability/prometheus.md)/[StatsD](./observability/statsd.md)
- [Automatic network partition healing](./deploy/cluster/introduction.md)
- [Online log based tracing](./observability/tracer.md)
- Erlang runtime tracing tools

### Extensibility

- [Plugins](./extensions/plugins.md)
- [Hooks](./extensions/hooks.md)
- [gRPC hook extension](./extensions/exhook.md)
- gRPC protocol extension
