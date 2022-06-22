# Introduction

*EMQX* is an Open-Source, Cloud-Native, Distributed MQTT Broker for IoT based on the Erlang/OTP platform.

Erlang/OTP is an excellent Soft-Realtime, Low-Latency and Distributed development platform.

MQTT is a lightweight message exchange protocol using publish-subscribe pattern.

*EMQX* is a highly scalable distributed MQTT Broker" with a high-performance real-time message processing engine, powering event streaming for IoT devices at massive scale.

## Design goals

1. Connect Any Device: Connect any device via the open standard IoT protocols MQTT, CoAP, and LwM2M. Compatible with all MQTT clients. Tested with open-source libraries like Eclipse Paho and custom MQTT clients.
2. At Any Scale: In our tests a single node can support up to 2 million connections. Effortlessly handle tens of millions concurrent MQTT connections with an EMQX cluster (Version 5 reached 100 million connections record).
3. Secured Communication: Secured communication with MQTT over TLS/SSL and various authentication mechanisms using username/password, JWT, PSK, X.509 certificates, and more.
4. Real-time Event Processing: Low-code event processing with rich SQL queries powered by the built-in IoT rule engine. Stream millions of real-time IoT events, from device-to-cloud and cloud-to-devices.
5. Ops-Friendly & Great Observability: Easily manage EMQX via CLI, HTTP API, and an elegant dashboard. Monitor and alert with Datadog, Statsd, Prometheus, and Grafana.
6. Gateways & Plugins: Extend and customize the EMQX with Gateways and Plugins. Easily implement proprietary IoT protocols or integrate with micro-services and enterprise DBMS.
7. Run Anywhere: Adopt cloud-native architecture based on Kubernetes. Run anywhere in private, hybrid, and public clouds like AWS, GCP, and Microsoft Azure, from a single server to massive clusters.

## Features List

Below is a brief/incomplete highlighting a part of the features EMQX provides.

### Connectivity

- Full MQTT V3.1/V3.1.1 and V5.0 protocol specification support
  - QoS0, QoS1, QoS2 message support
  - Persistent conversation and offline message support
  - Retained message support
  - Last Will message support
  - Shared subscription($share/<group\>/topic) support
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
  - Stomp/SockJS
  - GB/T 32960 (Enterprise edition) <!--cannot use 'emqxee' macro inside list-->
  - JT/T 808 (Enterprise edition)

Add more features to MQTT:

- Delayed Publish ($delay/topic)
- Auto subscription
- Topic rewrite

### Security

- Authentication based on user name/password supported using built-in database, Redis, MySQL, PostgreSQL, MongoDB, or HTTP as data sources
- Authentication based JWT supported using JWKs
- Enhanced Authentication
- PSK authentication
- Access control (Authorization) based on client ID, IP address, and user name supported using built-in database, Redis, MySQL, PostgreSQL, MongoDB, or HTTP as data sources
- API to blacklisting clients

### Scalability

- Multi-server node cluster (Cluster)
- Support manual, mcast, dns, etcd, k8s and other cluster discovery methods
- Multi-server node bridge (Bridge)

### Data Integration

- SQL syntax rule-engine to extract, filter, enrich, and transform MQTT message or internal events to formats expected by external data platforms
- Supports data bridging with other brokers or IoT platforms using MQTT(such as EMQX Cloud, AWS IoT Core, Azure IoT Hub)
- Supports integration with other apps using WebHook
{emqxee}
- Enterprise: TODO
{endemqxee}

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
