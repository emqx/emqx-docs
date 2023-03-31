# Introduction

{% emqxce %}
EMQX is an open source MQTT broker with a high-performance real-time message processing engine, powering event streaming for IoT devices at massive scale.

As the most scalable MQTT broker, EMQX can help you connect any device, at any scale. Move and process your IoT data anywhere.

{% endemqxce %}

{% emqxee %}
EMQX Enterprise is the world’s most scalable and reliable MQTT messaging platform to connect, move and process your data in business-critical scenarios for the IoT era.

{% endemqxee %}

## Benefits

{% emqxce %}

- **Massive Scale**: Each EMQX node supports up to 2 million concurrent MQTT connections, and each EMQX cluster can support up to 10 million concurrent MQTT connections. 
- **High Performance**: Move and process millions of MQTT messages per second in a single broker.
- **Low Latency**: Guarantee sub-millisecond latency in message delivery with the soft real-time runtime.
- **Fully MQTT 5.0**: 100% compliant with MQTT 5.0 and 3.x standard for better scalability, security, and reliability.
- **High Availability**: Achieve high availability and horizontal scalability through a masterless distributed architecture.
- **Cloud-Native & K8s**: Easy to deploy on-premises or in public clouds with Kubernetes Operator and Terraform.

{% endemqxce %}

{% emqxee %}

### 100% MQTT Compliant

- 100% compliant with MQTT v5.0 and v3.x standards.
- Fully supports QoS 0, 1 & 2 of MQTT message delivery.
- Works with all MQTT clients and libraries like Eclipse Paho.

### Data Security & Privacy

- Ensure data security with MQTT over TLS/SSL.
- Authenticate with LDAP, JWT, PSK, X.509 certificates, and more.
- Rich data management APIs to help keeping your data protection policy in line with EU’s GDPR.

### Cloud-Native

- Adopt cloud-native architecture based on Kubernetes. One-click deployment for on-premises or fully managed service.

### Run Anywhere

- Run Anywhere
- Run anywhere in private, hybrid, and public clouds like AWS, GCP, and Azure without vendor lock-in.

### High ROI (Return on Investment) with Low TCO (Total Cost of Ownership)

- Efficiently use network and server resources.
- Pay for annual subscription, no costly one-off purchases.
- Get high ROI from your IoT solutions with low TCO.

### Global Technical Support

- 7 sites globally, covering USA, Europe, Japan and China.
- More than ten business partners in Europe, USA and India.
- 24/7 worldwide technical support service.

{% endemqxee %}

## Features List

Below is a brief/incomplete feature list, highlighting the features EMQX provides.

### Connectivity

- Full MQTT v3.1, v3.1.1 and v5.0 protocol specification support
  - QoS 0, QoS 1, QoS 2 message support
  - Persistent conversation and offline message support
  - Retained message support
  - Last Will message support
  - Shared subscription support
  - `$SYS/` system topic support
- MQTT supports 4 transport protocols
  - TCP
  - TLS
  - WebSocket
- HTTP message publishing interface support
- Gateways
  - CoAP
  - LwM2M
  - MQTT-SN
  - Stomp
  - GB/T 32960 (Enterprise edition) <!--cannot use 'emqxee' macro inside list-->
  - JT/T 808 (Enterprise edition)
  - OCPP (Enterprise edition)

Advanced features for MQTT:

- Delayed Publish
- Auto subscription
- Topic rewrite

### Security

- Authentication based on username/password supported using built-in database, Redis, MySQL, PostgreSQL, MongoDB as data sources, and HTTP server to provide authentication services
- Authentication based JWT supported using JWKs
- Enhanced Authentication for MQTT 5.0
- PSK authentication
- Access control (Authorization) based on Client ID, IP address, and username supported using built-in database, Redis, MySQL, PostgreSQL, MongoDB as data sources, and HTTP Server to provide authorization services
- API to ban clients

### Scalability

- Multi-server node cluster (Cluster)
- Support `manual`, `dns`, `etcd`, `k8s` cluster discovery methods
- Multi-server node bridge (Bridge)

### Data Integration

- SQL syntax data integrations to extract, filter, enrich, and transform MQTT message or internal events to desired format and export them to external data platforms
- Supports data bridging with other brokers or IoT platforms using MQTT (such as EMQX Cloud, AWS IoT Core, Azure IoT Hub)
- Supports integration with other apps using WebHook
- 30+ enterprise system integrations including Kafka, RDS, various SQL / NoSQL / time-series databases, and enterprise systems such as Oracle and SAP (Enterprise edition)

### Reliability

- Overload protection
- Message rate limit
- Connection rate limit

### Observability

- Client online status query
- Cluster status and metrics query
- Integration with Prometheus/StatsD
- Online log based tracing
- Runtime tracing tools

### Extensibility

- Plugins
- Hooks
- gRPC hook extension
- gRPC protocol extension
