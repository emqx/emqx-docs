# Feature Comparison

This page lists features supported across different deployment types in detail.

## Core / Enterprise Features

| Features                         | Self-Hosted (Enterprise)                         | MQTT as a Service (Serverless) | MQTT as a Service (Dedicated Flex) | Notes and Links                                              |
| -------------------------------- | ------------------------------------------------ | ------------------------------ | ---------------------------------- | ------------------------------------------------------------ |
| **MQTT 5.0 Broker**              | ✅                                                | ✅                              | ✅                                  | Complete MQTT 5.0 protocol implementation                    |
| **MQTT over QUIC**               | ✅                                                | ✅                              | ✅                                  | Globally pioneering support<br>In development for Cloud      |
| **MQTT Add-ons**                 | ✅                                                | ❌                              | ✅                                  | [Shared subscription](../messaging/mqtt-shared-subscription.md)<br>[Exclusive subscription](../messaging/mqtt-exclusive-subscription.md)<br>[Delayed publish](../messaging/mqtt-delayed-publish.md)<br>[Auto-subscription](../messaging/mqtt-auto-subscription.md)<br>[Topic rewrite](../messaging/mqtt-topic-rewrite.md)<br>More customization options |
| **Multi-protocol Gateways**      | ✅                                                | ❌                              | ✅                                  | More industry device access                                  |
| **Multi-tenancy**                | ✅                                                | ❌                              | ✅                                  | Higher system flexibility and utilization (Coming soon)      |
| **Cluster Linking**              | ✅                                                | ❌                              | ✅                                  | Seamless connection of devices and application data          |
| **Message Queue**                | ✅                                                | ❌                              | ✅                                  | Unified architecture for data transmission and analysis (Coming soon) |
| **Stream Processing**            | ✅                                                | ❌                              | ✅                                  | Higher reliability and disaster recovery capabilities<br>(Coming soon) |
| **Data Persistence**             | ✅ Built-in RocksDB backend or external databases | N/A                            | N/A                                | [Improved stability and reliability](https://docs.emqx.com/en/emqx/latest/durability/durability_introduction.html) |
| **Schema Registry**              | ✅                                                | ❌                              | ✅                                  | [Schema Registry](https://docs.emqx.com/en/emqx/latest/data-integration/schema-registry.html) ensures data consistency and compatibility |
| **Message Codec**                | ✅                                                | ❌                              | ✅                                  | Flexible message format conversion for:<br>JSON, Avro, Protobuf, Custom codec (HTTP/gRPC) |
| **Schema Validation**            | ✅                                                | ✅                              | ✅                                  | Ensuring integrity and legality of messages                  |
| **Rule Engine**                  | ✅                                                | ✅                              | ✅                                  | [SQL-based built-in Rule Engine and real-time data processing](https://docs.emqx.com/en/emqx/latest/data-integration/rules.html) |
| **Flow Designer**                | ✅                                                | ❌                              | ✅                                  | [Easy orchestration of data integration](https://docs.emqx.com/en/emqx/latest/flow-designer/introduction.html) |
| **File Transfer**                | ✅                                                | ❌                              | ✅ In product roadmap               | Unified platform data transmission                           |
| **Kafka Integration**            | ✅                                                | ✅                              | ✅                                  | [Stream MQTT Data into Apache Kafka](https://docs.emqx.com/en/emqx/latest/data-integration/data-bridge-kafka.html) |
| **Enterprise Data Integrations** | ✅ 40+                                            | ✅                              | ✅ 40+                              | [Accelerate business development and delivery speed](https://www.emqx.com/en/integrations) |
| **Troubleshooting**              | ✅                                                | ❌                              | ✅                                  | [Log Trace](https://docs.emqx.com/en/emqx/latest/observability/tracer.html)<br>[Slow Subscriptions](https://docs.emqx.com/en/emqx/latest/observability/slow-subscribers-statistics.html) |
| **Cloud-Native & K8s**           | ✅                                                | N/A                            | N/A                                | [Reduce system deployment and management costs](https://www.emqx.com/en/deployments) |
| **Edge Computing**               | ✅                                                | ✅                              | ✅                                  | Reduce data transmission latency and costs<br>[Neuron](https://www.emqx.com/en/products/neuronex)<br>[NanoMQ](https://www.emqx.com/en/products/nanomq) |

## Scalability and Performance

| Scalability / Performance | Self-Hosted (Enterprise)                                     | MQTT as a Service (Serverless) | MQTT as a Service (Dedicated Flex) | Notes and Links                                              |
| ------------------------- | ------------------------------------------------------------ | ------------------------------ | ---------------------------------- | ------------------------------------------------------------ |
| **Scalability**           | Up to 100 nodes cluster<br>Up to 100 million MQTT connections per cluster | 1000 auto scale                | 1000 - unlimited                   | [Reaching 100M MQTT connections with EMQX 5.0](https://www.emqx.com/en/blog/reaching-100m-mqtt-connections-with-emqx-5-0) |
| **Availability**          | Core-Replica cluster                                         | Masterless cluster             | Masterless cluster                 |                                                              |
| **Reliability**           | Data persistence in RocksDB with highly available replication | Session persistence            | Session persistence                | [Highly Reliable MQTT Data Persistence Based on RocksDB](https://www.emqx.com/en/blog/mqtt-persistence-based-on-rocksdb) |
| **Performance**           | 5M+ MQTT messages per second                                 | 1000 MQTT messages per second  | 5M+ MQTT messages per second       |                                                              |
| **Latency**               | 1~5 millisecond                                              | 1~5 millisecond                | 1~5 millisecond                    |                                                              |
| **SLA**                   | N/A                                                          | 99.9% uptime                   | Up to 99.99% uptime                |                                                              |

## Clustering Architecture

| Clustering Architecture                      | Self-Hosted (Enterprise) | MQTT as a Service (Serverless) | MQTT as a Service (Dedicated Flex) | Notes and Links                                              |
| -------------------------------------------- | ------------------------ | ------------------------------ | ---------------------------------- | ------------------------------------------------------------ |
| **Nodes of Cluster**                         | 100+                     | confidential                   | confidential                       | Large-scale clusters                                         |
| **Elastic and resilient scaling at runtime** | ✅                        | ❌                              | ✅                                  | Higher system stability and resource utilization             |
| **Autoscaling**                              | ✅                        | ✅                              | ✅                                  |                                                              |
| **Consistency**                              | ✅                        | ✅                              | ✅                                  |                                                              |
| **Transaction**                              | ✅                        | ✅                              | ✅                                  | Ensuring the atomicity and reliability of data operations    |
| **Network Split Recovery**                   | ✅                        | ✅                              | ✅                                  | Cluster failure automatic repair                             |
| **Node Evacuation & Cluster Rebalance**      | ✅                        | N/A                            | N/A                                | Non-stop cluster maintenance                                 |
| **Autocluster Discoveries**                  | ✅                        | N/A                            | N/A                                | static: Discovery based on a static node list<br>mcast: Discovery with UDP multicast mode<br>dns: Discovery based on DNS records<br>etcd: Discovery via etcd<br>k8s: Discovery via Kubernetes service |
| **Zero Downtime / Hot Upgrades**             | ✅                        | N/A                            | N/A                                | Immediate repair of system vulnerabilities                   |
| **Hot Patch**                                | ✅                        | N/A                            | N/A                                | Ensuring stable system operation                             |
| **Overload Protection**                      | ✅                        | N/A                            | N/A                                | Improving system management efficiency                       |
| **Multi-cluster Management**                 | ✅                        | N/A                            | N/A                                | Enhancing system stability                                   |
| **Cluster Metrics**                          | ✅                        | N/A                            | N/A                                |                                                              |

## MQTT and Connectivity

| MQTT & Connectivity        | Self-Hosted (Enterprise) | MQTT as a Service (Serverless) | MQTT as a Service (Dedicated Flex) | Notes and Links                                              |
| -------------------------- | ------------------------ | ------------------------------ | ---------------------------------- | ------------------------------------------------------------ |
| **MQTT 3.x**               | ✅                        | ✅                              | ✅                                  |                                                              |
| **MQTT 5.0**               | ✅                        | ✅                              | ✅                                  |                                                              |
| **MQTT Retainer**          | ✅                        | ✅                              | ✅                                  |                                                              |
| **MQTT over TCP**          | ✅                        | ❌                              | ✅                                  |                                                              |
| **MQTT over TLS**          | ✅                        | ✅                              | ✅                                  |                                                              |
| **MQTT over WebSocket**    | ✅                        | ✅                              | ✅                                  |                                                              |
| **MQTT over QUIC**         | ✅                        | ❌                              | ✅                                  | EMQX is now the only MQTT broker in the world that supports QUIC transport. |
| **LB (Proxy Protocol)**    | ✅                        | ❌                              | ✅                                  | Proxy protocol v1, v2                                        |
| **LB (Custom)**            | ✅                        | ❌                              | ✅                                  | GmSSL<br>Smooth connection migration                         |
| **IPv6 Support**           | ✅                        | ❌                              | ✅                                  |                                                              |
| **Multi-protocol Gateway** | ✅                        | ❌                              | ✅                                  |                                                              |
| **MQTT-SN**                | ✅                        | ❌                              | ✅                                  |                                                              |
| **STOMP**                  | ✅                        | ❌                              | ❌                                  |                                                              |
| **CoAP**                   | ✅                        | ❌                              | ✅                                  |                                                              |
| **LwM2M**                  | ✅                        | ❌                              | ❌                                  |                                                              |
| **ExProto**                | ✅                        | ❌                              | ❌                                  |                                                              |
| **OCPP**                   | ✅                        | ❌                              | ❌                                  |                                                              |
| **JT/808**                 | ✅                        | ❌                              | ✅                                  |                                                              |
| **GB/T 32960**             | ✅                        | ❌                              | ❌                                  |                                                              |

## Security

| Security                      | Self-Hosted (Enterprise) | MQTT as a Service (Serverless) | MQTT as a Service (Dedicated Flex) | Notes and Links                                              |
| ----------------------------- | ------------------------ | ------------------------------ | ---------------------------------- | ------------------------------------------------------------ |
| **TLS/SSL**                   | ✅                        | ✅                              | ✅                                  | Protect data transmission security: TLS 1.1, 1.2, 1.3        |
| **QUIC**                      | ✅                        | ❌                              | ✅                                  | Enhance efficiency of weak network and mobile network data transmission |
| **OCSP Stapling**             | ✅                        | ❌                              | ✅                                  | Provide more flexible security practices                     |
| **Flapping Detect**           | ✅                        | ✅                              | ✅ In product roadmap               | Detect and intercept frequent online and offline connections |
| **Audit Logs**                | ✅                        | ✅                              | ✅                                  | Support audit tracing for important operations               |
| **Dashboard SSO**             | ✅                        | ✅                              | ✅                                  | Secure and simplified authentication processes               |
| **Dashboard / REST API RBAC** | ✅                        | ✅                              | ✅                                  | Minimize permissions to ensure system security               |

## Authentication and Authorization

| Authentication / Authorization       | Self-Hosted (Enterprise) | MQTT as a Service (Serverless) | MQTT as a Service (Dedicated Flex) | Notes and Links                                              |
| ------------------------------------ | ------------------------ | ------------------------------ | ---------------------------------- | ------------------------------------------------------------ |
| **Username / Password**              | ✅                        | ✅                              | ✅                                  | [Password-Based Authentication](https://docs.emqx.com/en/emqx/latest/access-control/authn/pwoverview.html) |
| **JWT**                              | ✅                        | ❌                              | ✅                                  | [JWT Authentication](https://docs.emqx.com/en/emqx/latest/access-control/authn/jwt.html) |
| **MQTT 5.0 Enhanced Authentication** | ✅                        | N/A                            | N/A                                | [MQTT 5.0 Enhanced Authentication](https://docs.emqx.com/en/emqx/latest/access-control/authn/scram.html) |
| **LDAP Authentication**              | ✅                        | ❌                              | ✅ In product roadmap               |                                                              |
| **PSK Authentication**               | ✅                        | ❌                              | ✅                                  | [Enable PSK Authentication](https://docs.emqx.com/en/emqx/latest/network/psk-authentication.html#enable-psk-authentication) |
| **X.509 Certificates**               | ✅                        | ✅ Managed by EMQX Cloud        | ✅                                  |                                                              |
| **Fine-grained Access Control**      | ✅                        | ✅                              | ✅                                  |                                                              |
| **Authentication Database Backends** | ✅                        | ❌                              | ✅                                  |                                                              |
| **ACL Database Backends**            | ✅                        | ❌                              | ✅                                  |                                                              |


## Data Integration

As EMQX evolves, it supports an expanding range of data integrations, including but not limited to those listed below.

| Data Integration             | Self-Hosted (Enterprise) | MQTT as a Service (Serverless) | MQTT as a Service (Dedicated Flex) |
| ---------------------------- | ------------------------ | ------------------------------ | ---------------------------------- |
| **MQTT Bridge**              | ✅                        | ❌                              | ✅                                  |
| **Webhook / HTTP Server**    | ✅                        | ✅                              | ✅                                  |
| **Aliyun Tablestore**        | ✅                        | ✅                              | ✅                                  |
| **Apache Kafka / Confluent** | ✅                        | ✅                              | ✅                                  |
| **Apache IoTDB**             | ✅                        | ❌                              | ✅                                  |
| **Apache Pulsar**            | ✅                        | ❌                              | ✅                                  |
| **AWS Kinesis**              | ✅                        | ❌                              | ✅                                  |
| **AWS S3**                   | ✅                        | ❌                              | ✅                                  |
| **Azure Event Hubs**         | ✅                        | ❌                              | ✅                                  |
| **Azure Blob Storage**       | ✅                        | ❌                              | ✅                                  |
| **Cassandra**                | ✅                        | ❌                              | ✅                                  |
| **ClickHouse**               | ✅                        | ❌                              | ✅                                  |
| **Couchbase**                | ✅                        | ❌                              | ✅                                  |
| **DynamoDB**                 | ✅                        | ❌                              | ✅                                  |
| **Elasticsearch**            | ✅                        | ❌                              | ✅                                  |
| **GCP PubSub**               | ✅                        | ❌                              | ✅                                  |
| **GreptimeDB**               | ✅                        | ❌                              | ✅                                  |
| **HStreamDB**                | ✅                        | ❌                              | ✅                                  |
| **InfluxDB**                 | ✅                        | ❌                              | ✅                                  |
| **Microsoft SQL Server**     | ✅                        | ❌                              | ✅                                  |
| **MongoDB**                  | ✅                        | ❌                              | ✅                                  |
| **MySQL**                    | ✅                        | ❌                              | ✅                                  |
| **OpenTSDB**                 | ✅                        | ❌                              | ✅                                  |
| **Oracle Database**          | ✅                        | ❌                              | ✅                                  |
| **PostgreSQL**               | ✅                        | ❌                              | ✅                                  |
| **RabbitMQ**                 | ✅                        | ❌                              | ✅                                  |
| **Redis**                    | ✅                        | ❌                              | ✅                                  |
| **RocketMQ**                 | ✅                        | ❌                              | ✅                                  |
| **Snowflake**                | ✅                        | ❌                              | ✅                                  |
| **Syskeeper**                | ✅                        | ❌                              | ✅                                  |
| **TDengine**                 | ✅                        | ❌                              | ✅                                  |
| **TimeScaleDB**              | ✅                        | ❌                              | ✅                                  |
## Rule Engine

| Rule Engine                | Self-Hosted (Enterprise) | MQTT as a Service (Serverless) | MQTT as a Service (Dedicated Flex) | Notes and Links                                              |
| -------------------------- | ------------------------ | ------------------------------ | ---------------------------------- | ------------------------------------------------------------ |
| **Schema Registry**        | ✅                        | ❌                              | ✅                                  | Ensure data format consistency                               |
| **JSON Codec**             | ✅                        | ❌                              | ✅                                  |                                                              |
| **Avro Codec**             | ✅                        | ❌                              | ✅                                  |                                                              |
| **Protobuf Codec**         | ✅                        | ❌                              | ✅                                  |                                                              |
| **Sparkplug B Codec**      | ✅                        | ❌                              | ✅                                  |                                                              |
| **JSON Schema Validation** | ✅                        | ❌                              | ✅                                  |                                                              |
| **Avro Validation**        | ✅                        | ❌                              | ✅                                  |                                                              |
| **ProtoBuf Validation**    | ✅                        | ❌                              | ✅                                  |                                                              |
| **Built-in Functions**     | ✅                        | ❌                              | ✅                                  | [Functions available in SQL statements, rich built-in libraries, support custom extensions](https://docs.emqx.com/en/emqx/latest/data-integration/rule-sql-builtin-functions.html) |
| **jq Functions**           | ✅                        | ❌                              | ✅                                  | Efficient JSON data processing                               |
| **Event Trigger**          | ✅                        | ❌                              | ✅                                  | [Client events](https://docs.emqx.com/en/emqx/latest/data-integration/rule-sql-events-and-fields.html#mqtt-events), event-driven business development |
| **Schema Validation**      | ✅                        | ❌                              | ✅                                  |                                                              |
| **Message Transformation** | ✅                        | ❌                              | ✅                                  |                                                              |

## Extensibility

| Extensibility                | Self-Hosted (Enterprise) | MQTT as a Service (Serverless) | MQTT as a Service (Dedicated Flex) | Notes and Links                                              |
| ---------------------------- | ------------------------ | ------------------------------ | ---------------------------------- | ------------------------------------------------------------ |
| **Hooks**                    | ✅                        | N/A                            | N/A                                | [Hooks](https://docs.emqx.com/en/emqx/latest/extensions/hooks.html#hooks) |
| **Plugins**                  | ✅                        | N/A                            | N/A                                | [Plugins](https://docs.emqx.com/en/emqx/latest/extensions/plugins.html#plugins) |
| **Plugin Hot Loading**       | ✅                        | N/A                            | N/A                                |                                                              |
| **Plugin Hot Configuration** | ✅                        | N/A                            | N/A                                |                                                              |
| **Gateways**                 | ✅                        | N/A                            | N/A                                |                                                              |
| **ExHooks / gRPC**           | ✅                        | N/A                            | N/A                                |                                                              |

## Operability

| Operability              | Self-Hosted (Enterprise) | MQTT as a Service (Serverless) | MQTT as a Service (Dedicated Flex) | Notes and Links                                              |
| ------------------------ | ------------------------ | ------------------------------ | ---------------------------------- | ------------------------------------------------------------ |
| **Dashboard**            | ✅                        | ✅                              | ✅                                  | EMQX dashboard is feature-rich.<br>Configs can be hot updated through the dashboard. |
| **Configuration**        | ✅ HOCON                  | N/A                            | N/A                                | The HOCON format is simple and concise.                      |
| **HTTP API**             | ✅                        | ✅                              | ✅                                  |                                                              |
| **CLI**                  | ✅                        | ✅                              | ✅                                  |                                                              |
| **Config Hot Updates**   | ✅                        | N/A                            | N/A                                |                                                              |
| **Operational Auditing** | ✅                        | ✅                              | ✅                                  |                                                              |

## Observability

| Observability                    | Self-Hosted (Enterprise) | MQTT as a Service (Serverless) | MQTT as a Service (Dedicated Flex) | Notes and Links                                         |
| -------------------------------- | ------------------------ | ------------------------------ | ---------------------------------- | ------------------------------------------------------- |
| **Dashboard**                    | ✅                        | ✅                              | ✅                                  | Monitor clusters in real time with an elegant dashboard |
| **Metrics**                      | ✅                        | ✅                              | ✅                                  | Node metrics                                            |
| **Grafana**                      | ✅                        | ❌                              | ✅                                  |                                                         |
| **Prometheus**                   | ✅                        | ❌                              | ✅                                  |                                                         |
| **Datadog**                      | ✅                        | ❌                              | ✅                                  |                                                         |
| **OpenTelemetry**                | ✅                        | ❌                              | ✅                                  |                                                         |
| **Cluster Metrics**              | ✅                        | N/A                            | N/A                                |                                                         |
| **Alarm Alerts**                 | ✅                        | ✅                              | ✅                                  |                                                         |
| **Slow Subscription Monitoring** | ✅                        | ❌                              | ✅                                  |                                                         |
| **Topic Monitoring**             | ✅                        | ❌                              | ✅                                  |                                                         |
| **Client Monitoring**            | ✅                        | ✅                              | ✅                                  |                                                         |
| **Log Trace**                    | ✅                        | ✅                              | ✅                                  |                                                         |

## Cloud Native and K8S

| Cloud Native & K8s      | Self-Hosted (Enterprise) | MQTT as a Service (Serverless) | MQTT as a Service (Dedicated Flex) | Notes and Links                                              |
| ----------------------- | ------------------------ | ------------------------------ | ---------------------------------- | ------------------------------------------------------------ |
| **Docker**              | ✅                        | N/A                            | N/A                                | [emqx - Official Image \| Docker Hub](https://hub.docker.com/_/emqx)<br>[Docker](https://hub.docker.com/r/emqx/emqx) |
| **Kubernetes Operator** | ✅                        | N/A                            | N/A                                | [EMQX Kubernetes Operator](https://www.emqx.com/en/emqx-kubernetes-operator) |
| **Terraform**           | ✅                        | N/A                            | N/A                                | [EMQX Terraform](https://www.emqx.com/en/emqx-terraform)     |

## Cloud Platform Availability

| Cloud Platform        | Self-Hosted (Enterprise) | MQTT as a Service (Serverless) | MQTT as a Service (Dedicated Flex) | Notes and Links                                              |
| --------------------- | ------------------------ | ------------------------------ | ---------------------------------- | ------------------------------------------------------------ |
| **AWS Marketplace**   | ✅                        | ✅                              | ✅                                  | [AWS Marketplace: EMQX Platform (Pay as you go)](https://aws.amazon.com/marketplace/pp/prodview-g6zejrbcad6mu) |
| **Azure Marketplace** | ✅                        | ✅                              | ✅                                  | [Azure Marketplace: EMQX Platform](https://marketplace.microsoft.com/en-us/product/saas/emqtechnologiesincorporated1678779968155.emqx_cloud?tab=Overview) |
| **GCP Marketplace**   | ✅                        | ✅                              | ✅                                  | [GCP Marketplace](https://console.cloud.google.com/marketplace/product/emq-public-380104/emqx-cloudpay-as-you-go) |
| **AWS**               | ✅                        | ✅                              | ✅                                  |                                                              |
| **Azure**             | ✅                        | ✅                              | ✅                                  |                                                              |
| **GCP**               | ✅                        | ✅                              | ✅                                  |                                                              |


## MQTT Tools and SDKs

| MQTT Tools & SDKs       | Self-Hosted (Enterprise) | MQTT as a Service (Serverless) | MQTT as a Service (Dedicated Flex) | Notes and Links                                              |
| ----------------------- | ------------------------ | ------------------------------ | ---------------------------------- | ------------------------------------------------------------ |
| **MQTT Desktop Client** | ✅                        | ✅                              | ✅                                  | MQTT X - The best tool for learning MQTT.<br>[MQTTX: Your All-in-one MQTT Client Toolbox](https://mqttx.app/) |
| **MQTT CLI**            | ✅                        | ✅                              | ✅                                  | [MQTTX CLI: A Powerful and Easy-to-use MQTT CLI Tool](https://mqttx.app/cli) |
| **MQTT Web Tool**       | ✅                        | ✅                              | ✅                                  | Feature-rich and Easy-to-use.<br>[MQTTX Web: Easy-to-use MQTT Websocket Client Tool](https://mqttx.app/web) |
| **MQTT Benchmark**      | ✅                        | ✅                              | ✅                                  | [GitHub - emqx/emqtt-bench: Lightweight MQTT benchmark tool written in Erlang](https://github.com/emqx/emqtt-bench) |
| **MQTT & JMeter**       | ✅ JMeter Plugin          | ✅                              | ✅                                  | [GitHub - emqx/mqtt-jmeter: MQTT JMeter Plugin](https://github.com/emqx/mqtt-jmeter) |
| **MQTT SDK for C**      | ✅ NanoSDK                | ✅                              | ✅                                  | [GitHub - nanomq/NanoSDK: MQTT 5.0-compliant SDK with QUIC support in NNG flavor](https://github.com/nanomq/NanoSDK) |
| **MQTT Erlang SDK**     | ✅                        | ✅                              | ✅                                  | [GitHub - emqx/emqtt: Erlang MQTT 5.0 Client](https://github.com/emqx/emqtt) |
| **MQTT iOS SDK**        | ✅                        | ✅                              | ✅                                  | [GitHub - emqx/CocoaMQTT: MQTT 5.0 client library for iOS and macOS written in Swift](https://github.com/emqx/CocoaMQTT)<br />[GitHub - emqx/swift-mqtt: An MQTT Client over TCP and QUIC protocol](https://github.com/emqx/swift-mqtt) |
| **MQTT QUIC Client**    | ✅                        | ✅                              | ✅                                  | [GitHub - emqx/quic: QUIC protocol for Erlang & Elixir](https://github.com/emqx/quic) |


## Support Services

| Support Services            | Self-Hosted (Enterprise) | MQTT as a Service (Serverless) | MQTT as a Service (Dedicated Flex) | Notes and Links |
| --------------------------- | ------------------------ | ------------------------------ | ---------------------------------- | --------------- |
| **Technical Support**       | 5×8, 7×24 Global Support | 5×8 Global Support             | 5×8, 7×24 Global Support           |                 |
| **Architecture Consulting** | ✅                        | ❌                              | ✅                                  |                 |
| **Project Integration**     | ✅                        | ❌                              | ❌                                  |                 |
| **Custom Development**      | ✅                        | ❌                              | ❌                                  |                 |
