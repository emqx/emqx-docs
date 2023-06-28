# Introduction

{% emqxce %}

EMQX is an [open-source](https://github.com/emqx/emqx), highly scalable, and feature-rich MQTT broker designed for IoT and real-time messaging applications. It supports up to 100 million concurrent IoT device connections per cluster while maintaining a throughput of 1 million messages per second and a sub-millisecond latency.

EMQX supports various protocols, including MQTT (3.1, 3.1.1, and 5.0), HTTP, QUIC, and WebSocket. It also provides secure bi-directional communication with MQTT over TLS/SSL and various authentication mechanisms, ensuring reliable and efficient communication infrastructure for IoT devices and applications.

<img src="./assets/architecture_image.png" alt="architecture_image" style="zoom:50%;" />

With a built-in powerful SQL-based [rules engine](https://www.emqx.com/en/solutions/iot-rule-engine), EMQX can extract, filter, enrich, and transform IoT data in real-time. EMQX also ensures high availability and horizontal scalability with a masterless distributed architecture and provides an operations-friendly user experience with excellent observability.

EMQX has been adopted by over 20,000 enterprise users, connecting more than 100 million IoT devices. Over 400 customers, including renowned brands like HPE, VMware, Verifone, SAIC Volkswagen, and Ericsson, trust EMQX for their mission-critical IoT scenarios. 

{% endemqxce %}

{% emqxee %}

EMQX Enterprise is the world’s most scalable and reliable MQTT messaging platform to connect, move and process your data in business-critical scenarios for the IoT era.

<img src="./assets/EMQX-enterprise.png" alt="EMQX-enterprise" style="zoom:50%;" />


{% endemqxee %}

## Key Benefits

{% emqxce %}

[**Massive Scale**](https://www.emqx.com/en/blog/how-emqx-5-0-achieves-100-million-mqtt-connections)

EMQX enables scaling up to **100 million** concurrent MQTT connections in a single cluster, making it one of the most scalable MQTT brokers available.

**High Performance**

EMQX is capable of processing and handling **millions** of MQTT messages per second within a single broker.

**Low Latency**

EMQX offers almost real-time message delivery, with a sub-millisecond latency guarantee, ensuring that messages are received almost instantly.

[**Fully MQTT 5.0**](https://www.emqx.com/en/blog/introduction-to-mqtt-5)

EMQX is **fully** compliant with both **MQTT 5.0 and 3.x** standards, providing better scalability, security, and reliability.

[**High Availability**](https://www.emqx.io/docs/en/v5.0/deploy/cluster/mria-introduction.html)

EMQX enables high availability and horizontal scalability through a masterless distributed architecture, ensuring reliable and scalable performance.

[**Cloud-Native & K8s**](https://www.emqx.com/en/emqx-kubernetes-operator)

EMQX can be easily deployed on-premises or in public clouds using **Kubernetes Operator** and **Terraform**.

<!-- Add a section called Use Cases when optimizing the use case-->

{% endemqxce %}

{% emqxee %}

- **[Massive Scale](https://www.emqx.com/en/blog/how-emqx-5-0-achieves-100-million-mqtt-connections)**: Scale horizontally to 20+ nodes in a single cluster for 100M MQTT connections.
- **Business-Critical Reliability**: Up to 99.99% SLA. Ensure no data loss with built-in RocksDB data persistence.
- **Data Security**: End-to-end data encryption and fine-grained access control to protect your data.
- **[Multiple protocols support](https://www.emqx.com/en/blog/iot-protocols-mqtt-coap-lwm2m)**: MQTT, QUIC, CoAP, Stomp, LwM2M, and more
- **High Performance**: Ingest and process millions of MQTT messages efficiently per second per node.
- **Low Latency**: Guarantee sub-millisecond latency in message delivery with the soft real-time runtime.
- **Complete Observability**: Monitoring, alerting, and advanced end-to-end analysis with real-time MQTT tracing.

{% endemqxee %}

## Product Comparison

{% emqxce %}

EMQ provides four deployment options for EMQX: two managed services (EMQX Cloud Serverless and EMQX Dedicated Cloud) and two self-hosted options (EMQX Open Source and EMQX Enterprise). To help you choose the best deployment option for your requirements, this page lists a comparison of feature support across different deployment types.

<div style="text-align: center;">
<table>
<thead>
  <tr>
    <th>Features</th>
    <th colspan="2">MQTT as a Service</th>
    <th colspan="2">Self-Hosted</th>
  </tr>
</thead>
<tbody>
  <tr>
    <td width="10%" rowspan="2"></td>
    <td width="22%">EMQX Cloud Serverless</td>
    <td width="23%">EMQX Dedicated Cloud</td>
    <td width="22%">EMQX Open Source</td>
    <td width="23%">EMQX Enterprise</td>
  </tr>
  <tr>
    <td><a href="https://accounts.emqx.com/signup?continue=https%3A%2F%2Fcloud-intl.emqx.com%2Fconsole%2Fdeployments%2F0%3Foper%3Dnew">Get Free Serverless</a></td>
    <td><a href="https://www.emqx.com/en/try?product=broker">14-day Free Trial</a></td>
    <td><a href="https://www.emqx.com/en/try?product=broker">Open Source Download</a></td>
    <td><a href="https://www.emqx.com/en/apply-licenses/emqx">Get a Free Trial License</a></td>
  </tr>
  <tr>
    <td><b>Scalability</b></td>
    <td>1000 auto scale</td>
    <td>1000 - unlimited</td>
    <td style="text-align:left;">Up to 100M MQTT connections per cluster</td>
    <td style="text-align:left;">Up to 100M MQTT connections per cluster. Forever free for &lt;100 connections.</td>
  </tr>
  <tr>
    <td><b>Throughput</b></td>
    <td>1000 TPS</td>
    <td>Same as enterprise</td>
    <td>5M+ MQTT messages per second</td>
    <td>5M+ MQTT messages per second</td>
  </tr>
  <tr>
    <td><b>Reliability</b></td>
    <td>Maintained by EMQX Cloud Team</td>
    <td>Maintained by EMQX Cloud Team</td>
    <td>Data storage in memory</td>
    <td>Data persistence in RocksDB (Coming soon)</td>
  </tr>
  <tr>
    <td><b>Latency</b></td>
    <td>1~5 millisecond</td>
    <td>1~5 millisecond</td>
    <td>1~5 millisecond</td>
    <td>1~5 millisecond</td>
  </tr>
  <tr>
    <td><b>Integrations (Out-of-the-box)</b></td>
    <td>No supported</td>
    <td style="text-align:left;">Supports over 40 data integrations, including MQTT data bridge, Webhook, MySQL, PostgreSQL, Kafka, MongoDB, Oracle, etc.</td>
    <td style="text-align:left;">Supports Webhook and MQTT data bridge.</td>
    <td style="text-align:left;">Supports over 40 data integrations, including MQTT data bridge, Webhook, MySQL, PostgreSQL, Kafka, MongoDB, Oracle, etc.</td>
  </tr>
  <tr>
    <td><b>MQTT 5.0 Broker</b></td>
    <td><img src="./assets/check_mark_64.png" style="zoom:40%;" /></td>
    <td><img src="./assets/check_mark_64.png" style="zoom:40%;" /></td>
    <td><img src="./assets/check_mark_64.png" style="zoom:40%;" /></td>
    <td><img src="./assets/check_mark_64.png" style="zoom:40%;" /></td>
  </tr>
  <tr>
    <td><b>MQTT over QUIC</b></td>
    <td><img src="./assets/cross_mark_64.png" style="zoom:40%;" /></td>
    <td><img src="./assets/cross_mark_64.png" style="zoom:40%;" /></td>
    <td><img src="./assets/check_mark_64.png" style="zoom:40%;" /></td>
    <td><img src="./assets/check_mark_64.png" style="zoom:40%;" /></td>
  </tr>
  <tr>
    <td><b>MQTT Add-ons</b></td>
    <td><img src="./assets/cross_mark_64.png" style="zoom:40%;" /></td>
    <td><img src="./assets/cross_mark_64.png" style="zoom:40%;" /></td>
    <td><img src="./assets/check_mark_64.png" style="zoom:40%;" /></td>
    <td><img src="./assets/check_mark_64.png" style="zoom:40%;" /></td>
  </tr>
  <tr>
    <td><b>Multi-Protocol Gateways</b></td>
    <td><img src="./assets/cross_mark_64.png" style="zoom:40%;" /></td>
    <td><img src="./assets/check_mark_64.png" style="zoom:40%;" /></td>
    <td><img src="./assets/check_mark_64.png" style="zoom:40%;" /></td>
    <td><img src="./assets/check_mark_64.png" style="zoom:40%;" /></td>
  </tr>
  <tr>
    <td><b>Multi-Tenancy</b></td>
    <td><img src="./assets/cross_mark_64.png" style="zoom:40%;" /></td>
    <td><img src="./assets/cross_mark_64.png" style="zoom:40%;" /></td>
    <td><img src="./assets/cross_mark_64.png" style="zoom:40%;" /></td>
    <td><img src="./assets/check_mark_64.png" style="zoom:40%;" /></td>
  </tr>
  <tr>
    <td><b>Geo-Replication</b></td>
    <td><img src="./assets/cross_mark_64.png" style="zoom:40%;" /></td>
    <td><img src="./assets/cross_mark_64.png" style="zoom:40%;" /></td>
    <td><img src="./assets/cross_mark_64.png" style="zoom:40%;" /></td>
    <td><img src="./assets/check_mark_64.png" style="zoom:40%;" /></td>
  </tr>
  <tr>
    <td><b>Data Persistence</b></td>
    <td><img src="./assets/cross_mark_64.png" style="zoom:40%;" /></td>
    <td><img src="./assets/check_mark_64.png" style="zoom:40%;" /></td>
    <td><img src="./assets/cross_mark_64.png" style="zoom:40%;" /></td>
    <td><img src="./assets/check_mark_64.png" style="zoom:40%;" /></td>
  </tr>
  <tr>
    <td><b>Schema Registry</b></td>
    <td><img src="./assets/cross_mark_64.png" style="zoom:40%;" /></td>
    <td><img src="./assets/cross_mark_64.png" style="zoom:40%;" /></td>
    <td><img src="./assets/cross_mark_64.png" style="zoom:40%;" /></td>
    <td><img src="./assets/check_mark_64.png" style="zoom:40%;" /></td>
  </tr>
  <tr>
    <td><b>Message Codec</b></td>
    <td><img src="./assets/cross_mark_64.png" style="zoom:40%;" /></td>
    <td><img src="./assets/check_mark_64.png" style="zoom:40%;" /></td>
    <td><img src="./assets/cross_mark_64.png" style="zoom:40%;" /></td>
    <td><img src="./assets/check_mark_64.png" style="zoom:40%;" /></td>
  </tr>
  <tr>
    <td><b>Rule Engine</b></td>
    <td><img src="./assets/cross_mark_64.png" style="zoom:40%;" /></td>
    <td><img src="./assets/check_mark_64.png" style="zoom:40%;" /></td>
    <td><img src="./assets/check_mark_64.png" style="zoom:40%;" /></td>
    <td><img src="./assets/check_mark_64.png" style="zoom:40%;" /></td>
  </tr>
  <tr>
    <td><b>Flow Editor</b></td>
    <td><img src="./assets/cross_mark_64.png" style="zoom:40%;" /></td>
    <td><img src="./assets/check_mark_64.png" style="zoom:40%;" /></td>
    <td><img src="./assets/cross_mark_64.png" style="zoom:40%;" /></td>
    <td><img src="./assets/check_mark_64.png" style="zoom:40%;" /></td>
  </tr>
  <tr>
    <td><b>File Transfer</b></td>
    <td><img src="./assets/cross_mark_64.png" style="zoom:40%;" /></td>
    <td><img src="./assets/cross_mark_64.png" style="zoom:40%;" /></td>
    <td><img src="./assets/cross_mark_64.png" style="zoom:40%;" /></td>
    <td><img src="./assets/check_mark_64.png" style="zoom:40%;" /></td>
  </tr>
  <tr>
    <td><b>Troubleshooting</b></td>
    <td><img src="./assets/check_mark_64.png" style="zoom:40%;" /></td>
    <td><img src="./assets/check_mark_64.png" style="zoom:40%;" /></td>
    <td><img src="./assets/check_mark_64.png" style="zoom:40%;" /></td>
    <td><img src="./assets/check_mark_64.png" style="zoom:40%;" /></td>
  </tr>
  <tr>
    <td><b>Cloud-Native &amp; K8s</b></td>
    <td><img src="./assets/check_mark_64.png" style="zoom:40%;" /></td>
    <td><img src="./assets/check_mark_64.png" style="zoom:40%;" /></td>
    <td><img src="./assets/check_mark_64.png" style="zoom:40%;" /></td>
    <td><img src="./assets/check_mark_64.png" style="zoom:40%;" /></td>
  </tr>
  <tr>
    <td><b>Edge Computing</b></td>
    <td><img src="./assets/cross_mark_64.png" style="zoom:40%;" /></td>
    <td><img src="./assets/cross_mark_64.png" style="zoom:40%;" /></td>
    <td><img src="./assets/cross_mark_64.png" style="zoom:40%;" /></td>
    <td><img src="./assets/check_mark_64.png" style="zoom:40%;" /></td>
  </tr>
  <tr>
    <td><b>SLA</b></td>
    <td>99.9%</td>
    <td>99.99%</td>
    <td>99.99%</td>
    <td>Up to 99.999%</td>
  </tr>
  <tr>
    <td><b>License Model</b></td>
    <td>SaaS - pay as you go</td>
    <td>SaaS - hourly billing</td>
    <td>Apache Version 2.0</td>
    <td>Commercial license (Business source license)</td>
  </tr>
  <tr>
    <td><b>Technical Support</b></td>
    <td>8/5 Global support</td>
    <td>24/7 Global support</td>
    <td>Open source community</td>
    <td>24/7 Global support</td>
  </tr>
</tbody>
</table>
</div>


{% endemqxce %}

{% emqxee %}

The following is a list of feature comparison between EMQX Enterprise and Open Source edition.

| **Items**                         | **EMQX Enterprise**                                         | **EMQX Open Source**                                        |
| :-------------------------------- | :---------------------------------------------------------- | :---------------------------------------------------------- |
| **Positioning**                   | The reliable and scalable enterprise MQTT platform          | The world’s #1 open source MQTT broker                      |
| **Scalability**                   | Up to 100M MQTT connections per cluster                     | Up to 100M MQTT connections per cluster                     |
| **Performance**                   | 5M+ MQTT messages per second                                | 5M+ MQTT messages per second                                |
| **Reliability**                   | Data persistence in RocksDB(Coming soon)                    | Data storage in memory                                      |
| **Latency**                       | 1~5 millisecond                                             | 1~5 millisecond                                             |
| **SLA**                           | Up to 99.999%                                               | 99.99%                                                      |
| **Integrations (Out-of-the-box)** | 40+                                                         | 3                                                           |
| **License Model**                 | Commercial license (Business source license)                | Apache Version 2.0                                          |
| **Technical Support**             | 24/7 Global support                                         | Open source community                                       |
| **MQTT 5.0 Broker**               | <img src="./assets/check_mark_64.png"  style="zoom:40%;" /> | <img src="./assets/check_mark_64.png"  style="zoom:40%;" /> |
| **MQTT over QUIC**                | <img src="./assets/check_mark_64.png"  style="zoom:40%;" /> | <img src="./assets/check_mark_64.png"  style="zoom:40%;" /> |
| **MQTT Add-ons**                  | <img src="./assets/check_mark_64.png"  style="zoom:40%;" /> | <img src="./assets/check_mark_64.png"  style="zoom:40%;" /> |
| **Multi-protocol Gateways**       | <img src="./assets/check_mark_64.png"  style="zoom:40%;" /> | <img src="./assets/check_mark_64.png"  style="zoom:40%;" /> |
| **Multi-Tenancy**                 | <img src="./assets/check_mark_64.png"  style="zoom:40%;" /> | <img src="./assets/cross_mark_64.png" style="zoom:40%;" />  |
| **Geo-Replication**               | <img src="./assets/check_mark_64.png"  style="zoom:40%;" /> | <img src="./assets/cross_mark_64.png" style="zoom:40%;" />  |
| **Data Persistence**              | <img src="./assets/check_mark_64.png"  style="zoom:40%;" /> | <img src="./assets/cross_mark_64.png" style="zoom:40%;" />  |
| **Schema Registry**               | <img src="./assets/check_mark_64.png"  style="zoom:40%;" /> | <img src="./assets/cross_mark_64.png" style="zoom:40%;" />  |
| **Message Codec**                 | <img src="./assets/check_mark_64.png"  style="zoom:40%;" /> | <img src="./assets/cross_mark_64.png" style="zoom:40%;" />  |
| **Rule Engine**                   | <img src="./assets/check_mark_64.png" style="zoom:40%;" />  | <img src="./assets/check_mark_64.png" style="zoom:40%;" />  |
| **Flow Editor**                   | <img src="./assets/check_mark_64.png" style="zoom:40%;" />  | <img src="./assets/cross_mark_64.png" style="zoom:40%;" />  |
| **File Transfer**                 | <img src="./assets/check_mark_64.png" style="zoom:40%;" />  | <img src="./assets/cross_mark_64.png" style="zoom:40%;" />  |
| **Kafka Integration**             | <img src="./assets/check_mark_64.png" style="zoom:40%;" />  | <img src="./assets/cross_mark_64.png" style="zoom:40%;" />  |
| **Enterprise Integrations**       | <img src="./assets/check_mark_64.png" style="zoom:40%;" />  | <img src="./assets/cross_mark_64.png" style="zoom:40%;" />  |
| **Troubleshooting**               | <img src="./assets/check_mark_64.png" style="zoom:40%;" />  | <img src="./assets/check_mark_64.png" style="zoom:40%;" />  |
| **Cloud-Native & K8s**            | <img src="./assets/check_mark_64.png" style="zoom:40%;" />  | <img src="./assets/check_mark_64.png" style="zoom:40%;" />  |
| **Edge Computing**                | <img src="./assets/check_mark_64.png" style="zoom:40%;" />  | <img src="./assets/cross_mark_64.png" style="zoom:40%;" />  |

{% endemqxee %}
