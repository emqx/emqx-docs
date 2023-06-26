# Introduction

{% emqxce %}

EMQX is an [open-source](https://github.com/emqx/emqx), highly scalable, and feature-rich MQTT broker designed for IoT and real-time messaging applications. It supports up to 100 million concurrent IoT device connections per cluster while maintaining a throughput of 1 million messages per second and a sub-millisecond latency.

EMQX supports various protocols, including MQTT (3.1, 3.1.1, and 5.0), HTTP, QUIC, and WebSocket. It also provides secure bi-directional communication with MQTT over TLS/SSL and various authentication mechanisms, ensuring reliable and efficient communication infrastructure for IoT devices and applications.

<img src="./assets/architecture_image.png" alt="architecture_image" style="zoom:50%;" />

With a built-in powerful SQL-based [rules engine](https://www.emqx.com/en/solutions/iot-rule-engine), EMQX can extract, filter, enrich, and transform IoT data in real-time. EMQX also ensures high availability and horizontal scalability with a masterless distributed architecture and provides an operations-friendly user experience with excellent observability.

EMQX has been adopted by over 20,000 enterprise users, connecting more than 100 million IoT devices. Over 400 customers, including renowned brands like HPE, VMware, Verifone, SAIC Volkswagen, and Ericsson, trust EMQX for their mission-critical IoT scenarios. 

{% endmeqxce %}

{% emqxee %}

EMQX Enterprise is the world’s most scalable and reliable MQTT messaging platform to connect, move and process your data in business-critical scenarios for the IoT era.

<img src="/Users/emqx/Documents/GitHub/emqx-docs/en_US/assets/EMQX-enterprise.png" alt="EMQX-enterprise" style="zoom:50%;" />



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

<style type="text/css">
.tg  {border-collapse:collapse;border-spacing:0;}
.tg td{border-color:black;border-style:solid;border-width:1px;font-family:Arial, sans-serif;font-size:14px;
  overflow:hidden;padding:10px 5px;word-break:normal;}
.tg th{border-color:black;border-style:solid;border-width:1px;font-family:Arial, sans-serif;font-size:14px;
  font-weight:normal;overflow:hidden;padding:10px 5px;word-break:normal;}
.tg .tg-c3ow{border-color:inherit;text-align:center;vertical-align:top}
.tg .tg-0pky{border-color:inherit;text-align:left;vertical-align:top}
</style>
<table class="tg">
<thead>
  <tr>
    <th class="tg-c3ow">Features</th>
    <th class="tg-c3ow" colspan="2">MQTT as a Service</th>
    <th class="tg-c3ow" colspan="2">Self-Hosted</th>
  </tr>
</thead>
<tbody>
  <tr>
    <td class="tg-0pky"><span style="font-weight:normal"> </span></td>
    <td class="tg-c3ow">EMQX Cloud Serverless</td>
    <td class="tg-c3ow">EMQX Dedicated Cloud</td>
    <td class="tg-c3ow">EMQX Open Source</td>
    <td class="tg-c3ow">EMQX Enterprise</td>
  </tr>
  <tr>
    <td class="tg-0pky"><span style="font-weight:normal"></span></td>
    <td class="tg-c3ow"><a href="https://accounts.emqx.com/signup?continue=https%3A%2F%2Fcloud-intl.emqx.com%2Fconsole%2Fdeployments%2F0%3Foper%3Dnew"><span style="text-decoration:none;color:var(--ds-link, #0052CC)">Get Free Serverless</span></td>
    <td class="tg-c3ow"><a href="https://www.emqx.com/en/try?product=broker"><span style="text-decoration:none;color:var(--ds-link, #0052CC)">14-day Free Trial</span></td>
    <td class="tg-c3ow"><a href="https://www.emqx.com/en/try?product=broker"><span style="text-decoration:none;color:var(--ds-link, #0052CC)">Open Source Download</span></a></td>
    <td class="tg-c3ow"><a href="https://www.emqx.com/en/apply-licenses/emqx"><span style="text-decoration:none;color:var(--ds-link, #0052CC)">Get a Free Trial License</span></a></td>
  </tr>
  <tr>
    <td class="tg-0pky"><span style="font-weight:normal">Scalability</span></td>
    <td class="tg-0pky"><span style="font-weight:normal">1000 auto scale</span></td>
    <td class="tg-0pky"><span style="font-weight:normal">1000 - unlimited</span></td>
    <td class="tg-0pky"><span style="font-weight:normal">Up to 100M MQTT connections per cluster</span></td>
    <td class="tg-0pky"><span style="font-weight:normal">Up to 100M MQTT connections per cluster</span><br><span style="font-weight:normal">Forever free for &lt;100 connections</span></td>
  </tr>
  <tr>
    <td class="tg-0pky"><span style="font-weight:normal">Throughput</span></td>
    <td class="tg-0pky"><span style="font-weight:normal">1000 TPS</span></td>
    <td class="tg-0pky"><span style="font-weight:normal">Same as enterprise</span></td>
    <td class="tg-0pky"><span style="font-weight:normal">5M+ MQTT messages per second</span></td>
    <td class="tg-0pky"><span style="font-weight:normal">5M+ MQTT messages per second</span></td>
  </tr>
  <tr>
    <td class="tg-0pky"><span style="font-weight:normal">Reliability</span></td>
    <td class="tg-0pky"><span style="font-weight:normal">Maintained by EMQX Cloud Team</span></td>
    <td class="tg-0pky"><span style="font-weight:normal">Maintained by EMQX Cloud Team</span></td>
    <td class="tg-0pky"><span style="font-weight:normal">Data storage in memory</span></td>
    <td class="tg-0pky"><span style="font-weight:normal">Data persistence in RocksDB(Coming soon)</span></td>
  </tr>
  <tr>
    <td class="tg-0pky"><span style="font-weight:normal">Latency</span></td>
    <td class="tg-0pky"><span style="font-weight:normal">1~5 millisecond</span></td>
    <td class="tg-0pky"><span style="font-weight:normal">1~5 millisecond</span></td>
    <td class="tg-0pky"><span style="font-weight:normal">1~5 millisecond</span></td>
    <td class="tg-0pky"><span style="font-weight:normal">1~5 millisecond</span></td>
  </tr>
  <tr>
    <td class="tg-0pky"><span style="font-weight:normal">Integrations (Out-of-the-box)</span></td>
    <td class="tg-c3ow"><span style="font-weight:normal">No supported</span></td>
    <td class="tg-0pky"><span style="font-weight:normal">Supports over 40 data integrations, including Webhook, MQTT data bridge, MySQL, PostgreSQL, Kafka, MongoDB, Oracle, etc.</span></td>
    <td class="tg-0pky"><span style="font-weight:normal">Supports Webhook and MQTT data bridge.</span></td>
    <td class="tg-0pky"><span style="font-weight:normal">Supports over 40 data integrations, including Webhook, MQTT data bridge, MySQL, PostgreSQL, Kafka, MongoDB, Oracle, etc.</span></td>
  </tr>
  <tr>
    <td class="tg-0pky"><span style="font-weight:normal">MQTT 5.0 Broker</span></td>
    <td class="tg-c3ow"><span style="font-weight:normal"><img src="./assets/check_mark_64.png" style="zoom:40%;" /></span></td>
    <td class="tg-c3ow"><span style="font-weight:normal"><img src="./assets/check_mark_64.png" style="zoom:40%;" /></span></td>
    <td class="tg-c3ow"><span style="font-weight:normal"><img src="./assets/check_mark_64.png" style="zoom:40%;" /></span></td>
    <td class="tg-c3ow"><span style="font-weight:normal"><img src="./assets/check_mark_64.png" style="zoom:40%;" /></span></td>
  </tr>
  <tr>
    <td class="tg-0pky"><span style="font-weight:normal">MQTT over QUIC</span></td>
    <td class="tg-c3ow"><span style="font-weight:normal"><img src="./assets/cross_mark_64.png" style="zoom:40%;" /></span></td>
    <td class="tg-c3ow"><span style="font-weight:normal"><img src="./assets/cross_mark_64.png" style="zoom:40%;" /></span></td>
    <td class="tg-c3ow"><span style="font-weight:normal"><img src="./assets/check_mark_64.png" style="zoom:40%;" /></span></td>
    <td class="tg-c3ow"><span style="font-weight:normal"><img src="./assets/check_mark_64.png" style="zoom:40%;" /></span></td>
  </tr>
  <tr>
    <td class="tg-0pky"><span style="font-weight:normal">MQTT Add-ons</span></td>
    <td class="tg-c3ow"><span style="font-weight:normal"><img src="./assets/cross_mark_64.png" style="zoom:40%;" /></span></td>
    <td class="tg-c3ow"><span style="font-weight:normal"><img src="./assets/cross_mark_64.png" style="zoom:40%;" /></span></td>
    <td class="tg-c3ow"><span style="font-weight:normal"><img src="./assets/check_mark_64.png" style="zoom:40%;" /></span></td>
    <td class="tg-c3ow"><span style="font-weight:normal"><img src="./assets/check_mark_64.png" style="zoom:40%;" /></span></td>
  </tr>
  <tr>
    <td class="tg-0pky"><span style="font-weight:normal">Multi-Protocol Gateways</span></td>
    <td class="tg-c3ow"><span style="font-weight:normal"><img src="./assets/cross_mark_64.png" style="zoom:40%;" /></span></td>
    <td class="tg-c3ow"><span style="font-weight:normal"><img src="./assets/check_mark_64.png" style="zoom:40%;" /></span></td>
    <td class="tg-c3ow"><span style="font-weight:normal"><img src="./assets/check_mark_64.png" style="zoom:40%;" /></span></td>
    <td class="tg-c3ow"><span style="font-weight:normal"><img src="./assets/check_mark_64.png" style="zoom:40%;" /></span></td>
  </tr>
  <tr>
    <td class="tg-0pky"><span style="font-weight:normal">Multi-Tenancy</span></td>
    <td class="tg-c3ow"><span style="font-weight:normal"><img src="./assets/cross_mark_64.png" style="zoom:40%;" /></span></td>
    <td class="tg-c3ow"><span style="font-weight:normal"><img src="./assets/cross_mark_64.png" style="zoom:40%;" /></span></td>
    <td class="tg-c3ow"><span style="font-weight:normal"><img src="./assets/cross_mark_64.png" style="zoom:40%;" /></span></td>
    <td class="tg-c3ow"><span style="font-weight:normal"><img src="./assets/check_mark_64.png" style="zoom:40%;" /></span></td>
  </tr>
  <tr>
    <td class="tg-0pky"><span style="font-weight:normal">Geo-Replication</span></td>
    <td class="tg-c3ow"><span style="font-weight:normal"><img src="./assets/cross_mark_64.png" style="zoom:40%;" /></span></td>
    <td class="tg-c3ow"><span style="font-weight:normal"><img src="./assets/cross_mark_64.png" style="zoom:40%;" /></span></td>
    <td class="tg-c3ow"><span style="font-weight:normal"><img src="./assets/cross_mark_64.png" style="zoom:40%;" /></span></td>
    <td class="tg-c3ow"><span style="font-weight:normal"><img src="./assets/check_mark_64.png" style="zoom:40%;" /></span></td>
  </tr>
  <tr>
    <td class="tg-0pky"><span style="font-weight:normal">Data Persistence</span></td>
    <td class="tg-c3ow"><span style="font-weight:normal"><img src="./assets/cross_mark_64.png" style="zoom:40%;" /></span></td>
    <td class="tg-c3ow"><span style="font-weight:normal"><img src="./assets/check_mark_64.png" style="zoom:40%;" /></span></td>
    <td class="tg-c3ow"><span style="font-weight:normal"><img src="./assets/cross_mark_64.png" style="zoom:40%;" /></span></td>
    <td class="tg-c3ow"><span style="font-weight:normal"><img src="./assets/check_mark_64.png" style="zoom:40%;" /></span></td>
  </tr>
  <tr>
    <td class="tg-0pky"><span style="font-weight:normal">Schema Registry</span></td>
    <td class="tg-c3ow"><span style="font-weight:normal"><img src="./assets/cross_mark_64.png" style="zoom:40%;" /></span></td>
    <td class="tg-c3ow"><span style="font-weight:normal"><img src="./assets/cross_mark_64.png" style="zoom:40%;" /></span></td>
    <td class="tg-c3ow"><span style="font-weight:normal"><img src="./assets/cross_mark_64.png" style="zoom:40%;" /></span></td>
    <td class="tg-c3ow"><span style="font-weight:normal"><img src="./assets/check_mark_64.png" style="zoom:40%;" /></span></td>
  </tr>
  <tr>
    <td class="tg-0pky"><span style="font-weight:normal">Message Codec</span></td>
    <td class="tg-c3ow"><span style="font-weight:normal"><img src="./assets/cross_mark_64.png" style="zoom:40%;" /></span></td>
    <td class="tg-c3ow"><span style="font-weight:normal"><img src="./assets/check_mark_64.png" style="zoom:40%;" /></span></td>
    <td class="tg-c3ow"><span style="font-weight:normal"><img src="./assets/cross_mark_64.png" style="zoom:40%;" /></span></td>
    <td class="tg-c3ow"><span style="font-weight:normal"><img src="./assets/check_mark_64.png" style="zoom:40%;" /></span></td>
  </tr>
  <tr>
    <td class="tg-0pky"><span style="font-weight:normal">Rule Engine</span></td>
    <td class="tg-c3ow"><span style="font-weight:normal"><img src="./assets/cross_mark_64.png" style="zoom:40%;" /></span></td>
    <td class="tg-c3ow"><span style="font-weight:normal"><img src="./assets/check_mark_64.png" style="zoom:40%;" /></span></td>
    <td class="tg-c3ow"><span style="font-weight:normal"><img src="./assets/check_mark_64.png" style="zoom:40%;" /></span></td>
    <td class="tg-c3ow"><span style="font-weight:normal"><img src="./assets/check_mark_64.png" style="zoom:40%;" /></span></td>
  </tr>
  <tr>
    <td class="tg-0pky"><span style="font-weight:normal">Flow Editor</span></td>
    <td class="tg-c3ow"><span style="font-weight:normal"><img src="./assets/cross_mark_64.png" style="zoom:40%;" /></span></td>
    <td class="tg-c3ow"><span style="font-weight:normal"><img src="./assets/check_mark_64.png" style="zoom:40%;" /></span></td>
    <td class="tg-c3ow"><span style="font-weight:normal"><img src="./assets/cross_mark_64.png" style="zoom:40%;" /></span></td>
    <td class="tg-c3ow"><span style="font-weight:normal"><img src="./assets/check_mark_64.png" style="zoom:40%;" /></span></td>
  </tr>
  <tr>
    <td class="tg-0pky"><span style="font-weight:normal">File Transfer</span></td>
    <td class="tg-c3ow"><span style="font-weight:normal"><img src="./assets/cross_mark_64.png" style="zoom:40%;" /></span></td>
    <td class="tg-c3ow"><span style="font-weight:normal"><img src="./assets/cross_mark_64.png" style="zoom:40%;" /></span></td>
    <td class="tg-c3ow"><span style="font-weight:normal"><img src="./assets/cross_mark_64.png" style="zoom:40%;" /></span></td>
    <td class="tg-c3ow"><span style="font-weight:normal"><img src="./assets/check_mark_64.png" style="zoom:40%;" /></span></td>
  </tr>
  <tr>
    <td class="tg-0pky"><span style="font-weight:normal">Troubleshooting</span></td>
    <td class="tg-c3ow"><span style="font-weight:normal"><img src="./assets/check_mark_64.png" style="zoom:40%;" /></span></td>
    <td class="tg-c3ow"><span style="font-weight:normal"><img src="./assets/check_mark_64.png" style="zoom:40%;" /></span></td>
    <td class="tg-c3ow"><span style="font-weight:normal"><img src="./assets/check_mark_64.png" style="zoom:40%;" /></span></td>
    <td class="tg-c3ow"><span style="font-weight:normal"><img src="./assets/check_mark_64.png" style="zoom:40%;" /></span></td>
  </tr>
  <tr>
    <td class="tg-0pky"><span style="font-weight:normal">Cloud-Native &amp; K8s</span></td>
    <td class="tg-c3ow"><span style="font-weight:normal"><img src="./assets/check_mark_64.png" style="zoom:40%;" /></span></td>
    <td class="tg-c3ow"><span style="font-weight:normal"><img src="./assets/check_mark_64.png" style="zoom:40%;" /></span></td>
    <td class="tg-c3ow"><span style="font-weight:normal"><img src="./assets/check_mark_64.png" style="zoom:40%;" /></span></td>
    <td class="tg-c3ow"><span style="font-weight:normal"><img src="./assets/check_mark_64.png" style="zoom:40%;" /></span></td>
  </tr>
  <tr>
    <td class="tg-0pky"><span style="font-weight:normal">Edge Computing</span></td>
    <td class="tg-c3ow"><span style="font-weight:normal"><img src="./assets/cross_mark_64.png" style="zoom:40%;" /></span></td>
    <td class="tg-c3ow"><span style="font-weight:normal"><img src="./assets/cross_mark_64.png" style="zoom:40%;" /></span></td>
    <td class="tg-c3ow"><span style="font-weight:normal"><img src="./assets/cross_mark_64.png" style="zoom:40%;" /></span></td>
    <td class="tg-c3ow"><span style="font-weight:normal"><img src="./assets/check_mark_64.png" style="zoom:40%;" /></span></td>
  </tr>
  <tr>
    <td class="tg-0pky"><span style="font-weight:normal">SLA</span></td>
    <td class="tg-0pky"><span style="font-weight:normal">99.9%</span></td>
    <td class="tg-0pky"><span style="font-weight:normal">99.99%</span></td>
    <td class="tg-0pky"><span style="font-weight:normal">99.99%</span></td>
    <td class="tg-0pky"><span style="font-weight:normal">Up to 99.999%</span></td>
  </tr>
  <tr>
    <td class="tg-0pky"><span style="font-weight:normal">License Model</span></td>
    <td class="tg-0pky"><span style="font-weight:normal">SaaS - pay as you go</span></td>
    <td class="tg-0pky"><span style="font-weight:normal">SaaS - hourly billing</span></td>
    <td class="tg-0pky"><span style="font-weight:normal">Apache Version 2.0</span></td>
    <td class="tg-0pky"><span style="font-weight:normal">Commercial license (Business source license)</span></td>
  </tr>
  <tr>
    <td class="tg-0pky"><span style="font-weight:normal">Technical Support</span></td>
    <td class="tg-0pky"><span style="font-weight:normal">8/5 Global support</span></td>
    <td class="tg-0pky"><span style="font-weight:normal">24/7 Global support</span></td>
    <td class="tg-0pky"><span style="font-weight:normal">Open source community</span></td>
    <td class="tg-0pky"><span style="font-weight:normal">24/7 Global support</span></td>
  </tr>
</tbody>
</table>


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
