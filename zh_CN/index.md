---
titleTemplate: false
---

{% emqxee %}
# EMQX 企业版文档
{% endemqxee %}

{% emqxce %}
# EMQX 文档
{% endemqxce %}


{% emqxce %}

EMQX 是一款[开源](https://github.com/emqx/emqx)的大规模分布式 MQTT 消息服务器，功能丰富，专为物联网和实时通信应用而设计。EMQX 5.0 单集群支持 MQTT 并发连接数高达 1 亿条，单服务器的传输与处理吞吐量可达每秒百万级 MQTT 消息，并保证延迟在亚毫秒级。

EMQX 支持多种协议，包括 MQTT (3.1、3.1.1 和 5.0)、HTTP、QUIC 和 WebSocket 等，保证各种网络环境和硬件设备的可访问性。EMQX 还提供了全面的 SSL/TLS 功能支持，比如双向认证以及多种身份验证机制，为物联网设备和应用程序提供可靠和高效的通信基础设施。

<img src="./assets/architecture_image.png" alt="architecture_image" style="zoom:50%;" />

内置基于 SQL 的[规则引擎](https://www.emqx.com/zh/solutions/iot-rule-engine)，EMQX 可以实时提取、过滤、丰富和转换物联网数据。此外，EMQX 采用了无主分布式架构，以确保高可用性和水平扩展性，并提供操作友好的用户体验和出色的可观测性。

EMQX 拥有来自 50 多个国家的 20,000 多家企业用户，连接全球超过 1 亿台物联网设备，服务企业数字化、实时化、智能化转型。

{% endemqxce %}

{% emqxee %}

[EMQX 企业版](https://www.emqx.com/zh/products/emqx)是一个「无限连接，任意集成，随处运行」大规模分布式物联网接入平台。

EMQX 企业版提供一体化的分布式 MQTT 消息服务和强大的 IoT 规则引擎，为高可靠、高性能的物联网实时数据移动、处理和集成提供动力，助力企业快速构建关键业务的 IoT 平台与应用。

<img src="./assets/EMQX-enterprise.png" alt="EMQX-enterprise" style="zoom:50%;" />

{% endemqxee %}

## 产品优势

{% emqxce %}

[**超大规模**](https://www.emqx.com/zh/blog/how-emqx-5-0-achieves-100-million-mqtt-connections)

EMQX 5.0 单集群可支持 MQTT 并发连接数高达 **1 亿**条。

**高性能**

单服务器的传输与处理吞吐量可达**每秒百万级** MQTT 消息。

**低延时**

近乎实时的信息传递，保证延迟在亚毫秒级。

[**全面支持 MQTT 5.0 标准**](https://www.emqx.com/zh/blog/introduction-to-mqtt-5)

100% 符合 MQTT 5.0 和 3.x 标准，具有更好的可扩展性、安全性和可靠性。

[**高可用**](https://www.emqx.io/docs/zh/v5.0/deploy/cluster/mria-introduction.html)

通过无主节点分布式架构实现高可用和水平扩展性。

[**云原生**](https://www.emqx.com/zh/emqx-kubernetes-operator)

通过 Kubernetes Operator 和 Terraform，可以轻松地在企业内部和公共云中进行部署。

<!-- Add a section called Use Cases when optimizing the use case-->

{% endemqxce %}

{% emqxee %}

- **[海量连接](https://www.emqx.com/zh/blog/reaching-100m-mqtt-connections-with-emqx-5-0)**：单节点支持 500 万 MQTT 设备连接，集群可水平扩展至支持 1 亿并发的 MQTT 连接。

- **高可靠**：弹性伸缩，无单点故障。内置 RocksDB 可靠地持久化 MQTT 消息，确保无数据损失。

- **数据安全**：端到端数据加密（支持国密），细粒度访问控制，保障数据安全，满足企业合规需求。

- **[多协议](https://www.emqx.com/zh/blog/iot-protocols-mqtt-coap-lwm2m)**：支持 MQTT、HTTP、QUIC、WebSocket、LwM2M/CoAP 或专有协议连接任何设备。

- **高性能**：单节点支持每秒实时接收、处理与分发数百万条的 MQTT 消息。毫秒级消息交付时延。

- **易运维**：图形化配置、操作与管理，实时监测运行状态。支持 MQTT 跟踪进行端到端问题分析。

{% endemqxee %}

## 产品对比

{% emqxce %}

EMQX 有 4 种部署模式，包括两种云服务模式（EMQX Cloud Serverless 和 EMQX Cloud 专有版）和两种自托管模式（EMQX 开源版 和 EMQX 企业版）。以下列出了这些部署模式的主要功能对比，以帮助您根据业务需求进行选择。

<div style="text-align: center;">
<table>
<thead>
  <tr>
    <th>主要特性</th>
    <th colspan="2">云服务模式</th>
    <th colspan="2">自托管模式</th>
  </tr>
</thead>
<tbody>
  <tr>
    <td width="12%" rowspan="2"></td>
    <td width="22%">EMQX Cloud Serverless</td>
    <td width="22%">EMQX Cloud 专有版</td>
    <td width="22%">EMQX 开源版</td>
    <td width="22%">EMQX 企业版</td>
  </tr>
  <tr>
    <td><a href="https://accounts-zh.emqx.com/signup?continue=https%3A%2F%2Fcloud.emqx.com%2Fconsole%2Fdeployments%2F0%3Foper%3Dnew">免费使用 Serverless</a></td>
    <td><a href="https://accounts-zh.emqx.com/signup?continue=https%3A%2F%2Fcloud.emqx.com%2Fconsole%2Fdeployments%2F0%3Foper%3Dnew">14 天免费试用</a></td>
    <td><a href="https://www.emqx.com/zh/try?product=broker">立即下载</a></td>
    <td><a href="https://www.emqx.com/zh/apply-licenses/emqx">免费试用</a></td>
  </tr>
  <tr>
    <td><b>可扩展性</b></td>
    <td>自动扩展，最多 1,000 条连接</td>
    <td>无限制</td>
    <td style="text-align:left;">单集群支持 MQTT 并发连接数高达 1 亿条</td>
    <td style="text-align:left;">单集群支持 MQTT 并发连接数高达 1 亿条</td>
  </tr>
  <tr>
    <td><b>吞吐量</b></td>
    <td>1000 TPS</td>
    <td>> 500 万 MQTT 消息每秒</td>
    <td>> 500 万 MQTT 消息每秒</td>
    <td>> 500 万 MQTT 消息每秒</td>
  </tr>
  <tr>
    <td><b>延迟</b></td>
    <td>1~5 毫秒</td>
    <td>1~5 毫秒</td>
    <td>1~5 毫秒</td>
    <td>1~5 毫秒</td>
  </tr>
  <tr>
    <td><b>数据集成（开箱即用）</b></td>
    <td>不支持</td>
    <td style="text-align:left;">支持 40 多种数据集成，包括 Webhook、MQTT 数据桥接、MySQL、PostgreSQL、Kafka、MongoDB、Oracle 等。</td>
    <td style="text-align:left;">支持 Webhook 和 MQTT 数据桥接</td>
    <td style="text-align:left;">支持 40 多种数据集成，包括 Webhook、MQTT 数据桥接、MySQL、PostgreSQL、Kafka、MongoDB、Oracle 等。</td>
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
    <td><b>MQTT 扩展</b></td>
    <td><img src="./assets/cross_mark_64.png" style="zoom:40%;" /></td>
    <td><img src="./assets/cross_mark_64.png" style="zoom:40%;" /></td>
    <td><img src="./assets/check_mark_64.png" style="zoom:40%;" /></td>
    <td><img src="./assets/check_mark_64.png" style="zoom:40%;" /></td>
  </tr>
  <tr>
    <td><b>多协议网关</b></td>
    <td><img src="./assets/cross_mark_64.png" style="zoom:40%;" /></td>
    <td><img src="./assets/check_mark_64.png" style="zoom:40%;" /></td>
    <td><img src="./assets/check_mark_64.png" style="zoom:40%;" /></td>
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
    <td><b>消息编解码</b></td>
    <td><img src="./assets/cross_mark_64.png" style="zoom:40%;" /></td>
    <td><img src="./assets/check_mark_64.png" style="zoom:40%;" /></td>
    <td><img src="./assets/cross_mark_64.png" style="zoom:40%;" /></td>
    <td><img src="./assets/check_mark_64.png" style="zoom:40%;" /></td>
  </tr>
  <tr>
    <td><b>规则引擎</b></td>
    <td><img src="./assets/cross_mark_64.png" style="zoom:40%;" /></td>
    <td><img src="./assets/check_mark_64.png" style="zoom:40%;" /></td>
    <td><img src="./assets/check_mark_64.png" style="zoom:40%;" /></td>
    <td><img src="./assets/check_mark_64.png" style="zoom:40%;" /></td>
  </tr>
  <tr>
    <td><b>文件传输</b></td>
    <td><img src="./assets/cross_mark_64.png" style="zoom:40%;" /></td>
    <td><img src="./assets/cross_mark_64.png" style="zoom:40%;" /></td>
    <td><img src="./assets/cross_mark_64.png" style="zoom:40%;" /></td>
    <td><img src="./assets/check_mark_64.png" style="zoom:40%;" /></td>
  </tr>
  <tr>
    <td><b>故障排查</b></td>
    <td><img src="./assets/check_mark_64.png" style="zoom:40%;" /></td>
    <td><img src="./assets/check_mark_64.png" style="zoom:40%;" /></td>
    <td><img src="./assets/check_mark_64.png" style="zoom:40%;" /></td>
    <td><img src="./assets/check_mark_64.png" style="zoom:40%;" /></td>
  </tr>
  <tr>
    <td><b>Cloud-Native &amp; K8s</b></td>
    <td>N/A</td>
    <td>N/A</td>
    <td><img src="./assets/check_mark_64.png" style="zoom:40%;" /></td>
    <td><img src="./assets/check_mark_64.png" style="zoom:40%;" /></td>
  </tr>
  <tr>
    <td><b>SLA 等级</b></td>
    <td>99.9%</td>
    <td>99.99%</td>
    <td>N/A</td>
    <td>N/A</td>
  </tr>
  <tr>
    <td><b>License</b></td>
    <td>SaaS 模式，按需计费</td>
    <td>SaaS 模式，按小时计费</td>
    <td>Apache Version 2.0</td>
    <td>商业许可证（商业源代码许可证）</td>
  </tr>
  <tr>
    <td><b>技术支持</b></td>
    <td>5x8 全球支持</td>
    <td>7x24 全球支持</td>
    <td>开源社区</td>
    <td>7x24 全球支持</td>
  </tr>
</tbody>
</table>
</div>

{% endemqxce %}

{% emqxee %}

以下列出了 EMQX 企业版和开源版的主要功能对比。

| **项目**                 | **EMQX 企业版**                                             | **EMQX 开源版**                                             |
| ------------------------ | ----------------------------------------------------------- | ----------------------------------------------------------- |
| **伸缩性**               | 单集群至多 1 亿 MQTT 连接                                   | 单集群至多 1 亿 MQTT 连接                                   |
| **性能**                 | > 500 万 MQTT 消息每秒                                      | > 500 万 MQTT 消息每秒                                      |
| **可靠性**               | RocksDB 数据存储（即将支持）                                | 内存数据存储                                                |
| **延迟**                 | 1~5 毫秒                                                    | 1~5 毫秒                                                    |
| **数据集成（开箱即用）** | 40+                                                         | 2                                                           |
| **License**              | Commercial License (Business source license)                | Apache Version 2.0                                          |
| **技术支持**             | 7x24 全球支持                                               | 开源社区                                                    |
| **MQTT 5.0**             | <img src="./assets/check_mark_64.png"  style="zoom:40%;" /> | <img src="./assets/check_mark_64.png"  style="zoom:40%;" /> |
| **MQTT over QUIC**       | <img src="./assets/check_mark_64.png"  style="zoom:40%;" /> | <img src="./assets/check_mark_64.png"  style="zoom:40%;" /> |
| **MQTT 扩展**            | <img src="./assets/check_mark_64.png"  style="zoom:40%;" /> | <img src="./assets/check_mark_64.png"  style="zoom:40%;" /> |
| **多协议网关**           | <img src="./assets/check_mark_64.png"  style="zoom:40%;" /> | <img src="./assets/check_mark_64.png"  style="zoom:40%;" /> |
| **Schema Registry**      | <img src="./assets/check_mark_64.png"  style="zoom:40%;" /> | <img src="./assets/cross_mark_64.png" style="zoom:40%;" />  |
| **消息编解码**           | <img src="./assets/check_mark_64.png"  style="zoom:40%;" /> | <img src="./assets/cross_mark_64.png" style="zoom:40%;" />  |
| **规则引擎**             | <img src="./assets/check_mark_64.png"  style="zoom:40%;" /> | <img src="./assets/check_mark_64.png"  style="zoom:40%;" /> |
| **文件传输**             | <img src="./assets/check_mark_64.png"  style="zoom:40%;" /> | <img src="./assets/cross_mark_64.png" style="zoom:40%;" />  |
| **企业系统集成**         | <img src="./assets/check_mark_64.png"  style="zoom:40%;" /> | <img src="./assets/cross_mark_64.png" style="zoom:40%;" />  |
| **故障排查**             | <img src="./assets/check_mark_64.png"  style="zoom:40%;" /> | <img src="./assets/check_mark_64.png"  style="zoom:40%;" /> |
| **云原生 & K8s**         | <img src="./assets/check_mark_64.png"  style="zoom:40%;" /> | <img src="./assets/check_mark_64.png"  style="zoom:40%;" /> |

{% endemqxee %}
