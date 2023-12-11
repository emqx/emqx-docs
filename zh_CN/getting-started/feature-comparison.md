# 功能对比

本页详细列出了不同部署模式所支持的具体功能对比。

## 核心功能

<div style="text-align: center;">
<table>
<thead>
  <tr>
    <th rowspan="2">核型/企业版功能</th>
    <th colspan="2">自托管模式</th>
    <th colspan="2">云服务模式</th>
    <th rowspan="2">备注和链接</th>
  </tr>
  <tr>
    <td>EMQX 开源版</td>
    <td>EMQX 企业版</td>
    <td>EMQX Cloud Serverless</td>
    <td>EMQX Cloud 专有版</td>
  </tr>
</thead>
<tbody>
  <tr>
    <td><b>MQTT 5.0 Broker</b></td>
    <td><img src="./assets/check_mark_64.png" style="zoom:40%;" /></td>
    <td><img src="./assets/check_mark_64.png" style="zoom:40%;" /></td>
    <td><img src="./assets/check_mark_64.png" style="zoom:40%;" /></td>
    <td><img src="./assets/check_mark_64.png" style="zoom:40%;" /></td>
    <td> </td>
  </tr>
  <tr>
    <td><b>MQTT over QUIC</b></td>
    <td><img src="./assets/check_mark_64.png" style="zoom:40%;" /></td>
    <td><img src="./assets/check_mark_64.png" style="zoom:40%;" /></td>
    <td><img src="./assets/check_mark_64.png" style="zoom:40%;" /></td>
    <td><img src="./assets/check_mark_64.png" style="zoom:40%;" /></td>
    <td></td>
  </tr>
  <tr>
    <td><b>MQTT 扩展</b></td>
    <td><img src="./assets/check_mark_64.png" style="zoom:40%;" /></td>
    <td><img src="./assets/check_mark_64.png" style="zoom:40%;" /></td>
    <td><img src="./assets/cross_mark_64.png" style="zoom:40%;" /></td>
    <td><img src="./assets/check_mark_64.png" style="zoom:40%;" /></td>
    <td><a src="./advanced/shared-subscriptions.html">共享订阅</a><br><a src="./messaging/mqtt-exclusive-subscription.html">排他订阅</a><br><a src="./advanced/delayed-publish.html">延迟发布</a><br><a src="./advanced/auto-subscriptions.html">自动订阅</a><br><a src="./advanced/topic-rewrite.html">主题重写</a></td>
  </tr>
  <tr>
    <td><b>多协议网关</b></td>
    <td><img src="./assets/check_mark_64.png" style="zoom:40%;" /></td>
    <td><img src="./assets/check_mark_64.png" style="zoom:40%;" /></td>
    <td><img src="./assets/cross_mark_64.png" style="zoom:40%;" /></td>
    <td><img src="./assets/check_mark_64.png" style="zoom:40%;" /></td>
    <td></td>
  </tr>
  <tr>
    <td><b>异地备份</b></td>
    <td><img src="./assets/cross_mark_64.png" style="zoom:40%;" /></td>
    <td><img src="./assets/check_mark_64.png" style="zoom:40%;" /></td>
    <td><img src="./assets/check_mark_64.png" style="zoom:40%;" /></td>
    <td><img src="./assets/check_mark_64.png" style="zoom:40%;" /></td>
    <td><a src="./roadmap.html"></a>已规划</td>
  </tr>
  <tr>
    <td><b>数据持久化</b></td>
    <td><img src="./assets/cross_mark_64.png" style="zoom:40%;" /></td>
    <td><img src="./assets/check_mark_64.png" style="zoom:40%;" /> 内置的 RocksDB 数据库或外部数据库</td>
    <td>N/A</td>
    <td>N/A</td>
    <td>将于 EMQX 5.4 发布（预览功能）<br><a href="https://www.emqx.com/zh/blog/mqtt-persistence-based-on-rocksdb">基于 RocksDB 实现高可靠、低时延的 MQTT 数据持久化</a></td>
  </tr>
  <tr>
    <td><b>Schema Registry</b></td>
    <td><img src="./assets/cross_mark_64.png" style="zoom:40%;" /></td>
    <td><img src="./assets/check_mark_64.png" style="zoom:40%;" /></td>
    <td><img src="./assets/cross_mark_64.png" style="zoom:40%;" /></td>
    <td><img src="./assets/check_mark_64.png" style="zoom:40%;" /></td>
    <td><a href="https://docs.emqx.com/zh/enterprise/latest/data-integration/schema-registry.html">编解码</a></td>
  </tr>
  <tr>
    <td><b>消息编解码</b></td>
    <td><img src="./assets/cross_mark_64.png" style="zoom:40%;" /></td>
    <td><img src="./assets/check_mark_64.png" style="zoom:40%;" /></td>
    <td><img src="./assets/cross_mark_64.png" style="zoom:40%;" /></td>
    <td><img src="./assets/check_mark_64.png" style="zoom:40%;" /></td>
    <td>JSON<br>Avro<br>Protobuf<br>Custom codec (HTTP/gRPC)</td>
  </tr>
  <tr>
    <td><b>规则引擎</b></td>
    <td><img src="./assets/check_mark_64.png" style="zoom:40%;" /></td>
    <td><img src="./assets/check_mark_64.png" style="zoom:40%;" /></td>
    <td><img src="./assets/check_mark_64.png" style="zoom:40%;" /></td>
    <td><img src="./assets/check_mark_64.png" style="zoom:40%;" /></td>
    <td><a href="https://www.emqx.io/docs/en/v5.0/data-integration/rules.html">基于 SQL 的内置规则引擎</a></td>
  </tr>
  <tr>
    <td><b>Flow 设计器</b></td>
    <td><img src="./assets/cross_mark_64.png" style="zoom:40%;" /></td>
    <td><img src="./assets/check_mark_64.png" style="zoom:40%;" /></td>
    <td><img src="./assets/check_mark_64.png" style="zoom:40%;" /> 已规划</td>
    <td><img src="./assets/check_mark_64.png" style="zoom:40%;" /> 已规划</td>
    <td><a href="https://docs.emqx.com/zh/enterprise/latest/flow-designer/introduction.html">Flow 设计器</a></td>
  </tr>
  <tr>
    <td><b>文件传输</b></td>
    <td><img src="./assets/cross_mark_64.png" style="zoom:40%;" /></td>
    <td><img src="./assets/check_mark_64.png" style="zoom:40%;" /></td>
    <td><img src="./assets/check_mark_64.png" style="zoom:40%;" /> 已规划</td>
    <td><img src="./assets/check_mark_64.png" style="zoom:40%;" /> 已规划</td>
    <td></td>
  </tr>
  <tr>
    <td><b>Kafka 集成</b></td>
    <td><img src="./assets/cross_mark_64.png" style="zoom:40%;" /></td>
    <td><img src="./assets/cross_mark_64.png" style="zoom:40%;" /></td>
    <td><img src="./assets/check_mark_64.png" style="zoom:40%;" /></td>
    <td><img src="./assets/check_mark_64.png" style="zoom:40%;" /></td>
    <td><a href="https://docs.emqx.com/zh/enterprise/latest/data-integration/data-bridge-kafka.html">将 MQTT 数据传输到 Apache Kafka</a></td>
  </tr>
  <tr>
    <td><b>企业级数据集成</b></td>
    <td><img src="./assets/cross_mark_64.png" style="zoom:40%;" /></td>
    <td><img src="./assets/check_mark_64.png" style="zoom:40%;" /> 40+</td>
    <td><img src="./assets/check_mark_64.png" style="zoom:40%;" /></td>
    <td><img src="./assets/check_mark_64.png" style="zoom:40%;" /> 40+</td>
    <td><a href="https://www.emqx.com/zh/integrations">规则引擎与数据桥接，任意企业系统集成</a></td>
  </tr>
  <tr>
    <td><b>故障排查</b></td>
    <td><img src="./assets/check_mark_64.png" style="zoom:40%;" /></td>
    <td><img src="./assets/check_mark_64.png" style="zoom:40%;" /></td>
    <td><img src="./assets/check_mark_64.png" style="zoom:40%;" /></td>
    <td><img src="./assets/check_mark_64.png" style="zoom:40%;" /></td>
    <td><a src="./observability/tracer.html">日志追踪 (Trace)</a><br><a src="./observability/slow_subscribers_statistics.html">慢订阅统计</a></td>
  </tr>
  <tr>
    <td><b>Cloud-Native &amp; K8s</b></td>
    <td><img src="./assets/check_mark_64.png" style="zoom:40%;" /></td>
    <td><img src="./assets/check_mark_64.png" style="zoom:40%;" /></td>
    <td>N/A</td>
    <td>N/A</td>
    <td><a href="https://www.emqx.com/zh/deployments">部署方式</a></td>
  </tr>
  <tr>
    <td><b>边缘计算</b></td>
    <td><img src="./assets/cross_mark_64.png" style="zoom:40%;" /></td>
    <td><img src="./assets/check_mark_64.png" style="zoom:40%;" /></td>
    <td><img src="./assets/check_mark_64.png" style="zoom:40%;" /></td>
    <td><img src="./assets/check_mark_64.png" style="zoom:40%;" /></td>
    <td><a href="https://neugates.io/">Neuron</a><br><a href="https://nanomq.io/">NanoMQ</a></td>
  </tr>
</tbody>
</table>
</div>


## 可扩展性与性能

<div style="text-align: center;">
<table>
<thead>
  <tr>
    <th rowspan="2">可扩展性/性能</th>
    <th colspan="2">自托管模式</th>
    <th colspan="2">云服务模式</th>
    <th rowspan="2">备注和链接</th>
  </tr>
  <tr>
    <td>EMQX 开源版</td>
    <td>EMQX 企业版</td>
    <td>EMQX Cloud Serverless</td>
    <td>EMQX Cloud 专有版</td>
  </tr>
</thead>
<tbody>
  <tr>
    <td><b>可扩展性</b></td>
    <td><span style="font-weight:normal">单集群支持 MQTT 并发连接数高达 1 亿条</span></td>
    <td><span style="font-weight:normal">单集群支持 MQTT 并发连接数高达 1 亿条</span></td>
    <td><span style="font-weight:normal">自动扩展，最多 1,000 条连接</span></td>
    <td><span style="font-weight:normal">无限制</span></td>
    <td><a href="https://www.emqx.com/zh/blog/reaching-100m-mqtt-connections-with-emqx-5-0">高度可扩展，EMQX 5.0 达成 1 亿 MQTT 连接</a></td>
  </tr>
  <tr>
    <td><b>高可用性</b></td>
    <td><span style="font-weight:normal">无主集群</span></td>
    <td><span style="font-weight:normal">无主集群</span></td>
    <td><span style="font-weight:normal">无主集群</span></td>
    <td><span style="font-weight:normal">无主集群</span></td>
    <td><span style="font-weight:normal"> </span></td>
  </tr>
  <tr>
    <td><b>可靠性</b></td>
    <td><span style="font-weight:normal">数据存储在内存中</span></td>
    <td><span style="font-weight:normal">数据持久化到 RocksDB</span></td>
    <td><span style="font-weight:normal">N/A</span></td>
    <td><span style="font-weight:normal">N/A</span></td>
    <td><a href="https://www.emqx.com/zh/blog/mqtt-persistence-based-on-rocksdb">基于 RocksDB 实现高可靠、低时延的 MQTT 数据持久化</a></td>
  </tr>
  <tr>
    <td><b>吞吐量</b></td>
    <td><span style="font-weight:normal">&lt; 10 万 MQTT 消息每秒</span></td>
    <td><span style="font-weight:normal">500 万+ MQTT 消息每秒</span></td>
    <td><span style="font-weight:normal">1000 MQTT 消息每秒</span></td>
    <td><span style="font-weight:normal">500 万+ MQTT 消息每秒</span></td>
    <td><span style="font-weight:normal"> </span></td>
  </tr>
  <tr>
    <td><b>延迟</b></td>
    <td><span style="font-weight:normal">1~5 毫秒</span></td>
    <td><span style="font-weight:normal">1~5 毫秒</span></td>
    <td><span style="font-weight:normal">1~5 毫秒</span></td>
    <td><span style="font-weight:normal">1~5 毫秒</span></td>
    <td><span style="font-weight:normal"> </span></td>
  </tr>
  <tr>
    <td><b>SLA 等级</b></td>
    <td><span style="font-weight:normal">N/A</span></td>
    <td><span style="font-weight:normal">N/A</span></td>
    <td><span style="font-weight:normal">99.9% 正常运行时间</span></td>
    <td><span style="font-weight:normal">最高可达 99.99%</span><br><span style="font-weight:normal">正常运行时间</span></td>
    <td><span style="font-weight:normal"> </span></td>
  </tr>
</tbody>
</table>
</div>

## 集群架构

<div style="text-align: center;">
<table>
<thead>
  <tr>
    <th rowspan="2">集群架构<br></th>
    <th colspan="2">自托管模式<br></th>
    <th colspan="2">云服务模式<br></th>
    <th rowspan="2">备注和链接<br></th>
  </tr>
  <tr>
    <td>EMQX 开源版</td>
    <td>EMQX 企业版</td>
    <td>EMQX Cloud Serverless</td>
    <td>EMQX Cloud 专有版</td>
  </tr>
</thead>
<tbody>
  <tr>
    <td><b>集群节点数</b></td>
    <td>20+</td>
    <td>20+</td>
    <td>保密信息</td>
    <td>保密信息</td>
    <td> </td>
  </tr>
  <tr>
    <td><b>运行时弹性和韧性扩展</b></td>
    <td><img src="./assets/cross_mark_64.png" style="zoom:40%;" /></td>
    <td><img src="./assets/check_mark_64.png" style="zoom:40%;" /></td>
    <td><img src="./assets/cross_mark_64.png" style="zoom:40%;" /></td>
    <td><img src="./assets/check_mark_64.png" style="zoom:40%;" /></td>
    <td> </td>
  </tr>
  <tr>
    <td><b>自动扩展</b></td>
    <td><img src="./assets/cross_mark_64.png" style="zoom:40%;" /></td>
    <td><img src="./assets/check_mark_64.png" style="zoom:40%;" /></td>
    <td><img src="./assets/check_mark_64.png" style="zoom:40%;" /></td>
    <td><img src="./assets/check_mark_64.png" style="zoom:40%;" /></td>
    <td> </td>
  </tr>
  <tr>
    <td><b>高一致性</b></td>
    <td><img src="./assets/check_mark_64.png" style="zoom:40%;" /></td>
    <td><img src="./assets/check_mark_64.png" style="zoom:40%;" /></td>
    <td><img src="./assets/check_mark_64.png" style="zoom:40%;" /></td>
    <td><img src="./assets/check_mark_64.png" style="zoom:40%;" /></td>
    <td></td>
  </tr>
  <tr>
    <td><b>事务处理</b></td>
    <td><img src="./assets/check_mark_64.png" style="zoom:40%;" /></td>
    <td><img src="./assets/check_mark_64.png" style="zoom:40%;" /></td>
    <td><img src="./assets/check_mark_64.png" style="zoom:40%;" /></td>
    <td><img src="./assets/check_mark_64.png" style="zoom:40%;" /></td>
    <td> </td>
  </tr>
  <tr>
    <td><b>网络分区恢复</b></td>
    <td><img src="./assets/check_mark_64.png" style="zoom:40%;" /></td>
    <td><img src="./assets/check_mark_64.png" style="zoom:40%;" /></td>
    <td><img src="./assets/check_mark_64.png" style="zoom:40%;" /></td>
    <td><img src="./assets/check_mark_64.png" style="zoom:40%;" /></td>
    <td> </td>
  </tr>
  <tr>
    <td><b>节点疏散 &amp; 集群重平衡</b></td>
    <td><img src="./assets/cross_mark_64.png" style="zoom:40%;" /></td>
    <td><img src="./assets/check_mark_64.png" style="zoom:40%;" /></td>
    <td>N/A</td>
    <td>N/A</td>
    <td> </td>
  </tr>
  <tr>
    <td><b>自动集群发现</b></td>
    <td><img src="./assets/check_mark_64.png" style="zoom:40%;" /></td>
    <td><img src="./assets/check_mark_64.png" style="zoom:40%;" /></td>
    <td>N/A</td>
    <td>N/A</td>
    <td>static: 静态节点列表自动集群<br>mcast: 采用 UDP 组播模式的自动群集<br>dns: DNS A 记录自动集群<br>etcd: Discovery via etcd<br>k8s: Kubernetes 服务自动集群</td>
  </tr>
  <tr>
    <td><b>Zero Downtime/Hot Upgrades</b></td>
    <td><img src="./assets/cross_mark_64.png" style="zoom:40%;" /></td>
    <td><img src="./assets/check_mark_64.png" style="zoom:40%;" /></td>
    <td>N/A</td>
    <td>N/A</td>
    <td> </td>
  </tr>
  <tr>
    <td><b>Hot Patch</b></td>
    <td><img src="./assets/cross_mark_64.png" style="zoom:40%;" /></td>
    <td><img src="./assets/check_mark_64.png" style="zoom:40%;" /></td>
    <td>N/A</td>
    <td>N/A</td>
    <td> </td>
  </tr>
  <tr>
    <td><b>Overload Protection</b></td>
    <td><img src="./assets/check_mark_64.png" style="zoom:40%;" /></td>
    <td><img src="./assets/check_mark_64.png" style="zoom:40%;" /></td>
    <td>N/A</td>
    <td>N/A</td>
    <td> </td>
  </tr>
  <tr>
    <td><b>Multi-cluster Management</b></td>
    <td><img src="./assets/cross_mark_64.png" style="zoom:40%;" /></td>
    <td><img src="./assets/check_mark_64.png" style="zoom:40%;" /></td>
    <td>N/A</td>
    <td>N/A</td>
    <td> </td>
  </tr>
  <tr>
    <td><b>Cluster Metrics</b></td>
    <td><img src="./assets/cross_mark_64.png" style="zoom:40%;" /></td>
    <td><img src="./assets/check_mark_64.png" style="zoom:40%;" /></td>
    <td>N/A</td>
    <td>N/A</td>
    <td> </td>
  </tr>
</tbody>
</table>
</div>

## MQTT 与其他连接协议支持

<div style="text-align: center;">
<table>
<thead>
  <tr>
    <th rowspan="2">MQTT &amp; 其他连接协议<br></th>
    <th colspan="2">自托管模式<br></th>
    <th colspan="2">云服务模式<br></th>
    <th rowspan="2">备注和链接<br></th>
  </tr>
  <tr>
    <td>EMQX 开源版</td>
    <td>EMQX 企业版</td>
    <td>EMQX Cloud Serverless</td>
    <td>EMQX Cloud 专有版</td>
  </tr>
</thead>
<tbody>
  <tr>
    <td><b>MQTT 3.x</b></td>
    <td><img src="./assets/check_mark_64.png" style="zoom:40%;" /></td>
    <td><img src="./assets/check_mark_64.png" style="zoom:40%;" /></td>
    <td><img src="./assets/check_mark_64.png" style="zoom:40%;" /></td>
    <td><img src="./assets/check_mark_64.png" style="zoom:40%;" /></td>
    <td> </td>
  </tr>
  <tr>
    <td><b>MQTT 5.0</b></td>
    <td><img src="./assets/check_mark_64.png" style="zoom:40%;" /></td>
    <td><img src="./assets/check_mark_64.png" style="zoom:40%;" /></td>
    <td><img src="./assets/check_mark_64.png" style="zoom:40%;" /></td>
    <td><img src="./assets/check_mark_64.png" style="zoom:40%;" /></td>
    <td> </td>
  </tr>
  <tr>
    <td><b>MQTT 保留消息</b></td>
    <td><img src="./assets/check_mark_64.png" style="zoom:40%;" /></td>
    <td><img src="./assets/check_mark_64.png" style="zoom:40%;" /></td>
    <td><img src="./assets/check_mark_64.png" style="zoom:40%;" /></td>
    <td><img src="./assets/check_mark_64.png" style="zoom:40%;" /></td>
    <td> </td>
  </tr>
  <tr>
    <td><b>MQTT over TCP</b></td>
    <td><img src="./assets/check_mark_64.png" style="zoom:40%;" /></td>
    <td><img src="./assets/check_mark_64.png" style="zoom:40%;" /></td>
    <td><img src="./assets/cross_mark_64.png" style="zoom:40%;" /></td>
    <td><img src="./assets/check_mark_64.png" style="zoom:40%;" /></td>
    <td> </td>
  </tr>
  <tr>
    <td><b>MQTT over TLS</b></td>
    <td><img src="./assets/check_mark_64.png" style="zoom:40%;" /></td>
    <td><img src="./assets/check_mark_64.png" style="zoom:40%;" /></td>
    <td><img src="./assets/check_mark_64.png" style="zoom:40%;" /></td>
    <td><img src="./assets/check_mark_64.png" style="zoom:40%;" /></td>
    <td> </td>
  </tr>
  <tr>
    <td><b>MQTT over WebSocket</b></td>
    <td><img src="./assets/check_mark_64.png" style="zoom:40%;" /></td>
    <td><img src="./assets/check_mark_64.png" style="zoom:40%;" /></td>
    <td><img src="./assets/check_mark_64.png" style="zoom:40%;" /></td>
    <td><img src="./assets/check_mark_64.png" style="zoom:40%;" /></td>
    <td> </td>
  </tr>
  <tr>
    <td><b>MQTT over QUIC</b></td>
    <td><img src="./assets/check_mark_64.png" style="zoom:40%;" /></td>
    <td><img src="./assets/check_mark_64.png" style="zoom:40%;" /></td>
    <td><img src="./assets/check_mark_64.png" style="zoom:40%;" /> 规划中</td>
    <td><img src="./assets/check_mark_64.png" style="zoom:40%;" /> 规划中</td>
    <td>EMQX 是现在全球唯一支持 QUIC 协议的 MQTT 消息服务器。<br></td>
  </tr>
  <tr>
    <td><b>LB (代理协议)</b></td>
    <td><img src="./assets/check_mark_64.png" style="zoom:40%;" /></td>
    <td><img src="./assets/check_mark_64.png" style="zoom:40%;" /></td>
    <td><img src="./assets/cross_mark_64.png" style="zoom:40%;" /></td>
    <td><img src="./assets/check_mark_64.png" style="zoom:40%;" /></td>
    <td>Proxy protocol v1, v2</td>
  </tr>
  <tr>
    <td><b>IPv6 支持</b></td>
    <td><img src="./assets/check_mark_64.png" style="zoom:40%;" /></td>
    <td><img src="./assets/check_mark_64.png" style="zoom:40%;" /></td>
    <td><img src="./assets/cross_mark_64.png" style="zoom:40%;" /></td>
    <td><img src="./assets/check_mark_64.png" style="zoom:40%;" /></td>
    <td> </td>
  </tr>
  <tr>
    <td><b>多协议网关</b></b></td>
    <td><img src="./assets/check_mark_64.png" style="zoom:40%;" /></td>
    <td><img src="./assets/check_mark_64.png" style="zoom:40%;" /></td>
    <td><img src="./assets/cross_mark_64.png" style="zoom:40%;" /></td>
    <td><img src="./assets/check_mark_64.png" style="zoom:40%;" /></td>
    <td> </td>
  </tr>
  <tr>
    <td><b>MQTT-SN</b></td>
    <td><img src="./assets/check_mark_64.png" style="zoom:40%;" /></td>
    <td><img src="./assets/check_mark_64.png" style="zoom:40%;" /></td>
    <td><img src="./assets/cross_mark_64.png" style="zoom:40%;" /></td>
    <td><img src="./assets/check_mark_64.png" style="zoom:40%;" /></td>
    <td> </td>
  </tr>
  <tr>
    <td><b>STOMP</b></td>
    <td><img src="./assets/check_mark_64.png" style="zoom:40%;" /></td>
    <td><img src="./assets/check_mark_64.png" style="zoom:40%;" /></td>
    <td><img src="./assets/cross_mark_64.png" style="zoom:40%;" /></td>
    <td><img src="./assets/cross_mark_64.png" style="zoom:40%;" /></td>
    <td> </td>
  </tr>
  <tr>
    <td><b>CoAP</b></td>
    <td><img src="./assets/check_mark_64.png" style="zoom:40%;" /></td>
    <td><img src="./assets/check_mark_64.png" style="zoom:40%;" /></td>
    <td><img src="./assets/cross_mark_64.png" style="zoom:40%;" /></td>
    <td><img src="./assets/check_mark_64.png" style="zoom:40%;" /></td>
    <td> </td>
  </tr>
  <tr>
    <td><b>LwM2M</b></td>
    <td><img src="./assets/cross_mark_64.png" style="zoom:40%;" /></td>
    <td><img src="./assets/check_mark_64.png" style="zoom:40%;" /></td>
    <td><img src="./assets/cross_mark_64.png" style="zoom:40%;" /></td>
    <td><img src="./assets/cross_mark_64.png" style="zoom:40%;" /></td>
    <td> </td>
  </tr>
  <tr>
    <td><b>ExProto</b></td>
    <td><img src="./assets/cross_mark_64.png" style="zoom:40%;" /></td>
    <td><img src="./assets/check_mark_64.png" style="zoom:40%;" /></td>
    <td><img src="./assets/cross_mark_64.png" style="zoom:40%;" /></td>
    <td><img src="./assets/cross_mark_64.png" style="zoom:40%;" /></td>
    <td> </td>
  </tr>
  <tr>
    <td><b>OCPP</b></td>
    <td><img src="./assets/cross_mark_64.png" style="zoom:40%;" /></td>
    <td><img src="./assets/check_mark_64.png" style="zoom:40%;" /></td>
    <td><img src="./assets/cross_mark_64.png" style="zoom:40%;" /></td>
    <td><img src="./assets/cross_mark_64.png" style="zoom:40%;" /></td>
    <td> </td>
  </tr>
  <tr>
    <td><b>JT/808</b></td>
    <td><img src="./assets/cross_mark_64.png" style="zoom:40%;" /></td>
    <td><img src="./assets/check_mark_64.png" style="zoom:40%;" /> 将于 EMQX v5.4 发布</td>
    <td><img src="./assets/cross_mark_64.png" style="zoom:40%;" /></td>
    <td><img src="./assets/check_mark_64.png" style="zoom:40%;" /></td>
    <td> </td>
  </tr>
  <tr>
    <td><b>GBT32960</b></td>
    <td><img src="./assets/cross_mark_64.png" style="zoom:40%;" /></td>
    <td><img src="./assets/check_mark_64.png" style="zoom:40%;" /> 将于 EMQX v5.4 发布</td>
    <td><img src="./assets/cross_mark_64.png" style="zoom:40%;" /></td>
    <td><img src="./assets/cross_mark_64.png" style="zoom:40%;" /></td>
    <td> </td>
  </tr>
</tbody>
</table>
</div>

## 安全

<div style="text-align: center;">
<table>
<thead>
  <tr>
    <th rowspan="2">安全<br></th>
    <th colspan="2">自托管模式<br></th>
    <th colspan="2">云服务模式<br></th>
    <th rowspan="2">备注和链接<br></th>
  </tr>
  <tr>
    <td>EMQX 开源版</td>
    <td>EMQX 企业版</td>
    <td>EMQX Cloud Serverless</td>
    <td>EMQX Cloud 专有版</td>
  </tr>
</thead>
<tbody>
  <tr>
    <td><b>TLS/SSL</b></td>
    <td><img src="./assets/check_mark_64.png" style="zoom:40%;" /></td>
    <td><img src="./assets/check_mark_64.png" style="zoom:40%;" /></td>
    <td><img src="./assets/check_mark_64.png" style="zoom:40%;" /></td>
    <td><img src="./assets/check_mark_64.png" style="zoom:40%;" /></td>
    <td>EMQX: TLS 1.1, 1.2, 1.3</td>
  </tr>
  <tr>
    <td><b>OCSP Stapling</b></td>
    <td><img src="./assets/check_mark_64.png" style="zoom:40%;" /></td>
    <td><img src="./assets/check_mark_64.png" style="zoom:40%;" /></td>
    <td><img src="./assets/check_mark_64.png" style="zoom:40%;" /></td>
    <td><img src="./assets/check_mark_64.png" style="zoom:40%;" /></td>
    <td></td>
  </tr>
  <tr>
    <td><b>审计日志</b></td>
    <td><img src="./assets/cross_mark_64.png" style="zoom:40%;" /></td>
    <td><img src="./assets/check_mark_64.png" style="zoom:40%;" /></td>
    <td><img src="./assets/check_mark_64.png" style="zoom:40%;" /></td>
    <td><img src="./assets/check_mark_64.png" style="zoom:40%;" /></td>
    <td> </td>
  </tr>
  <tr>
    <td><b>SQL 注入保护</b></td>
    <td><img src="./assets/cross_mark_64.png" style="zoom:40%;" /></td>
    <td><img src="./assets/check_mark_64.png" style="zoom:40%;" /></td>
    <td><img src="./assets/check_mark_64.png" style="zoom:40%;" /></td>
    <td><img src="./assets/check_mark_64.png" style="zoom:40%;" /></td>
    <td> </td>
  </tr>
  <tr>
    <td><b>黑鸭分析</b></td>
    <td><img src="./assets/check_mark_64.png" style="zoom:40%;" /></td>
    <td><img src="./assets/check_mark_64.png" style="zoom:40%;" /></td>
    <td>N/A</td>
    <td>N/A</td>
    <td>与 Synopsis 合作</td>
  </tr>
</tbody>
</table>
</div>

## 认证与授权

<div style="text-align: center;">
<table>
<thead>
  <tr>
    <th rowspan="2">认证/授权<br></th>
    <th colspan="2">自托管模式<br></th>
    <th colspan="2">云服务模式<br></th>
    <th rowspan="2">备注和链接<br></th>
  </tr>
  <tr>
    <td>EMQX 开源版</td>
    <td>EMQX 企业版</td>
    <td>EMQX Cloud Serverless</td>
    <td>EMQX Cloud 专有版</td>
  </tr>
</thead>
<tbody>
  <tr>
    <td><b>用户名/密码</b></td>
    <td><img src="./assets/check_mark_64.png" style="zoom:40%;" /></td>
    <td><img src="./assets/check_mark_64.png" style="zoom:40%;" /></td>
    <td><img src="./assets/check_mark_64.png" style="zoom:40%;" /></td>
    <td><img src="./assets/check_mark_64.png" style="zoom:40%;" /></td>
    <td><a src="./access-control/authn/pwoverview.html">密码认证</a></td>
  </tr>
  <tr>
    <td><b>JWT</b></td>
    <td><img src="./assets/check_mark_64.png" style="zoom:40%;" /></td>
    <td><img src="./assets/check_mark_64.png" style="zoom:40%;" /></td>
    <td><img src="./assets/cross_mark_64.png" style="zoom:40%;" /></td>
    <td><img src="./assets/check_mark_64.png" style="zoom:40%;" /></td>
    <td><a src="./security/authn/jwt.html">JWT 认证</a></td>
  </tr>
  <tr>
    <td><b>MQTT 5.0 增强认证</b></td>
    <td><img src="./assets/check_mark_64.png" style="zoom:40%;" /></td>
    <td><img src="./assets/check_mark_64.png" style="zoom:40%;" /></td>
    <td>N/A</td>
    <td>N/A</td>
    <td><a src="./security/authn/scram.html">MQTT 5.0 增强认证</a></td>
  </tr>
  <tr>
    <td><b>PSK</b></td>
    <td><img src="./assets/check_mark_64.png" style="zoom:40%;" /></td>
    <td><img src="./assets/check_mark_64.png" style="zoom:40%;" /></td>
    <td><img src="./assets/cross_mark_64.png" style="zoom:40%;" /></td>
    <td><img src="./assets/check_mark_64.png" style="zoom:40%;" /></td>
    <td><a src="./security/ssl.html#psk-authentication">启用 PSK 验证</a></td>
  </tr>
  <tr>
    <td><b>X.509 证书</b></td>
    <td><img src="./assets/check_mark_64.png" style="zoom:40%;" /></td>
    <td><img src="./assets/check_mark_64.png" style="zoom:40%;" /></td>
    <td><img src="./assets/check_mark_64.png" style="zoom:40%;" /> 由 EMQX Cloud 管理</td>
    <td><img src="./assets/check_mark_64.png" style="zoom:40%;" /></td>
    <td> </td>
  </tr>
  <tr>
    <td><b>SSO</b></td>
    <td><img src="./assets/check_mark_64.png" style="zoom:40%;" /></td>
    <td><img src="./assets/check_mark_64.png" style="zoom:40%;" /></td>
    <td><img src="./assets/check_mark_64.png" style="zoom:40%;" /></td>
    <td><img src="./assets/check_mark_64.png" style="zoom:40%;" /></td>
    <td><a src="./dashboard/sso.html">单点登录</a> </td>
  </tr>
  <tr>
    <td><b>LDAP</b></td>
    <td><img src="./assets/check_mark_64.png" style="zoom:40%;" /></td>
    <td><img src="./assets/check_mark_64.png" style="zoom:40%;" /></td>
    <td><img src="./assets/cross_mark_64.png" style="zoom:40%;" /></td>
    <td><img src="./assets/check_mark_64.png" style="zoom:40%;" /> 已规划</td>
    <td> </td>
  </tr>
  <tr>
    <td><b>RBAC</b></td>
    <td><img src="./assets/cross_mark_64.png" style="zoom:40%;" /></td>
    <td><img src="./assets/check_mark_64.png" style="zoom:40%;" /></td>
    <td><img src="./assets/check_mark_64.png" style="zoom:40%;" /></td>
    <td><img src="./assets/check_mark_64.png" style="zoom:40%;" /></td>
    <td><a href ="https://docs.emqx.com/zh/enterprise/latest/dashboard/system.html#%E7%94%A8%E6%88%B7">基于角色的权限管理</a></td>
  </tr>
  <tr>
    <td><b>细粒度访问控制</b></td>
    <td><img src="./assets/check_mark_64.png" style="zoom:40%;" /></td>
    <td><img src="./assets/check_mark_64.png" style="zoom:40%;" /></td>
    <td><img src="./assets/check_mark_64.png" style="zoom:40%;" /></td>
    <td><img src="./assets/check_mark_64.png" style="zoom:40%;" /></td>
    <td> </td>
  </tr>
  <tr>
    <td><b>认证数据源</b></td>
    <td><img src="./assets/check_mark_64.png" style="zoom:40%;" /></td>
    <td><img src="./assets/check_mark_64.png" style="zoom:40%;" /></td>
    <td><img src="./assets/check_mark_64.png" style="zoom:40%;" /></td>
    <td><img src="./assets/check_mark_64.png" style="zoom:40%;" /></td>
    <td> </td>
  </tr>
  <tr>
    <td><b>ACL 数据源</b></td>
    <td><img src="./assets/check_mark_64.png" style="zoom:40%;" /></td>
    <td><img src="./assets/check_mark_64.png" style="zoom:40%;" /></td>
    <td><img src="./assets/check_mark_64.png" style="zoom:40%;" /></td>
    <td><img src="./assets/check_mark_64.png" style="zoom:40%;" /></td>
    <td> </td>
  </tr>
</tbody>
</table>
</div>

## 数据集成

<div style="text-align: center;">
<table>
<thead>
  <tr>
    <th rowspan="2">数据集成<br></th>
    <th colspan="2">自托管模式<br></th>
    <th colspan="2">云服务模式<br></th>
  </tr>
  <tr>
    <td>EMQX 开源版</td>
    <td>EMQX 企业版</td>
    <td>EMQX Cloud Serverless</td>
    <td>EMQX Cloud 专有版</td>
  </tr>
</thead>
<tbody>
  <tr>
    <td><b>规则引擎</b></td>
    <td><img src="./assets/check_mark_64.png" style="zoom:40%;" /></td>
    <td><img src="./assets/check_mark_64.png" style="zoom:40%;" /></td>
    <td><img src="./assets/check_mark_64.png" style="zoom:40%;" /></td>
    <td><img src="./assets/check_mark_64.png" style="zoom:40%;" /></td>
  </tr>
  <tr>
    <td><b>数据桥接</b></td>
    <td><img src="./assets/cross_mark_64.png" style="zoom:40%;" /></td>
    <td><img src="./assets/check_mark_64.png" style="zoom:40%;" /></td>
    <td><img src="./assets/cross_mark_64.png" style="zoom:40%;" /></td>
    <td><img src="./assets/check_mark_64.png" style="zoom:40%;" /></td>
  </tr>
  <tr>
    <td><b>MQTT 服务</b></td>
    <td><img src="./assets/check_mark_64.png" style="zoom:40%;" /></td>
    <td><img src="./assets/check_mark_64.png" style="zoom:40%;" /></td>
    <td><img src="./assets/cross_mark_64.png" style="zoom:40%;" /></td>
    <td><img src="./assets/check_mark_64.png" style="zoom:40%;" /></td>
  </tr>
  <tr>
    <td><b>Webhook</b></td>
    <td><img src="./assets/check_mark_64.png" style="zoom:40%;" /></td>
    <td><img src="./assets/check_mark_64.png" style="zoom:40%;" /></td>
    <td><img src="./assets/check_mark_64.png" style="zoom:40%;" /></td>
    <td><img src="./assets/check_mark_64.png" style="zoom:40%;" /></td>
  </tr>
  <tr>
    <td><b>Apache Kafka/Confluent</b></td>
    <td><img src="./assets/cross_mark_64.png" style="zoom:40%;" /></td>
    <td><img src="./assets/check_mark_64.png" style="zoom:40%;" /></td>
    <td><img src="./assets/check_mark_64.png" style="zoom:40%;" /></td>
    <td><img src="./assets/check_mark_64.png" style="zoom:40%;" /></td>
  </tr>
  <tr>
    <td><b>Apache IoTDB</b></td>
    <td><img src="./assets/cross_mark_64.png" style="zoom:40%;" /></td>
    <td><img src="./assets/check_mark_64.png" style="zoom:40%;" /></td>
    <td><img src="./assets/check_mark_64.png" style="zoom:40%;" /></td>
    <td><img src="./assets/check_mark_64.png" style="zoom:40%;" /></td>
  </tr>
  <tr>
    <td><b>Apache Pulsar</b></td>
    <td><img src="./assets/cross_mark_64.png" style="zoom:40%;" /></td>
    <td><img src="./assets/check_mark_64.png" style="zoom:40%;" /></td>
    <td><img src="./assets/check_mark_64.png" style="zoom:40%;" /></td>
    <td><img src="./assets/check_mark_64.png" style="zoom:40%;" /></td>
  </tr>
  <tr>
    <td><b>AWS Kinesis</b></td>
    <td><img src="./assets/cross_mark_64.png" style="zoom:40%;" /></td>
    <td><img src="./assets/check_mark_64.png" style="zoom:40%;" /></td>
    <td><img src="./assets/check_mark_64.png" style="zoom:40%;" /></td>
    <td><img src="./assets/check_mark_64.png" style="zoom:40%;" /></td>
  </tr>
  <tr>
    <td><b>Azure Event Hubs</b></td>
    <td><img src="./assets/cross_mark_64.png" style="zoom:40%;" /></td>
    <td><img src="./assets/check_mark_64.png" style="zoom:40%;" /></td>
    <td><img src="./assets/cross_mark_64.png" style="zoom:40%;" /></td>
    <td><img src="./assets/check_mark_64.png" style="zoom:40%;" /></td>
  </tr>
  <tr>
    <td><b>Cassandra</b></td>
    <td><img src="./assets/cross_mark_64.png" style="zoom:40%;" /></td>
    <td><img src="./assets/check_mark_64.png" style="zoom:40%;" /></td>
    <td><img src="./assets/cross_mark_64.png" style="zoom:40%;" /></td>
    <td><img src="./assets/check_mark_64.png" style="zoom:40%;" /></td>
  </tr>
  <tr>
    <td><b>ClickHouse</b></td>
    <td><img src="./assets/cross_mark_64.png" style="zoom:40%;" /></td>
    <td><img src="./assets/check_mark_64.png" style="zoom:40%;" /></td>
    <td><img src="./assets/cross_mark_64.png" style="zoom:40%;" /></td>
    <td><img src="./assets/check_mark_64.png" style="zoom:40%;" /></td>
  </tr>
  <tr>
    <td><b>DynamoDB</b></td>
    <td><img src="./assets/cross_mark_64.png" style="zoom:40%;" /></td>
    <td><img src="./assets/check_mark_64.png" style="zoom:40%;" /></td>
    <td><img src="./assets/cross_mark_64.png" style="zoom:40%;" /></td>
    <td><img src="./assets/check_mark_64.png" style="zoom:40%;" /></td>
  </tr>
  <tr>
    <td><b>GCP PubSub</b></td>
    <td><img src="./assets/cross_mark_64.png" style="zoom:40%;" /></td>
    <td><img src="./assets/check_mark_64.png" style="zoom:40%;" /></td>
    <td><img src="./assets/cross_mark_64.png" style="zoom:40%;" /></td>
    <td><img src="./assets/check_mark_64.png" style="zoom:40%;" /></td>
  </tr>
  <tr>
    <td><b>GreptimeDB</b></td>
    <td><img src="./assets/cross_mark_64.png" style="zoom:40%;" /></td>
    <td><img src="./assets/check_mark_64.png" style="zoom:40%;" /></td>
    <td><img src="./assets/cross_mark_64.png" style="zoom:40%;" /></td>
    <td><img src="./assets/check_mark_64.png" style="zoom:40%;" /></td>
  </tr>
  <tr>
    <td><b>HStreamDB</b></td>
    <td><img src="./assets/cross_mark_64.png" style="zoom:40%;" /></td>
    <td><img src="./assets/check_mark_64.png" style="zoom:40%;" /></td>
    <td><img src="./assets/cross_mark_64.png" style="zoom:40%;" /></td>
    <td><img src="./assets/check_mark_64.png" style="zoom:40%;" /></td>
  </tr>
  <tr>
    <td><b>HTTP Server</b></td>
    <td><img src="./assets/cross_mark_64.png" style="zoom:40%;" /></td>
    <td><img src="./assets/check_mark_64.png" style="zoom:40%;" /></td>
    <td><img src="./assets/cross_mark_64.png" style="zoom:40%;" /></td>
    <td><img src="./assets/check_mark_64.png" style="zoom:40%;" /></td>
  </tr>
  <tr>
    <td><b>InfluxDB</b></td>
    <td><img src="./assets/cross_mark_64.png" style="zoom:40%;" /></td>
    <td><img src="./assets/check_mark_64.png" style="zoom:40%;" /></td>
    <td><img src="./assets/cross_mark_64.png" style="zoom:40%;" /></td>
    <td><img src="./assets/check_mark_64.png" style="zoom:40%;" /></td>
  </tr>
  <tr>
    <td><b>Microsoft SQL Server</b></td>
    <td><img src="./assets/cross_mark_64.png" style="zoom:40%;" /></td>
    <td><img src="./assets/check_mark_64.png" style="zoom:40%;" /></td>
    <td><img src="./assets/cross_mark_64.png" style="zoom:40%;" /></td>
    <td><img src="./assets/check_mark_64.png" style="zoom:40%;" /></td>
  </tr>
  <tr>
    <td><b>MongoDB</b></td>
    <td><img src="./assets/cross_mark_64.png" style="zoom:40%;" /></td>
    <td><img src="./assets/check_mark_64.png" style="zoom:40%;" /></td>
    <td><img src="./assets/cross_mark_64.png" style="zoom:40%;" /></td>
    <td><img src="./assets/check_mark_64.png" style="zoom:40%;" /></td>
  </tr>
  <tr>
    <td><b>MySQL</b></td>
    <td><img src="./assets/cross_mark_64.png" style="zoom:40%;" /></td>
    <td><img src="./assets/check_mark_64.png" style="zoom:40%;" /></td>
    <td><img src="./assets/cross_mark_64.png" style="zoom:40%;" /></td>
    <td><img src="./assets/check_mark_64.png" style="zoom:40%;" /></td>
    <td> </td>
  </tr>
  <tr>
    <td><b>OpenTSDB</b></td>
    <td><img src="./assets/cross_mark_64.png" style="zoom:40%;" /></td>
    <td><img src="./assets/check_mark_64.png" style="zoom:40%;" /></td>
    <td><img src="./assets/cross_mark_64.png" style="zoom:40%;" /></td>
    <td><img src="./assets/check_mark_64.png" style="zoom:40%;" /></td>
  </tr>
  <tr>
    <td><b>Oracle Database</b></td>
    <td><img src="./assets/cross_mark_64.png" style="zoom:40%;" /></td>
    <td><img src="./assets/check_mark_64.png" style="zoom:40%;" /></td>
    <td><img src="./assets/cross_mark_64.png" style="zoom:40%;" /></td>
    <td><img src="./assets/check_mark_64.png" style="zoom:40%;" /></td>
  </tr>
  <tr>
    <td><b>PostgreSQL</b></td>
    <td><img src="./assets/cross_mark_64.png" style="zoom:40%;" /></td>
    <td><img src="./assets/check_mark_64.png" style="zoom:40%;" /></td>
    <td><img src="./assets/cross_mark_64.png" style="zoom:40%;" /></td>
    <td><img src="./assets/check_mark_64.png" style="zoom:40%;" /></td>
  </tr>
  <tr>
    <td><b>RabbitMQ</b></td>
    <td><img src="./assets/cross_mark_64.png" style="zoom:40%;" /></td>
    <td><img src="./assets/check_mark_64.png" style="zoom:40%;" /></td>
    <td><img src="./assets/cross_mark_64.png" style="zoom:40%;" /></td>
    <td><img src="./assets/check_mark_64.png" style="zoom:40%;" /></td>
  </tr>
  <tr>
    <td><b>Redis</b></td>
    <td><img src="./assets/cross_mark_64.png" style="zoom:40%;" /></td>
    <td><img src="./assets/check_mark_64.png" style="zoom:40%;" /></td>
    <td><img src="./assets/cross_mark_64.png" style="zoom:40%;" /></td>
    <td><img src="./assets/check_mark_64.png" style="zoom:40%;" /></td>
  </tr>
  <tr>
    <td><b>RocketMQ</b></td>
    <td><img src="./assets/cross_mark_64.png" style="zoom:40%;" /></td>
    <td><img src="./assets/check_mark_64.png" style="zoom:40%;" /></td>
    <td><img src="./assets/cross_mark_64.png" style="zoom:40%;" /></td>
    <td><img src="./assets/check_mark_64.png" style="zoom:40%;" /></td>
  </tr>
  <tr>
    <td><b>TDengine</b></td>
    <td><img src="./assets/cross_mark_64.png" style="zoom:40%;" /></td>
    <td><img src="./assets/check_mark_64.png" style="zoom:40%;" /></td>
    <td><img src="./assets/cross_mark_64.png" style="zoom:40%;" /></td>
    <td><img src="./assets/check_mark_64.png" style="zoom:40%;" /></td>
  </tr>
  <tr>
    <td><b>TimeScaleDB</b></td>
    <td><img src="./assets/cross_mark_64.png" style="zoom:40%;" /></td>
    <td><img src="./assets/check_mark_64.png" style="zoom:40%;" /></td>
    <td><img src="./assets/cross_mark_64.png" style="zoom:40%;" /></td>
    <td><img src="./assets/check_mark_64.png" style="zoom:40%;" /></td>
  </tr>
  <tr>
    <td><b>SAP Event Mesh</b></td>
    <td><img src="./assets/cross_mark_64.png" style="zoom:40%;" /></td>
    <td><img src="./assets/check_mark_64.png" style="zoom:40%;" /></td>
    <td><img src="./assets/cross_mark_64.png" style="zoom:40%;" /></td>
    <td><img src="./assets/check_mark_64.png" style="zoom:40%;" /></td>
  </tr>
</tbody>
</table>
</div>
## 规则引擎

<div style="text-align: center;">
<table>
<thead>
  <tr>
    <th rowspan="2">规则引擎<br></th>
    <th colspan="2">自托管模式<br></th>
    <th colspan="2">云服务模式<br></th>
    <th rowspan="2">备注和链接<br></th>
  </tr>
  <tr>
    <td>EMQX 开源版</td>
    <td>EMQX 企业版</td>
    <td>EMQX Cloud Serverless</td>
    <td>EMQX Cloud 专有版</td>
  </tr>
</thead>
<tbody>
  <tr>
    <td><b>客户端事件处理</b></td>
    <td><img src="./assets/check_mark_64.png" style="zoom:40%;" /></td>
    <td><img src="./assets/check_mark_64.png" style="zoom:40%;" /></td>
    <td><img src="./assets/cross_mark_64.png" style="zoom:40%;" /></td>
    <td><img src="./assets/check_mark_64.png" style="zoom:40%;" /></td>
    <td><a src="./data-integration/rule-sql-events-and-fields.html#客户端事件">客户端事件</a></td>
  </tr>
  <tr>
    <td><b>内置 SQL 函数</b></td>
    <td><img src="./assets/check_mark_64.png" style="zoom:40%;" /></td>
    <td><img src="./assets/check_mark_64.png" style="zoom:40%;" /></td>
    <td><img src="./assets/cross_mark_64.png" style="zoom:40%;" /></td>
    <td><img src="./assets/check_mark_64.png" style="zoom:40%;" /></td>
    <td><a src="./rule/rule-engine_buildin_function.html">内置 SQL 函数</a></td>
  </tr>
  <tr>
    <td><b>jq 函数</b></td>
    <td><img src="./assets/check_mark_64.png" style="zoom:40%;" /></td>
    <td><img src="./assets/check_mark_64.png" style="zoom:40%;" /></td>
    <td><img src="./assets/cross_mark_64.png" style="zoom:40%;" /></td>
    <td><img src="./assets/check_mark_64.png" style="zoom:40%;" /></td>
    <td> </td>
  </tr>
  <tr>
    <td><b>编解码</b></td>
    <td><img src="./assets/cross_mark_64.png" style="zoom:40%;" /></td>
    <td><img src="./assets/check_mark_64.png" style="zoom:40%;" /></td>
    <td><img src="./assets/cross_mark_64.png" style="zoom:40%;" /></td>
    <td><img src="./assets/check_mark_64.png" style="zoom:40%;" /></td>
    <td> </td>
  </tr>
  <tr>
    <td><b>JSON 编解码</b></td>
    <td><img src="./assets/check_mark_64.png" style="zoom:40%;" /></td>
    <td><img src="./assets/check_mark_64.png" style="zoom:40%;" /></td>
    <td><img src="./assets/cross_mark_64.png" style="zoom:40%;" /></td>
    <td><img src="./assets/check_mark_64.png" style="zoom:40%;" /></td>
    <td> </td>
  </tr>
  <tr>
    <td><b>Avro 编解码</b></td>
    <td><img src="./assets/cross_mark_64.png" style="zoom:40%;" /></td>
    <td><img src="./assets/check_mark_64.png" style="zoom:40%;" /></td>
    <td><img src="./assets/cross_mark_64.png" style="zoom:40%;" /></td>
    <td><img src="./assets/check_mark_64.png" style="zoom:40%;" /></td>
    <td> </td>
  </tr>
  <tr>
    <td><b>ProtoBuf 编解码</b></td>
    <td><img src="./assets/cross_mark_64.png" style="zoom:40%;" /></td>
    <td><img src="./assets/check_mark_64.png" style="zoom:40%;" /></td>
    <td><img src="./assets/cross_mark_64.png" style="zoom:40%;" /></td>
    <td><img src="./assets/check_mark_64.png" style="zoom:40%;" /></td>
    <td> </td>
  </tr>
</tbody>
</table>
</div>

## 可扩展性

<div style="text-align: center;">
<table>
<thead>
  <tr>
    <th rowspan="2">可扩展性<br></th>
    <th colspan="2">自托管模式<br></th>
    <th colspan="2">云服务模式<br></th>
    <th rowspan="2">备注和链接<br></th>
  </tr>
  <tr>
    <td>EMQX 开源版</td>
    <td>EMQX 企业版</td>
    <td>EMQX Cloud Serverless</td>
    <td>EMQX Cloud 专有版</td>
  </tr>
</thead>
<tbody>
  <tr>
    <td><b>钩子</b></td>
    <td><img src="./assets/check_mark_64.png" style="zoom:40%;" /></td>
    <td><img src="./assets/check_mark_64.png" style="zoom:40%;" /></td>
    <td>N/A</td>
    <td>N/A</td>
    <td><a src="./extensions/hooks.html">钩子</a></td>
  </tr>
  <tr>
    <td><b>插件</b></td>
    <td><img src="./assets/check_mark_64.png" style="zoom:40%;" /></td>
    <td><img src="./assets/check_mark_64.png" style="zoom:40%;" /></td>
    <td>N/A</td>
    <td>N/A</td>
    <td><a src="./extensions/plugins.html">插件</a></td>
  </tr>
  <tr>
    <td><b>插件热加载</b></td>
    <td><img src="./assets/check_mark_64.png" style="zoom:40%;" /></td>
    <td><img src="./assets/check_mark_64.png" style="zoom:40%;" /></td>
    <td>N/A</td>
    <td>N/A</td>
    <td> </td>
  </tr>
  <tr>
    <td><b>网关</b></td>
    <td><img src="./assets/check_mark_64.png" style="zoom:40%;" /></td>
    <td><img src="./assets/check_mark_64.png" style="zoom:40%;" /></td>
    <td>N/A</td>
    <td>N/A</td>
    <td> </td>
  </tr>
  <tr>
    <td><b>ExHooks/gRPC</b></td>
    <td><img src="./assets/check_mark_64.png" style="zoom:40%;" /></td>
    <td><img src="./assets/check_mark_64.png" style="zoom:40%;" /></td>
    <td>N/A</td>
    <td>N/A</td>
    <td></td>
  </tr>
</tbody>
</table>
</div>

## 可操作性

<div style="text-align: center;">
<table>
<thead>
  <tr>
    <th rowspan="2">可操作性<br></th>
    <th colspan="2">自托管模式<br></th>
    <th colspan="2">云服务模式<br></th>
    <th rowspan="2">备注和链接<br></th>
  </tr>
  <tr>
    <td>EMQX 开源版</td>
    <td>EMQX 企业版</td>
    <td>EMQX Cloud Serverless</td>
    <td>EMQX Cloud 专有版</td>
  </tr>
</thead>
<tbody>
  <tr>
    <td><b>Dashboard</b></td>
    <td><img src="./assets/check_mark_64.png" style="zoom:40%;" /></td>
    <td><img src="./assets/check_mark_64.png" style="zoom:40%;" /></td>
    <td><img src="./assets/check_mark_64.png" style="zoom:40%;" /></td>
    <td><img src="./assets/check_mark_64.png" style="zoom:40%;" /></td>
    <td>EMQX Dashboard 具备丰富的功能。<br>可以通过 Dashboard 进行配置的热更新。</td>
  </tr>
  <tr>
    <td><b>功能配置</b></td>
    <td><img src="./assets/check_mark_64.png" style="zoom:40%;" />HOCON</td>
    <td><img src="./assets/check_mark_64.png" style="zoom:40%;" />HOCON</td>
    <td>N/A</td>
    <td>N/A</td>
    <td>简洁明了的 HOCON 格式。</td>
  </tr>
  <tr>
    <td><b>HTTP API</b></td>
    <td><img src="./assets/check_mark_64.png" style="zoom:40%;" /></td>
    <td><img src="./assets/check_mark_64.png" style="zoom:40%;" /></td>
    <td><img src="./assets/check_mark_64.png" style="zoom:40%;" /></td>
    <td><img src="./assets/check_mark_64.png" style="zoom:40%;" /></td>
    <td> </td>
  </tr>
  <tr>
    <td><b>CLI</b></td>
    <td><img src="./assets/check_mark_64.png" style="zoom:40%;" /></td>
    <td><img src="./assets/check_mark_64.png" style="zoom:40%;" /></td>
    <td><img src="./assets/check_mark_64.png" style="zoom:40%;" /></td>
    <td><img src="./assets/check_mark_64.png" style="zoom:40%;" /></td>
    <td> </td>
  </tr>
  <tr>
    <td><b>配置热升级</b></td>
    <td><img src="./assets/check_mark_64.png" style="zoom:40%;" /></td>
    <td><img src="./assets/check_mark_64.png" style="zoom:40%;" /></td>
    <td>N/A</td>
    <td>N/A</td>
    <td> </td>
  </tr>
  <tr>
    <td><b>运行审计</b></td>
    <td><img src="./assets/cross_mark_64.png" style="zoom:40%;" /></td>
    <td><img src="./assets/check_mark_64.png" style="zoom:40%;" /></td>
    <td><img src="./assets/check_mark_64.png" style="zoom:40%;" /></td>
    <td><img src="./assets/check_mark_64.png" style="zoom:40%;" /></td>
    <td> </td>
  </tr>
</tbody>
</table>
</div>
## 可观测性

<div style="text-align: center;">
<table>
<thead>
  <tr>
    <th rowspan="2">可观测性<br></th>
    <th colspan="2">自托管模式<br></th>
    <th colspan="2">云服务模式<br></th>
    <th rowspan="2">备注和链接<br></th>
  </tr>
  <tr>
    <td>EMQX 开源版</td>
    <td>EMQX 企业版</td>
    <td>EMQX Cloud Serverless</td>
    <td>EMQX Cloud 专有版</td>
  </tr>
</thead>
<tbody>
  <tr>
    <td><b>Dashboard</b></td>
    <td><img src="./assets/check_mark_64.png" style="zoom:40%;" /></td>
    <td><img src="./assets/check_mark_64.png" style="zoom:40%;" /></td>
    <td><img src="./assets/check_mark_64.png" style="zoom:40%;" /></td>
    <td><img src="./assets/check_mark_64.png" style="zoom:40%;" /></td>
    <td>通过优雅的 Dashboard 实时监控集群。</td>
  </tr>
  <tr>
    <td><b>单节点指标</b></td>
    <td><img src="./assets/check_mark_64.png" style="zoom:40%;" /></td>
    <td><img src="./assets/check_mark_64.png" style="zoom:40%;" /></td>
    <td><img src="./assets/check_mark_64.png" style="zoom:40%;" /></td>
    <td><img src="./assets/check_mark_64.png" style="zoom:40%;" /></td>
    <td></td>
  </tr>
  <tr>
    <td><b>Grafana</b></td>
    <td><img src="./assets/check_mark_64.png" style="zoom:40%;" /></td>
    <td><img src="./assets/check_mark_64.png" style="zoom:40%;" /></td>
    <td><img src="./assets/cross_mark_64.png" style="zoom:40%;" /></td>
    <td><img src="./assets/check_mark_64.png" style="zoom:40%;" /></td>
    <td> </td>
  </tr>
  <tr>
    <td><b>集群指标</b></td>
    <td><img src="./assets/check_mark_64.png" style="zoom:40%;" /></td>
    <td><img src="./assets/check_mark_64.png" style="zoom:40%;" /></td>
    <td>N/A</td>
    <td>N/A</td>
    <td> </td>
  </tr>
  <tr>
    <td><b>告警</b></td>
    <td><img src="./assets/check_mark_64.png" style="zoom:40%;" /></td>
    <td><img src="./assets/check_mark_64.png" style="zoom:40%;" /></td>
    <td><img src="./assets/check_mark_64.png" style="zoom:40%;" /></td>
    <td><img src="./assets/check_mark_64.png" style="zoom:40%;" /></td>
    <td> </td>
  </tr>
  <tr>
    <td><b>慢订阅监控</b></td>
    <td><img src="./assets/check_mark_64.png" style="zoom:40%;" /></td>
    <td><img src="./assets/check_mark_64.png" style="zoom:40%;" /></td>
    <td><img src="./assets/check_mark_64.png" style="zoom:40%;" /> 已规划</td>
    <td><img src="./assets/check_mark_64.png" style="zoom:40%;" /> 已规划</td>
    <td> </td>
  </tr>
  <tr>
    <td><b>主题监控</b></b></td>
    <td><img src="./assets/check_mark_64.png" style="zoom:40%;" /></td>
    <td><img src="./assets/check_mark_64.png" style="zoom:40%;" /></td>
    <td><img src="./assets/check_mark_64.png" style="zoom:40%;" /> 已规划</td>
    <td><img src="./assets/check_mark_64.png" style="zoom:40%;" /> 已规划</td>
    <td> </td>
  </tr>
  <tr>
    <td><b>客户端监控</b></td>
    <td><img src="./assets/check_mark_64.png" style="zoom:40%;" /></td>
    <td><img src="./assets/check_mark_64.png" style="zoom:40%;" /></td>
    <td><img src="./assets/check_mark_64.png" style="zoom:40%;" /></td>
    <td><img src="./assets/check_mark_64.png" style="zoom:40%;" /></td>
    <td> </td>
  </tr>
  <tr>
    <td><b>Prometheus</b></td>
    <td><img src="./assets/check_mark_64.png" style="zoom:40%;" /></td>
    <td><img src="./assets/check_mark_64.png" style="zoom:40%;" /></td>
    <td><img src="./assets/cross_mark_64.png" style="zoom:40%;" /></td>
    <td><img src="./assets/check_mark_64.png" style="zoom:40%;" /></td>
    <td> </td>
  </tr>
  <tr>
    <td><b>Datadog</b></td>
    <td><img src="./assets/cross_mark_64.png" style="zoom:40%;" /></td>
    <td><img src="./assets/check_mark_64.png" style="zoom:40%;" /></td>
    <td><img src="./assets/cross_mark_64.png" style="zoom:40%;" /> 已规划</td>
    <td><img src="./assets/cross_mark_64.png" style="zoom:40%;" /> 已规划</td>
    <td>将于 EMQX v5.4 发布</td>
  </tr>
</tbody>
</table>
</div>

## 云原生与 K8S

<div style="text-align: center;">
<table>
<thead>
  <tr>
    <th rowspan="2">云原生 &amp; K8s<br></th>
    <th colspan="2">自托管模式<br></th>
    <th colspan="2">云服务模式<br></th>
    <th rowspan="2">备注和链接<br></th>
  </tr>
  <tr>
    <td>EMQX 开源版</td>
    <td>EMQX 企业版</td>
    <td>EMQX Cloud Serverless</td>
    <td>EMQX Cloud 专有版</td>
  </tr>
</thead>
<tbody>
  <tr>
    <td><b>Docker</b></td>
    <td><img src="./assets/check_mark_64.png" style="zoom:40%;" /></td>
    <td><img src="./assets/check_mark_64.png" style="zoom:40%;" /></td>
    <td>N/A</td>
    <td>N/A</td>
    <td><a href="https://hub.docker.com/_/emqx">emqx - Official Image | Docker Hub</a> <br><a href="https://hub.docker.com/r/emqx/emqx">Docker</a></td>
  </tr>
  <tr>
    <td><b>Kubernetes Operator</b></td>
    <td><img src="./assets/check_mark_64.png" style="zoom:40%;" /></td>
    <td><img src="./assets/check_mark_64.png" style="zoom:40%;" /></td>
    <td>N/A</td>
    <td>N/A</td>
    <td><a href="https://www.emqx.com/zh/emqx-kubernetes-operator">EMQX Kubernetes Operator</a></td>
  </tr>
  <tr>
    <td><b>Terraform</b></td>
    <td><img src="./assets/check_mark_64.png" style="zoom:40%;" /></td>
    <td><img src="./assets/check_mark_64.png" style="zoom:40%;" /></td>
    <td>N/A</td>
    <td>N/A</td>
    <td><a href="https://www.emqx.com/zh/emqx-terraform">EMQX Terraform</a></td>
  </tr>
  <tr>
    <td><b>AWS Marketplace</b></td>
    <td><img src="./assets/check_mark_64.png" style="zoom:40%;" /></td>
    <td><img src="./assets/check_mark_64.png" style="zoom:40%;" /></td>
    <td><img src="./assets/check_mark_64.png" style="zoom:40%;" /></td>
    <td><img src="./assets/check_mark_64.png" style="zoom:40%;" /></td>
    <td>EMQX 已上架 AWS 市场。<br><a href="https://aws.amazon.com/marketplace/pp/prodview-cwa2e6xbrwtzi">AWS Marketplace: EMQX Enterprise on Ubuntu 20.04</a> </td>
  </tr>
</tbody>
</table>
</div>

## 云平台支持

<div style="text-align: center;">
<table>
<thead>
  <tr>
    <th rowspan="2">云平台<br></th>
    <th colspan="2">自托管模式<br></th>
    <th colspan="2">云服务模式<br></th>
    <th rowspan="2">备注和链接<br></th>
  </tr>
  <tr>
    <td>EMQX 开源版</td>
    <td>EMQX 企业版</td>
    <td>EMQX Cloud Serverless</td>
    <td>EMQX Cloud 专有版</td>
  </tr>
</thead>
<tbody>
  <tr>
    <td><b>AWS Marketplace</b></td>
    <td><img src="./assets/check_mark_64.png" style="zoom:40%;" /></td>
    <td><img src="./assets/check_mark_64.png" style="zoom:40%;" /></td>
    <td><img src="./assets/check_mark_64.png" style="zoom:40%;" /></td>
    <td><img src="./assets/check_mark_64.png" style="zoom:40%;" /></td>
    <td>EMQX 已上架 AWS 市场。<br><a href="https://aws.amazon.com/marketplace/pp/prodview-cwa2e6xbrwtzi">AWS Marketplace: EMQX Enterprise on Ubuntu 20.04</a> </td>
  </tr>
  <tr>
    <td><b>Azure Marketplace</b></td>
    <td><img src="./assets/check_mark_64.png" style="zoom:40%;" /></td>
    <td><img src="./assets/check_mark_64.png" style="zoom:40%;" /></td>
    <td><img src="./assets/check_mark_64.png" style="zoom:40%;" /></td>
    <td><img src="./assets/check_mark_64.png" style="zoom:40%;" /></td>
  </tr>
  <tr>
    <td><b>GCP Marketplace</b></td>
    <td><img src="./assets/cross_mark_64.png" style="zoom:40%;" /></td>
    <td><img src="./assets/check_mark_64.png" style="zoom:40%;" /></td>
    <td><img src="./assets/check_mark_64.png" style="zoom:40%;" /></td>
    <td><img src="./assets/check_mark_64.png" style="zoom:40%;" /></td>
    <td></td>
  </tr>
  <tr>
    <td><b>AWS</b></td>
    <td><img src="./assets/check_mark_64.png" style="zoom:40%;" /></td>
    <td><img src="./assets/check_mark_64.png" style="zoom:40%;" /></td>
    <td><img src="./assets/check_mark_64.png" style="zoom:40%;" /></td>
    <td><img src="./assets/check_mark_64.png" style="zoom:40%;" /></td>
    <td></td>
  </tr>
  <tr>
    <td><b>Azure</b></td>
    <td><img src="./assets/check_mark_64.png" style="zoom:40%;" /></td>
    <td><img src="./assets/check_mark_64.png" style="zoom:40%;" /></td>
    <td><img src="./assets/cross_mark_64.png" style="zoom:40%;" /></td>
    <td><img src="./assets/check_mark_64.png" style="zoom:40%;" /></td>
    <td></td>
  </tr>
  <tr>
    <td><b>GCP</b></td>
    <td><img src="./assets/check_mark_64.png" style="zoom:40%;" /></td>
    <td><img src="./assets/check_mark_64.png" style="zoom:40%;" /></td>
    <td><img src="./assets/check_mark_64.png" style="zoom:40%;" /></td>
    <td><img src="./assets/check_mark_64.png" style="zoom:40%;" /></td>
    <td></td>
  </tr>
</tbody>
</table>
</div>

## MQTT 开发工具与 SDKs

<div style="text-align: center;">
<table>
<thead>
  <tr>
    <th rowspan="2">MQTT 开发工具 &amp; SDKs<br></th>
    <th colspan="2">自托管模式<br></th>
    <th colspan="2">云服务模式<br></th>
    <th rowspan="2">备注和链接<br></th>
  </tr>
  <tr>
    <td>EMQX 开源版</td>
    <td>EMQX 企业版</td>
    <td>EMQX Cloud Serverless</td>
    <td>EMQX Cloud 专有版</td>
  </tr>
</thead>
<tbody>
  <tr>
    <td><b>MQTTX 桌面版</b></td>
    <td><img src="./assets/check_mark_64.png" style="zoom:40%;" /></td>
    <td><img src="./assets/check_mark_64.png" style="zoom:40%;" /></td>
    <td><img src="./assets/check_mark_64.png" style="zoom:40%;" /></td>
    <td><img src="./assets/check_mark_64.png" style="zoom:40%;" /></td>
    <td>MQTTX - 学习 MQTT 最好的工具<br><a href="https://mqttx.app/zh">MQTTX: 你的全功能 MQTT 客户端工具</a></td>
  </tr>
  <tr>
    <td><b>MQTTX 命令行版</b></td>
    <td><img src="./assets/check_mark_64.png" style="zoom:40%;" /></td>
    <td><img src="./assets/check_mark_64.png" style="zoom:40%;" /></td>
    <td><img src="./assets/check_mark_64.png" style="zoom:40%;" /></td>
    <td><img src="./assets/check_mark_64.png" style="zoom:40%;" /></td>
    <td><a href="https://mqttx.app/zh/cli">MQTTX CLI: 强大易用的 MQTT 5.0 命令行工具</a></td>
  </tr>
  <tr>
    <td><b>MQTTX Web 版</b></td>
    <td><img src="./assets/check_mark_64.png" style="zoom:40%;" /></td>
    <td><img src="./assets/check_mark_64.png" style="zoom:40%;" /></td>
    <td><img src="./assets/check_mark_64.png" style="zoom:40%;" /></td>
    <td><img src="./assets/check_mark_64.png" style="zoom:40%;" /></td>
    <td>功能丰富并且简单易用<br><a href="https://mqttx.app/zh/web">MQTTX Web: 易用的 MQTT 5.0 Websocket 客户端工具</a></td>
  </tr>
  <tr>
    <td><b>MQTT 基准测试工具</b></td>
    <td><img src="./assets/check_mark_64.png" style="zoom:40%;" /></td>
    <td><img src="./assets/check_mark_64.png" style="zoom:40%;" /></td>
    <td><img src="./assets/check_mark_64.png" style="zoom:40%;" /></td>
    <td><img src="./assets/check_mark_64.png" style="zoom:40%;" /></td>
    <td><a href="https://github.com/emqx/emqtt-bench">GitHub - emqx/emqtt-bench: 用 Erlang 编写的轻量级 MQTT 基准测试工具。</a></td>
  </tr>
  <tr>
    <td><b>MQTT 负载测试工具</b></td>
    <td><img src="./assets/cross_mark_64.png" style="zoom:40%;" /></td>
    <td><img src="./assets/check_mark_64.png" style="zoom:40%;" /> XMeter</td>
    <td><img src="./assets/check_mark_64.png" style="zoom:40%;" /></td>
    <td><img src="./assets/check_mark_64.png" style="zoom:40%;" /></td>
    <td>XMeter - 世界第一的 MQTT 负载测试工具。</td>
  </tr>
  <tr>
    <td><b>MQTT &amp; JMeter</b></td>
    <td><img src="./assets/check_mark_64.png" style="zoom:40%;" /> JMeter 插件</td>
    <td><img src="./assets/check_mark_64.png" style="zoom:40%;" /> XMeter</td>
    <td><img src="./assets/check_mark_64.png" style="zoom:40%;" /></td>
    <td><img src="./assets/check_mark_64.png" style="zoom:40%;" /></td>
    <td><a href="https://github.com/emqx/mqtt-jmeter">GitHub - emqx/mqtt-jmeter: MQTT JMeter 插件</a></td>
  </tr>
  <tr>
    <td><b>MQTT SDK for C</b></td>
    <td><img src="./assets/check_mark_64.png" style="zoom:40%;" /> NanoSDK</td>
    <td><img src="./assets/check_mark_64.png" style="zoom:40%;" /> NanoSDK</td>
    <td><img src="./assets/check_mark_64.png" style="zoom:40%;" /></td>
    <td><img src="./assets/check_mark_64.png" style="zoom:40%;" /></td>
    <td><a href="https://github.com/nanomq/NanoSDK">GitHub - nanomq/NanoSDK: NanoSDK - 支持 QUIC 的 MQTT 5.0 兼容 SDK，采用 NNG 风格。</a></td>
  </tr>
  <tr>
    <td><b>MQTT Erlang SDK</b></td>
    <td><img src="./assets/check_mark_64.png" style="zoom:40%;" /></td>
    <td><img src="./assets/check_mark_64.png" style="zoom:40%;" /></td>
    <td><img src="./assets/check_mark_64.png" style="zoom:40%;" /></td>
    <td><img src="./assets/check_mark_64.png" style="zoom:40%;" /></td>
    <td><a href="https://github.com/emqx/emqtt">GitHub - emqx/emqtt: Erlang MQTT 5.0 客户端</a></td>
  </tr>
  <tr>
    <td><b>MQTT iOS SDK</b></td>
    <td><img src="./assets/check_mark_64.png" style="zoom:40%;" /></td>
    <td><img src="./assets/check_mark_64.png" style="zoom:40%;" /></td>
    <td><img src="./assets/check_mark_64.png" style="zoom:40%;" /></td>
    <td><img src="./assets/check_mark_64.png" style="zoom:40%;" /></td>
    <td><a href="https://github.com/emqx/CocoaMQTT">GitHub - emqx/CocoaMQTT: 为 iOS 和 macOS 编写的 MQTT 5.0 客户端库，使用 Swift 语言。</a></td>
  </tr>
  <tr>
    <td><b>MQTT QUIC 客户端</b></td>
    <td><img src="./assets/check_mark_64.png" style="zoom:40%;" /></td>
    <td><img src="./assets/check_mark_64.png" style="zoom:40%;" /></td>
    <td><img src="./assets/check_mark_64.png" style="zoom:40%;" /></td>
    <td><img src="./assets/check_mark_64.png" style="zoom:40%;" /></td>
    <td><a href="https://github.com/emqx/quic">GitHub - emqx/quic: 用于 Erlang 和 Elixir 的 QUIC 协议。</a></td>
  </tr>
</tbody>
</table>
</div>

## 客户支持服务

<div style="text-align: center;">
<table>
<thead>
  <tr>
    <th rowspan="2">客户支持<br></th>
    <th colspan="2">自托管模式<br></th>
    <th colspan="2">云服务模式<br></th>
    <th rowspan="2">备注和链接<br></th>
  </tr>
  <tr>
    <td>EMQX 开源版</td>
    <td>EMQX 企业版</td>
    <td>EMQX Cloud Serverless</td>
    <td>EMQX Cloud 专有版</td>
  </tr>
</thead>
<tbody>
  <tr>
    <td><b>技术支持</b></td>
    <td>社区和论坛</td>
    <td>5*8, 7*24 全球支持</td>
    <td>5*8 全球支持</td>
    <td>5*8, 7*24 全球支持</td>
    <td> </td>
  </tr>
  <tr>
    <td><b>架构咨询</b></td>
    <td><img src="./assets/cross_mark_64.png" style="zoom:40%;" /></td>
    <td><img src="./assets/check_mark_64.png" style="zoom:40%;" /></td>
    <td><img src="./assets/cross_mark_64.png" style="zoom:40%;" /></td>
    <td><img src="./assets/check_mark_64.png" style="zoom:40%;" /></td>
    <td> </td>
  </tr>
  <tr>
    <td><b>项目集成</b></td>
    <td><img src="./assets/cross_mark_64.png" style="zoom:40%;" /></td>
    <td><img src="./assets/check_mark_64.png" style="zoom:40%;" /></td>
    <td><img src="./assets/cross_mark_64.png" style="zoom:40%;" /></td>
    <td><img src="./assets/cross_mark_64.png" style="zoom:40%;" /></td>
    <td> </td>
  </tr>
  <tr>
    <td><b>定制开发</b></td>
    <td><img src="./assets/cross_mark_64.png" style="zoom:40%;" /></td>
    <td><img src="./assets/check_mark_64.png" style="zoom:40%;" /></td>
    <td><img src="./assets/cross_mark_64.png" style="zoom:40%;" /></td>
    <td><img src="./assets/cross_mark_64.png" style="zoom:40%;" /></td>
    <td> </td>
  </tr>
</tbody>
</table>
</div>