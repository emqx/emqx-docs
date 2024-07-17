# 功能对比

本页详细列出了不同部署模式所支持的具体功能对比。

## 核心 / 企业功能

<div style="text-align: center;">
<table>
<thead>
  <tr>
    <th rowspan="2">功能</th>
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
    <td> 完整实现 MQTT 5.0 协议</td>
  </tr>
  <tr>
    <td><b>MQTT over QUIC</b></td>
    <td><img src="./assets/cross_mark_64.png" style="zoom:40%;" /></td>
    <td><img src="./assets/check_mark_64.png" style="zoom:40%;" /></td>
    <td><img src="./assets/check_mark_64.png" style="zoom:40%;" /></td>
    <td><img src="./assets/check_mark_64.png" style="zoom:40%;" /></td>
    <td> 全球领先支持</td>
  </tr>
  <tr>
    <td><b>MQTT 扩展</b></td>
    <td><img src="./assets/check_mark_64.png" style="zoom:40%;" /></td>
    <td><img src="./assets/check_mark_64.png" style="zoom:40%;" /></td>
    <td><img src="./assets/cross_mark_64.png" style="zoom:40%;" /></td>
    <td><img src="./assets/check_mark_64.png" style="zoom:40%;" /></td>
    <td><a href="https://docs.emqx.com/zh/enterprise/latest/messaging/mqtt-shared-subscription.html">共享订阅</a><br><a href="https://docs.emqx.com/zh/enterprise/latest/messaging/mqtt-exclusive-subscription.html">排他订阅</a><br><a href="https://docs.emqx.com/zh/enterprise/latest/messaging/mqtt-delayed-publish.html">延迟发布</a><br><a href="https://docs.emqx.com/zh/enterprise/latest/messaging/mqtt-auto-subscription.html">自动订阅</a><br><a href="https://docs.emqx.com/zh/enterprise/latest/messaging/mqtt-topic-rewrite.html">主题重写</a><br>更多个性化选项</td>
  </tr>
  <tr>
    <td><b>多协议网关</b></td>
    <td><img src="./assets/check_mark_64.png" style="zoom:40%;" /></td>
    <td><img src="./assets/check_mark_64.png" style="zoom:40%;" /></td>
    <td><img src="./assets/cross_mark_64.png" style="zoom:40%;" /></td>
    <td><img src="./assets/check_mark_64.png" style="zoom:40%;" /></td>
    <td> 更多行业设备接入</td>
  </tr>
  <tr>
    <td><b>多租户</b></td>
    <td><img src="./assets/cross_mark_64.png" style="zoom:40%;" /></td>
    <td><img src="./assets/check_mark_64.png" style="zoom:40%;" /></td>
    <td><img src="./assets/cross_mark_64.png" style="zoom:40%;" /></td>
    <td><img src="./assets/check_mark_64.png" style="zoom:40%;" /></td>
    <td> 更高的系统灵活性和利用率<br>（即将发布）</td>
  </tr>
  <tr>
    <td><b>集群连接</b></td>
    <td><img src="./assets/cross_mark_64.png" style="zoom:40%;" /></td>
    <td><img src="./assets/check_mark_64.png" style="zoom:40%;" /></td>
    <td><img src="./assets/cross_mark_64.png" style="zoom:40%;" /></td>
    <td><img src="./assets/check_mark_64.png" style="zoom:40%;" /></td>
    <td> 设备和应用数据的无缝连接<br>（即将发布）</td>
  </tr>
  <tr>
    <td><b>事件历史</b></td>
    <td><img src="./assets/cross_mark_64.png" style="zoom:40%;" /></td>
    <td><img src="./assets/check_mark_64.png" style="zoom:40%;" /></td>
    <td><img src="./assets/cross_mark_64.png" style="zoom:40%;" /></td>
    <td><img src="./assets/check_mark_64.png" style="zoom:40%;" /></td>
    <td> 客户端故障诊断和行为审计</td>
  </tr>
  <tr>
    <td><b>消息队列</b></td>
    <td><img src="./assets/cross_mark_64.png" style="zoom:40%;" /></td>
    <td><img src="./assets/check_mark_64.png" style="zoom:40%;" /></td>
    <td><img src="./assets/cross_mark_64.png" style="zoom:40%;" /></td>
    <td><img src="./assets/check_mark_64.png" style="zoom:40%;" /></td>
    <td> 数据传输和分析的统一架构<br>（即将发布）</td>
  </tr>
  <tr>
    <td><b>流处理</b></td>
    <td><img src="./assets/cross_mark_64.png" style="zoom:40%;" /></td>
    <td><img src="./assets/check_mark_64.png" style="zoom:40%;" /></td>
    <td><img src="./assets/cross_mark_64.png" style="zoom:40%;" /></td>
    <td><img src="./assets/check_mark_64.png" style="zoom:40%;" /></td>
    <td> 更高的可靠性和灾难恢复能力<br>（即将发布）</td>
  </tr>
  <tr>
    <td><b>数据持久化</b></td>
    <td><img src="./assets/cross_mark_64.png" style="zoom:40%;" /></td>
    <td><img src="./assets/check_mark_64.png" style="zoom:40%;" /> 内置的 RocksDB 数据库或外部数据库</td>
    <td>N/A</td>
    <td>N/A</td>
    <td> <a href="https://docs.emqx.com/zh/enterprise/latest/durability/durability_introduction.html">提高了稳定性和可靠性</a></td>
  </tr>
  <tr>
    <td><b>Schema Registry</b></td>
    <td><img src="./assets/cross_mark_64.png" style="zoom:40%;" /></td>
    <td><img src="./assets/check_mark_64.png" style="zoom:40%;" /></td>
    <td><img src="./assets/cross_mark_64.png" style="zoom:40%;" /></td>
    <td><img src="./assets/check_mark_64.png" style="zoom:40%;" /></td>
    <td><a href="https://docs.emqx.com/zh/enterprise/latest/data-integration/schema-registry.html">编解码</a>保证数据一致性和可兼容性</td>
  </tr>
  <tr>
    <td><b>消息编解码</b></td>
    <td><img src="./assets/cross_mark_64.png" style="zoom:40%;" /></td>
    <td><img src="./assets/check_mark_64.png" style="zoom:40%;" /></td>
    <td><img src="./assets/cross_mark_64.png" style="zoom:40%;" /></td>
    <td><img src="./assets/check_mark_64.png" style="zoom:40%;" /></td>
    <td>灵活地消息格式转换：JSON<br>Avro<br>Protobuf<br>Custom codec (HTTP/gRPC)</td>
  </tr>
  <tr>
    <td><b>消息验证</b></td>
    <td><img src="./assets/cross_mark_64.png" style="zoom:40%;" /></td>
    <td><img src="./assets/check_mark_64.png" style="zoom:40%;" /></td>
    <td><img src="./assets/cross_mark_64.png" style="zoom:40%;" /></td>
    <td><img src="./assets/check_mark_64.png" style="zoom:40%;" /> 已规划</td>
    <td> 确保消息的完整性和合法性</td>
  </tr>
  <tr>
    <td><b>规则引擎</b></td>
    <td><img src="./assets/check_mark_64.png" style="zoom:40%;" /></td>
    <td><img src="./assets/check_mark_64.png" style="zoom:40%;" /></td>
    <td><img src="./assets/check_mark_64.png" style="zoom:40%;" /></td>
    <td><img src="./assets/check_mark_64.png" style="zoom:40%;" /></td>
    <td><a href="https://docs.emqx.com/zh/enterprise/latest/data-integration/rules.html">基于 SQL 的内置规则引擎</a></td>
  </tr>
  <tr>
    <td><b>Flow 设计器</b></td>
    <td><img src="./assets/cross_mark_64.png" style="zoom:40%;" /></td>
    <td><img src="./assets/check_mark_64.png" style="zoom:40%;" /></td>
    <td><img src="./assets/cross_mark_64.png" style="zoom:40%;" /></td>
    <td><img src="./assets/check_mark_64.png" style="zoom:40%;" /> 已规划</td>
    <td><a href="https://docs.emqx.com/zh/enterprise/latest/flow-designer/introduction.html">更简便的数据集成编排</a></td>
  </tr>
  <tr>
    <td><b>文件传输</b></td>
    <td><img src="./assets/cross_mark_64.png" style="zoom:40%;" /></td>
    <td><img src="./assets/check_mark_64.png" style="zoom:40%;" /></td>
    <td><img src="./assets/cross_mark_64.png" style="zoom:40%;" /></td>
    <td><img src="./assets/check_mark_64.png" style="zoom:40%;" /> 已规划</td>
    <td> 统一平台数据传输</td>
  </tr>
  <tr>
    <td><b>Kafka 集成</b></td>
    <td><img src="./assets/cross_mark_64.png" style="zoom:40%;" /></td>
    <td><img src="./assets/check_mark_64.png" style="zoom:40%;" /></td>
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
    <td><a href="https://www.emqx.com/zh/integrations">提升业务开发和发布速度</a></td>
  </tr>
  <tr>
    <td><b>故障排查</b></td>
    <td><img src="./assets/cross_mark_64.png" style="zoom:40%;" /></td>
    <td><img src="./assets/check_mark_64.png" style="zoom:40%;" /></td>
    <td><img src="./assets/cross_mark_64.png" style="zoom:40%;" /></td>
    <td><img src="./assets/check_mark_64.png" style="zoom:40%;" /></td>
    <td><a href="https://docs.emqx.com/zh/enterprise/latest/observability/tracer.html">日志追踪 (Trace)</a><br><a href="https://docs.emqx.com/zh/enterprise/latest/observability/slow-subscribers-statistics.html">慢订阅统计</a></td>
  </tr>
  <tr>
    <td><b>Cloud-Native &amp; K8s</b></td>
    <td><img src="./assets/check_mark_64.png" style="zoom:40%;" /></td>
    <td><img src="./assets/check_mark_64.png" style="zoom:40%;" /></td>
    <td>N/A</td>
    <td>N/A</td>
    <td><a href="https://www.emqx.com/zh/deployments">降低系统部署和管理成本</a></td>
  </tr>
  <tr>
    <td><b>边缘计算</b></td>
    <td><img src="./assets/check_mark_64.png" style="zoom:40%;" /></td>
    <td><img src="./assets/check_mark_64.png" style="zoom:40%;" /></td>
    <td><img src="./assets/check_mark_64.png" style="zoom:40%;" /></td>
    <td><img src="./assets/check_mark_64.png" style="zoom:40%;" /></td>
    <td> 降低数据传输延迟和成本<br><a href="https://www.emqx.com/zh/products/neuronex">Neuron</a><br><a href="https://www.emqx.com/zh/products/nanomq">NanoMQ</a></td>
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
    <td><span style="font-weight:normal">支持最多 3 个节点的集群<br>在生产环境中支持最多 100,000 个连接</span></td>
    <td><span style="font-weight:normal">支持最多 100 个节点的集群<br>每个集群最多支持 1 亿个 MQTT 连接</span></td>
    <td><span style="font-weight:normal">自动扩展，最多 1,000 条连接</span></td>
    <td><span style="font-weight:normal">无限制</span></td>
    <td><a href="https://www.emqx.com/zh/blog/reaching-100m-mqtt-connections-with-emqx-5-0">高度可扩展，EMQX 5.0 达成 1 亿 MQTT 连接</a></td>
  </tr>
  <tr>
    <td><b>高可用性</b></td>
    <td><span style="font-weight:normal">无主集群</span></td>
    <td><span style="font-weight:normal">核心-副本集群</span></td>
    <td><span style="font-weight:normal">无主集群</span></td>
    <td><span style="font-weight:normal">无主集群</span></td>
    <td><span style="font-weight:normal"> </span></td>
  </tr>
  <tr>
    <td><b>可靠性</b></td>
    <td><span style="font-weight:normal">会话持久化</span></td>
    <td><span style="font-weight:normal">RocksDB 消息持久化，具备高可用副本</span></td>
    <td><span style="font-weight:normal">会话持久化</span></td>
    <td><span style="font-weight:normal">会话持久化</span></td>
    <td><a href="https://www.emqx.com/zh/blog/mqtt-persistence-based-on-rocksdb">基于 RocksDB 实现高可靠、低时延的 MQTT 数据持久化</a></td>
  </tr>
  <tr>
    <td><b>吞吐量</b></td>
    <td><span style="font-weight:normal">10 万 MQTT 消息每秒</span></td>
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
    <td>最多3个</td>
    <td>100+</td>
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
    <td> 更高系统稳定性和资源利用率</td>
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
    <td> 确保数据操作的原子性和可靠性</td>
  </tr>
  <tr>
    <td><b>网络分区恢复</b></td>
    <td><img src="./assets/check_mark_64.png" style="zoom:40%;" /></td>
    <td><img src="./assets/check_mark_64.png" style="zoom:40%;" /></td>
    <td><img src="./assets/check_mark_64.png" style="zoom:40%;" /></td>
    <td><img src="./assets/check_mark_64.png" style="zoom:40%;" /></td>
    <td> 集群故障自动修复</td>
  </tr>
  <tr>
    <td><b>节点疏散 &amp; 集群重平衡</b></td>
    <td><img src="./assets/cross_mark_64.png" style="zoom:40%;" /></td>
    <td><img src="./assets/check_mark_64.png" style="zoom:40%;" /></td>
    <td>N/A</td>
    <td>N/A</td>
    <td> 不停机集群维护</td>
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
    <td><b>零停机/热升级</b></td>
    <td><img src="./assets/cross_mark_64.png" style="zoom:40%;" /></td>
    <td><img src="./assets/check_mark_64.png" style="zoom:40%;" /></td>
    <td>N/A</td>
    <td>N/A</td>
    <td> 系统漏洞的即时修复</td>
  </tr>
  <tr>
    <td><b>热补丁</b></td>
    <td><img src="./assets/cross_mark_64.png" style="zoom:40%;" /></td>
    <td><img src="./assets/check_mark_64.png" style="zoom:40%;" /></td>
    <td>N/A</td>
    <td>N/A</td>
    <td> 确保稳定的系统操作</td>
  </tr>
  <tr>
    <td><b>过载保护</b></td>
    <td><img src="./assets/check_mark_64.png" style="zoom:40%;" /></td>
    <td><img src="./assets/check_mark_64.png" style="zoom:40%;" /></td>
    <td>N/A</td>
    <td>N/A</td>
    <td> 提高系统管理效率</td>
  </tr>
  <tr>
    <td><b>多集群管理</b></td>
    <td><img src="./assets/cross_mark_64.png" style="zoom:40%;" /></td>
    <td><img src="./assets/check_mark_64.png" style="zoom:40%;" /></td>
    <td>N/A</td>
    <td>N/A</td>
    <td> 提高系统稳定性</td>
  </tr>
  <tr>
    <td><b>集群指标</b></td>
    <td><img src="./assets/check_mark_64.png" style="zoom:40%;" /></td>
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
    <td><img src="./assets/cross_mark_64.png" style="zoom:40%;" /></td>
    <td><img src="./assets/check_mark_64.png" style="zoom:40%;" /></td>
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
    <td><b>LB (Custom)</b></td>
    <td><img src="./assets/cross_mark_64.png" style="zoom:40%;" /></td>
    <td><img src="./assets/check_mark_64.png" style="zoom:40%;" /></td>
    <td><img src="./assets/cross_mark_64.png" style="zoom:40%;" /></td>
    <td><img src="./assets/check_mark_64.png" style="zoom:40%;" /></td>
    <td>GmSSL<br>平滑的连接迁移</td>
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
    <td><b>多协议网关</b></td>
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
    <td><img src="./assets/check_mark_64.png" style="zoom:40%;" /></td>
    <td><img src="./assets/check_mark_64.png" style="zoom:40%;" /></td>
    <td><img src="./assets/cross_mark_64.png" style="zoom:40%;" /></td>
    <td><img src="./assets/cross_mark_64.png" style="zoom:40%;" /></td>
    <td> </td>
  </tr>
  <tr>
    <td><b>ExProto</b></td>
    <td><img src="./assets/check_mark_64.png" style="zoom:40%;" /></td>
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
    <td><img src="./assets/check_mark_64.png" style="zoom:40%;" /></td>
    <td><img src="./assets/cross_mark_64.png" style="zoom:40%;" /></td>
    <td><img src="./assets/check_mark_64.png" style="zoom:40%;" /></td>
    <td> </td>
  </tr>
  <tr>
    <td><b>GBT32960</b></td>
    <td><img src="./assets/cross_mark_64.png" style="zoom:40%;" /></td>
    <td><img src="./assets/check_mark_64.png" style="zoom:40%;" /></td>
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
    <td>保障数据传输安全: TLS 1.1, 1.2, 1.3</td>
  </tr>
  <tr>
    <td><b>QUIC</b></td>
    <td><img src="./assets/check_mark_64.png" style="zoom:40%;" /></td>
    <td><img src="./assets/check_mark_64.png" style="zoom:40%;" /></td>
    <td><img src="./assets/cross_mark_64.png" style="zoom:40%;" /></td>
    <td><img src="./assets/check_mark_64.png" style="zoom:40%;" /></td>
    <td> 增强弱网络和移动网络数据传输的效率</td>
  </tr>
  <tr>
    <td><b>OCSP Stapling</b></td>
    <td><img src="./assets/check_mark_64.png" style="zoom:40%;" /></td>
    <td><img src="./assets/check_mark_64.png" style="zoom:40%;" /></td>
    <td><img src="./assets/cross_mark_64.png" style="zoom:40%;" /></td>
    <td><img src="./assets/check_mark_64.png" style="zoom:40%;" /></td>
    <td> 提供更灵活的安全措施</td>
  </tr>
  <tr>
    <td><b>连接抖动</b></td>
    <td><img src="./assets/check_mark_64.png" style="zoom:40%;" /></td>
    <td><img src="./assets/check_mark_64.png" style="zoom:40%;" /></td>
    <td><img src="./assets/check_mark_64.png" style="zoom:40%;" /></td>
    <td><img src="./assets/check_mark_64.png" style="zoom:40%;" /> 规划中</td>
    <td> 检测并拦截频繁的上下线连接</td>
  </tr>
  <tr>
    <td><b>审计日志</b></td>
    <td><img src="./assets/cross_mark_64.png" style="zoom:40%;" /></td>
    <td><img src="./assets/check_mark_64.png" style="zoom:40%;" /></td>
    <td><img src="./assets/check_mark_64.png" style="zoom:40%;" /></td>
    <td><img src="./assets/check_mark_64.png" style="zoom:40%;" /></td>
    <td> 支持重要操作的审计追踪</td>
  </tr>
  <tr>
    <td><b>Dashboard 单点登录（SSO）</b></td>
    <td><img src="./assets/cross_mark_64.png" style="zoom:40%;" /></td>
    <td><img src="./assets/check_mark_64.png" style="zoom:40%;" /></td>
    <td><img src="./assets/check_mark_64.png" style="zoom:40%;" /></td>
    <td><img src="./assets/check_mark_64.png" style="zoom:40%;" /></td>
    <td><a src="https://docs.emqx.com/zh/enterprise/latest/dashboard/sso.html">安全并简便的认证流程</a></td>
  </tr>
  <tr>
    <td><b>Dashboard/REST API 基于角色的访问控制 (RBAC)</b></td>
    <td><img src="./assets/cross_mark_64.png" style="zoom:40%;" /></td>
    <td><img src="./assets/check_mark_64.png" style="zoom:40%;" /></td>
    <td><img src="./assets/check_mark_64.png" style="zoom:40%;" /></td>
    <td><img src="./assets/check_mark_64.png" style="zoom:40%;" /></td>
    <td> 最小化权限以确保系统安全</td>
  </tr>
  <tr>
    <td><b>黑鸭分析</b></td>
    <td><img src="./assets/cross_mark_64.png" style="zoom:40%;" /></td>
    <td><img src="./assets/check_mark_64.png" style="zoom:40%;" /></td>
    <td>N/A</td>
    <td>N/A</td>
    <td>安全并简便的认证流程</td>
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
    <td><a href="https://docs.emqx.com/zh/enterprise/latest/access-control/authn/pwoverview.html">密码认证</a></td>
  </tr>
  <tr>
    <td><b>JWT</b></td>
    <td><img src="./assets/check_mark_64.png" style="zoom:40%;" /></td>
    <td><img src="./assets/check_mark_64.png" style="zoom:40%;" /></td>
    <td><img src="./assets/cross_mark_64.png" style="zoom:40%;" /></td>
    <td><img src="./assets/check_mark_64.png" style="zoom:40%;" /></td>
    <td><a href="https://docs.emqx.com/zh/enterprise/latest/access-control/authn/jwt.html">JWT 认证</a></td>
  </tr>
  <tr>
    <td><b>MQTT 5.0 增强认证</b></td>
    <td><img src="./assets/check_mark_64.png" style="zoom:40%;" /></td>
    <td><img src="./assets/check_mark_64.png" style="zoom:40%;" /></td>
    <td>N/A</td>
    <td>N/A</td>
    <td><a href="https://docs.emqx.com/zh/enterprise/latest/access-control/authn/scram.html">MQTT 5.0 增强认证</a></td>
  </tr>
  <tr>
    <td><b>LDAP 认证</b></td>
    <td><img src="./assets/check_mark_64.png" style="zoom:40%;" /></td>
    <td><img src="./assets/check_mark_64.png" style="zoom:40%;" /></td>
    <td><img src="./assets/cross_mark_64.png" style="zoom:40%;" /></td>
    <td><img src="./assets/check_mark_64.png" style="zoom:40%;" /> 已规划</td>
    <td> </td>
  </tr>
  <tr>
    <td><b>PSK 验证</b></td>
    <td><img src="./assets/check_mark_64.png" style="zoom:40%;" /></td>
    <td><img src="./assets/check_mark_64.png" style="zoom:40%;" /></td>
    <td><img src="./assets/cross_mark_64.png" style="zoom:40%;" /></td>
    <td><img src="./assets/check_mark_64.png" style="zoom:40%;" /></td>
    <td><a src="https://docs.emqx.com/zh/enterprise/latest/network/psk-authentication.html">启用 PSK 验证</a></td>
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
    <td><img src="./assets/cross_mark_64.png" style="zoom:40%;" /></td>
    <td><img src="./assets/check_mark_64.png" style="zoom:40%;" /></td>
    <td> </td>
  </tr>
  <tr>
    <td><b>ACL 数据源</b></td>
    <td><img src="./assets/check_mark_64.png" style="zoom:40%;" /></td>
    <td><img src="./assets/check_mark_64.png" style="zoom:40%;" /></td>
    <td><img src="./assets/cross_mark_64.png" style="zoom:40%;" /></td>
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
    <td><b>MQTT 服务</b></td>
    <td><img src="./assets/check_mark_64.png" style="zoom:40%;" /></td>
    <td><img src="./assets/check_mark_64.png" style="zoom:40%;" /></td>
    <td><img src="./assets/cross_mark_64.png" style="zoom:40%;" /></td>
    <td><img src="./assets/check_mark_64.png" style="zoom:40%;" /></td>
  </tr>
  <tr>
    <td><b>Webhook/HTTP Server</b></td>
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
    <td><img src="./assets/cross_mark_64.png" style="zoom:40%;" /></td>
    <td><img src="./assets/check_mark_64.png" style="zoom:40%;" /></td>
  </tr>
  <tr>
    <td><b>Apache Pulsar</b></td>
    <td><img src="./assets/cross_mark_64.png" style="zoom:40%;" /></td>
    <td><img src="./assets/check_mark_64.png" style="zoom:40%;" /></td>
    <td><img src="./assets/cross_mark_64.png" style="zoom:40%;" /></td>
    <td><img src="./assets/check_mark_64.png" style="zoom:40%;" /></td>
  </tr>
  <tr>
    <td><b>AWS Kinesis</b></td>
    <td><img src="./assets/cross_mark_64.png" style="zoom:40%;" /></td>
    <td><img src="./assets/check_mark_64.png" style="zoom:40%;" /></td>
    <td><img src="./assets/cross_mark_64.png" style="zoom:40%;" /></td>
    <td><img src="./assets/check_mark_64.png" style="zoom:40%;" /></td>
  </tr>
  <tr>
    <td><b>AWS S3</b></td>
    <td><img src="./assets/cross_mark_64.png" style="zoom:40%;" /></td>
    <td><img src="./assets/check_mark_64.png" style="zoom:40%;" /></td>
    <td><img src="./assets/cross_mark_64.png" style="zoom:40%;" /></td>
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
    <td><b>Elasticsearch</b></td>
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
    <td><b>Syskeeper</b></td>
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
    <td><b>编解码</b></td>
    <td><img src="./assets/cross_mark_64.png" style="zoom:40%;" /></td>
    <td><img src="./assets/check_mark_64.png" style="zoom:40%;" /></td>
    <td><img src="./assets/cross_mark_64.png" style="zoom:40%;" /></td>
    <td><img src="./assets/check_mark_64.png" style="zoom:40%;" /></td>
    <td> 确保消息格式一致性</td>
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
  <tr>
    <td><b>Sparkplug B Codec</b></td>
    <td><img src="./assets/cross_mark_64.png" style="zoom:40%;" /></td>
    <td><img src="./assets/check_mark_64.png" style="zoom:40%;" /></td>
    <td><img src="./assets/cross_mark_64.png" style="zoom:40%;" /></td>
    <td><img src="./assets/check_mark_64.png" style="zoom:40%;" /></td>
    <td> </td>
  </tr>
  <tr>
    <td><b>JSON Schema 验证</b></td>
    <td><img src="./assets/cross_mark_64.png" style="zoom:40%;" /></td>
    <td><img src="./assets/check_mark_64.png" style="zoom:40%;" /></td>
    <td><img src="./assets/cross_mark_64.png" style="zoom:40%;" /></td>
    <td><img src="./assets/check_mark_64.png" style="zoom:40%;" /></td>
    <td> </td>
  </tr>
  <tr>
    <td><b>Avro 消息验证</b></td>
    <td><img src="./assets/cross_mark_64.png" style="zoom:40%;" /></td>
    <td><img src="./assets/check_mark_64.png" style="zoom:40%;" /></td>
    <td><img src="./assets/cross_mark_64.png" style="zoom:40%;" /></td>
    <td><img src="./assets/check_mark_64.png" style="zoom:40%;" /></td>
    <td> </td>
  </tr>
  <tr>
    <td><b>ProtoBuf 消息验证</b></td>
    <td><img src="./assets/cross_mark_64.png" style="zoom:40%;" /></td>
    <td><img src="./assets/check_mark_64.png" style="zoom:40%;" /></td>
    <td><img src="./assets/cross_mark_64.png" style="zoom:40%;" /></td>
    <td><img src="./assets/check_mark_64.png" style="zoom:40%;" /></td>
    <td> </td>
  </tr>
  <tr>
    <td><b>内置 SQL 函数</b></td>
    <td><img src="./assets/check_mark_64.png" style="zoom:40%;" /></td>
    <td><img src="./assets/check_mark_64.png" style="zoom:40%;" /></td>
    <td><img src="./assets/cross_mark_64.png" style="zoom:40%;" /></td>
    <td><img src="./assets/check_mark_64.png" style="zoom:40%;" /></td>
    <td><a href="https://docs.emqx.com/zh/enterprise/latest/data-integration/rule-sql-builtin-functions.html">丰富的内置 SQL 函数，支持自定义扩展</a></td>
  </tr>
  <tr>
    <td><b>jq 函数</b></td>
    <td><img src="./assets/check_mark_64.png" style="zoom:40%;" /></td>
    <td><img src="./assets/check_mark_64.png" style="zoom:40%;" /></td>
    <td><img src="./assets/cross_mark_64.png" style="zoom:40%;" /></td>
    <td><img src="./assets/check_mark_64.png" style="zoom:40%;" /></td>
    <td> 高效的 JSON 数据处理</td>
  </tr>
  <tr>
    <td><b>客户端事件处理</b></td>
    <td><img src="./assets/check_mark_64.png" style="zoom:40%;" /></td>
    <td><img src="./assets/check_mark_64.png" style="zoom:40%;" /></td>
    <td><img src="./assets/cross_mark_64.png" style="zoom:40%;" /></td>
    <td><img src="./assets/check_mark_64.png" style="zoom:40%;" /></td>
    <td><a href="https://docs.emqx.com/zh/enterprise/latest/data-integration/rule-sql-events-and-fields.html#%E5%AE%A2%E6%88%B7%E7%AB%AF%E4%BA%8B%E4%BB%B6">客户端事件</a>，事件驱动的业务开发</td>
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
    <td><a href="https://docs.emqx.com/zh/emqx/latest/extensions/hooks.html">钩子</a></td>
  </tr>
  <tr>
    <td><b>插件</b></td>
    <td><img src="./assets/check_mark_64.png" style="zoom:40%;" /></td>
    <td><img src="./assets/check_mark_64.png" style="zoom:40%;" /></td>
    <td>N/A</td>
    <td>N/A</td>
    <td><a ref="https://docs.emqx.com/zh/emqx/latest/extensions/plugins.html">插件</a></td>
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
    <td><b>插件热配置</b></td>
    <td><img src="./assets/cross_mark_64.png" style="zoom:40%;" /></td>
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
    <td><b>Prometheus</b></td>
    <td><img src="./assets/check_mark_64.png" style="zoom:40%;" /></td>
    <td><img src="./assets/check_mark_64.png" style="zoom:40%;" /></td>
    <td><img src="./assets/cross_mark_64.png" style="zoom:40%;" /></td>
    <td><img src="./assets/check_mark_64.png" style="zoom:40%;" /></td>
    <td> </td>
  </tr>
  <tr>
    <td><b>Datadog</b></td>
    <td><img src="./assets/check_mark_64.png" style="zoom:40%;" /></td>
    <td><img src="./assets/check_mark_64.png" style="zoom:40%;" /></td>
    <td><img src="./assets/cross_mark_64.png" style="zoom:40%;" /></td>
    <td><img src="./assets/check_mark_64.png" style="zoom:40%;" /> 已规划</td>
    <td></td>
  </tr>
  <tr>
    <td><b>OpenTelemetry</b></td>
    <td><img src="./assets/check_mark_64.png" style="zoom:40%;" /></td>
    <td><img src="./assets/check_mark_64.png" style="zoom:40%;" /></td>
    <td><img src="./assets/cross_mark_64.png" style="zoom:40%;" /></td>
    <td><img src="./assets/check_mark_64.png" style="zoom:40%;" /> 已规划</td>
    <td></td>
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
    <td><img src="./assets/cross_mark_64.png" style="zoom:40%;" /></td>
    <td><img src="./assets/check_mark_64.png" style="zoom:40%;" /></td>
    <td><img src="./assets/check_mark_64.png" style="zoom:40%;" /></td>
    <td><img src="./assets/check_mark_64.png" style="zoom:40%;" /></td>
    <td> </td>
  </tr>
  <tr>
    <td><b>慢订阅监控</b></td>
    <td><img src="./assets/cross_mark_64.png" style="zoom:40%;" /></td>
    <td><img src="./assets/check_mark_64.png" style="zoom:40%;" /></td>
    <td><img src="./assets/cross_mark_64.png" style="zoom:40%;" /></td>
    <td><img src="./assets/check_mark_64.png" style="zoom:40%;" /> 已规划</td>
    <td> </td>
  </tr>
  <tr>
    <td><b>主题监控</b></td>
    <td><img src="./assets/cross_mark_64.png" style="zoom:40%;" /></td>
    <td><img src="./assets/check_mark_64.png" style="zoom:40%;" /></td>
    <td><img src="./assets/cross_mark_64.png" style="zoom:40%;" /></td>
    <td><img src="./assets/check_mark_64.png" style="zoom:40%;" /> 已规划</td>
    <td> </td>
  </tr>
  <tr>
    <td><b>客户端监控</b></td>
    <td><img src="./assets/cross_mark_64.png" style="zoom:40%;" /></td>
    <td><img src="./assets/check_mark_64.png" style="zoom:40%;" /></td>
    <td><img src="./assets/check_mark_64.png" style="zoom:40%;" /></td>
    <td><img src="./assets/check_mark_64.png" style="zoom:40%;" /></td>
    <td> </td>
  </tr>
   <tr>
    <td><b>日志追踪</b></td>
    <td><img src="./assets/cross_mark_64.png" style="zoom:40%;" /></td>
    <td><img src="./assets/check_mark_64.png" style="zoom:40%;" /></td>
    <td><img src="./assets/check_mark_64.png" style="zoom:40%;" /></td>
    <td><img src="./assets/check_mark_64.png" style="zoom:40%;" /></td>
    <td> </td>
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
    <td><b>阿里云</b></td>
    <td><img src="./assets/check_mark_64.png" style="zoom:40%;" /></td>
    <td><img src="./assets/check_mark_64.png" style="zoom:40%;" /></td>
    <td><img src="./assets/check_mark_64.png" style="zoom:40%;" /></td>
    <td><img src="./assets/check_mark_64.png" style="zoom:40%;" /></td>
    <td></td>
  </tr>
  <tr>
    <td><b>华为云</b></td>
    <td><img src="./assets/check_mark_64.png" style="zoom:40%;" /></td>
    <td><img src="./assets/check_mark_64.png" style="zoom:40%;" /></td>
    <td><img src="./assets/check_mark_64.png" style="zoom:40%;" /> 规划中</td>
    <td><img src="./assets/check_mark_64.png" style="zoom:40%;" /></td>
    <td></td>
  </tr>
  <tr>
    <td><b>腾讯云</b></td>
    <td><img src="./assets/check_mark_64.png" style="zoom:40%;" /></td>
    <td><img src="./assets/check_mark_64.png" style="zoom:40%;" /></td>
    <td><img src="./assets/check_mark_64.png" style="zoom:40%;" /> 规划中</td>
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
    <td><img src="./assets/check_mark_64.png" style="zoom:40%;" /></td>
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
