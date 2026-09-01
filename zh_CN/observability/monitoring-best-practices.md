# 生产监控最佳实践

生产部署需要使用 EMQX Dashboard 以外的监控系统。Dashboard 可以显示 broker 的当前状态，但 broker 或主机不可用时，无法通知运维人员。本页的监控建议可帮助您及早发现服务中断、冗余丢失和资源耗尽，以便及时采取措施。

本页适用于 EMQX Enterprise 的生产部署。请将示例阈值作为起点，并根据服务等级目标（Service-Level Objective，SLO）、经过测试的容量、流量模式和恢复时间进行调整。

## 设计生产监控系统

设计监控系统时，请遵循以下原则：

1. **将 EMQX 指标导出到外部监控系统。**

   建议使用 [Prometheus Pull 模式](./prometheus.md#配置-pull-模式集成)进行全面监控。请直接抓取每个 EMQX 节点，而不要通过负载均衡器抓取，避免健康节点掩盖发生故障或被隔离的节点。监控每个目标的 Prometheus `up` 指标。

2. **转发 EMQX 内置告警。**

   根据运行环境配置 EMQX 内置告警阈值，并通过 [Webhook 或系统主题](./alarms.md#获取告警信息)将告警事件发送到外部通知系统。不要依赖运维人员在 Dashboard 中主动发现告警。

3. **从集群外部运行端到端 MQTT 检查。**

   合成客户端应通过与生产客户端相同的负载均衡器、TLS 监听器和认证路径进行连接。客户端应发布一条具有唯一标识的消息，通过订阅接收该消息，并测量总时延。此检查可以发现仅依靠 broker 指标无法检测的故障。

4. **监控主机或容器平台。**

   EMQX 不能替代操作系统、Kubernetes 或云平台监控。请采集 CPU 节流、内存压力、磁盘容量和时延、文件描述符使用量、网络错误、容器重启和时间同步状态。

5. **集中收集日志。**

   将每个节点的 warning、error 和 critical 日志发送到 EMQX 集群外部的存储系统。建议使用 JSON 格式，以便告警规则匹配结构化的 `msg`、`node` 和其他上下文字段。日志可以揭示指标或内置告警未覆盖的情况。

6. **保持监控系统独立于 EMQX。**

   当 EMQX 节点、可用区或整个集群不可用时，监控和通知链路必须仍然可用。

::: tip

指标采集间隔应短于要求的故障检测时间。例如，抓取间隔为 15 秒并在连续 2 次失败后触发告警时，通常可以在 1 分钟内检测到不可访问的目标，同时避免因单次漏抓取而发送紧急通知。

:::

## 建立 SLO、容量基线和告警阈值

不要直接在生产环境中套用固定阈值。请使用以下流程：

1. 为连接成功率、消息发布到交付的成功率和时延定义用户可感知的 SLO。
2. 运行具有代表性的[性能测试](../performance/overview.md)，记录系统达到饱和前的资源使用量、消息速率和时延。
3. 观察至少 1 个正常业务周期，确定每天或每周的峰值。
4. 将 warning 阈值设置在经过测试的安全容量以下，为扩容或安排维护留出足够时间。将 critical 阈值设置为一旦达到便需要立即采取措施的值。
5. 在流量增长、拓扑变化、升级或持久会话和数据集成发生变化后，重新评估阈值。

不要只根据固定百分比触发告警。趋势和预测告警通常可以留出更实用的维护准备时间，例如预测磁盘将在 24 小时内耗尽，或连接数将在 1 周内达到经过测试的容量。

## 监控先行指标

预防性告警应在集群仍能处理流量时发现持续恶化的情况。warning 阈值应为调查和维护留出时间；critical 阈值用于需要立即采取措施的情况。对于每种情况，以下内容列出了相关信号和建议运维人员采取的措施。

### 集群和运行时健康状态

**Mria 复制压力**

- **预警条件：**复制延迟或队列持续高于正常峰值，或者持续增长而没有回落。
- **相关信号：**在复制节点上，监控 `emqx_mria_lag`、`emqx_mria_message_queue_len` 和 `emqx_mria_replayq_len`。在核心节点上，监控 `emqx_mria_server_mql` 和 `emqx_mria_weight`。
- **建议措施：**结合复制节点及其上游核心节点的日志分析指标变化。检查 Mria 复制延迟指标采集失败、繁忙的分布式端口、长时间调度暂停、Mnesia 过载和 Mria 复制错误。然后检查网络时延和丢包、CPU 以及磁盘 I/O。在复制节点进一步落后之前，降低写入压力或增加核心节点容量。

`emqx_mria_lag` 表示复制节点分片落后于上游核心节点分片的事务数量，不是以秒为单位的时长。写入突增期间的短时峰值可能是正常现象。当该值持续高于代表性峰值流量期间观察到的最大值，或者该值与 Mria 队列指标持续呈正增长趋势时，应触发告警。按节点和 `shard` 对告警分组，因为某个分片可能异常，而其他分片仍能正常复制。有关各项 Mria 指标的详细信息，请参阅[集群监控和故障排查](../deploy/cluster/mria-introduction.md#集群监控和故障排查)。

**配置收敛**

- **预警条件：**节点间的 `emqx_conf_sync_txid` 差异持续时间超过正常配置发布所需时间。
- **相关信号：**每个节点的 `emqx_conf_sync_txid` 和配置同步日志。
- **建议措施：**停止继续更改配置，找出落后的节点，并检查集群连接和配置同步错误。在维护或再次更改配置之前，确保各节点的配置重新收敛。

**运行时积压**

- **预警条件：**运行队列或邮箱大小持续高于已建立的基线。
- **相关信号：**`emqx_vm_run_queue`、`emqx_vm_mnesia_tm_mailbox_size`、`emqx_vm_broker_pool_max_mailbox_size`、内置过载告警和 `busy_dist_port` 事件。
- **建议措施：**在请求时延和队列进一步增长前，调查持续过载、存储缓慢或集群通信问题。

### 资源和容量

**CPU 压力**

- **预警条件：**CPU 使用率持续高于正常峰值 10 到 15 分钟。
- **相关信号：**`emqx_vm_cpu_use`、主机 CPU、负载和容器节流。
- **建议措施：**找出导致用量增长的工作负载或集成。在资源饱和前重新平衡流量或增加容量。EMQX 内置 CPU 告警的默认阈值为 80%。

**内存压力**

- **预警条件：**内存持续高于 warning 阈值，或者持续增长并接近主机或容器限制。
- **相关信号：**`emqx_vm_used_memory`、`emqx_vm_total_memory`、主机或容器内存和 EMQX 内存告警。
- **建议措施：**检查连接、会话、队列和集成的增长情况。在操作系统终止进程之前，增加容量或消除增长来源。EMQX 内置系统内存告警的默认阈值为 70%。

**过载保护活动**

- **预警条件：**过载保护计数器增长，尤其是连接关闭或延迟超时计数器。
- **相关信号：**`emqx_overload_protection_new_conn`、`emqx_overload_protection_delay_timeout`、`emqx_overload_protection_delay_ok`、`emqx_overload_protection_gc` 和 `emqx_overload_protection_hibernation`。只有启用过载保护后，才会导出这些指标。
- **建议措施：**此时 broker 已开始缓解资源压力。结合 CPU、内存、运行队列、邮箱和连接抖动分析相关事件。在更多客户端流量受到影响前，降低负载或增加容量。

**磁盘压力**

- **预警条件：**可用空间低于运维预留空间，或者预计将在下一个维护窗口前耗尽。
- **相关信号：**主机或卷的可用字节数、可用 inode、I/O 时延和磁盘增长率。
- **建议措施：**按照保留策略删除数据或扩展卷。常见起始值为可用空间低于 20% 时触发 warning 告警，低于 10% 时触发 critical 告警。

**Broker 容量**

- **预警条件：**连接、会话、订阅或主题数量接近经过测试或 License 规定的运行限制。
- **相关信号：**`emqx_connections_count`、`emqx_sessions_count`、`emqx_subscriptions_count`、`emqx_topics_count`，以及 EMQX Enterprise 中的 `emqx_license_max_sessions`。
- **建议措施：**将增长趋势与容量测试结果进行比较。在达到限制前添加节点或转移流量。不要将历史 `*_max` 仪表类型指标视为配置的容量限制。

### 消息交付和依赖项

**消息丢失**

- **预警条件：**非预期的丢弃计数器增长。
- **相关信号：**`emqx_messages_dropped_*` 和 `emqx_delivery_dropped_*`。
- **建议措施：**调查具体原因。因队列已满、超出配额、达到接收上限或消息过期而丢弃消息，可能表示系统过载或限制配置不正确。在某些应用中，`no_subscribers` 和 `no_local` 丢弃可能是预期行为。

**认证和授权依赖项健康状态**

- **预警条件：**已启用的提供者或数据源未连接（状态为 `0`）、认证或授权时延高于正常峰值，或者认证失败或授权拒绝数量异常增加。
- **相关信号：**`/api/v5/prometheus/auth` 中的 `emqx_authn_enable`、`emqx_authn_status`、`emqx_authn_latency`、`emqx_authn_failed`、`emqx_authz_enable`、`emqx_authz_status`、`emqx_authz_latency` 和 `emqx_authz_deny`。
- **建议措施：**检查外部数据库、HTTP 服务、LDAP 服务器、网络和连接池。结合客户端流量分析失败数量的突增，以区分后端问题、无效凭据、应用变更或攻击。

**数据集成健康状态**

- **预警条件：**已启用的连接器或动作断开连接；`emqx_action_queuing` 或 `emqx_action_inflight` 持续增长而没有回落；或者延迟回复、重试、失败或丢弃数量增加。
- **相关信号：**`/api/v5/prometheus/data_integration` 中的 `emqx_connector_enable`、`emqx_connector_status`、`emqx_action_enable`、`emqx_action_status`、`emqx_action_queuing`、`emqx_action_inflight` 和相关动作指标，以及 EMQX `resource` 告警。
- **建议措施：**检查外部服务和网络，然后确认缓冲区容量和重试行为。队列和进行中请求持续增长，可以在失败和丢弃开始前提供预警。

### 到期风险

**证书和 License 到期**

- **预警条件：**到期时间进入组织预留的续期提前期。
- **相关信号：**`emqx_cert_expiry_at`，以及 EMQX Enterprise 中的 `emqx_license_expiry_at`。
- **建议措施：**续订并部署证书或 License。常见起始值为到期前 30 天触发 warning 告警，到期前 7 天触发 critical 告警。

### 验证指标可用性

有关 Dashboard 中显示的 broker 计数器说明，请参阅[统计指标](./metrics-and-stats.md)。基本 broker 指标、认证和授权指标以及数据集成指标通过不同的 Prometheus 端点公开。指标可用性可能因版本形态和启用的功能而异。创建规则前，请检查部署环境中的相应端点。

## 集中收集日志并选择性告警

### 在集群外部收集日志

不要只在节点本地保留日志。节点故障后，诊断所需的日志可能无法访问。将每个节点的日志发送到 EMQX 集群外部的集中式系统，并添加集群、节点、节点角色、EMQX 版本和可用区标签。

使用 [JSON 日志格式](./log.md#日志格式)，并至少保留 warning、error 和 critical 事件。可以从控制台或文件输出中收集日志，也可以通过 [OpenTelemetry](./opentelemetry/logs.md) 导出日志。有关配置和生产环境采集指南，请参阅[日志](./log.md)。

使用日志收集器和传输组件的健康指标，或者不依赖应用日志量的显式心跳，监控日志收集器和传输链路。不要仅因节点没有产生日志而触发告警；空闲或健康节点在配置的日志级别下可能没有需要报告的内容。

### 定义定向日志告警

使用以下事件和建议定义日志告警规则：

| 情况 | 日志信号 | 告警建议 |
| --- | --- | --- |
| Mria 复制延迟指标采集失败 | `prometheus_mria_shard_lag_refresh_exception` | 重复发生时触发告警。导出器会缓存 Mria 延迟；刷新超时后，导出器可能继续导出之前的值，使其看起来保持稳定。 |
| Erlang VM 或节点间通信压力 | `busy_dist_port`、`long_schedule`、`long_gc` 和 Mnesia 过载消息 | 根据持续发生率或重复事件触发告警，并结合 Mria 队列、CPU 和时延进行分析。这些事件可能先于客户端可感知的性能下降出现。 |
| Mria 复制或拓扑故障 | `gap_in_the_tlog` 和 `mria_lb_split_brain` | 立即通知负责的运维人员。从结构化字段中获取节点、分片、agent、预期序列号和实际序列号。 |
| 缓冲或消息队列压力 | `data_bridge_buffer_overflow`、`unrecoverable_resource_error` 和 `dropped_msg_due_to_mqueue_is_full` | 当这些事件并非预期行为或超过应用可接受的消息丢失率时触发告警，并结合动作和消息丢弃计数器进行分析。 |
| 配置同步失败 | `sync_data_from_node_failed` 和 `cluster_rpc_apply_failed` | 在更改配置或启动节点期间发生时立即触发告警，并验证所有节点是否已收敛到预期配置。 |

并非每个 warning 级别日志都需要立即通知运维人员。例如，认证失败和格式错误的客户端流量在低发生率下可能是预期行为。告警规则应基于选定的 `msg` 值、严重级别、持续事件发生率或相对于正常基线的偏差。将非预期的 critical 事件视为需要立即处理的事件。

### 考虑日志限流

EMQX 会限制选定的重复日志事件，因此日志查询结果可能少于原始事件数量。在 Dashboard 和告警中包含 `log_events_throttled_during_last_period`，并使用其 `dropped` 字段确定被抑制的消息。有关详细信息，请参阅[日志限流](./log.md#日志限流)。

## 单独检测故障

[监控先行指标](#监控先行指标)中的指标可以提供提前预警，但不能替代故障检测告警。以下情况表示服务已经中断或冗余能力已经丧失。发生其中任何一种情况时，应配置告警以立即通知负责的运维人员：

- Prometheus `up == 0`
- 端到端 MQTT 合成检查失败
- `emqx_cluster_nodes_running` 低于计划的集群规模
- `emqx_cluster_nodes_stopped` 增长
- `emqx_vm_uptime_ms` 意外重置
- EMQX `partition` 告警

使用[监控先行指标](#监控先行指标)中所述的预警指标及早发现持续恶化的情况，为在故障发生前安排维护留出足够时间。

## Prometheus 告警规则示例

以下是一份可整体复制的 Prometheus 告警规则起始配置。用于生产环境前，请完成以下调整：

- 示例使用 [Prometheus 服务器配置示例](./prometheus.md#prometheus-服务器配置示例)中的 job 名称。如果抓取 job 使用其他名称，请更新 `job` 匹配器。
- 集群节点丢失规则假设计划的集群规模为 3 个节点。请将 `3` 替换为计划的集群规模。
- 根据部署环境调整其他示例阈值。
- 如果一个 Prometheus job 包含多个集群，请按集群标签聚合配置收敛规则。
- 根据峰值流量基线，为 Mria 趋势规则添加绝对阈值。示例规则检测持续呈正增长趋势的队列或延迟，但规模较大且保持稳定的积压也应触发告警。
- 针对磁盘耗尽、内存限制、容器重启和网络健康状态添加主机或平台特定规则。

```yaml
groups:
  - name: emqx-early-warning
    rules:
      - alert: EMQXMRIAReplicationLagGrowing
        expr: deriv(emqx_mria_lag{job="emqx_stats"}[10m]) > 0
        for: 10m
        labels:
          severity: warning
        annotations:
          summary: "Mria replication lag is growing on {{ $labels.instance }} shard {{ $labels.shard }}"

      - alert: EMQXMRIAReplicationQueueGrowing
        expr: deriv(emqx_mria_server_mql{job="emqx_stats"}[10m]) > 0 or deriv(emqx_mria_message_queue_len{job="emqx_stats"}[10m]) > 0 or deriv(emqx_mria_replayq_len{job="emqx_stats"}[10m]) > 0
        for: 10m
        labels:
          severity: warning
        annotations:
          summary: "A Mria replication queue is growing on {{ $labels.instance }} shard {{ $labels.shard }}"

      - alert: EMQXSustainedHighCPU
        expr: emqx_vm_cpu_use{job="emqx_stats"} > 80
        for: 10m
        labels:
          severity: warning
        annotations:
          summary: "EMQX CPU usage is high on {{ $labels.instance }}"

      - alert: EMQXSustainedHighMemory
        expr: 100 * emqx_vm_used_memory{job="emqx_stats"} / emqx_vm_total_memory{job="emqx_stats"} > 70
        for: 10m
        labels:
          severity: warning
        annotations:
          summary: "EMQX host memory usage is high on {{ $labels.instance }}"

      - alert: EMQXOverloadProtectionActive
        expr: sum by (instance) (increase(emqx_overload_protection_new_conn{job="emqx_stats"}[5m])) > 0 or sum by (instance) (increase(emqx_overload_protection_delay_timeout{job="emqx_stats"}[5m])) > 0
        labels:
          severity: warning
        annotations:
          summary: "EMQX overload protection is closing or timing out client work on {{ $labels.instance }}"

      - alert: EMQXConfigurationNotConverged
        expr: max(emqx_conf_sync_txid{job="emqx_stats"}) != min(emqx_conf_sync_txid{job="emqx_stats"})
        for: 5m
        labels:
          severity: warning
        annotations:
          summary: "EMQX nodes report different configuration transaction IDs"

      - alert: EMQXDeliveryQueueFullDrops
        expr: sum by (instance) (increase(emqx_delivery_dropped_queue_full{job="emqx_stats"}[5m])) > 0
        labels:
          severity: warning
        annotations:
          summary: "EMQX dropped messages because a delivery queue was full"

      - alert: EMQXActionQueueGrowing
        expr: deriv(emqx_action_queuing{job="emqx_data_integration"}[10m]) > 0
        for: 10m
        labels:
          severity: warning
        annotations:
          summary: "EMQX data integration action {{ $labels.id }} has a growing queue on {{ $labels.instance }}"

      - alert: EMQXActionFailures
        expr: sum by (instance, id) (increase(emqx_action_failed{job="emqx_data_integration"}[5m])) > 0
        labels:
          severity: warning
        annotations:
          summary: "EMQX data integration action {{ $labels.id }} is failing"

      - alert: EMQXAuthenticationBackendUnavailable
        expr: (emqx_authn_enable{job="emqx_auth"} == 1 and on (instance, id) emqx_authn_status{job="emqx_auth"} == 0) or (emqx_authz_enable{job="emqx_auth"} == 1 and on (instance, type) emqx_authz_status{job="emqx_auth"} == 0)
        for: 2m
        labels:
          severity: warning
        annotations:
          summary: "An enabled EMQX authentication or authorization backend is unavailable on {{ $labels.instance }}"

      - alert: EMQXCertificateExpiresSoon
        expr: emqx_cert_expiry_at{job="emqx_stats"} > 0 and (emqx_cert_expiry_at{job="emqx_stats"} - time()) < 30 * 24 * 60 * 60
        for: 1h
        labels:
          severity: warning
        annotations:
          summary: "EMQX listener certificate expires within 30 days"

  - name: emqx-failure-detection
    rules:
      - alert: EMQXMetricsTargetDown
        expr: up{job="emqx_stats"} == 0
        for: 30s
        labels:
          severity: critical
        annotations:
          summary: "EMQX metrics target {{ $labels.instance }} is unreachable"

      - alert: EMQXClusterLostNode
        expr: min by (job) (emqx_cluster_nodes_running{job="emqx_stats"}) < 3
        for: 2m
        labels:
          severity: critical
        annotations:
          summary: "EMQX cluster has fewer than 3 running nodes"
```

计数器指标通常只会增长。请根据一段时间内的变化率或增量触发告警，而不要根据绝对值触发告警。为资源 Gauge 指标配置 `for` 持续时间，避免短时流量峰值触发不必要的告警。

## 使告警可执行

1. **定义告警上下文和负责人。**

   每个可执行告警都应标明受影响的集群；如适用，还应标明受影响的节点、当前值和阈值。告警还应包含 Dashboard 链接、负责人和 runbook（运维操作手册），说明如何调查告警、缓解影响并解决问题。runbook 应说明如何确认告警触发条件、保护服务、恢复冗余，以及如何决定扩容、重平衡、重启或修复。

2. **测试告警发送和恢复。**

   依赖告警链路前，应对其进行完整测试。在非生产环境或批准的测试窗口内，主动停止抓取目标、降低测试阈值并断开测试集成。确认告警能够发送给正确的运维人员、包含足够的上下文，并在恢复后清除。

3. **准备维护流程。**

   在冗余仍然可用时，使用 warning 告警安排维护。更改集群前，请确认备份可用、其余节点能够承载负载，并且告警系统运行正常。相关流程包括[备份与恢复](../operations/backup-restore.md)、[节点疏散和集群负载重平衡](../deploy/cluster/rebalancing.md)以及 [EMQX Enterprise 滚动升级](../deploy/rolling-upgrades.md)。

## 生产就绪检查清单

- 外部监控系统可以监控每个 EMQX 节点及其主机或容器。
- 内置告警已转发到 EMQX 外部并经过测试。
- 每个节点的 warning、error 和 critical 日志均集中存储，并且日志收集链路受到监控。
- 外部 MQTT 合成检查覆盖生产客户端路径。
- Mria 复制、配置收敛和运行时积压告警已有明确的负责人和 runbook。
- 过载保护、CPU、内存、磁盘和 broker 容量告警已有明确的负责人和 runbook。
- 认证和授权、消息丢弃及数据集成告警已有明确的负责人和 runbook。
- 证书和 License 到期告警已有明确的负责人和 runbook。
- 已为选定的 Mria、VM 压力、缓冲区溢出和配置同步日志事件配置与严重级别相符的基于发生率或立即触发的告警。
- 单独的目标不可用、MQTT 合成检查、集群规模和集群分区告警可以检测故障，并立即通知负责的运维人员。
- warning 阈值为团队正常的维护和容量扩充流程留出足够时间。
- Dashboard 同时显示当前值以及相关业务周期内的趋势。
- 定期测试告警通知、备份恢复和滚动维护流程。
