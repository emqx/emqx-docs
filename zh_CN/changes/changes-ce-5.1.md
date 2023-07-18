# 版本发布

## v5.1.0

### 增强

- [#10858](https://github.com/emqx/emqx/pull/10858) 规则引擎 SQL 语言新增了一个实用函数 timezone_to_offset_seconds/1。该函数将时区字符串（例如"+02:00"、"Z"和"local"）转换为相应的偏移秒数。
- [#10754](https://github.com/emqx/emqx/pull/10754) 对 MQTT 桥接进行了增强，利用连接池和可用的并行性，大大提高了吞吐量。因此，单个 MQTT 桥接现在使用一组 `clientid` 连接到远程代理。
- [#10782](https://github.com/emqx/emqx/pull/10782) 在保留器（retainer）配置中添加了一个新的 `deliver_rate` 选项，它可以限制保留器中每个会话的最大传递速率。
- [#10598](https://github.com/emqx/emqx/pull/10598) 在 ExProto 中提供了一种 Unary 类型的回调方法，以避免可能的消息乱序问题。
- [#10790](https://github.com/emqx/emqx/pull/10790) 通过优化配置读取机制减少读取配置的开销。
- [#10910](https://github.com/emqx/emqx/pull/10910) 数据桥接资源选项 `auto_restart_interval` 已被弃用，改为使用 `health_check_interval`，而 `request_timeout` 则被重命名为 `request_ttl`。此外，默认的 `request_ttl` 值从 15 秒增加到了 45 秒。
  之前同时存在 `auto_restart_interval` 和 `health_check_interval` 会导致混淆，因为这两个参数都会影响数据桥接在故障下的恢复。这两个参数的配置不一致可能导致消息过期而无法重试。现在，`health_check_interval` 用于控制进行健康检查的间隔，健康检查会将数据桥接转换为 `disconnected` 或 `connecting` 状态，也可以让数据桥接从 `disconnected` 状态中进行恢复。
- [#10929](https://github.com/emqx/emqx/pull/10929) 升级 Erland/OTP 到 25.3.2-1。
- [#10909](https://github.com/emqx/emqx/pull/10909) 移除了网关已弃用的 HTTP API。
- [#10933](https://github.com/emqx/emqx/pull/10933) 支持在 MQTT/TCP 和 MQTT/SSL 监听器中配置 TCP keep-alive。
- [#10948](https://github.com/emqx/emqx/pull/10948) 在一些 HTTP API 中添加了 live_connections 字段，例如：
  -   `/monitor_current，/monitor_current/nodes/{node}`
  -   `/monitor/nodes/{node}，/monitor`
  -   `/node/{node}，/nodes`
- [#10941](https://github.com/emqx/emqx/pull/10941) 设置 `prometheus.vm_dist_collector=disabled` 且度量指标 `erlang_vm_statistics_run_queues_length_total` 被重命名为`erlang_vm_statistics_run_queues_length`，提高 Prometheus 指标的收集速度。
- [#10985](https://github.com/emqx/emqx/pull/10985) 将 `emqx ctl` 命令的名称从 `cluster_call` 更名为 `conf cluster_sync`。旧命令 `cluster_call` 仍然是一个有效的命令，但不包含在帮助信息中。
- [#10988](https://github.com/emqx/emqx/pull/10988) 改进日志安全性，确保在数据桥接创建失败时敏感数据始终被模糊化。
- [#10926](https://github.com/emqx/emqx/pull/10926) 允许在监听器的状态标志中使用 `enable` 和 "`enabled`。
  在此更改之前，可以通过在 `enabled` 配置上设置 `true` 或 `false` 来启用/禁用监听器。与系统中其他状态标志的命名略有不同。现在，添加了 `enable` 标志作为监听器的别名。
- [#10970](https://github.com/emqx/emqx/pull/10970) 已向 Kafka 生产者桥接添加了一个 query_mode 参数。该参数允许您指定在向 Kafka 发送数据时桥接应该使用异步模式还是同步模式。默认为异步模式。
- [#10676](https://github.com/emqx/emqx/pull/10676) 新增了用于导入/导出配置和用户数据的命令 `emqx ctl export ` 和 `emqx ctl import`， 允许从正在运行的 EMQX 集群中导出配置和内置数据库数据，然后将其导入到相同或另一个正在运行的 EMQX 集群中。
- [#10961](https://github.com/emqx/emqx/pull/10961) 通过允许在配置和 HTTP API 中的 `max_connections` 字段中使用无限大（infinity）作为有效值，为网关监听器添加了对无限制最大连接数的支持。
- [#11019](https://github.com/emqx/emqx/pull/11019) 改进了 JWT 的日志安全性，现在在打印之前将进行模糊化处理。
- [#11034](https://github.com/emqx/emqx/pull/11034) 隐藏 "broker" 配置， 并将 `broker.shared_subscription_strategy` 改为 `mqtt.shared_subscription_strategy` 因为它属于 mqtt 的特性。
- [#11045](https://github.com/emqx/emqx/pull/11045) 监听器认证和分区相关 api 在 `5.1.0`版本中被正式移除。
- [#11062](https://github.com/emqx/emqx/pull/11062) 将 `log.file.to` 更名为 `log.file.path`。
- [#10833](https://github.com/emqx/emqx/pull/10833) 在遥测报告中只包括了已启用的认证器和授权检查器而不是全部的认证器和授权检查器。

### Bug Fixes

- [#11018](https://github.com/emqx/emqx/pull/11018) 修复了 Stomp 网关的多个问题，包括：

  -   修复了关于 is_superuser 无法正常工作的问题。
  -   修复了 mountpoint 在消息发送中没有被移除的问题。
  -   消息或订阅请求失败后，Stomp 客户端应该在回复错误信息后立即断开。

- [#11051](https://github.com/emqx/emqx/pull/11051) 增加了对证书`层级`（监听器 SSL 选项）须为非负整数的验证。

- [#10884](https://github.com/emqx/emqx/pull/10884) 修复了在节点加入集群时尝试获取规则信息或指标可能导致崩溃的问题。

- [#10887](https://github.com/emqx/emqx/pull/10887) 修复了一个潜在问题，即对桥接器的请求可能需要很长时间才能进行重试。
  这只影响低吞吐量的情况，其中缓冲层可能需要很长时间才能检测到连接和驱动程序问题。

- [#10878](https://github.com/emqx/emqx/pull/10878) 已修复了 RabbitMQ 桥接程序中的一个漏洞，该漏洞可能会将密码暴露到日志文件中。

- [#10871](https://github.com/emqx/emqx/pull/10871) 修复了一个问题，即在 CoAP 连接断开后，Dashboard 仍显示连接存在，但删除和消息发布请求不生效。

- [#10880](https://github.com/emqx/emqx/pull/10880) 新增了一个 REST API `POST /clients/kickout/bulk`，用于批量踢出多个客户端。

- [#10913](https://github.com/emqx/emqx/pull/10913) 修复了某个节点离开集群后，其插件状态 REST API 仍会包含集群节点状态问题。

- [#10923](https://github.com/emqx/emqx/pull/10923) 修复了通道信息注册中的竞态条件。
  在此修复之前，当系统负载较重时，可能出现客户端已断开连接（或会话已过期），但仍可在 Dashboard 的客户端页面中找到的情况。其中一个可能的原因是期间发生的竞态条件：连接在通道数据注册过程中被中断。

- [#10930](https://github.com/emqx/emqx/pull/10930) 增加了对持续时间数据类型的 schema 验证，以避免使用无效值。
  在此修复之前，可以在 schema 中使用不合理的值，超出系统限制，从而导致崩溃。

- [#10952](https://github.com/emqx/emqx/pull/10952) 如果设置了`verify = verify_none`，则禁止在监听器 SSL 选项中启用 `fail_if_no_peer_cert`。
  设置 `fail_if_no_peer_cert = true` 和 `verify = verify_none` 会导致连接错误，因为选项不兼容。此修复在创建或更新监听器时验证选项，以避免这些错误。

  注意：应用此修复后，任何具有 `fail_if_no_peer_cert = true` 和 `verify = verify_none` 的旧监听器配置将无法加载，并且必须手动修复。

- [#10951](https://github.com/emqx/emqx/pull/10951) 修复了 MQTT-SN 网关中发布消息时挂载点未生效的问题。

- [#10943](https://github.com/emqx/emqx/pull/10943) 弃用了集群发现的 UDP 组播机制。
  该功能自5.0版本以来一直计划弃用，主要是因为实际生产中缺乏使用。尽管该功能的代码在5.1版本中尚未移除，但文档接口已经被降级处理。

- [#10902](https://github.com/emqx/emqx/pull/10902) 避免从运行较新版本的节点同步 `cluster.hocon` 文件。
  在集群滚动升级期间，如果旧版本节点由于任何原因需要重新启动，如果它从较新版本的节点复制 `cluster.hocon` 文件，可能会导致启动失败。在此修复后，旧版本节点将不会从较新版本的节点复制 `cluster.hocon` 文件，而是使用自己的 `cluster.hocon` 文件进行启动。

- [#10911](https://github.com/emqx/emqx/pull/10911) 修改了当尝试创建一个名称超过255个字节的桥接时出现的错误消息和日志条目的内容，使其更易于理解。

- [#10983](https://github.com/emqx/emqx/pull/10983) 修复了一个问题，即当 MQTT 客户端尝试通过配置为仅使用 TLS v1.3 的监听器进行连接时，无法建立TLS连接。
  问题在于 TLS 连接尝试使用与 TLS v1.3 不兼容的选项。

- [#10977](https://github.com/emqx/emqx/pull/10977) 修复了订阅计数指标更新延迟以及 Stomp 网关中的配置问题。

- [#10950](https://github.com/emqx/emqx/pull/10950) 修复了在 MQTT-SN 网关中使 `enable_qos` 选项无效的问题。

- [#10994](https://github.com/emqx/emqx/pull/10994) 在 HTTP 连接器中，对 `proxy-authorization headers` 进行了屏蔽处理，以防止将机密信息泄露到日志文件中。

- [#10996](https://github.com/emqx/emqx/pull/10996) 对于任何未知的 HTTP/API 请求，默认返回404错误，而不是返回 Dashboard 的 index.html 页面。

- [#11005](https://github.com/emqx/emqx/pull/11005) 修复了在 AuthN HTTP 的跟踪日志中无法正确打印方法字段的问题。

- [#10955](https://github.com/emqx/emqx/pull/10955) 修复了 MQTT-SN 网关中对预定义主题的配置删除无效的问题。

- [#11030](https://github.com/emqx/emqx/pull/11030) 改进了在使用 Listeners HTTP API 时发生验证错误所产生的错误信息。

- [#11033](https://github.com/emqx/emqx/pull/11033) 在 ExProto 网关中，弃用了 `AuthenticateRequest` 中的 `mountpoint` 字段。
  该字段在 e4.x 版本中引入，但实际上，在 e5.0 版本中，我们已经提供了 `gateway.exproto.mountpoint` 进行配置，因此无需通过 Authenticate 请求来覆盖它。

  此外，将 `subscriptions_max`、`inflight_max` 和 `mqueue_max` 的默认值更新为无限大。

- [#11042](https://github.com/emqx/emqx/pull/11042) 修复了当监听器的 max_connections 配置设为字符串时 REST API `GET /listeners` 崩溃的问题。

- [#11028](https://github.com/emqx/emqx/pull/11028) 禁止在监听器配置中同时使用包括 tlsv1.3 但排除 tlsv1.2 的多个 TLS 版本。
  使用具有这种版本差异的 TLS 配置会导致连接错误。此外，删除和记录与所选 TLS 版本不兼容的 TLS 选项。

  注意：应用此修复后，任何包含上述版本差异的旧监听器配置将无法加载，必须手动修复。

- [#11056](https://github.com/emqx/emqx/pull/11056) 修复了新创建的监听器有时无法正确启动的问题。当您删除一个名为 "default" 的系统默认监听器并添加一个新的同名监听器时，它将无法正确启动。

  -   修复了某些节点上的配置失败可能导致 Dashboard 无法使用的错误。

- [#11070](https://github.com/emqx/emqx/pull/11070) 修复了 `cluster.autoclean` 配置项无法生效的问题。

- [#11092](https://github.com/emqx/emqx/pull/11092) and [#11100](https://github.com/emqx/emqx/pull/11100) 修复了当复制节点由于 `mria_lb:core_nodes()` 调用超时而无法连接到核心节点的问题。 相关的 mria pull request：[emqx/mria#143](https://github.com/emqx/mria/pull/143)

## [已知问题](https://github.com/emqx/emqx-docs/blob/release-5.1/en_US/changes/known-issues-5.1.0.md)