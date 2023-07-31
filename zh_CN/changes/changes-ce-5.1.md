# 版本发布

## v5.1.3

### 修复

- [#11306](https://github.com/emqx/emqx/pull/11306) 修复了规则动作指标的不一致性，未计算被丢弃的请求。
- [#11327](https://github.com/emqx/emqx/pull/11327) 更新了 ekka 到版本 0.15.8，更新了 mria 到版本 0.5.10，并更新了 optvar 到版本 1.0.5。 这修复了偶发的断言失败： `{{badmatch,noproc},[{optvar,read,2,[{file,"optvar.erl"},{line,140}]},{optvar,read,1,[{file,"optvar.erl"},{line,124}]},...`
- [#11337](https://github.com/emqx/emqx/pull/11337) 修复了当发布主题重写规则针对带通配符的主题时出现的 HTTP API 错误。现在返回错误码 400（Bad Match），而不是错误码 500（Internal Error）。
- [#11346](https://github.com/emqx/emqx/pull/11346) 更新了 ekka 到版本 0.15.9。 这修复了如果获取锁超时，会出现悬挂的 etcd 锁。
- [#11352](https://github.com/emqx/emqx/pull/11352) 修复了在 Windows 或其他不支持 RocksDB 的平台上启动时出现的崩溃问题 [#11345](https://github.com/emqx/emqx/issues/11345)。

## v5.1.2

### 增强

- [#11124](https://github.com/emqx/emqx/pull/11124) 发布适用于 Amazon Linux 2023 的软件包。
- [#11226](https://github.com/emqx/emqx/pull/11226) 统一监听器开关为 `enable`，同时兼容之前的 `enabled` 设置。
- [#11249](https://github.com/emqx/emqx/pull/11249) 加了支持通过 REST API 设置 License 连接使用配额的告警阈值。
- [#11251](https://github.com/emqx/emqx/pull/11251) 新增 `/cluster/topology` REST API 端点。发送 `GET` 请求到该端点将返回集群拓扑信息，显示 RLOG 核心节点和复制节点之间的连接。
- [#11253](https://github.com/emqx/emqx/pull/11253) Webhook/HTTP 桥接已重构为独立的 Erlang 应用程序。为将来的使用提供了灵活性，并允许将桥接作为独立应用程序运行。
- [#11289](https://github.com/emqx/emqx/pull/11289) 发布适用于 Debian 12 的软件包。
- [#11290](https://github.com/emqx/emqx/pull/11290) `jq` 依赖项更新到版本 0.3.10，其中包括 `oniguruma` 库版本更新到 6.9.8，包含了几个轻微安全修复。
- [#11291](https://github.com/emqx/emqx/pull/11291) 通过将 ekka 到版本 0.15.6， 更新了 RocksDB 版本到 1.8.0-emqx-1。
- [#11236](https://github.com/emqx/emqx/pull/11236) 在 REST API `/clients` 端点中改进客户端查询速度，使用默认参数时性能更好。

### 修复

- [#11065](https://github.com/emqx/emqx/pull/11065) 避免 EMQX 关闭期间记录无关的错误消息。
- [#11077](https://github.com/emqx/emqx/pull/11077) 修复更新绑定时使用非整数端口导致崩溃的问题。
- [#11184](https://github.com/emqx/emqx/pull/11184) 将 `max_packet_size` 的最大配置值设置为协议定义的 256MB。现在该限制得到强制执行，任何大于该值的配置都将导致错误。
- [#11192](https://github.com/emqx/emqx/pull/11192) 修复使用 atom 类型时生成有效的 HOCON 文件。删除 HOCON 文件中不必要的 `"`。
- [#11195](https://github.com/emqx/emqx/pull/11195) 避免 Stomp 网关中通过 HTTP API 或客户端创建重复的订阅。
- [#11206](https://github.com/emqx/emqx/pull/11206) 在连接模式中，将 CoAP 客户端的用户名和密码参数设置为可选。
- [#11208](https://github.com/emqx/emqx/pull/11208) 修复 LwM2M 客户端异常数据统计问题。
- [#11211](https://github.com/emqx/emqx/pull/11211) 修复 REST API 中对于错误的处理。现针对不存在的资源执行 `DELETE` 操作会返回 `404` 错误码。
- [#11214](https://github.com/emqx/emqx/pull/11214) 修复节点配置在加入集群时可能无法正确同步的错误。
- [#11229](https://github.com/emqx/emqx/pull/11229) 修复了通过 `emqx ctl conf load `更改配置后插件无法启动/停止的问题。
- [#11237](https://github.com/emqx/emqx/pull/11237) `/prometheus` API 中的 `headers` 默认值应为映射而不是列表。
- [#11250](https://github.com/emqx/emqx/pull/11250) 修复 WebSocket 包含多个 MQTT 包的情况下，它们的顺序被颠倒的问题。
- [#11271](https://github.com/emqx/emqx/pull/11271) 确保 REST API 和配置中的百分比类型范围为 0% 到 100%。
- [#11272](https://github.com/emqx/emqx/pull/11272) 修复日志中的一个错误，EMQX 接收到异常的 `PUBREL` 包时错误地将 `pubrel` 拼写成 `pubrec`。
- [#11281](https://github.com/emqx/emqx/pull/11281) 恢复对特殊的共享订阅主题前缀  `$queue/` 的支持。
- [#11294](https://github.com/emqx/emqx/pull/11294) 修复 `emqx_ctl cluster join`、`leave` 和 `status` 命令。
- [#11309](https://github.com/emqx/emqx/pull/11309) 改进 EMQX 应用程序的启动顺序，简化构建脚本并改进代码复用。
- [#11322](https://github.com/emqx/emqx/pull/11322) 从 EMQX 备份文件（`emqx ctl import` 命令）中导入其他配置项：
  - rule_engine（之前由于错误未导入）
  - topic_metrics（之前未实现）
  - slow_subs（之前未实现）。

## v5.1.1

### 增强

- [#10667](https://github.com/emqx/emqx/pull/10667) 将 MongoDB 连接器和桥接重构为独立的应用程序，以改进代码结构。
- [#11115](https://github.com/emqx/emqx/pull/11115) 添加信息日志，标示由于生存时间（TTL）过期而丢弃的缓冲消息。
- [#11133](https://github.com/emqx/emqx/pull/11133) 将 `retainer` 配置中的 `deliver_rate` 重命名为 `delivery_rate`。
- [#11137](https://github.com/emqx/emqx/pull/11137) 重构了 Dashboard 监听器配置，使用嵌套的 `ssl_options` 字段来配置 SSL 设置。
- [#11138](https://github.com/emqx/emqx/pull/11138) 将 k8s `api_server` 默认值从 `http://127.0.0.1:9091` 更改为 `https://kubernetes.default.svc:443`。
  - 当 `discovery_strategy=static` 时，`emqx_ctl conf show cluster` 不再显示不相关的配置项。 与`etcd/k8s/dns` 相关的配置信息将不再显示。
  - 从 `emqx_ctl conf show_keys` 中删除了 `zones`（已弃用的配置键）。
- [#11165](https://github.com/emqx/emqx/pull/11165) 从 `swagger.json` 中删除了 `/configs/limiter` API。仅删除了 API 文档，`/configs/limiter` API 的功能未更改。
- [#11166](https://github.com/emqx/emqx/pull/11166) 在规则引擎中添加了 3 个随机 SQL 函数：
  - `random()`: 生成介于 0 到 1 之间的随机数（0.0 =< X < 1.0）。
  - `uuid_v4()`: 生成随机 UUID（版本 4）字符串。
  - `uuid_v4_no_hyphen()`: 生成无连字符的随机 UUID（版本 4）字符串。
- [#11180](https://github.com/emqx/emqx/pull/11180) 添加了新的配置 API `/configs`(GET/PUT)，支持重新加载 hocon 格式配置文件。
- [#11020](https://github.com/emqx/emqx/pull/11020) 升级了 emqtt 依赖项，以避免调试日志中的敏感数据泄漏。
- [#11135](https://github.com/emqx/emqx/pull/11135) 改进了规则引擎中的时间偏移解析器，并返回统一的错误代码。

### 修复

- [#11004](https://github.com/emqx/emqx/pull/11004) 主题重写不再允许目标主题中使用通配符。

- [#11026](https://github.com/emqx/emqx/pull/11026) 修复规则引擎中 `div` 和 `mod` 运算使用的一致性问题。之前，`div` 运算只能用作中缀运算，`mod` 只能通过函数调用应用。现在，`div` 和 `mod` 都可以通过函数调用和中缀语法使用。

- [#11037](https://github.com/emqx/emqx/pull/11037) 启动 HTTP 连接器时，如果系统无法连接到远程目标系统，EMQX 现在会返回描述性错误。

- [#11039](https://github.com/emqx/emqx/pull/11039) 修复 Redis 连接器的数据库验证问题。之前，负数被接受作为有效的数据库。

- [#11074](https://github.com/emqx/emqx/pull/11074) 修复了有关 MQTT-5.0 [MQTT-3.8.3-4] 协议内容的问题。

- [#11094](https://github.com/emqx/emqx/pull/11094) 修复了 Kafka 桥接生产者模式在重新连接时无法报告连接错误的问题。

- [#11103](https://github.com/emqx/emqx/pull/11103) 更新了 `erlcloud` 依赖项。

- [#11106](https://github.com/emqx/emqx/pull/11106) 添加了对桥接资源 `worker_pool_size` 的最大数量的验证。现在最大数量为 1024，以避免因不合理数量的工作进程导致大内存消耗。

- [#11118](https://github.com/emqx/emqx/pull/11118) 确保 REST API 响应中的验证错误信息更加明确。现在，如果有超出范围的错误，将呈现为`{"value": 42, "reason": {"expected": "1..10"}, ...}`，替换了先前使用的 `expected_type`，改为使用 `expected`。

- [#11126](https://github.com/emqx/emqx/pull/11126) 修复了规则下有异步模式的桥接时，规则的失败指标计数问题。

- [#11134](https://github.com/emqx/emqx/pull/11134) 修复了日志中敏感字段大写 `authorization` header 的值不被混淆的问题。

- [#11139](https://github.com/emqx/emqx/pull/11139) Redis 连接器已重构为独立的 Erlang 应用程序，以改进代码结构。

- [#11145](https://github.com/emqx/emqx/pull/11145) 在 Ekka 和 Mria 中添加了几个修复和改进。

  Ekka:

  - 改进集群发现日志消息，以一致地描述实际事件。[Ekka PR](https://github.com/emqx/ekka/pull/204)
  - 删除了弃用的集群自动清理配置参数（已移动到 Mria）。[Ekka PR](https://github.com/emqx/ekka/pull/203)

  Mria:

  - 现在 ping 仅在 replicant 节点上运行。之前，`mria_lb` 尝试对已停止和正在运行的 replicant 节点进行 ping，可能导致超时错误。[Mria PR](https://github.com/emqx/mria/pull/146)
  - 在复制 `$mria_rlog_sync` 表时使用 `null_copies` 存储。该修复现在对 EMQX 没有影响，因为 `$mria_rlog_sync` 仅在 `mria:sync_transaction/2,3,4` 中使用，而 EMQX 不使用该函数。[Mria PR](https://github.com/emqx/mria/pull/144)

- [#11148](https://github.com/emqx/emqx/pull/11148) 修复当一个节点离开集群时，其他节点仍然尝试将配置更新操作同步到其中的问题。

- [#11150](https://github.com/emqx/emqx/pull/11150) 在启动 `emqx_psk` 应用程序时等待 Mria 表，确保即使没有初始化 PSK 文件，PSK 数据也能同步到 replicant 节点。

- [#11151](https://github.com/emqx/emqx/pull/11151) MySQL 连接器已重构为独立的 Erlang 应用程序，以改进代码结构。

- [#11158](https://github.com/emqx/emqx/pull/11158) 在 mnesia 后端 retainer 启动时等待 Mria 表，以避免在加入集群时 retainer 出现可能的错误。

- [#11164](https://github.com/emqx/emqx/pull/11164) 重新支持从规则动作消息中提取数据的嵌套占位符（例如：`${payload.a.b.c}`），无需先调用 `json_decode(payload)`。

- [#11172](https://github.com/emqx/emqx/pull/11172) 修复规则引擎 SQL 中 `payload` 字段在以下情况下被重复的问题：

  - 使用 `foreach` 子句且没有 `as` 子表达式时，并选择所有字段（使用 `*` 或省略 `do` 子表达式）。例如：

    `FOREACH payload.sensors FROM "t/#"`

  - 选择 `payload` 字段和所有字段。例如：

    `SELECT payload.sensors, * FROM "t/#"`

- [#11174](https://github.com/emqx/emqx/pull/11174) 修复来自 MQTT 桥接 ingress 的 `server` 键的编码问题。

  在修复之前，它被编码为一系列整数，对应于服务器字符串的 ASCII 字符。

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

### 修复

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