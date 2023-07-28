# 版本发布

## 增强

- [#10667](https://github.com/emqx/emqx/pull/10667) 将 MongoDB 连接器和桥接重构为单独的应用程序，以改进代码结构。

- [#11115](https://github.com/emqx/emqx/pull/11115) 添加了信息日志，标示由于存活时间（TTL）过期而丢弃的缓冲消息。

- [#11133](https://github.com/emqx/emqx/pull/11133) 在 `retainer` 的配置中将 `deliver_rate` 重命名为 `delivery_rate`，兼容之前的 `deliver_rate`。

- [#11137](https://github.com/emqx/emqx/pull/11137) 重构了 Dashboard 监听器配置，使用嵌套的 `ssl_options` 字段来进行 SSL 设置。

- [#11138](https://github.com/emqx/emqx/pull/11138)  将k8s `api_server `的默认值从 `http://127.0.0.1:9091 ` 更改为 `https://kubernetes.default.svc:443`。

  - 当 `discovery_strategy=static` 时，`emqx_ctl conf show cluster` 不再显示不相关的配置项。 与`etcd/k8s/dns` 相关的配置信息将不再显示。
  - 从 `emqx_ctl conf show_keys` 中删除了 `zones`（已弃用的配置键）。

- [#11165](https://github.com/emqx/emqx/pull/11165) 从 `swagger.json` 中删除了 `/configs/limiter` API 文档，API 仍然保留。

- [#11166](https://github.com/emqx/emqx/pull/11166) 在规则引擎 SQL 中添加了 3 个随机函数：

  - `random()`: 生成 0 到 1 之间的随机数（0.0 =< X < 1.0）。
  - `uuid_v4()`: 生成随机 UUID（版本4）字符串。
  - `uuid_v4_no_hyphen()`: 生成无连字符的随机 UUID（版本4）字符串。

- [#11180](https://github.com/emqx/emqx/pull/11180) 添加了一个新的配置 API `/configs`（GET/PUT），支持重新加载 HOCON 格式的配置文件。

- [#11226](https://github.com/emqx/emqx/pull/11226) 统一监听器开关配置项为 `enable`，同时兼容之前的`enabled`。

- [#11249](https://github.com/emqx/emqx/pull/11249) 添加了支持通过 REST API 设置 License 连接使用配额的告警阈值。

- [#11251](https://github.com/emqx/emqx/pull/11251) 添加了 `GET /cluster/topology` REST API，用来返回集群拓扑，显示 RLOG 核心节点和复制节点之间的连接。

- [#11253](https://github.com/emqx/emqx/pull/11253) 将 Webhook/HTTP 桥接重构为单独的 Erlang 应用，为将来的使用提供了灵活性，并允许将桥接作为独立应用程序运行。

- [#11079](https://github.com/emqx/emqx/pull/11079) Kafka 桥接，生产者模式下为消息添加了自定义 headers 支持。

- [#11132](https://github.com/emqx/emqx/pull/11132) MQTT 授权检查添加了 QoS 级别和保留消息条件，现在 EMQX 可以验证客户端是否有权限使用指定的 QoS 级别进行发布/订阅，以及有权限发布保留消息。

- [#11207](https://github.com/emqx/emqx/pull/11207) 更新了多个数据桥接的驱动版本，以增强安全性并确保敏感数据不会泄露。包括：

  - TDengine
  - MongoDB
  - MySQL
  - Clickhouse

- [#11241](https://github.com/emqx/emqx/pull/11241) 将 Schema Registry 重构为单独的 Erlang 应用以提供灵活性。

- [#11020](https://github.com/emqx/emqx/pull/11020) 升级了 emqtt 依赖项，以防止调试日志中的敏感数据泄漏。

- [#11135](https://github.com/emqx/emqx/pull/11135) 改进了规则引擎中的时间偏移解析器，并返回统一的错误代码。

- [#11236](https://github.com/emqx/emqx/pull/11236) 改进了默认参数下 `GET /clients` REST API 客户端查询的速度。

## 修复

- [#11004](https://github.com/emqx/emqx/pull/11004) 主题重写不再允许在目标主题中使用通配符。

- [#11026](https://github.com/emqx/emqx/pull/11026) 解决了规则引擎中 `div` 和 `mod` 操作使用的一致性问题。之前，`div` 操作只能作为中缀操作使用，`mod ` 只能通过函数调用使用用。现在，`div ` 和 `mod` 都可以通过函数调用语法和中缀语法使用。

- [#11037](https://github.com/emqx/emqx/pull/11037) 启动 HTTP 连接器时，如果系统无法连接到远程目标系统，EMQX 现在会返回一个描述性错误。

- [#11039](https://github.com/emqx/emqx/pull/11039) 修复了 Redis 连接器的数据库验证问题。之前，负数被接受为有效的数据库。

- [#11074](https://github.com/emqx/emqx/pull/11074) 修复了有关 MQTT-5.0 [MQTT-3.8.3-4] 协议内容的问题。

- [#11077](https://github.com/emqx/emqx/pull/11077) 修复了更新监听器时使用非整数的端口可能发生崩溃的问题。

- [#11094](https://github.com/emqx/emqx/pull/11094) 修复了 Kafka 桥接生产者模式在重新连接时无法报告连接错误的问题。

- [#11103](https://github.com/emqx/emqx/pull/11103) 更新了 `erlcloud` 依赖项。

- [#11106](https://github.com/emqx/emqx/pull/11106) 添加了对桥接资源 `worker_pool_size` 的最大数量的验证。现在最大数量为 1024，以避免因不合理数量的工作进程导致大内存消耗。

- [#11118](https://github.com/emqx/emqx/pull/11118) 确保 REST API 响应中的验证错误信息更加明确。现在，如果有超出范围的错误，将呈现为`{"value": 42, "reason": {"expected": "1..10"}, ...}`，替换了先前使用的 `expected_type`，改为使用 `expected`。

- [#11126](https://github.com/emqx/emqx/pull/11126) 修复了规则下有异步模式的桥接时，规则的失败指标计数问题。

- [#11134](https://github.com/emqx/emqx/pull/11134) 修复了日志中敏感字段大写 `authorization` header 的值不被混淆的问题。

- [#11139](https://github.com/emqx/emqx/pull/11139) 将 Redis 桥接器重构为单独的 Erlang 应用，以改进代码结构。

- [#11145](https://github.com/emqx/emqx/pull/11145) 在 Ekka 和 Mria 中进行了几处修复和改进。

  Ekka:

  - 改进了集群发现日志消息，以一致地描述实际事件。 [Ekka PR](https://github.com/emqx/ekka/pull/204)
  - 删除了弃用的集群自动清理配置参数（已移动到 Mria）。[Ekka PR](https://github.com/emqx/ekka/pull/203)

  Mria:

  - 现在 Ping 只在复制节点上运行。之前，`mria_lb `尝试同时 ping 停止和运行中的复制节点，可能导致超时错误。 [Mria PR](https://github.com/emqx/mria/pull/146)
  - 在复制 `$mria_rlog_sync` 表时使用 `null_copies` 存储。 此修复对 EMQX 目前没有影响，因为 `$mria_rlog_sync`仅在 `mria:sync_transaction/2,3,4` 中使用，而这在 EMQX 中未被使用。 [Mria PR](https://github.com/emqx/mria/pull/144)

- [#11148](https://github.com/emqx/emqx/pull/11148) 修复当一个节点离开集群时，其他节点仍然尝试将配置更新操作同步到其中的问题。

- [#11150](https://github.com/emqx/emqx/pull/11150) 在启动 emqx_psk 应用程序时等待 Mria 表，确保 PSK 数据在复制节点上同步，即使它们没有初始化的 PSK 文件。

- [#11151](https://github.com/emqx/emqx/pull/11151) 将 MySQL 桥接重构为单独的 Erlang 应用，以改进代码结构。

- [#11158](https://github.com/emqx/emqx/pull/11158) 在 Mnesia 后端的 retainer 启动时等待 Mria  表，避免加入集群时出现错误。

- [#11162](https://github.com/emqx/emqx/pull/11162) 修复 Webhook 桥接异步模式下，4XX 和 5XX HTTP 状态码被统计为成功指标的问题。

- [#11164](https://github.com/emqx/emqx/pull/11164) 重新引入对嵌套占位符（例如：`${payload.a.b.c}`）的支持，对于 JSON 格式的 Payload，从规则动作与数据桥接中提取数据时无需先调用 `json_decode(payload)`。

- [#11172](https://github.com/emqx/emqx/pull/11172) 修复了特定情况下 `payload` 重复的问题：

  - 使用不带 `as` 子表达式的 `foreach` 语句，并选择所有字段，例如：

    ```
    FOREACH payload.sensors FROM "t/#"
    ```
  
  - 在选择 `payload` 字段和所有字段的情况下，例如：

    ```
    SELECT payload.sensors, * FROM "t/#"
    ```

- [#11174](https://github.com/emqx/emqx/pull/11174) 修复了 MQTT 桥接 Ingress 的 `server` 字段的编码问题。在修复之前，它被编码为 ASCII 字符对应的整数列表。

- [#11184](https://github.com/emqx/emqx/pull/11184) 配置项 `mqtt.max_packet_size` 最大值设置为协议定义 256MB。

- [#11192](https://github.com/emqx/emqx/pull/11192) 修复了在使用 atom 类型生成有效 HOCON 文件时，从 HOCON 文件中删除不必要的 `"`。

- [#11195](https://github.com/emqx/emqx/pull/11195) 修复了 REST API 可以为 Stomp 网关指定客户端创建重复订阅的问题。

- [#11206](https://github.com/emqx/emqx/pull/11206) 连接到 CoAP 网关时，不再要求客户端的 `username` 和 `password` 参数必填。

- [#11208](https://github.com/emqx/emqx/pull/11208) 修复了 LwM2M 客户端异常数据统计的问题。

- [#11211](https://github.com/emqx/emqx/pull/11211) `DELETE` 操作操作不存在的资源时，一致性地返回 `404` 状态码。

- [#11214](https://github.com/emqx/emqx/pull/11214) 修复了节点配置可能在加入集群时无法正确同步的问题。

- [#11229](https://github.com/emqx/emqx/pull/11229) 修复了在通过 `emqx ctl conf load` 更改配置后，插件无法启动/停止的问题。

- [#11237](https://github.com/emqx/emqx/pull/11237)  `/prometheus` API中 `headers` 的默认值应为对象而不是列表。

- [#11250](https://github.com/emqx/emqx/pull/11250) 修复了 WebSocket 数据包中包含多个 MQTT 数据包时其顺序被颠倒的问题。


- [#11271](https://github.com/emqx/emqx/pull/11271) 确保 REST API 与配置中百分比类型的范围为 0% 到 100%。比如 `sysom.os.sysmem_high_watermark=101%` 现在是无效的设置。

- [#11272](https://github.com/emqx/emqx/pull/11272) 修复了日志中的拼写错误，错误地将异常 `PUBREL` 数据包称为 `pubrec`。

- [#11281](https://github.com/emqx/emqx/pull/11281) 恢复对特殊的 `$queue/` 共享订阅主题前缀的支持。

- [#11294](https://github.com/emqx/emqx/pull/11294) 修复了`emqx_ctl cluster join`，`leave `和 `status `命令。

- [#11306](https://github.com/emqx/emqx/pull/11306) 修复了规则动作指标不一致的问题，未计算丢弃的请求。

- [#11309](https://github.com/emqx/emqx/pull/11309) 改进了 EMQX 应用程序的启动顺序。简化了构建脚本，并改进了代码重用。

- [#11322](https://github.com/emqx/emqx/pull/11322) 支持从 EMQX 备份文件（`emqx ctl import `命令）中导入了其他配置：

  - rule_engine（以前由于错误未导入）
  - topic_metrics（以前未实现）
  - slow_subs（以前未实现）。

- [#10645](https://github.com/emqx/emqx/pull/10645) 更改了对 Oracle、PostgreSQL、MySQL 和 Kafka 生产者数据桥接的健康检查，以确保目标表/主题存在。

- [#11107](https://github.com/emqx/emqx/pull/11107) 现在在测试 MongoDB 桥接时返回健康检查失败原因。

- [#11139](https://github.com/emqx/emqx/pull/11139) 将 Redis 桥接重构为单独的 Erlang 应用，以改进代码结构和易维护性。

- [#11151](https://github.com/emqx/emqx/pull/11151) 将 MySQL 桥接重构为单独的 Erlang 应用，以改进代码结构和易维护性。

- [#11163](https://github.com/emqx/emqx/pull/11163) 隐藏 MondoDB 桥接中的 `topology.pool_size`，并将其固定为 1 以避免混淆。

- [#11175](https://github.com/emqx/emqx/pull/11175) 现在，REST API 创建 MySQL 连接时如果使用不存在的主机名，将返回 400 错误而不是 503 错误。

- [#11198](https://github.com/emqx/emqx/pull/11198) 修复了复制节点上全局再平衡状态评估策略。之前，`/api/v5/load_rebalance/global_status` API method 可能在由复制节点处理时返回不完整的结果。

- [#11223](https://github.com/emqx/emqx/pull/11223) InfluxDB 桥接写入配置中，某个字段混用小数和整数时可能会导致 Influx Line Protocol 序列化失败，并且无法写入 InfluxDB 桥接（当小数位为 0 时小数点会被忽略，InfluxDB 误以为其是整数）。

  另请参阅：[InfluxDB v2.7 Line-Protocol](https://docs.influxdata.com/influxdb/v2.7/reference/syntax/line-protocol/#float)。

- [#11225](https://github.com/emqx/emqx/pull/11225) 修复了PostgreSQL/Timescale/MatrixDB 桥接没有验证 `username` 可能为空的问题。

- [#11242](https://github.com/emqx/emqx/pull/11242) 当节点加入集群时重新启动 emqx_ee_schema_registry。因为 emqx_ee_schema_registry 使用 Mria 表，所以节点加入集群需要重新启动此应用程序，以启动相关的 Mria 分片进程，确保在核心/复制节点模式下正确工作。

- [#11266](https://github.com/emqx/emqx/pull/11266) 修复和改进对 TDengine `insert `语法的支持：

  1. 支持在模板中插入多表。

     例如：

     `insert into table_1 values (${ts}, ${val}) into table_2 values (${ts}, ${val})`

  2. 支持在模版中混合前缀/后缀和占位符。

     例如：

     `insert into table_${topic} values (${ts}, '${id}', '${topic}')`

     注意：这是一个破坏性变更。此前，字符类型的占位符会被自动转义加上单引号，而现在需要手动加上单引号。

     例如：

     `insert into table values (${ts}, '${a_string}')`

- [#11307](https://github.com/emqx/emqx/pull/11307) 在 Oracle 桥接中检查表是否存在时返回更友好的错误信息。

- [#11316](https://github.com/emqx/emqx/pull/11316) 修复了在 Oracle 桥接中未考虑 Pool Size 值的问题。

- [#11326](https://github.com/emqx/emqx/pull/11326) 修复了在 Oracle 桥接中返回错误检查的问题。

### [已知问题](https://github.com/emqx/emqx-docs/blob/release-5.1/en_US/changes/known-issues-5.1.1.md)

## e5.1.0

### 增强

-   [#11035](https://github.com/emqx/emqx/pull/11035) 升级 Cassandra 驱动以避免用户名密码在数据桥接日志中泄漏。
-   [#10584](https://github.com/emqx/emqx/pull/10584) 为 SSL 通信增加日志等级配置。
-   [#10678](https://github.com/emqx/emqx/pull/10678) 优化计数器递增调用以避免在递增为0的情况下计数。
-   [#10690](https://github.com/emqx/emqx/pull/10690) 为 Webhook 桥接添加了重试机制，旨在尝试提高吞吐量。
    这个优化让客户端可以在请求失败时进行重试，而不会阻塞缓冲层，从而在高消息传输率的情况下提高吞吐量。
-   [#10702](https://github.com/emqx/emqx/pull/10702) 引入了一个更直观的配置选项 `keepalive_multiplier`，并废弃了旧的 `keepalive_backoff` 配置。在改进之后，EMQX 通过将"客户端请求的 Keepalive 间隔"与 `keepalive_multiplier` 相乘来周期性地检查客户端的 Keepalive 超时状态。
-   [#10698](https://github.com/emqx/emqx/pull/10698) 优化了在运行时访问配置的内存使用。
-   [#10778](https://github.com/emqx/emqx/pull/10778) 重构 Pulsar 生产者桥接，避免在初始化阶段出现崩溃时资源泄漏。
-   [#10813](https://github.com/emqx/emqx/pull/10813) 重构了Kafka 生产者和消费者桥接，避免在初始化阶段出现崩溃时资源泄漏。
-   [#10858](https://github.com/emqx/emqx/pull/10858) 规则引擎 SQL 语言新增了一个实用函数 timezone_to_offset_seconds/1。该函数将时区字符串（例如"+02:00"、"Z"和"local"）转换为相应的偏移秒数。
-   [#10841](https://github.com/emqx/emqx/pull/10841) 为 Kafka 和 Pulsar 生产者桥接添加了参数校验，以确保在选择了 `key_dispatch` 策略时消息键参数不为空。
-   [#10754](https://github.com/emqx/emqx/pull/10754) 对 MQTT 桥接进行了增强，利用连接池和可用的并行性，大大提高了吞吐量。因此，单个 MQTT 桥接现在使用一组 `clientid` 连接到远程代理。
-   [#10782](https://github.com/emqx/emqx/pull/10782) 在保留器（retainer）配置中添加了一个新的 `deliver_rate` 选项，它可以限制保留器中每个会话的最大传递速率。
-   [#10877](https://github.com/emqx/emqx/pull/10877) 升级 RocketMQ 驱动程序以增强处理敏感数据的安全性。
-   [#10598](https://github.com/emqx/emqx/pull/10598) 在 ExProto 中提供了一种 Unary 类型的回调方法，以避免可能的消息乱序问题。
-   [#10895](https://github.com/emqx/emqx/pull/10895) 重构了大部分桥接，避免在初始化阶段出现崩溃时资源泄漏。
-   [#10790](https://github.com/emqx/emqx/pull/10790) 通过优化配置读取机制减少读取配置的开销。
-   [#10892](https://github.com/emqx/emqx/pull/10892) 在创建 Oracle 数据库桥接时要求设置 SID 或服务名称。
-   [#10910](https://github.com/emqx/emqx/pull/10910) 数据桥接资源选项 `auto_restart_interval` 已被弃用，改为使用 `health_check_interval`，而 `request_timeout` 则被重命名为 `request_ttl`。此外，默认的 `request_ttl` 值从 15 秒增加到了 45 秒。
    之前同时存在 `auto_restart_interval` 和 `health_check_interval` 会导致混淆，因为这两个参数都会影响数据桥接在故障下的恢复。这两个参数的配置不一致可能导致消息过期而无法重试。现在，`health_check_interval` 用于控制进行健康检查的间隔，健康检查会将数据桥接转换为 `disconnected` 或 `connecting` 状态，也可以让数据桥接从 `disconnected` 状态中进行恢复。
-   [#10929](https://github.com/emqx/emqx/pull/10929) 升级 Erland/OTP 到 25.3.2-1。
-   [#10909](https://github.com/emqx/emqx/pull/10909) 移除了网关已弃用的 HTTP API。
-   [#10908](https://github.com/emqx/emqx/pull/10908) 重构了 RocketMQ 桥接，避免在初始化阶段出现崩溃时资源泄漏。
-   [#10924](https://github.com/emqx/emqx/pull/10924) 重构了 Influxdb 桥接，避免在初始化阶段出现崩溃时资源泄漏。
-   [#10944](https://github.com/emqx/emqx/pull/10944) 改进了 GCP PubSub 桥接，以避免在重启节点时可能出现消息发送失败的潜在问题。
-   [#10933](https://github.com/emqx/emqx/pull/10933) 支持在 MQTT/TCP 和 MQTT/SSL 监听器中配置 TCP keep-alive。
-   [#10948](https://github.com/emqx/emqx/pull/10948) 在一些 HTTP API 中添加了 live_connections 字段，例如：
    -   `/monitor_current，/monitor_current/nodes/{node}`
    -   `/monitor/nodes/{node}，/monitor`
    -   `/node/{node}，/nodes`
-   [#10941](https://github.com/emqx/emqx/pull/10941) 设置 `prometheus.vm_dist_collector=disabled` 且度量指标 `erlang_vm_statistics_run_queues_length_total` 被重命名为`erlang_vm_statistics_run_queues_length`，提高 Prometheus 指标的收集速度。
-   [#10985](https://github.com/emqx/emqx/pull/10985) 将 `emqx ctl` 命令的名称从 `cluster_call` 更名为 `conf cluster_sync`。旧命令 `cluster_call` 仍然是一个有效的命令，但不包含在帮助信息中。
-   [#10988](https://github.com/emqx/emqx/pull/10988) 改进日志安全性，确保在数据桥接创建失败时敏感数据始终被模糊化。
-   [#10926](https://github.com/emqx/emqx/pull/10926) 允许在监听器的状态标志中使用 `enable` 和 "`enabled`。
    在此更改之前，可以通过在 `enabled` 配置上设置 `true` 或 `false` 来启用/禁用监听器。与系统中其他状态标志的命名略有不同。现在，添加了 `enable` 标志作为监听器的别名。
-   [#10970](https://github.com/emqx/emqx/pull/10970) 已向 Kafka 生产者桥接添加了一个 query_mode 参数。该参数允许您指定在向 Kafka 发送数据时桥接应该使用异步模式还是同步模式。默认为异步模式。
-   [#10676](https://github.com/emqx/emqx/pull/10676) 新增了用于导入/导出配置和用户数据的命令 `emqx ctl export ` 和 `emqx ctl import`， 允许从正在运行的 EMQX 集群中导出配置和内置数据库数据，然后将其导入到相同或另一个正在运行的 EMQX 集群中。
-   [#11003](https://github.com/emqx/emqx/pull/11003) 在 Kafka 数据桥接中添加一个配置 TCP keepalive 的选项。
-   [#10961](https://github.com/emqx/emqx/pull/10961) 通过允许在配置和 HTTP API 中的 `max_connections` 字段中使用无限大（infinity）作为有效值，为网关监听器添加了对无限制最大连接数的支持。
-   [#11019](https://github.com/emqx/emqx/pull/11019) 改进了 JWT 的日志安全性，现在在打印之前将进行模糊化处理。
-   [#11024](https://github.com/emqx/emqx/pull/11024) 添加了一个小的改进，以减少在创建/更新 Pulsar 生产者桥接时看到`connecting` 状态的机会。
-   [#11034](https://github.com/emqx/emqx/pull/11034) 隐藏 "broker" 配置， 并将 `broker.shared_subscription_strategy` 改为 `mqtt.shared_subscription_strategy` 因为它属于 mqtt 的特性。
-   [#11045](https://github.com/emqx/emqx/pull/11045) 监听器认证和分区相关 api 在 `5.1.0`版本中被正式移除。
-   [#11062](https://github.com/emqx/emqx/pull/11062) 将 `log.file.to` 更名为 `log.file.path`。

### 修复

-   [#11018](https://github.com/emqx/emqx/pull/11018) 修复了 Stomp 网关的多个问题，包括：
    -   修复了关于 is_superuser 无法正常工作的问题。
    -   修复了 mountpoint 在消息发送中没有被移除的问题。
    -   消息或订阅请求失败后，Stomp 客户端应该在回复错误信息后立即断开。
-   [#11051](https://github.com/emqx/emqx/pull/11051) 增加了对证书`层级`（监听器 SSL 选项）须为非负整数的验证。
-   [#10563](https://github.com/emqx/emqx/pull/10563) 修复了订阅时 no_local flag 无法正常工作的问题。
-   [#10653](https://github.com/emqx/emqx/pull/10653) 将网关认证的 TLS 证书和密钥保存到数据目录以修复内存泄漏问题。
-   [#10682](https://github.com/emqx/emqx/pull/10682) 修正了在会话创建时为遗嘱消息赋予时间戳这个错误，现在该时间戳为会话断开时间。
-   [#10701](https://github.com/emqx/emqx/pull/10701) Amazon Linux 2 的 EMQX RPM 软件包不支持 TLS v1.3，因为它是使用内置 openssl 1.0 的 Erlang/OTP 构建的。
-   [#10677](https://github.com/emqx/emqx/pull/10677) 修复了规则 API 中的问题：当尝试删除不存在的规则时，会响应 404 HTTP 错误代码。
-   [#10715](https://github.com/emqx/emqx/pull/10715) 支持在客户端连接钩子函数(client.connected hooks)中获取客户端证书。之前，为了减少内存消耗在建立连接后移除了该数据。
-   [#10737](https://github.com/emqx/emqx/pull/10737) 修复了网关的 HTTP API 接口无法处理包含特殊字符的 ClientID 的问题，例如：`!@#$%^&*()_+{}:"<>?/`。
-   [#10809](https://github.com/emqx/emqx/pull/10809) 解决节点关闭或重启时出现的 `** ERROR ** Mnesia post_commit hook failed: error:badarg` 错误信息。Mria pull request: [https://github.com/emqx/mria/pull/142](https://github.com/emqx/mria/pull/142)
-   [#10807](https://github.com/emqx/emqx/pull/10807) 在 debug 级别下，不再输出 license 检查相关的日志，该日志产生过于频繁，可能会干扰日志记录。
-   [#10818](https://github.com/emqx/emqx/pull/10818) 修复了 `emqx_ctl traces` 命令错误，其中 `emqx_mgmt_cli` 模块中的 `traces start `命令在某些过滤器下无法正常工作。
-   [#10600](https://github.com/emqx/emqx/pull/10600) 删除了 emqx_statsd 应用。
-   [#10820](https://github.com/emqx/emqx/pull/10820) 修复了集群更新 license 后，新加入的节点不会应用新 license 而是继续使用旧 license 的问题。
    有时新节点必须使用过时的 license。例如，在 license 过期后使用 emqx-operator 进行部署，并且需要扩展规模。此时，集群的 license 已经通过 API/CLI 更新，但新节点不会使用它。
-   [#10851](https://github.com/emqx/emqx/pull/10851) 在错误的 API 日志中对敏感数据进行了混淆处理。
-   [#10884](https://github.com/emqx/emqx/pull/10884) 修复了在节点加入集群时尝试获取规则信息或指标可能导致崩溃的问题。
-   [#10887](https://github.com/emqx/emqx/pull/10887) 修复了一个潜在问题，即对桥接器的请求可能需要很长时间才能进行重试。
    这只影响低吞吐量的情况，其中缓冲层可能需要很长时间才能检测到连接和驱动程序问题。
-   [#10878](https://github.com/emqx/emqx/pull/10878) 已修复了 RabbitMQ 桥接程序中的一个漏洞，该漏洞可能会将密码暴露到日志文件中。
-   [#10871](https://github.com/emqx/emqx/pull/10871) 修复了一个问题，即在 CoAP 连接断开后，Dashboard 仍显示连接存在，但删除和消息发布请求不生效。
-   [#10880](https://github.com/emqx/emqx/pull/10880) 新增了一个 REST API `POST /clients/kickout/bulk`，用于批量踢出多个客户端。
-   [#10913](https://github.com/emqx/emqx/pull/10913) 修复了某个节点离开集群后，其插件状态 REST API 仍会包含集群节点状态问题。
-   [#10923](https://github.com/emqx/emqx/pull/10923) 修复了通道信息注册中的竞态条件。
    在此修复之前，当系统负载较重时，可能出现客户端已断开连接（或会话已过期），但仍可在 Dashboard 的客户端页面中找到的情况。其中一个可能的原因是期间发生的竞态条件：连接在通道数据注册过程中被中断。
-   [#10930](https://github.com/emqx/emqx/pull/10930) 增加了对持续时间数据类型的 schema 验证，以避免使用无效值。
    在此修复之前，可以在 schema 中使用不合理的值，超出系统限制，从而导致崩溃。
-   [#10952](https://github.com/emqx/emqx/pull/10952) 如果设置了`verify = verify_none`，则禁止在监听器 SSL 选项中启用 `fail_if_no_peer_cert`。
    设置 `fail_if_no_peer_cert = true` 和 `verify = verify_none` 会导致连接错误，因为选项不兼容。此修复在创建或更新监听器时验证选项，以避免这些错误。

    注意：应用此修复后，任何具有 `fail_if_no_peer_cert = true` 和 `verify = verify_none` 的旧监听器配置将无法加载，并且必须手动修复。
-   [#10951](https://github.com/emqx/emqx/pull/10951) 修复了 MQTT-SN 网关中发布消息时挂载点未生效的问题。
-   [#10943](https://github.com/emqx/emqx/pull/10943) 弃用了集群发现的 UDP 组播机制。
    该功能自5.0版本以来一直计划弃用，主要是因为实际生产中缺乏使用。尽管该功能的代码在5.1版本中尚未移除，但文档接口已经被降级处理。
-   [#10902](https://github.com/emqx/emqx/pull/10902) 避免从运行较新版本的节点同步 `cluster.hocon` 文件。
    在集群滚动升级期间，如果旧版本节点由于任何原因需要重新启动，如果它从较新版本的节点复制 `cluster.hocon` 文件，可能会导致启动失败。在此修复后，旧版本节点将不会从较新版本的节点复制 `cluster.hocon` 文件，而是使用自己的 `cluster.hocon` 文件进行启动。
-   [#10967](https://github.com/emqx/emqx/pull/10967) 修复了重平衡 API 中错误消息的格式问题：之前它们可能以不清晰的 Erlang 内部结构转储的形式显示。
    在节点疏散的 CLI 和 API 中添加了 `wait_health_check` 选项。这是一个时间间隔，节点在此期间报告为"不健康状态"，但不会开始实际的疏散操作。我们需要这个选项来允许负载均衡器（如果有）将已疏散的节点从负载均衡中移除，并且不将（重新）连接的客户端转发到已疏散的节点。
-   [#10911](https://github.com/emqx/emqx/pull/10911) 修改了当尝试创建一个名称超过255个字节的桥接时出现的错误消息和日志条目的内容，使其更易于理解。
-   [#10983](https://github.com/emqx/emqx/pull/10983) 修复了一个问题，即当 MQTT 客户端尝试通过配置为仅使用 TLS v1.3 的监听器进行连接时，无法建立TLS连接。
    问题在于 TLS 连接尝试使用与 TLS v1.3 不兼容的选项。
-   [#10977](https://github.com/emqx/emqx/pull/10977) 修复了订阅计数指标更新延迟以及 Stomp 网关中的配置问题。
-   [#10950](https://github.com/emqx/emqx/pull/10950) 修复了在 MQTT-SN 网关中使 `enable_qos` 选项无效的问题。
-   [#10999](https://github.com/emqx/emqx/pull/10999) 更改了 Kafka 字段 "Partition Count Refresh Interval" 和 "Offset Commit Interval" 的 schema 验证，以避免接受超过最大允许值的值。
-   [#10997](https://github.com/emqx/emqx/pull/10997) ClickHouse 桥接存在一个问题，即当 ClickHouse 服务器在发送消息时关闭时，即使请求的 ttl（time to live）设置为无限大，也可能导致消息丢失。通过将由于连接关闭引起的错误视为可恢复错误修复了该问题。
-   [#10994](https://github.com/emqx/emqx/pull/10994) 在 HTTP 连接器中，对 `proxy-authorization headers` 进行了屏蔽处理，以防止将机密信息泄露到日志文件中。
-   [#10996](https://github.com/emqx/emqx/pull/10996) 对于任何未知的 HTTP/API 请求，默认返回404错误，而不是返回 Dashboard 的 index.html 页面。
-   [#11005](https://github.com/emqx/emqx/pull/11005) 修复了在 AuthN HTTP 的跟踪日志中无法正确打印方法字段的问题。
-   [#11006](https://github.com/emqx/emqx/pull/11006) 修复了 QUIC 监听器的默认证书文件路径。
    在此更改之前，默认的证书文件路径以环境变量 `${EMQX_ETC_DIR}` 为前缀，但在 QUIC 监听器中使用之前未进行插值处理。
-   [#10998](https://github.com/emqx/emqx/pull/10998) 不允许为 MongoDB 桥接资源设置 `batch_size` 选项。当前的 MongoDB 连接器不支持批量处理，如果提供了 `batch_size` 配置值，它将被强制设置为1。
-   [#10955](https://github.com/emqx/emqx/pull/10955) 修复了 MQTT-SN 网关中对预定义主题的配置删除无效的问题。
-   [#11025](https://github.com/emqx/emqx/pull/11025) 修复了Pulsar Producer 桥接中可能在竞态条件下引发的 `case_clause` 错误。
-   [#11030](https://github.com/emqx/emqx/pull/11030) 改进了在使用 Listeners HTTP API 时发生验证错误所产生的错误信息。
-   [#11033](https://github.com/emqx/emqx/pull/11033) 在 ExProto 网关中，弃用了 `AuthenticateRequest` 中的 `mountpoint` 字段。
    该字段在 e4.x 版本中引入，但实际上，在 e5.0 版本中，我们已经提供了 `gateway.exproto.mountpoint` 进行配置，因此无需通过 Authenticate 请求来覆盖它。

    此外，将 `subscriptions_max`、`inflight_max` 和 `mqueue_max` 的默认值更新为无限大。
-   [#11040](https://github.com/emqx/emqx/pull/11040) 修复了 Kafka 生产者桥接的健康检查问题，当与 Kafka 代理的连接断开时该问题可能会导致消息丢失。
-   [#11038](https://github.com/emqx/emqx/pull/11038) 修复了 Pulsar 生产者的健康检查问题，当与 Pulsar 代理的连接断开时该问题可能会导致消息丢失。
-   [#11042](https://github.com/emqx/emqx/pull/11042) 修复了当监听器的 max_connections 配置设为字符串时 REST API `GET /listeners` 崩溃的问题。
-   [#11028](https://github.com/emqx/emqx/pull/11028) 禁止在监听器配置中同时使用包括 tlsv1.3 但排除 tlsv1.2 的多个 TLS 版本。
    使用具有这种版本差异的 TLS 配置会导致连接错误。此外，删除和记录与所选 TLS 版本不兼容的 TLS 选项。

    注意：应用此修复后，任何包含上述版本差异的旧监听器配置将无法加载，必须手动修复。
-   [#11031](https://github.com/emqx/emqx/pull/11031) 修复了创建桥接和检查 InfluxDB 桥接时的认证信息验证问题。
-   [#11056](https://github.com/emqx/emqx/pull/11056) 修复了新创建的监听器有时无法正确启动的问题。当您删除一个名为 "default" 的系统默认监听器并添加一个新的同名监听器时，它将无法正确启动。
    -   修复了某些节点上的配置失败可能导致 Dashboard 无法使用的错误。
-   [#11070](https://github.com/emqx/emqx/pull/11070) 修复了 `cluster.autoclean` 配置项无法生效的问题。
-   [#11100](https://github.com/emqx/emqx/pull/11100) 修复了复制节点由于 `mria_lb:core_nodes()` 调用超时而无法连接到核心节点的问题。
    相关的 mria pull request: [https://github.com/emqx/mria/pull/143](https://github.com/emqx/mria/pull/143)
-   [#11092](https://github.com/emqx/emqx/pull/11092) 修复复制节点因超时无法连接到核心节点的问题。

### [已知问题](https://github.com/emqx/emqx-docs/blob/release-5.1/en_US/changes/known-issues-5.1.0.md)
