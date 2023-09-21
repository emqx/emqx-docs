# 版本发布

## e5.2.1

### 增强

- [#11487](https://github.com/emqx/emqx/pull/11487) 将 bcrypt 的工作因子 (work factor) 限制在5-10的范围内，因为较高的值会消耗太多 CPU 资源。Bcrypt 库已更新以允许并行哈希计算。
- [#11568](https://github.com/emqx/emqx/pull/11568) 在消息重发布规则动作中，支持设置 MQTT 5.0 发布属性与用户属性。
- [#11612](https://github.com/emqx/emqx/pull/11612) 在节点疏散期间，疏散所有断开连接的会话，而不仅仅是那些以 `clean_start` 设置为 `false` 开始的会话。
- [#11532](https://github.com/emqx/emqx/pull/11532) 改进了解析无效数据包时的错误消息，以提供更清晰的错误提示。

### 修复

- [#11493](https://github.com/emqx/emqx/pull/11493) 修复了关于 `/api/v5/publish` 的 REST API 示例文档中错误请求响应的问题。之前的文档示例指出错误请求的响应可以在响应体中返回一个列表，但实际情况并非如此。
- [#11499](https://github.com/emqx/emqx/pull/11499) 升级 Erlang/OTP 至 25.3.2-2，此版本从 mnesia_hook 日志消息中排除了敏感数据。
- [#11506](https://github.com/emqx/emqx/pull/11506) 空的跟踪日志文件将不再被下载。在实施此修复后，尝试使用 GET 请求 `/api/v5/trace/clientempty/download` 下载空的跟踪日志文件时，服务器现在将返回 404 状态码以及以下 JSON 消息：`{"code":"NOT_FOUND","message":"Trace is empty"}`。如果在日志文件中未找到与跟踪条件匹配的事件，将触发此响应。
- [#11522](https://github.com/emqx/emqx/pull/11522) 在规则引擎的编解码功能中，改进了当 schema 名称超出允许的长度时出现的错误消息。
- [#11531](https://github.com/emqx/emqx/pull/11531) 修复了针对某个特定的客户端 ID，授权缓存清理 CLI 无法正常工作的问题。
- [#11564](https://github.com/emqx/emqx/pull/11564) 修复了集群分区自动恢复功能。实施了对分裂成多个分区的集群的自动恢复。
- [#11568](https://github.com/emqx/emqx/pull/11568) 修复了一个未明确定义的内置规则动作配置，以避免该配置被理解为自定义用户函数。
- [#11394](https://github.com/emqx/emqx/pull/11394) 将 Kafka 生产者客户端 `wolff` 从1.7.6版本升级到1.7.7版本。这个升级修复了一个潜在的竞态条件，可能会导致在有些 Kafka 生产者初始化失败时所有的 Kafka 生产者崩溃。
- [#11401](https://github.com/emqx/emqx/pull/11401) 修复了在 EMQX Dashboard 中对 SQL 语句进行测试时，规则 SQL 函数 `mongo_date` 的行为。规则 SQL 函数 `mongo_date` 现在在测试模式下返回具有格式 `ISODate(*)` 的字符串，其中 * 是 ISO 日期字符串。这个格式与 MongoDB 存储日期的方式保持一致。
- [#11547](https://github.com/emqx/emqx/pull/11547) 修复了几个 emqx_bridge 的问题：
  - 修复了 Cassandra 数据桥接在没有配置用户名/密码时出现连接错误的问题 （当配置为 `authenticator: AllowAllAuthenticator` 时，Cassandra 不需要用户凭据。）
  - 修复了因为空密码而导致 SQL Server 数据桥接连接错误的问题。
  - 将 Oracle 数据桥接中的 `username` 字段设置为必填项。
  - 修复了 IoTDB 数据桥接因未设置基础 URL 的模式（例如 `<host>:<port>`）而导致的错误。
- [#11630](https://github.com/emqx/emqx/pull/11630) 修复了核心节点可能会卡在 `mria_schema:bootstrap/0` 状态，导致新节点加入集群失败。

## e5.2.0

### 增强

- [#10697](https://github.com/emqx/emqx/pull/10697) 此增强功能允许配置 StatefulSet 的 `minReadySeconds`，从而允许在升级或重新启动命令触发的每个 pod 重新启动之间引入时间间隔。

- [#11124](https://github.com/emqx/emqx/pull/11124) 发布了适用于 Amazon Linux 2023 的软件包。

- [#11289](https://github.com/emqx/emqx/pull/11289) 发布了适用于 Debian 12 的软件包。

- [#11290](https://github.com/emqx/emqx/pull/11290) 更新了 `jq` 依赖项至版本 0.3.10，其引用的 `oniguruma` 库更新至版本 6.9.8，修复了一些小的安全问题。

- [#11291](https://github.com/emqx/emqx/pull/11291) 通过 ekka 更新至版本 0.15.6，将 RocksDB 版本更新至 1.8.0-emqx-1。

- [#11390](https://github.com/emqx/emqx/pull/11390) 向 EMQX 配置添加了 `node.broker_pool_size`、`node.generic_pool_size` 和 `node.channel_cleanup_batch_size` 选项。如果集群互连网络延迟较高，调整这些选项可以显著提高性能。

- [#11429](https://github.com/emqx/emqx/pull/11429) 在 MondoDB 连接和桥接中添加了配置检测遗留协议的选项。

- [#11436](https://github.com/emqx/emqx/pull/11436) 添加了新的 REST API `DELETE /banned`，用于清除所有黑名单数据。

- [#11438](https://github.com/emqx/emqx/pull/11438) 将 `mqtt.max_packet_size` 的类型从字符串更改为 byteSize，以更好地表示有效的数字范围。仍然支持字符串以确保向后兼容性。

- [#11469](https://github.com/emqx/emqx/pull/11469) 支持在 Redis 认证中指定用户名。

- [#11496](https://github.com/emqx/emqx/pull/11496) 默认情况下禁用 Erlang VM Prometheus 导出器，以提高性能和安全性。

- [#11497](https://github.com/emqx/emqx/pull/11497) 通过添加新的消息、过载保护、授权、身份验证指标，改进 OpenTelemetry 的命名一致性，增强了指标可观测性。

- [#10647](https://github.com/emqx/emqx/pull/10647) 新增了 [GreptimeDB](https://github.com/GreptimeTeam/greptimedb) 数据集成。

- [#11261](https://github.com/emqx/emqx/pull/11261) 新增了 Amazon Kinesis Data Streams 生产者数据集成。

- [#11329](https://github.com/emqx/emqx/pull/11329) 新增了 Azure Event Hub 生产者数据集成。

- [#11363](https://github.com/emqx/emqx/pull/11363) 为 RabbitMQ 桥接添加了 TLS 连接支持。

- [#11367](https://github.com/emqx/emqx/pull/11367) 从 EMQX 4.4 迁移了 GCP IoT Hub 认证支持。

- [#11386](https://github.com/emqx/emqx/pull/11386) 认证器新增了 LDAP 数据源。

- [#11392](https://github.com/emqx/emqx/pull/11392) 授权管理器新增了 LDAP 数据源。

- [#11402](https://github.com/emqx/emqx/pull/11402)  Kafka 消费者桥接支持使用占位符动态设置 MQTT 主题。

- [#11403](https://github.com/emqx/emqx/pull/11403) 添加了支持定义 GCP PubSub 生产者桥接的消息属性和排序键模板。还更新了我们的 HOCON 库，以修复一个问题，即数组中的对象即使位于不同的行上也会被串联在一起。

- [#11459](https://github.com/emqx/emqx/pull/11459) 添加了配置 Kafka 桥接的健康检查间隔的选项。

- [#11478](https://github.com/emqx/emqx/pull/11478) 添加了对 HStreamDB 桥接的支持（允许 TCP 和 TLS 连接），并适配了 HStreamDB `v0.16.1`。

  在 [PR#11530](https://github.com/emqx/emqx/pull/11530) 中更新了驱动程序至 `0.4.5+v0.16.1`。

- [#11389](https://github.com/emqx/emqx/pull/11389) 通过利用 Mria 0.6.0 中引入的新 API 将多个索引更新操作合并为单个 Mnesia 事务来提高保留消息发布的速度。

- [#11396](https://github.com/emqx/emqx/pull/11396) 为规则引擎运行时引入了主题索引，提高了消息主题与规则 SQL 中的主题过滤器匹配的速度，避免了对规则集的全面扫描，大幅提升了 EMQX 在处理大量规则时的性能。

- [#11399](https://github.com/emqx/emqx/pull/11399) 改进了规则引擎中的占位符语法。发布操作支持使用占位符语法动态填充 payload 变量中的内容。 占位符语法的格式为 `\${key}`。 在此改进之前，`\${key}` 中只能包含字母、数字和下划线。现在，`\${key}` 支持任何 UTF8 字符。

- [#11405](https://github.com/emqx/emqx/pull/11405) 改进了 `date_to_unix_ts` 的错误原因以便理解。

- [#11490](https://github.com/emqx/emqx/pull/11490) 为各种认证后端添加了未定义密码的快速错误处理。这提高了认证过程的一致性和用户友好性。

### 修复

- [#11065](https://github.com/emqx/emqx/pull/11065) 修复了在 EMQX 关闭过程中防止日志记录无关错误消息的问题。
- [#11279](https://github.com/emqx/emqx/pull/11279) 修复了当在 EMQX 启用了 debug/trace 日志记录时客户端无法发送包含大型 payload 消息的问题。
- [#11296](https://github.com/emqx/emqx/pull/11296) 添加了从 EMQX 备份文件中使用 `emqx ctl import` 命令导入附加配置的支持：
  - rule_engine（以前由于错误而未导入）
  - topic_metrics（以前未实现）
  - slow_subs（以前未实现）
- [#11327](https://github.com/emqx/emqx/pull/11327) 更新了 ekka 到版本 0.15.8，mria 到版本 0.15.8，以及 optvar 到 1.0.5。 这修复了偶发的断言失败问题。
- [#11346](https://github.com/emqx/emqx/pull/11346) 更新了 ekka 到版本 0.15.9。 这修复了在获取锁定超时时出现的悬挂的 etcd 锁定问题。
- [#11347](https://github.com/emqx/emqx/pull/11347) 确保 OCSP 请求路径正确进行了 URL 编码。
- [#11352](https://github.com/emqx/emqx/pull/11352) 修复了在 Windows 或其他不支持 RocksDB 的平台上启动时出现的崩溃问题。
- [#11388](https://github.com/emqx/emqx/pull/11388) 增加了 `emqx_router_sup` 重启强度，以提高对在正常情况下发生偶发崩溃的容忍度，而无需关闭整个 EMQX 应用程序。 例如， 如果核心节点正在停止、重新启动或处于不可用状态，从复制节点委派给 `emqx_router_helper` 的 mria 写入/删除调用可能会失败。修改后的重启强度确保系统保持稳定运行。
- [#11424](https://github.com/emqx/emqx/pull/11424) 添加了对 API 中时间戳的最大值的检查，以确保它是有效的 Unix 时间戳。
- [#11445](https://github.com/emqx/emqx/pull/11445) 删除了 Windows 平台上的 os_mon 应用程序监控支持，以防止虚拟机崩溃。 该功能仍然适用于非 Windows 平台。
- [#11454](https://github.com/emqx/emqx/pull/11454) 修复了在调试/跟踪大型 payload 时出现的崩溃问题（在 [#11279](https://github.com/emqx/emqx/pull/11279) 中引入）。
- [#11456](https://github.com/emqx/emqx/pull/11456) 移除了对 CA 证书文件强制要求非空 PEM 的验证，允许 CA 证书文件 PEM 为空。
- [#11466](https://github.com/emqx/emqx/pull/11466) 修复了将 `ssl_options.ciphers` 配置选项设置为空字符串（""）时出现崩溃的问题。
- [#11480](https://github.com/emqx/emqx/pull/11480) 改进了规则引擎中当规则函数接收到错误参数时的错误处理和 SQL 函数测试。
- [#11520](https://github.com/emqx/emqx/pull/11520) 修复了在发送带有非零 `ack_flag` 的 CONNACK 数据包时未增加 `packets_connack_sent` 指标的问题。
- [#11523](https://github.com/emqx/emqx/pull/11523) 更正了在为 `/configs` API 指定无效证书/密钥时出现的令人误解的提示。
- [#11534](https://github.com/emqx/emqx/pull/11534) 修复了当桥接状态不健康时数据桥接统计数据的增量。现在，发送到不健康桥接的消息将被计算为丢弃的消息。
- [#11540](https://github.com/emqx/emqx/pull/11540) 在尝试创建具有无效名称的桥接时，改进了 HTTP 响应。
- [#11548](https://github.com/emqx/emqx/pull/11548) 修复了在整个集群中更新插件顺序的问题。
- [#11366](https://github.com/emqx/emqx/pull/11366) 修复了在使用 EMQX Operator 在 `bootstrapConfig` 中指定一些桥接配置时可能会阻止 pod 启动的问题。
- [#11453](https://github.com/emqx/emqx/pull/11453) 修复了测试 InfluxDB 桥接的连接时可能产生虚假负面结果的问题。
- [#11461](https://github.com/emqx/emqx/pull/11461) 将测试桥接连接的超时更加紧密地与配置的健康检查超时保持一致。
- [#11492](https://github.com/emqx/emqx/pull/11492) 修复了测试 GreptimeDB 桥接的连接时可能产生虚假负面结果的问题。
- [#11508](https://github.com/emqx/emqx/pull/11508) 修复了 Kafka 桥接中将 header 翻译为无效值时的错误处理。
- [#11513](https://github.com/emqx/emqx/pull/11513) 修复了一个错误，该错误导致 Kafka 生产者桥接无法使用正确的模板的来处理 `timestamp` 字段。
- [#11527](https://github.com/emqx/emqx/pull/11527) 修复了与 Kafka header 模板处理相关的问题。该问题发生在占位符解析为键值对数组时（例如：`[{"key": "foo", "value": "bar"}]`）。
