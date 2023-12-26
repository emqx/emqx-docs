# v5.0

## 5.0.4

*发布日期: 2023-05-26*

## 增强功能

- [#10389](https://github.com/emqx/emqx/pull/10389) 统一 `cluster.core_nodes` 和 `cluster.statics.seeds` 配置格式，同时支持数组 `["emqx1@127.0.0.1", "emqx2@127.0.0.1"]` 或逗号分隔的字符串 `"emqx1@127.0.0.1,emqx2@127.0.0.1"` 两种格式。

- [#10392](https://github.com/emqx/emqx/pull/10392) 新增函数 date_to_unix_ts/3，用于将格式化的日期转换成整数类型的时间戳：

  `date_to_unix_ts(TimeUnit, FormatString, InputDateTimeString)`

- [#10426](https://github.com/emqx/emqx/pull/10426) 优化配置优先级机制，修复重启 EMQX 后 `etc/emqx.conf` 中的配置不生效的问题。有关配置优先级请参考[配置重写规则](https://docs.emqx.com/zh/enterprise/v5.0/configuration/configuration.html#%E9%85%8D%E7%BD%AE%E8%A6%86%E7%9B%96%E8%A7%84%E5%88%99)

- [#10457](https://github.com/emqx/emqx/pull/10457) 移除 StatsD 指标监控功能。

- [#10458](https://github.com/emqx/emqx/pull/10458) 调整插件配置在示例配置文件中的位置，大多数情况下用户都在 Dashboard 上管理插件，不需要手动修改配置，因此我们将插件配置调整到更靠后的位置。

- [#10491](https://github.com/emqx/emqx/pull/10491) 将 `etcd.ssl` 重命名为 `etcd.ssl_options` ，使其与配置文件中其他 SSL 相关配置项保持一致的命名。

- [#10512](https://github.com/emqx/emqx/pull/10512) 改进了数据文件存储格式，现已支持正常存储 Unicode 字符，例如包含 Unicode 字符的规则 SQL `SELECT * FROM "t/1" WHERE clientid = "-测试专用-"`。

- [#10568](https://github.com/emqx/emqx/pull/10568) 为 `emqx ctl listeners` 命令返回结果添加连接关闭计数指标 `shutdown_count`。

- [#10588](https://github.com/emqx/emqx/pull/10588) 将日志跟踪记录的时间精度从秒提高到微秒。例如，从 `2023-05-02T08:43:50+00:00` 到 `2023-05-02T08:43:50.237945+00:00` 。

- [#10623](https://github.com/emqx/emqx/pull/10623) 在 `force_shutdown` 配置中，将 `max_message_queue_len` 重命名为 `max_mailbox_size` ，旧名称作为别名被保留，保证向后兼容性。

- [#10713](https://github.com/emqx/emqx/pull/10713) 为减少歧义与配置复杂度，隐藏 WebHook `resource_option.request_timeout` 配置项，并使用 `http` `request_timeout` 来设置该值。

- [#10075](https://github.com/emqx/emqx/pull/10075) 新增节点再平衡/节点疏散功能，功能设计与使用请参考 [EIP 文档](https://github.com/emqx/eip/blob/main/active/0020-node-rebalance.md)。

- [#10378](https://github.com/emqx/emqx/pull/10378) 新增 Pulsar 数据桥接，目前仅支持单向数据集成即 Pulsar 生产者角色。

- [#10408](https://github.com/emqx/emqx/pull/10408) 规则 SQL 新增三个内置函数，用于写入日期类型的值到 MongoDB 中。

- [#10409](https://github.com/emqx/emqx/pull/10409) [#10337](https://github.com/emqx/emqx/pull/10337) 规则引擎新增 [Protocol Buffer](https://protobuf.dev/)  和 [Apache Avro](https://avro.apache.org/) 格式消息编解码功能。

- [#10425](https://github.com/emqx/emqx/pull/10425) 新增 OpenTSDB 数据桥接。

- [#10498](https://github.com/emqx/emqx/pull/10498) 新增 Oracle 数据桥接。

- [#10560](https://github.com/emqx/emqx/pull/10560) 新增 Apache IoTDB 数据桥接。

- [#10417](https://github.com/emqx/emqx/pull/10417) 通过解除临时引用来提高配置项读取性能。

- [#10430](https://github.com/emqx/emqx/pull/10430) 简化 `retainer` 的配置，将 `flow_control` 标记为不重要的字段。

- [#10511](https://github.com/emqx/emqx/pull/10511) 对资源相关的日志进行脱敏以提升隐私与安全性。

- [#10525](https://github.com/emqx/emqx/pull/10525) 提高 MQTT 数据包处理的性能。

- [#10528](https://github.com/emqx/emqx/pull/10528) 在代码的热路径（hot code path）中减少内存占用。热路径包括在消息处理、连接管理、认证授权等核心功能中被频繁执行的代码。

- [#10591](https://github.com/emqx/emqx/pull/10591) [#10625](https://github.com/emqx/emqx/pull/10625) 改进并简化速率限制器的配置：

  - 降低速率限制器配置的复杂度。

  - 更新 `configs/limiter` API。

  - 减少速率限制器配置内存占用。

- [#10487](https://github.com/emqx/emqx/pull/10487) 优化不设置速率限制(`infinity`)时速率限制器的性能表现，减少了内存和 CPU 的使用量。

- [#10490](https://github.com/emqx/emqx/pull/10490) 移除默认 `1000/s` 的连接速率限制。

- [#10077](https://github.com/emqx/emqx/pull/10077) QUIC TLS 现已支持密码保护证书文件。

## 错误修复

- [#10340](https://github.com/emqx/emqx/pull/10340) 修复通过 `systemd` 停止 EMQX 时可能导致的日志打印崩溃问题。

- [#10369](https://github.com/emqx/emqx/pull/10369) 修复 `/api/v5/monitor_current` API，修复前，当某些 EMQX 节点出现故障时，该 API 请求会返回 500 和以下信息：

  `{"code":"INTERNAL_ERROR","message":"error, badarg, [{erlang,'++',[{error,nodedown},[{node,'emqx@10.42.0.150'}]], ...`

- [#10407](https://github.com/emqx/emqx/pull/10407) 修复告警崩溃问题

  - 通过使用 Mnesia dirty 操作和避免不必要的调用。

  - 使用 'emqx_resource_manager' 来重新激活已经激活的告警，提高'emqx_alarm'性能。

  - 使用新的安全的 'emqx_alarm' API 来激活/停用告警，确保 emqx_resource_manager 不会因为警报超时而崩溃。

  - 当以下条件同时出现时告警系统容易崩溃：

    - 失败的资源数量相对较多，例如，桥接试图激活重复出现的错误的警报。

    - 系统经历了一个非常高的负载。

- [#10420](https://github.com/emqx/emqx/pull/10420) 修复认证和授权 HTTP 路径可能被多次编码的问题：

  - 由于无法假定外部服务器对原始和规范化 URL 的处理方式，因此将尽量避免不必要的 URL 编码，以免导致类似 [#10411](https://github.com/emqx/emqx/issues/10411) 的 bug。

- [#10422](https://github.com/emqx/emqx/pull/10422) 修正单节点集群中，外部插件无法通过环境变量进行配置的问题。

- [#10448](https://github.com/emqx/emqx/pull/10448) 修复由 e5.0.3 引入的速率限制器配置（用 `burst` 代替 `capacity` ）的兼容性问题，修复前，如 `capacity` 设为 `infinity` ，将无法从之前的版本进行升级。修复后，`capacity = infinity` 配置将自动转换为等同的 `burst = 0` 。

- [#10462](https://github.com/emqx/emqx/pull/10462) 废弃已经无效的 `broker.shared_dispatch_ack_enabled` 配置项 。该配置项旨在避免向存在连接断开客户端的共享订阅组中派发回消息。但实际从 e5.0.0 版本开始，消息会被重新派发到组内的其他已经连接的会话中，该配置已经失效。请参考: <https://github.com/emqx/emqx/pull/9104> 。

- [#10463](https://github.com/emqx/emqx/pull/10463) 修复数据桥接 API 的错误处理问题，例如 WebHook 桥接 URL 无效时， API 将返回 '400' 而非 '500'。

- [#10484](https://github.com/emqx/emqx/pull/10484) 修复滚动升级时无法设置配置优先级的问题。例如，在 e5.0.2 中修改了授权的优先级，当通过滚动升级升级到 e5.0.3 时，授权的优先级将被恢复为默认。

- [#10495](https://github.com/emqx/emqx/pull/10495) 恢复了被误删的速率限制器 API `/configs/limiter`。

- [#10500](https://github.com/emqx/emqx/pull/10500) 修复并增强了 Mria 中的一些功能：

  - 通过全局锁来保护 `mria:join/1,2` 函数，避免两个同时试图加入对方的节点之间的冲突 [Mria PR](https://github.com/emqx/mria/pull/137)

  - 提供了新函数 `mria:sync_transaction/4,3,2`，该函数会阻止调用者，直到事务被导入到本地节点（当本地节点是复制者时有效，否则它的行为与`mria:transaction/3,2` 函数完全一样）。

  - 优化  `mria:running_nodes/0`  函数 [Mria PR](https://github.com/emqx/mria/pull/135)

  - 优化单个复制节点调用  `mria:ro_transaction/2`  函数时的性能 [Mria PR](https://github.com/emqx/mria/pull/134).

- [#10518](https://github.com/emqx/emqx/pull/10518) 在 Mria 中增加以下修复和功能：

  - 在 mria_membership 中安全地调用 `mria_rlog:role/1`，以确保 mria_membership gen_server 在 RPC 到另一个节点失败时不会崩溃 [Mria PR](https://github.com/emqx/mria/pull/139)。

  - 在 `?rlog_sync` 表中添加额外的字段，以方便在未来的扩展这一功能 [Mria PR](https://github.com/emqx/mria/pull/138)。

- [#10556](https://github.com/emqx/emqx/pull/10556) 加密`emqx_connector_http`  携带的  `Authorization header`。

- [#10571](https://github.com/emqx/emqx/pull/10571) 尝试通过 API 获取主题统计中不存在的主题时，不会产生无意义的崩溃报告。

- [#10659](https://github.com/emqx/emqx/pull/10659) 修复当 `sysmon.os.mem_check_interval` 被禁用时，EMQX 无法启动的问题。

- [#10717](https://github.com/emqx/emqx/pull/10717) 修复当飞行窗口拥塞时，缓冲层进程 CPU 占用过高的问题。

- [#10724](https://github.com/emqx/emqx/pull/10724) 为 HTTP API 文档所有 API 添加摘要字段以便查找 API（访问地址为 "http://<emqx_host_name\>:18083/api-docs")。

- [#10726](https://github.com/emqx/emqx/pull/10726) 对健康检查间隔和自动重启间隔的范围进行验证，支持 1 ms 到 1 小时的时间范围。

- [#10728](https://github.com/emqx/emqx/pull/10728) 修复规则引擎 SQL `FOREACH - DO`语句执行问题，例如在以下 SQL 语句中：

  `FOREACH   payload.date as date, payload.array as elem DO   date, elem FROM "t/#"  -- {"date": "2023-05-06", "array": ["a"]}`

  修复前，由 `FOREACH` 导出的 `date` 变量无法在 `DO` 子句中访问，导致该 SQL 输出如下： `[{"elem": "a","date": "undefined"}]` 。

  修复后，该 SQL 语句将能正确输出：`[{"elem": "a","date": "2023-05-06"}]`

- [#10742](https://github.com/emqx/emqx/pull/10742) 在保存授权文件前对规则进行正确性检查，避免由于错误规则导致 EMQX 无法启动。

- [#10743](https://github.com/emqx/emqx/pull/10743) 修复节点加入集群后，尝试获取桥接信息或指标时可能导致的崩溃问题。

- [#10755](https://github.com/emqx/emqx/pull/10755) 修复了数据桥接资源更新中的竞争条件。

  在 EMQX 的资源更新中，进行了"删除+创建"的操作。如果桥接器的创建时间过长，可能导致 Dashboard 请求超时。如果在桥接创建完成之前进行更新操作，可能会导致桥接被删除。

  此修复解决了桥接器资源更新中的竞争条件，确保准确识别和添加新数据桥接，并保持运行时和配置文件状态的一致性。

- [#10761](https://github.com/emqx/emqx/pull/10761) 修复了 Dashboard Listener 的 SSL 证书的默认值没能被正确设置的问题，当 `verify_peer` 和 `cacertfile` 使用默认配置时，将导致 HTTPS 无法访问。

- [#10672](https://github.com/emqx/emqx/pull/10672) 修复当监听器中缺少 `ssl_options` 的默认值导致启动失败的问题。修复前， `EMQX_LISTENERS__WSS__DEFAULT__BIND='0.0.0.0:8089' ./bin/emqx console` 类似的命令将导致 EMQX 崩溃。

- [#10738](https://github.com/emqx/emqx/pull/10738) 修复 TDengine 数据桥不支持子表以及自动建表问题。在此之前，子表插入 SQL 语句将执行失败：

  - `insert into ${clientid} using msg TAGS (${clientid}) values (${ts},${msg})`

* [#10746](https://github.com/emqx/emqx/pull/10746)  在规则引擎的测试 API `rule_test` 中增加对事件`$events/delivery_dropped`的支持。
* [#10747](https://github.com/emqx/emqx/pull/10747)  移植 4.4 版本中一些规则引擎的时间格式化函数的修复到当前版本。
* [#10760](https://github.com/emqx/emqx/pull/10760)  修复在节点加入集群是访问数据桥接数据统计页面是出现 "internal error 500" 错误的问题。
* [#10801](https://github.com/emqx/emqx/pull/10801)  避免在 API `/topics/{topic}`  和  `/topics` 中对主题名进行双重百分号解码。
* [#10817](https://github.com/emqx/emqx/pull/10817)  修复对桥接配置项`auto_restart_interval`的值的处理，现在支持设置为`infinity`。

## 5.0.3

*发布日期: 2023-05-08*

### 优化

- [#10128](https://github.com/emqx/emqx/pull/10128) SSL MQTT 监听器增加对 OCSP Stapling 的支持。

- [#10156](https://github.com/emqx/emqx/pull/10156) 调整配置文件覆盖顺序机制。

  对于新安装的 EMQX，emqx.conf 和环境变量中的配置会覆盖 API 传入的配置（即 `cluster.hocon` 中的配置）

  对于从旧版本升级的 EMQX（即 `data` 文件夹中包含 `cluster-override.conf` 文件），保留之前的覆盖规则， 即 `cluster-override.conf` 中的配置会覆盖 `emqx.conf` 和环境变量的配置。

  注意：`data/configs/cluster-override.conf` 已弃用。升级后，建议您在 `emqx.conf` 中重新配置之前被 `cluster-override.conf` 覆盖的配置项，并将 cluster-override.conf 中的配置迁移到 `cluster.hocon` 中。

  升级后，EMQX 将像以前一样继续读取 `local-override.conf` (如果存在的话)，但建议您将配置合并到 `emqx.conf` 中。

- [#10164](https://github.com/emqx/emqx/pull/10164) TLS MQTT 监听器增加对 CRL 检查的支持。

- [#10207](https://github.com/emqx/emqx/pull/10207) 提高 OpenAPI (swagger) 文档的可读性。 在此更改之前，文档中有一些 `Summary` 字段冗长且缺乏翻译，现在使用了 i18n 数据库中更简洁的 `label` 字段。

- [#10210](https://github.com/emqx/emqx/pull/10210) 解决停止/重启 Mria 时 Mnesia callback 可能出现的问题。优化后，当 Mria 被停止前会取消 Mnesia callback 的注册。详情见 [Mria PR](https://github.com/emqx/mria/pull/133)。

- [#10224](https://github.com/emqx/emqx/pull/10224) 在 Helm 图表中增加自定义 `clusterIP` 选项，用户可以将其设置为固定 IP。

- [#10263](https://github.com/emqx/emqx/pull/10263) 添加用于评估 Elixir 表达式的命令 `eval-ex`。

- [#10278](https://github.com/emqx/emqx/pull/10278) 重构所有网关的目录结构。

- [#10206](https://github.com/emqx/emqx/pull/10206) 所有数据桥接支持异步查询模式。

  优化前，如将某项资源（如数据桥接）的查询模式设为 sync（同步模式），缓存将以同步的模式调用底层连接器，即使它支持异步调用。

- [#10306](https://github.com/emqx/emqx/pull/10306) 大多数数据桥接支持 async 查询模式。
  这是 [#10206](https://github.com/emqx/emqx/pull/10206) 的后续优化, 优化前，Cassandra、MongoDB、MySQL、Postgres、Redis、RocketMQ、TDengine 等数据桥接只支持同步查询模式。

- [#10318](https://github.com/emqx/emqx/pull/10318) 规则引擎中的 FROM 语句新增支持由单引号（'）包裹的字符串。

- [#10336](https://github.com/emqx/emqx/pull/10336) 添加 API Endpoint `/rule_engine`，用以管理规则引擎的配置。

- [#10354](https://github.com/emqx/emqx/pull/10354) 优化 `max_heap_size` 配置错误时的报错信息。当发生 `message_queue_too_long` 报错时，会在日志文件中记录当前值和最大值。

- [#10358](https://github.com/emqx/emqx/pull/10358) 隐藏 `flapping_detect/conn_congestion/stats` 配置。弃用 `flapping_detect.enable` 配置项。

- [#10359](https://github.com/emqx/emqx/pull/10359) 通过独立的 RPC 收集针对集群级别的指标，不再隐式收集不被 API 调用的指标。

- [#10373](https://github.com/emqx/emqx/pull/10373) 废弃 `trace.payload_encode` 配置项。可以在通过 HTTP API 创建的日志追踪时使用 `trace.payload_encode = [text, hidden, hex]` 字段替代。

- [#10381](https://github.com/emqx/emqx/pull/10381) 隐藏 `auto_subscribe` 配置项，后续将只能通过 HTTP API 来修改自动订阅规则。

- [#10391](https://github.com/emqx/emqx/pull/10391) 简化配置文件并隐藏大量的配置项，包括 `rewrite`、`topic_metric`、`persistent_session_store`、`overload_protection`、`flapping_detect`、`conn_congestion`、`stats`、`auto_subscribe`、`broker_perf`、`shared_subscription_group`、`slow_subs`、`ssl_options.user_lookup_fun`、 `node` 和 `dashboard` 相关的部分高级配置项，[#10358](https://github.com/emqx/emqx/pull/10358), [#10381](https://github.com/emqx/emqx/pull/10381), [#10385](https://github.com/emqx/emqx/pull/10385)。

- [#10404](https://github.com/emqx/emqx/pull/10404) 将缓冲区工作线程的默认队列模式更改为 `memory_only`。在此优化前，默认队列模式为 `volatile_offload`，当消息速率很高，资源无法满足该需求时，缓冲区性能会由于频繁的磁盘操作而受影响。

- [#10140](https://github.com/emqx/emqx/pull/10140) 新增 Cassandra 数据桥接，目前仅支持 Cassandra 3.x 版本，暂不支持 4.x 版本。

- [#10143](https://github.com/emqx/emqx/pull/10143) 新增 RocketMQ 数据桥接。

- [#10165](https://github.com/emqx/emqx/pull/10165) InfluxDB 数据桥接中的 `write_syntax` 中支持转义特殊字符。优化后，用户可根据 InfluxDB Line Protocol 在字符串中使用经转义的特殊字符。

- [#10211](https://github.com/emqx/emqx/pull/10211) 隐藏 `broker.broker_perf` 配置和相关 API 文档。其中的两个配置项 `route_lock_type` 和 `trie_compaction` 很少使用，而且需要重新启动整个集群才能生效，不必要暴露给用户。更多信息可阅读：<https://gist.github.com/zmstone/01ad5754b9beaeaf3f5b86d14d49a0b7/revisions>。

- [#10294](https://github.com/emqx/emqx/pull/10294) 配置 MongoDB 数据桥接时，现支持通过占位符 `${field}` 语法来引用消息中的字段，从而动态地选择要插入的数据集合。

- [#10363](https://github.com/emqx/emqx/pull/10363) 新增 Microsoft SQL Server 数桥接。

- [#10573](https://github.com/emqx/emqx/pull/10573) 提升了 WebHook 在同步请求模式下的性能表现，以及其他数据桥接在未配置批处理时的性能表现。

### 修复

- [#10145](https://github.com/emqx/emqx/pull/10145) 在针对 `GET /bridges/:id` 的 API 调用响应中，如桥接的状态为断开，且内部健康检查返回错误，添加 `status_reason` 字段说明错误原因。在相应的告警信息中，同样增加 `status_reason` 字段说明断开原因。

- [#10172](https://github.com/emqx/emqx/pull/10172) 修复默认 ACL 规则中不正确的正则表达式，从而允许 dashboard 用户名订阅 `$SYS/#` 主题。

- [#10174](https://github.com/emqx/emqx/pull/10174) 将库 `esockd` 从 5.9.4 升级至 5.9.6。如连接在代理发送代理协议头之前关闭，将不再产生的一条错误级别日志。

- [#10195](https://github.com/emqx/emqx/pull/10195) 对包含 HTML 的 API Schema 添加标签，解决之前会破坏生成文档格式的问题。

- [#10196](https://github.com/emqx/emqx/pull/10196) 针对用于生成在线文档菜单中的模式摘要和描述，使用小写字母。

- [#10209](https://github.com/emqx/emqx/pull/10209) 修复在断开禁用客户端时，此类客户端仍可发布遗嘱消息的错误。

- [#10225](https://github.com/emqx/emqx/pull/10225) 对于名称与已安装的插件开头相同的插件，用户仍可继续安装。例如：如果插件 `emqx_plugin_template_a` 已安装，用户仍可安装名为 `emqx_plugin_template` 的插件。

- [#10226](https://github.com/emqx/emqx/pull/10226) 在 `/bridges` API 验证错误时，返回 `400` 而非 `500`。

- [#10242](https://github.com/emqx/emqx/pull/10242) 修复日志中数据字段名称冲突。修复前，一些调试日志可能会报告错误的 Erlang PID，影响解决会话接管类问题。

- [#10257](https://github.com/emqx/emqx/pull/10257) 修复 LwM2M 网关中 `auto_observe` 无法正常工作的问题。

  修复前，在发送 `OBSERVE` 请求时没有发送 token，导致 LwM2M 网关无法处理客户端请求。

  修复后，LwM2M 网关可以正确观察到客户端携带的资源列表，此外，未知资源将被忽并打印以下警告日志：

  ```
  2023-03-28T18:50:27.771123+08:00 [warning] msg: ignore_observer_resource, mfa: emqx_lwm2m_session:observe_object_list/3, line: 522, peername: 127.0.0.1:56830, clientid: testlwm2mclient, object_id: 31024, reason: no_xml_definition
  ```

- [#10286](https://github.com/emqx/emqx/pull/10286) 优化 EMQX 启动失败时的日志记录行为。当 EMQX 由于配置文件破坏无法启动时，不会再产生过多的日志记录，也不会再产生崩溃转储文件。

- [#10297](https://github.com/emqx/emqx/pull/10297) 通过仅评估 Erlang 表达式实现 `eval` 命令与 v4 版本的向后兼容，该更新同样适用于 Elixir 节点。对于 Elixir 表达式，请使用 `eval-ex` 命令。

- [#10300](https://github.com/emqx/emqx/pull/10300) 针对通过 Elixir 构建的项目，修复无法通过环境变量配置插件的问题。

- [#10315](https://github.com/emqx/emqx/pull/10315) 修复在 `/mqtt/delayed/messages` API 调用中，在检查 `limit` 和 `page` 参数时的崩溃问题。

- [#10317](https://github.com/emqx/emqx/pull/10317) 在经过充分验证前，隐藏监听器级别的认证信息。

- [#10323](https://github.com/emqx/emqx/pull/10323) 出于安全原因，API 示例中 `password` 字段的值被替换为 `******`。

- [#10410](https://github.com/emqx/emqx/pull/10410) 针对由`emqx.conf` 配置的网关，修复配置检查失败问题。
  此问题最早在 v5.0.22 版本中由 [#10278](https://github.com/emqx/emqx/pull/10278)引入，启动时缺少配置检查。

- [#10533](https://github.com/emqx/emqx/pull/10533) 修复可能导致日志中的出现无害的噪音的问题。

  在对数据桥接进行同步调用时，一些迟到的回复可能会发送到不再期待回复的连接进程，导致产生错误日志，如：

  ```
  2023-04-19T18:24:35.350233+00:00 [error] msg: unexpected_info, mfa: emqx_channel:handle_info/2, line: 1278, peername: 172.22.0.1:36384, clientid: caribdis_bench_sub_1137967633_4788, info: {#Ref<0.408802983.1941504010.189402>,{ok,200,[{<<"cache-control">>,<<"max-age=0, ...">>}}
  ```

  这些日志是无害的，但它们可能会泛滥成灾，引起用户不必要的担心。

- [#10449](https://github.com/emqx/emqx/pull/10449) 在通过 HTTP 服务（`authn_http`）创建身份验证时，将进行 `ssl_options` 和 `header` 配置验证。在此修复前，用户通过错误的 ssl 配置也可成功创建身份验证，但该验证整体不生效。

- [#10548](https://github.com/emqx/emqx/pull/10548) 修复了 HTTP 驱动程序在竞争条件下会导致错误而不去重试的问题。
  相关的驱动程序修复：[emqx/ehttpc#45](https://github.com/emqx/ehttpc/pull/45)

- [#10201](https://github.com/emqx/emqx/pull/10201) 在 TDengine 数据桥接中，移除 SQL 模板中冗余的数据库名称。

- [#10270](https://github.com/emqx/emqx/pull/10270) 在创建 ClickHouse 数据桥接时，优化用户点击测试按钮时的错误信息。

- [#10324](https://github.com/emqx/emqx/pull/10324) 针对配置有误的 ClickHouse 数据桥接，当用户尝试通过 Dashboard 重连时，将收到报错。修复前，用户不会收到报错。

- [#10438](https://github.com/emqx/emqx/pull/10438) 修复 DynamoDB 数据桥接中的部分术语使用错误：

  - 将 `database` 修改为 `table`
  - 将 `username` 修改为 `aws_access_key_id`
  - 将 `password` 修改为 `aws_secret_access_key`

## 5.0.2

*发布日期: 2023-04-12*

### 优化

- [#10022](https://github.com/emqx/emqx/pull/10022) 发布 Rocky Linux 9 (兼容 Red Hat Enterprise Linux 9) 以及 macOS 12 Intel 平台的安装包。

- [#10139](https://github.com/emqx/emqx/pull/10139) 在 EMQX Helm Chart 中添加 `extraVolumeMounts`，可以挂载用户自己的文件到 EMQX 实例中，例如在 [#9052](https://github.com/emqx/emqx/issues/9052) 中提到的 ACL 规则文件。

- [#9893](https://github.com/emqx/emqx/pull/9893) 当使用 `clean_start=false` 标志连接时，EMQX 将过滤掉会话中被被黑名单功能禁止的客户端发布的消息。以前，在这种情况下，被黑名单功能禁止的客户端发送的消息仍可能被传递给订阅者。

- [#9986](https://github.com/emqx/emqx/pull/9986) 在 helm charts 中增加 MQTT ingress 并删除过时的 `mgmt` 引用。

- [#9564](https://github.com/emqx/emqx/pull/9564) 数据桥接新增 Kafka Consumer，支持从 Kafka 消费消息并将它们发布到 MQTT 主题。

- [#9881](https://github.com/emqx/emqx/pull/9881) 改进了与 InfluxDB 连接的健康检查相关的错误日志。

- [#9985](https://github.com/emqx/emqx/pull/9985) 添加 ClickHouse 数据桥接。

- [#10123](https://github.com/emqx/emqx/pull/10123) 改进了 `/bridges` API 的性能。避免了当集群节点数量较多时可能出现的请求响应超时。

- [#9998](https://github.com/emqx/emqx/pull/9998) 出于安全原因，在使用 HTTP 服务进行客户端认证时，对错误日志中的请求体进行脱敏处理。

- [#10026](https://github.com/emqx/emqx/pull/10026) 仅在 `/bridges/:id/metrics` API 中返回指标数据。

- [#10052](https://github.com/emqx/emqx/pull/10052) 改进守护进程模式下启动失败后的日志。

### 修复

- [#10013](https://github.com/emqx/emqx/pull/10013) 修复 `/gateways/:name/clients` API 在错误情况下的返回类型结构。

- [#10014](https://github.com/emqx/emqx/pull/10014) 当节点不存在时，`/monitor(_current)/nodes/:node` API 返回 `404` 错误代码而不是 `400` 。

- [#10027](https://github.com/emqx/emqx/pull/10027) 允许在 Docker 中通过 `EMQX_NODE__NAME` 设置节点名称。

- [#10050](https://github.com/emqx/emqx/pull/10050) 当调用 Bridge API 时若资源不能存在则返回 `404` 状态码。

- [#10055](https://github.com/emqx/emqx/pull/10055) 修复配置项 `mqtt.max_awaiting_rel` 设置无效的问题。

- [#10056](https://github.com/emqx/emqx/pull/10056) 修复 `/bridges` API 状态码返回错误。当被删除的 Bridge 存在依赖，或 Bridge 未启用时进行启用、停止、重启等操作时返回 `400` 状态码。

- [#10066](https://github.com/emqx/emqx/pull/10066) 优化 `/briges_probe` 和 `[/node/:node]/bridges/:id/:operation` API 调用的错误消息，使其更易理解。并修正错误状态码为`400`。

- [#10074](https://github.com/emqx/emqx/pull/10074) 增加 `PUT /authorization/sources/:type` 请求参数 `type` 的值与请求体中的实际类型的一致性检查。

- [#10079](https://github.com/emqx/emqx/pull/10079) 修复文档中关于 `shared_subscription_strategy` 的描述错误。

- [#10085](https://github.com/emqx/emqx/pull/10085) 对于 `/authorization/sources/:source[/*]` API 中不存在的资源的所有请求，始终返回 `404`。

- [#10098](https://github.com/emqx/emqx/pull/10098) 修复了当配置 MongoDB 授权时 MongoDB 连接器崩溃的问题。

- [#10100](https://github.com/emqx/emqx/pull/10100) 修复了当客户端使用增强认证时，认证消息发送缓慢或者消息丢失时客户端进程崩溃的问题。

- [#10107](https://github.com/emqx/emqx/pull/10107) 当调用 `bridges API` 时如果 `bridge-id` 不存在则返回 `404` 状态码。

- [#10117](https://github.com/emqx/emqx/pull/10117) 修复当加入的节点没有安装在集群其他节点上的插件时发生的错误。修复后，加入的节点将从其他节点复制所有必要的插件。

- [#10118](https://github.com/emqx/emqx/pull/10118) 修复手动添加 EMQX 副本类型节点到集群的相关问题。

- [#10119](https://github.com/emqx/emqx/pull/10119) 修复了当 `statsd.server` 设置为空字符串时会崩溃的问题。

- [#10124](https://github.com/emqx/emqx/pull/10124) 调整 MongoDB 的默认心跳周期，以降低日志文件记录过多的风险。

- [#10130](https://github.com/emqx/emqx/pull/10130) 修复了通过环境变量设置的值，在 Dashboard 中显示乱码的问题。

- [#10132](https://github.com/emqx/emqx/pull/10132) 修复了 `systemctl stop emqx` 命令无法正确停止 `jq` 和 `os_mon` 应用的问题。

- [#10144](https://github.com/emqx/emqx/pull/10144) 修复了当 emqx 目录只读时， emqx cli 设置 Erlang cookie 失败的问题。

- [#10154](https://github.com/emqx/emqx/pull/10154) 将数据桥接和连接器的 `resume_interval` 参数值设为 `health_check_interval` 和 `request_timeout / 3` 中的较小值，以解决请求超时的问题。

- [#10157](https://github.com/emqx/emqx/pull/10157) 修复在创建新的监听器时默认速率限制不生效的问题。

- [#10237](https://github.com/emqx/emqx/pull/10237) 当调用 `/nodes/:node[/metrics|/stats]` API，若节点不存在则返回 `404` 状态码。

- [#10251](https://github.com/emqx/emqx/pull/10251) 修复了当删除一个使用中的 ingress 类型的桥接时，未提示存在规则依赖的问题。

- [#10313](https://github.com/emqx/emqx/pull/10313) 确保在核心节点或副本节点启动时，仅从核心节点复制 `cluster-override.conf` 文件。

- [#10327](https://github.com/emqx/emqx/pull/10327) 数据桥接出现不可恢复的错误不再计入到 `actions.failed.unknown` 指标中。

- [#10095](https://github.com/emqx/emqx/pull/10095) 修复当 MySQL 连接器处于批处理模式时，会发生客户端在每个批次上不断使用不必要的 `PREPARE` 语句查询服务器，可能会导致服务器资源耗尽的问题。

## 5.0.1

*发布日期: 2023-03-10*

### 增强

- [#10019](https://github.com/emqx/emqx/pull/10019) 为 QUIC 监听器添加更多底层调优选项。
- [#10059](https://github.com/emqx/emqx/pull/10059) 规则引擎 API 在出错时返回用户可读的错误信息而不是原始的异常堆栈信息。
- [#9213](https://github.com/emqx/emqx/pull/9213) 在 Helm Chart 中支持 PodDisruptionBudget。
- [#9949](https://github.com/emqx/emqx/pull/9949) QUIC 支持多流传输与 TLS。
- [#9932](https://github.com/emqx/emqx/pull/9932) 添加 TDengine 数据桥接。
- [#9967](https://github.com/emqx/emqx/pull/9967) 新增 TLS 配置项 `hibernate_after`，通过在闲置一段时间后休眠 TLS 进程以减少其内存占用，默认： 5s 。

### 修复

- [#10009](https://github.com/emqx/emqx/pull/10009) `GET /trace/:name/log` API 添加 `bytes` 参数校验，长度不超过 32 位有符号整数。
- [#10015](https://github.com/emqx/emqx/pull/10015) 执行 CLI 时，如果节点 cookie 配置错误则快速抛出错误。
  在此修复前，即使 cookie 配置错误，EMQX 命令仍然会尝试去 ping EMQX 节点，
  并得到一个 'Node xxx not responding to pings' 的错误。
  修复后，如果发现 cookie 不一致，立即打印不一致的错误信息并退出。
- [#10020](https://github.com/emqx/emqx/pull/10020) 修复使用异步和批量配置的桥接计数不准确的问题。
- [#10021](https://github.com/emqx/emqx/pull/10021) 修正执行`emqx_ctl cluster join`命令时，目标节点未运行时的错误信息。
- [#10032](https://github.com/emqx/emqx/pull/10032) 当集群中某些节点上的资源仍处于 **初始化/连接中** 状态时，调用 `/bridges` API 获取 metrics 时将因为没有 metrics 数据而崩溃，此修复将忽略没有 metrics 数据的资源。
- [#10041](https://github.com/emqx/emqx/pull/10041) 为 InfluxDB 桥接配置项 `write_syntax` 描述文档添加了整数占位符注释说明。
  另外在配置中支持 `timestamp` 使用一个常量。
- [#10042](https://github.com/emqx/emqx/pull/10042) 当 core 节点离开集群时，改进 replicant 节点的行为。
  以前，直到所有 core 节点全部起来前， replicant 节点无法重新平衡与 core 节点的连接。
  该情况会打印以下日志：
  `[error] line: 182, mfa: mria_lb:list_core_nodes/1, msg: mria_lb_core_discovery divergent cluster`。
- [#10054](https://github.com/emqx/emqx/pull/10054) 修复了对于已有的 Data Bridge 进行测试连接时需要手工输入一遍密码的问题。
- [#10058](https://github.com/emqx/emqx/pull/10058) 优化未使用的 QUIC TLS 选项。
  QUIC 监听器只保留以下 TLS 选项:
  - cacertfile
  - certfile
  - keyfile
  - verify
- [#10076](https://github.com/emqx/emqx/pull/10076) 修复 Webhook 桥接的一个异常处理：连接超时错误发生后，发生错误的请求可以被重试。
  在此修复前，连接超时后，被当作不可重试类型的错误处理，导致请求被丢弃。
- [#10078](https://github.com/emqx/emqx/pull/10078) 修复了无效的 QUIC 监听器设置可能导致 segfault 的问题。
- [#10084](https://github.com/emqx/emqx/pull/10084) 修正将运行不同 EMQX 版本的 core 节点加入集群的问题。
- [#10086](https://github.com/emqx/emqx/pull/10086) HTTP 客户端库 `ehttpc` 升级到 0.4.7。
  在升级前，如果 HTTP 客户端，例如 认证，授权，webhook 等配置中使用了 content-type HTTP 头，但是没有配置 body，则可能会发生异常。
  详情见 [ehttpc PR#44](https://github.com/emqx/ehttpc/pull/44)。
- [#9939](https://github.com/emqx/emqx/pull/9939) 允许 `emqx ctl cluster join` 命令在 Mnesia 启动前调用。
  在此修复前， EMQX 的 `replicant` 类型节点无法使用 `manual` 集群发现策略。
- [#9958](https://github.com/emqx/emqx/pull/9958) 修复 `clients` API 在 Client ID 不存在时返回的错误码和错误提示。
- [#9961](https://github.com/emqx/emqx/pull/9961) 在 bin/emqx 脚本中，避免在运行非启动命令时解析 emqx.conf 来获取节点名称和 cookie。
- [#9974](https://github.com/emqx/emqx/pull/9974) Statsd 和 prometheus 使用跟 Dashboard 相同的内存用量数据源。
  在此修复前，内存的总量和用量统计使用了过时的（在容器环境中不准确）的数据源。
- [#9997](https://github.com/emqx/emqx/pull/9997) 修复生成 Swagger API 时没有遵循[标准](https://swagger.io/specification/)将元数据字段 `deprecated` 设置为布尔值的问题。
- [#10007](https://github.com/emqx/emqx/pull/10007) Kafka 桥接的配置参数 `memory_overload_protection` 默认值从 `true` 改成了 `false`。
  尽管内存过载后消息被丢弃会产生日志和计数，如果没有基于这些日志或计数的告警，系统管理员可能无法及时发现消息被丢弃。
  当前更好的选择是：让管理员显式的配置该项，迫使他们理解这个配置的好处以及风险。
- [#10087](https://github.com/emqx/emqx/pull/10087) 往 InfluxDB 中插入数据时，如果时间戳为空（未定义），则使用默认的占位符 `${timestamp}`。
  在此修复前，如果时间戳字段没有设置，InfluxDB 桥接使用了一个错误的时间戳。

## 5.0.0

*发布日期: 2023-02-03*

EMQX 企业版 5.0 是一个全新的版本。在这个版本中，我们采用了全新的集群架构，并对主要功能进行了改进，同时还引入了大量新功能。

### 集群水平扩展性与可靠性

基于开创性的 Core + Replica 集群架构，EMQX 企业版 5.0 获得了更好的集群水平扩展性和可靠性：

- 单个集群支持至多 23 个节点并能够承载超过 1 亿 MQTT 连接，相比 4.x 版本实现了 [10 倍的接入能力提升](https://www.emqx.com/zh/blog/how-emqx-5-0-achieves-100-million-mqtt-connections)。
- 使用 Core-Replicant 模式部署时，由于 Replicant 节点是无状态的，动态伸缩增减 Replicant 节点数量不会影响集群稳定性。
- 大规模部署时节点脑裂风险以及脑裂后对业务产生的影响显著降低，为企业用户提供更加稳定可靠的服务。

目前 [EMQX Kubernetes Operator](https://www.emqx.com/zh/emqx-kubernetes-operator) 已经针对新集群架构进行了适配，结合新集群架构能够将 EMQX 部署为一个弹性的、无状态的 MQTT 服务。

### MQTT over QUIC，下一代物联网传输协议

QUIC 是下一代互联网协议 HTTP/3 的底层传输协议，它的出现能够大大扩充现有物联网的应用场景。

EMQX 企业版 5.0 将 QUIC 作为传输层引入到 MQTT 中，以满足多样化的应用场景并统一接入设备进行管理。

MQTT over QUIC 适用于位置不固定、或是需要频繁断连不适合做长连接的设备。

多项与 TCP/TLS 测试的对比表明，在弱网与多变的网络环境下，QUIC 都能够满足其高质量、稳定的消息通信需求，弥补了现有 TCP/TLS 传输层的不足。车联网、移动数据采集等场景的用户将从中受益。

现在，用户可以创建一个 MQTT over QUIC 监听器并使用 EMQ 提供的 SDK 接入物联网设备，EMQ 也正在以 OASIS 成员身份推动 MQTT over QUIC 的标准化落地。

更多信息请参考： [从零开始上手 MQTT over QUIC：快速体验下一代物联网标准协议](https://www.emqx.com/zh/blog/getting-started-with-mqtt-over-quic-from-scratch)

### 可视化双向数据集成

EMQX 企业版 5.0 数据集成包括规则引擎与数据桥接功能，能够以灵活、低代码的配置方式进行物联网数据的实时处理和与第三方数据系统的集成。

**Flow Editor 可视化编排规则处理数据流**

通过可视化查看 Flows 页面，改善规则数量较多情况下的维护和管理难度。现在，用户可以清晰看到数据筛选、处理与桥接的整个步骤，并实时监控这一链路中每个步骤的状态。

后续版本 EMQX 将允许用户在 Dashboard 上以拖拽的方式自由编排规则和数据桥接，通过可视化界面将物联网硬件数据流轻松连接在一起，使开发者专注于自有业务开发。

**更灵活的双向数据集成**

除了将设备数据桥接至外部系统外，还能从外部数据系统如另一个 MQTT 服务、Kafka 中桥接数据至 EMQX，并经过规则处理后发送到指定客户端。

双向数据集成适用于云端下发场景，在支撑持续大规模消息实时处理与下发，为物联网业务开发提供了更多的可能性。

**数据桥接磁盘缓存**

为数据桥接集成增加了磁盘缓存功能，数据桥接连接异常的情况下能够将期间产生的消息缓存起来，待连接恢复后继续发送，这为数据集成提供了极佳的可靠性，大大提升业务可用性。

**数据集成支持情况**

EMQX 企业版 5.0 中首批已获支持的数据集成包括 Webhook、MQTT、Kafka、InfluxDB、MySQL、Redis、GCP PubSub 以及 MongoDB，更多数据集成将在后续的维护版本（5.0.x 版本）中陆续加入。

### 全面的安全保障

**更灵活的访问控制**

EMQX 企业版 5.0 提供了用户名/密码、LDAP、JWT、PSK 和 X.509 证书等多种身份认证，以及消息发布订阅授权检查功能。

访问控制功能可以通过 Dashboard 进行配置，无需重启操作即可为整个集群启用访问控制安全设置，拥有更灵活、易用的操作体验。

认证授权还具有详细的统计指标，可以分别统计集群以及单个节点上认证器和授权检查器的执行情况，包括以下数据：

- 允许数：允许通过认证或授权检查的次数
- 拒绝数：拒绝通过认证或授权检查的次数
- 不匹配数：没有找到用户凭证或权限列表的次数
- 当前速率：当前执行速度

通过认证授权统计指标，用户可以及时发现如大量的失败认证/授权检查，及时感知安全系统的异常情况。

**过载保护与速率限制**

引入了过载保护功能，新的速率限制系统也提供了精度更高的分层速率控制支持，能够从客户端、监听器以及节点多个层面限制客户端行为确保应用按照用户预期的负载运行。

两者结合可以避免客户端过于繁忙以及过多的请求流量导致服务不稳定或故障。

### 全新的 Dashboard

采用全新 UI 设计风格的 Dashboard，优化了关键数据和指标数据的显示方式与内容，在提升视觉体验的同时，也提供了更全面、强大、易用的内置功能，为用户使用 EMQX 进行更多物联网业务开发提供了便利。

主要改进如下：

- 提供了访问控制管理页面
- 改进的数据集成管理 UI，支持可视化数据集成查看
- 更强大的热配置功能
- 更多诊断工具，例如慢订阅和在线追踪
- 更多数据管理：能够在 Dashboard 上管理保留消息、延迟发布数据

### 更好的扩展性

**新的插件机制** 允许通过插件包的的形式编译、分发、安装插件，当用户需要扩展功能时，可以下载独立的插件安装包，在 Web 界面完成上传即可进行安装，整个过程甚至不需要重启 EMQX 集群。

同时，一个规范的插件将会随身附带使用说明、插件主页地址等信息，普通用户可以依照说明快速将插件用起来，也为插件开发者提供了与用户沟通的渠道。

**更易用的 ExHook/gRPC 扩展** 允许创建多个 ExHook 实例，并为每个实例提供了详细的使用情况统计，同时还可以查看每个 Exhook 实例注册的钩子以及钩子参数，能够更好地感知 Exhook 扩展负载情况。

**更"原生"的多协议接入**

网关以统一的设计语言重新实现，针对每种协议特性提供不同的客户端管理页面、安全认证配置，让用户以更原生的方式接入。

由于每个网关都可以配置自己独立的认证，不同网关设备的认证信息现在可以相互隔离，满足更多场景的需求。
