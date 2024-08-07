# v5 版本

## 5.7.2

*发布日期：2024-08-07*

### 增强

- [#13317](https://github.com/emqx/emqx/pull/13317) 增加了一种新的针对授权源的指标类型：`ignore`。当授权源尝试对请求进行授权但遇到授权源不适用或出现错误导致无法决定结果的情况时，此指标计数将递增。

- [#13336](https://github.com/emqx/emqx/pull/13336) 使用 CSV 或 JSON 格式的引导文件在空的 EMQX 节点或集群的内置数据库中初始化认证数据。此功能引入了新的配置项 `bootstrap_file` 和 `bootstrap_type`。

- [#13348](https://github.com/emqx/emqx/pull/13348) 在日志配置中新增字段 `payload_encode`，用于确定日志数据中 payload 的格式。

- [#13436](https://github.com/emqx/emqx/pull/13436) 在 JWKS 请求中添加了自定义请求头的选项。

- [#13507](https://github.com/emqx/emqx/pull/13507) 在规则引擎和变量表达式中引入了新的内置函数 `getenv`，用于访问环境变量。此函数遵循以下限制：

  - 在从操作系统环境变量读取之前，会添加前缀 `EMQXVAR_`。例如，`getenv('FOO_BAR')` 将读取 `EMQXVAR_FOO_BAR`。
  - 一旦从操作系统环境加载，这些值就是不可变的。

- [#13521](https://github.com/emqx/emqx/pull/13521) 解决了 LDAP 查询超时将导致底层连接不可用的问题，该问题可能会导致后续查询返回过时结果的情况。此修复确保系统在超时情况下能够自动重新连接。

- [#13528](https://github.com/emqx/emqx/pull/13528) 对数据集成中导致不可恢复错误的事件应用了日志截流。

- [#13548](https://github.com/emqx/emqx/pull/13548) 现在 EMQX 可以选择在通过 REST API 更新插件配置时调用 `on_config_changed/2` 回调函数。预期该回调函数由 `<PluginName>_app` 模块导出。 例如，如果插件名称和版本为 `my_plugin-1.0.0`，则回调函数为 `my_plugin_app:on_config_changed/2`。

- [#13386](https://github.com/emqx/emqx/pull/13386) 支持在空的 EMQX 节点或集群上使用CSV 格式的引导文件初始化封禁客户端列表。用于指定文件路径的相应配置条目是`banned.bootstrap_file`。该文件是一个以`,`为分隔符的CSV文件，第一行必须是标题行。所有有效的标题如下：

  - as :: 必需
  - who :: 必需
  - by :: 可选
  - reason :: 可选
  - at :: 可选
  - until :: 可选

  详细信息请参阅[配置手册](https://docs.emqx.com/zh/enterprise/v@EE_VERSION@/hocon/)中关于每个字段的说明。

  除标题行外，文件中的每一行必须包含与标题行相同数量的列，且列可以省略，其值为`undefined`。

- [#13518](https://github.com/emqx/emqx/pull/13518) 支持在 Kafka 生产者动作的 `topic` 配置中使用模板。

  需确保在 Kafka 中预先存在这些主题。如果消息发送到一个不存在的主题（如果 Kafka 禁用了主题自动创建），则该消息将失败，并显示不可恢复的错误。此外，如果消息缺少足够的信息来匹配配置的模板，也会导致不可恢复的错误。例如，模板为 `t-${t}`，但消息上下文中缺少 `t` 的定义。详细信息，参考 [配置 Kafka 动态主题](../data-integration/data-bridge-kafka.md#配置-kafka-动态主题)。

  此功能也适用于 Azure Event Hubs 和 Confluent 平台的生产者集成。

- [#13504](https://github.com/emqx/emqx/pull/13504) 为 `scram` 认证机制引入了 HTTP 后端。

  该后端实现利用外部网络资源提供 SCRAM 认证数据，包括客户端存储的密钥、服务器密钥和盐。它还支持额外的认证和授权扩展字段，如 `is_superuser`、`client_attrs`、`expire_at` 和 `acl`。

  注意：此增强并非关于 RFC 7804 （盐挑战响应 HTTP 认证机制）规范的实现。

- [#13441](https://github.com/emqx/emqx/pull/13441) 加强了 CoAP 网关连接模式。UDP 连接现在将始终通过 `clientid` 绑定到相应的网关连接。

### Bug Fixes

- [#13222](https://github.com/emqx/emqx/pull/13222) 解决了与`CONNECT`数据包中遗嘱消息相关的标志检查和错误处理问题。 详细的规范，请参考：
  - MQTT-v3.1.1-[MQTT-3.1.2-13], MQTT-v5.0-[MQTT-3.1.2-11]
  - MQTT-v3.1.1-[MQTT-3.1.2-14], MQTT-v5.0-[MQTT-3.1.2-12]
  - MQTT-v3.1.1-[MQTT-3.1.2-15], MQTT-v5.0-[MQTT-3.1.2-13]
- [#13307](https://github.com/emqx/emqx/pull/13307) 更新了 `ekka` 库至版本 0.19.5。该版本的 `ekka` 使用了 `mria` 0.8.8，增强了自动愈合功能。先前的自动愈合仅在所有核心节点可访问时生效。此更新允许在大多数核心节点处于运行状态时应用自动修复。详情请参考 [Mria PR](https://github.com/emqx/mria/pull/180)。
- [#13334](https://github.com/emqx/emqx/pull/13334) 实现了对 MQTT v3.1.1 CONNECT 数据包中 `PasswordFlag` 的严格模式检查，以符合协议规范。注意：此检查仅在严格模式下执行，以确保与现有错误兼容性。

- [#13344](https://github.com/emqx/emqx/pull/13344) 修复了当接收 API 请求的节点未能维持与指定 `clientid` 的连接时，`POST /clients/:clientid/subscribe/bulk` API无法正常工作的问题。
- [#13358](https://github.com/emqx/emqx/pull/13358) 修复了`authn_complete_event` 事件中 `reason` 字段显示不正确的问题。
- [#13375](https://github.com/emqx/emqx/pull/13375) 将 `infinity` 作为默认值添加到监听器配置字段 `max_conn_rate`、`messages_rate `和 `bytes_rate。
- [#13382](https://github.com/emqx/emqx/pull/13382) 更新了 `emqtt` 库至版本 0.4.14，解决了 `emqtt_pool` 无法重用处于不一致状态的池的问题。
- [#13389](https://github.com/emqx/emqx/pull/13389) 修复了 `pbkdf2` 中`Derived Key Length` 可能被设置为负整数的问题。
- [#13389](https://github.com/emqx/emqx/pull/13389) 修复了授权规则中可能会错误解析主题的问题。
- [#13393](https://github.com/emqx/emqx/pull/13393) 修复了节点加入集群后插件应用程序无法重新启动的问题，导致钩子未能正确安装并引起状态不一致。
- [#13398](https://github.com/emqx/emqx/pull/13398) 修复了使用命令行重新加载用于授权的内置数据库时，ACL 规则被错误地清除的问题。
- [#13403](https://github.com/emqx/emqx/pull/13403) 解决了一个安全问题，该问题导致环境变量配置覆盖会意外记录密码。此修复确保环境变量中的密码不会被记录。
- [#13408](https://github.com/emqx/emqx/pull/13408) 解决了因认证尝试中使用无效盐或密码类型而触发的 `function_clause` 崩溃问题。此修复增强了错误处理，更好地处理涉及不正确盐或密码类型的认证失败。
- [#13419](https://github.com/emqx/emqx/pull/13419) 修复了由 `/configs` API 引起崩溃的日志消息内容显示混乱的问题，此修复确保与 API 调用相关的日志消息清晰易懂。
- [#13422](https://github.com/emqx/emqx/pull/13422) 解决了无法将选项 `force_shutdown.max_heap_size` 设置为 0 以禁用此调试的问题。
- [#13442](https://github.com/emqx/emqx/pull/13442) 解决了动作和 Source 的健康检查间隔配置未被遵守的问题。之前，EMQX 忽略了为动作指定的健康检查间隔，并使用连接器的间隔。此修复确保 EMQX 现在正确使用为动作/ Source 配置的健康检查间隔，允许独立和准确的健康监控频率。
- [#13503](https://github.com/emqx/emqx/pull/13503) 修复了连接器在初始启动时未遵循配置的健康检查间隔的问题，需要更新或重启以应用正确的间隔。
- [#13515](https://github.com/emqx/emqx/pull/13515) 修复了当节点因某些原因宕机时，同一客户端无法订阅相同的排它主题的问题。
- [#13527](https://github.com/emqx/emqx/pull/13527) 在规则引擎中修复了一个问题，即当 `$bridges/...` 被包含在 `FROM` 子句中时，执行消息发布事件的 SQL 测试始终返回空结果。
- [#13541](https://github.com/emqx/emqx/pull/13541) 修复了禁用监听器的 CRL 检查需要监听器重启才能生效的问题。
- [#13305](https://github.com/emqx/emqx/pull/13305) 改进了 Redis 连接器的错误处理。之前，如果 Redis 连接器的 Redis 模式设置为 `single` 或 `sentinel`，且未提供用户名或密码，那么在 Dashboard 中进行连接器测试时会始终遇到连接超时错误。此更新确保用户现在能在这种情况下收到详细的错误信息。此外，还为所有 Redis 连接器类型添加了更详细的错误信息，以增强诊断和故障排除能力。
- [#13327](https://github.com/emqx/emqx/pull/13327) 修复了 Kafka、Confluent 和 Azure Event Hubs 集成中的问题，如果多个动作复用同一个 connector 并配置了相同的主题，当删除或禁用其中一个动作时可能会相互干扰，比如影响其他动作的数据写入。
- [#13345](https://github.com/emqx/emqx/pull/13345) 改进了 Schema Registry 的错误消息清晰度，现在在创建 Schema 时，如果名称超出长度限制或包含无效格式，将提供更明确的反馈。
- [#13420](https://github.com/emqx/emqx/pull/13420) 增加了对 Schema 验证配置的校验，防止在配置 Schema 验证时使用空主题过滤器列表。此前允许空列表可能导致创建缺乏实际功能的消息转换，因为它们不会应用于任何特定主题。
- [#13543](https://github.com/emqx/emqx/pull/13543) 修复了在 Schema Registry 中删除或更新 schema 后，Protobuf schemas 的内部缓存未能正确清理的问题。
- [#13332](https://github.com/emqx/emqx/pull/13332) 改进了错误消息，当 Amazon S3 连接器配置错误时，提供更具信息量且易于阅读的细节。
- [#13552](https://github.com/emqx/emqx/pull/13552) 添加了 EMQX 插件的启动超时限制，默认超时时间为 10 秒。在此更新之前，问题插件在启动期间可能会导致运行时错误，从而可能导致主启动进程在 EMQX 停止和重新启动时出现挂起。

## 5.7.1

*发布日期: 2024-06-26*

### 增强

- [#12983](https://github.com/emqx/emqx/pull/12983) 添加了新的规则引擎事件 `$events/client_check_authn_complete`，用于认证完成事件。

- [#13175](https://github.com/emqx/emqx/pull/13175) 为基于 Postgres 的连接器添加了 `disable_prepared_statements` 选项。

  此选项适用于不支持像预处理语句这样的会话功能的端点，例如 PGBouncer 和 Transaction 模式下的 Supabase。

- [#13180](https://github.com/emqx/emqx/pull/13180) 改进了 EMQX 在 Erlang/OTP 26 上运行时客户端消息处理性能，并在 fan-in 模式下将消息吞吐量提高了10%。

- [#13191](https://github.com/emqx/emqx/pull/13191) 将 EMQX Docker 镜像升级至运行在Erlang/OTP 26上。

  自 v5.5 版本以来，EMQX 一直在Erlang/OTP 26上运行，但 Docker 镜像仍在 Erlang/OTP 25 上。现在所有版本都升级到 Erlang/OTP 26。此升级解决了以下已知问题：

  当 EMQX 的旧版本加入含有新版本节点的集群时，旧版本节点的 Schema Registry 可能会遇到问题，日志如下：

  ```
  Error loading module '$schema_parser___CiYAWBja87PleCyKZ58h__SparkPlug_B_BUILT-IN':,
  This BEAM file was compiled for a later version of the runtime system than the current (Erlang/OTP 25).
  ```

  新版本已修复此问题。然而，对于旧版本，需要一个手动步骤。在旧版本 EMQX 加入集群之前，在集群的一个节点上执行以下命令：

  ```shell
  emqx eval 'lists:foreach(fun(Key) -> mnesia:dirty_delete(emqx_ee_schema_registry_protobuf_cache_tab, Key) end, mnesia:dirty_all_keys(emqx_ee_schema_registry_protobuf_cache_tab)).'
  ```

  如果旧版本的 EMQX 已经在集群中，请执行上述命令并重启受影响的节点。

- [#13242](https://github.com/emqx/emqx/pull/13242) 显著提高了 EMQX Dashboard 监听器的启动速度。

- [#13172](https://github.com/emqx/emqx/pull/13172) 添加了规则函数 `map_to_redis_hset_args`，帮助准备 redis HSET（或HMSET）多字段值。

  例如，如果 `payload.value` 是多个数据字段的映射， 此规则 `SELECT map_to_redis_hset_args(payload.value) as hset_fields FROM "t/#"` 可以准备 `hset_fields` 为redis动作来渲染命令模板如 `HMSET name1 ${hset_fields}`。

- [#13210](https://github.com/emqx/emqx/pull/13210) EMQX 现在能够验证 Schema Registry 中是否存在在插入或更新 Schema 验证时引用的 schema 和消息类型。

- [#13211](https://github.com/emqx/emqx/pull/13211) 增强了 TLS 监听器以支持更灵活的 TLS 验证。

- - `partial_chain` 支持：如果设置了 `partial_chain` 选项为 `true`，则允许具有不完整证书链的连接。更多详情请查看[配置手册](https://docs.emqx.com/en/enterprise/v@EE_VERSION@/hocon/)。
  - 证书密钥用途验证：新增了对必需的扩展密钥用途的支持，如在 [rfc5280](https://www.rfc-editor.org/rfc/rfc5280#section-4.2.1.12) 中定义。引入了一个新选项（`verify_peer_ext_key_usage`），在 TLS 握手期间强制执行对等证书中的特定密钥用途（如 "serverAuth"），以增强安全性，确保证书用于其预定目的，例如 "serverAuth,OID:1.3.6.1.5.5.7.3.2"。更多详情请查看[配置手册](https://docs.emqx.com/en/enterprise/v@EE_VERSION@/hocon/)。

- [#13274](https://github.com/emqx/emqx/pull/13274) RocketMQ 连接器现在支持配置 SSL 设置。

### 修复

- [#13156](https://github.com/emqx/emqx/pull/13156) 解决了更新至 EMQX v5.7.0 后，Dashboard 的监控页面崩溃的问题。

- [#13164](https://github.com/emqx/emqx/pull/13164) 修复了 HTTP 授权请求体编码问题。原本，HTTP 授权请求体的编码格式依据 `accept` 头部来设置。现在调整为根据 `content-type` 头部来确定编码格式。同时，为了兼容 v4 版本，添加了 `access` 模板变量。其中，SUBSCRIBE 操作的访问代码为 `1`，PUBLISH 操作的访问代码为 `2`。

- [#13238](https://github.com/emqx/emqx/pull/13238) 优化了在返回不支持的内容类型头部的 HTTP 授权请求时的错误消息记录。

- [#13258](https://github.com/emqx/emqx/pull/13258) 修复了 MQTT-SN 网关因依赖启动顺序错误而无法正确重启的问题。

- [#13273](https://github.com/emqx/emqx/pull/13273) 修复并改进了在以下几个配置中对 URI 的处理，具体包括：

  - 认证和授权配置：纠正了先前的错误，之前错误地拒绝了没有路径的有效 URI，如 `https://example.com?q=x`。这些 URI 现在被正确识别为有效。

  - 连接器配置：增强了检查，确保不再错误接受包含潜在问题组件的 URI，如用户信息或片段部分。

- [#13276](https://github.com/emqx/emqx/pull/13276) 修复了在设置新的存储代时，部分内部存储状态未被正确持久化的问题。这里的“代”是内部用于管理消息过期和清理的一个关键概念。此问题可能会导致EMQX重启后消息丢失。

- [#13291](https://github.com/emqx/emqx/pull/13291) 修复了一个问题，错误地将已宕机的持久存储站点报告为正常在线的情况。

- [#13290](https://github.com/emqx/emqx/pull/13290) 修复了使用 `$ bin/emqx ctl rules show rule_0hyd` 命令显示带有数据集成动作的规则时无输出的问题。

- [#13293](https://github.com/emqx/emqx/pull/13293) 通过自动重新索引导入到备份文件的保留消息，改进了数据备份恢复过程。之前，导入数据备份文件后需要手动使用 `emqx ctl retainer reindex start` 命令进行重新索引。

  此修复还扩展了功能，当 `retainer.backend.storage_type` 配置为 `ram` 时，现在也支持将保留消息导出到备份文件。之前，只有将存储类型配置为 `disc` 的设置支持导出保留消息。

- [#13147](https://github.com/emqx/emqx/pull/13147) 通过添加清晰的描述，改善了规则引擎 protobuf 解码功能中解码失败的错误消息，以指示消息解码失败的具体原因。

- [#13140](https://github.com/emqx/emqx/pull/13140) 修复了导致消息重发布动作的文本追踪崩溃并无法正确显示的问题。

- [#13148](https://github.com/emqx/emqx/pull/13148) 修复了在等待资源连接时 `/connectors/:connector-id/start` 可能返回 500 HTTP 状态码的问题。

- [#13181](https://github.com/emqx/emqx/pull/13181) 当尝试停止连接器操作超时时，EMQX 现在会强制关闭连接器进程。此修复还提高了当底层连接器无响应导致无法停用动作或源时，错误消息的清晰性和准确性。

- [#13216](https://github.com/emqx/emqx/pull/13216) 调整了 MQTT 桥接中 `clientid_prefix` 配置的处理方式。自 EMQX v5.4.1 起，MQTT 客户端 ID 的长度被限制为最多 23 字节。之前，系统将 `clientid_prefix` 计入原始更长客户端 ID 的哈希值中，这影响了最终的 ID 缩短处理。此修复涵盖以下具体变更：

  - 无前缀情况：处理方式保持不变。EMQX 对超过 23 字节的长客户端 ID 进行哈希处理，以符合 23 字节的限制。
  - 带前缀情况：
    - 前缀长度 ≤ 19 字节：保留前缀，并将客户端 ID 的剩余部分缩减到 4 字节，确保总长度不超过 23 字节。
    - 前缀长度 ≥ 20 字节：EMQX 将不尝试缩短客户端 ID，而是完全保留设定的前缀，无论其长度如何。

- [#13189](https://github.com/emqx/emqx/pull/13189) 修复了在与 Microsoft SQL Server 或 MySQL 的数据集成中，无法使用包含子字符串 `values` 的表名或列名的 SQL 模板的问题。

- [#13070](https://github.com/emqx/emqx/pull/13070) 优化了 Kafka 连接器的错误日志，增加了捕获特定错误详情（如无法连接的 advertised listeners）以提供更多诊断信息。为了控制日志的详细程度，只记录第一次出现的错误，并附上类似错误的总计数。

- [#13093](https://github.com/emqx/emqx/pull/13093) 提高了 Kafka 消费者群组的稳定性。在此更改之前，Kafka 消费者群组在 Kafka 群组协调器重启后有时需要重平衡两次。

- [#13277](https://github.com/emqx/emqx/pull/13277) 改善了 Kafka 生产者在遇到 `message_too_large` 错误时的错误处理。之前，Kafka 生产者会反复尝试重发超大消息批次，希望服务器端调整 `max.message.bytes`。

  现在，超大消息会自动分割成单条消息批次进行重试。如果消息仍超出大小限制，则将其丢弃以维持数据流。

- [#13130](https://github.com/emqx/emqx/pull/13130) 改进了 Redis 动作批量请求的跟踪消息格式。现在命令组件之间添加了空格，并在命令之间添加了分号，使跟踪消息更易于阅读。

- [#13136](https://github.com/emqx/emqx/pull/13136) 改善了 Oracle 动作的模板渲染跟踪，以提高可读性。

- [#13197](https://github.com/emqx/emqx/pull/13197) 修复了在 AWS S3 数据集成中的一个问题，该问题阻止了通过 Dashboard UI 或 Connector API 提供的 TLS 证书和密钥文件自动保存到文件系统。

- [#13227](https://github.com/emqx/emqx/pull/13227) 修复了在聚合模式下运行的 AWS S3 Sink 中的一个问题。在修复之前，配置中的无效键模板在 Sink 设置过程中被报告为错误，但实际上导致了一系列难以恢复的崩溃。

## 5.7.0

*发布日期: 2024-05-27*

### 增强

#### MQTT

实现了会话持久化（Durable Sessions）。将 MQTT 持久会话（Persistent Session）及其消息存储到磁盘上，并在 EMQX 集群的多个节点之间持续复制会话元数据和 MQTT 消息，实现了有效的故障转移和恢复机制，确保服务的连续性和高可用性，从而提高系统的可靠性。

向 Prometheus 添加了与 EMQX 持久存储相关的指标：

- `emqx_ds_egress_batches`
- `emqx_ds_egress_batches_retry`
- `emqx_ds_egress_batches_failed`
- `emqx_ds_egress_messages`
- `emqx_ds_egress_bytes`
- `emqx_ds_egress_flush_time`
- `emqx_ds_store_batch_time`
- `emqx_ds_builtin_next_time`
- `emqx_ds_storage_bitfield_lts_counter_seek`
- `emqx_ds_storage_bitfield_lts_counter_next`
- `emqx_ds_storage_bitfield_lts_counter_collision`

注意：这些指标仅在启用会话持久化时可见。

Dashboard 上也增加了持久消息的数量。

更多关于会话持久化功能的详细信息，参见 [MQTT 会话持久化](../durability/durability_introduction.md)。

#### 安全

[#12947](https://github.com/emqx/emqx/pull/12947) 对于 JWT 认证，支持新的 `disconnect_after_expire` 选项。启用时，客户端将在 JWT token 过期后断开连接。

注意：这是一个不兼容变更。此选项默认启用，因此默认行为已更改。以前，带有实际 JWT 的客户端可以连接到服务器并在 JWT token 过期后保持连接。现在，客户端将在 JWT token 过期后断开连接。要保留以前的行为，请将 `disconnect_after_expire` 设置为 `false`。

#### 数据处理和集成

- [#12711](https://github.com/emqx/emqx/pull/12711) 实现了 Schema 验证功能。通过此功能，一旦为某些主题过滤器配置了验证，发布的消息将进行配置的检查。如果未通过验证，消息将被丢弃，根据配置客户端可能会被断开连接。更多关于该功能的详细信息，参见 [Schema 验证](../data-integration/schema-validation.md)。
- [#12899](https://github.com/emqx/emqx/pull/12899) RocketMQ 数据集成添加了命名空间和键调度策略的支持。
- [#12671](https://github.com/emqx/emqx/pull/12671) 在规则引擎 SQL 语言中添加了一个 `unescape` 函数，用于处理字符串中转义序列的展开。之所以添加这个功能，是因为 SQL 语言中的字符串字面量不支持任何转义码（例如 `\n` 和 `\t`）。这一增强功能使得在 SQL 表达式中对字符串进行更灵活的处理成为可能。
- [#12898](https://github.com/emqx/emqx/pull/12898) IoTDB 数据集成支持 1.3.0 版本和批量插入（batch_size/batch_time）选项。
- [#12934](https://github.com/emqx/emqx/pull/12934) 为 AWS S3 数据集成添加了 CSV 格式文件聚合功能。

#### 可观测性

- [#12827](https://github.com/emqx/emqx/pull/12827) 现在可以使用新的规则 ID 追踪过滤器以及客户端 ID 过滤器来追踪规则。为测试目的，还可以使用新的 HTTP API （rules/:id/test）来模拟测试规则，并支持在渲染动作参数后停止其写入操作。
- [#12863](https://github.com/emqx/emqx/pull/12863) 现在可以通过在创建追踪模式时将格式化参数设置为 "json"，将追踪日志条目格式化为 JSON 对象。

#### 扩展

- [#12872](https://github.com/emqx/emqx/pull/12872) 实现了客户端属性功能。允许使用键值对的方式为每个客户端设置额外的属性。属性值可以从 MQTT 客户端连接信息（如用户名、客户端 ID、TLS 证书）处理生成，也可以从认证成功返回的附带的数据中设置。属性可以用于 EMQX 的认证授权、数据集成和 MQTT 扩展功能等功能中。相较于直接使用客户端 ID 等静态属性，客户端属性能够更灵活的用在各类业务场景中，并简化开发流程，增强开发工作的适应性和效率。

  **初始化客户端属性**

  `client_attrs` 字段可以从以下 `clientinfo` 字段之一初始填充：

  - `cn`: TLS 客户端证书中的通用名称。
  - `dn`: TLS 客户端证书中的专有名称，即证书的 "Subject"。
  - `clientid`: 客户端提供的 MQTT 客户端 ID。
  - `username`: 客户端提供的用户名。
  - `user_property`: 从 MQTT CONNECT 数据包的 'User-Property' 中提取属性值。

  **通过认证响应扩展**

  可以从认证响应中合并额外的属性到 `client_attrs` 中。支持的认证后端包括：

  - **HTTP**：可以通过 `client_attrs` 字段将属性包含在 HTTP 响应体的 JSON 对象中。
  - **JWT**：可以通过 JWT 中的 `client_attrs` 声明包含属性。

  **在认证和授权中的使用**

  如果在认证之前初始化了 `client_attrs`，它可以在外部认证请求中使用。例如，`${client_attrs.property1}` 可以在请求模板中使用，用于指向 HTTP 服务器进行真实性验证。

  - `client_attrs` 可以在授权配置或请求模板中使用，增强灵活性和控制。例如：

    在 `acl.conf` 中，使用 `{allow, all, all, ["${client_attrs.namespace}/#"]}` 来基于 `namespace` 属性应用权限。

  - 在其他授权后端中，可以在请求模板中使用 `${client_attrs.namespace}` 动态包含客户端属性。

  更多关于客户端属性功能的详细信息，参见[客户端属性](../client-attributes/client-attributes.md)。

- [#12910](https://github.com/emqx/emqx/pull/12910) 添加了插件配置管理和 schema 验证功能。还可以使用元数据注释 schema，以便在 Dashboard 中进行 UI 渲染。更多详细信息请参见[插件模板](https://github.com/emqx/emqx-plugin-template/pull/126)和[插件文档](../extensions/plugins.md)。

#### 运维和管理

<!-- This is not ready to GA in 5.7

- [#12798](https://github.com/emqx/emqx/pull/12798) 新增 `GET /api/v5/clients_v2` API，该 API 使用游标代替页码进行分页。这样比旧的 API 端点更高效，因为旧的 API 会多次遍历表。

  -->


- [#12923](https://github.com/emqx/emqx/pull/12923) 在将错误格式导入内置认证数据库时提供了更具体的错误信息。

- [#12940](https://github.com/emqx/emqx/pull/12940) 向 `PUT /configs` API 添加了 `ignore_readonly` 参数。

  在此更改之前，如果原始配置包含只读根键（`cluster`、`rpc` 和 `node`），EMQX 将返回 400（BAD_REQUEST）。

  这一增强功能后，可以调用 `PUT /configs?ignore_readonly=true`，在这种情况下 EMQX 将忽略只读根配置键并应用其余配置。出于可观察性目的，如果丢弃了任何只读键，会记录一条信息级别的日志。

  还修复了配置中存在错误的 HOCON 语法时出现的异常（返回 500）。现在错误的语法将导致 API 返回 400（BAD_REQUEST）。

- [#12957](https://github.com/emqx/emqx/pull/12957) 开始为 macOS 14（Apple Silicon）和 Ubuntu 24.04 Noble Numbat（LTS）构建包。

### 修复

#### 安全

- [#12887](https://github.com/emqx/emqx/pull/12887) 修复了使用 SASL SCRAM 的 MQTT 增强型身份验证。
- [#12962](https://github.com/emqx/emqx/pull/12962) TLS 客户端现在可以使用通配符证书验证服务器主机名。例如，如果证书是为主机 `*.example.com` 颁发的，则 TLS 客户端可以验证类似 `srv1.example.com` 的服务器主机名。

#### MQTT

- [#12996](https://github.com/emqx/emqx/pull/12996) 修复了 `emqx_retainer` 应用程序中的进程泄漏问题。以前，当接收到保留消息时客户端断开连接，可能会导致进程泄漏。

#### 数据处理和集成

- [#12653](https://github.com/emqx/emqx/pull/12653) 规则引擎函数 `bin2hexstr` 现在支持位字符串输入的位数不是8的倍数。规则引擎函数 `subbits` 可能返回这样的位字符串。

- [#12657](https://github.com/emqx/emqx/pull/12657) 规则引擎基于 SQL 的语言以前不允许在数组文字中将任何表达式作为数组元素（只允许常量和变量引用）。现在已修复此问题，可以将任何表达式用作数组元素。

  例如，现在可以执行以下操作：

  ```bash
  select
  [21 + 21, abs(-abs(-2)), [1 + 1], 4] as my_array
  from "t/#"
  ```

<!-- This is a fix for not new feature in this release

- [#12707](https://github.com/emqx/emqx/pull/12707) 在数据库中保留持久客户端会话的 IP 地址和端口。

  -->

- [#12932](https://github.com/emqx/emqx/pull/12932) 以前，如果 HTTP 操作请求收到 503（服务不可用）状态，它将被标记为失败，并且请求不会重试。现在已修复此问题，使得请求将重试配置的次数。

- [#12948](https://github.com/emqx/emqx/pull/12948) 修复了更新连接器后敏感的 HTTP 标头值（如 `Authorization`）被 `******` 替换的问题。

- [#12895](https://github.com/emqx/emqx/pull/12895) 为 DynamoDB 连接器和动作补充了一些必要但遗漏的键。

<!-- This is a fix for not new feature in this release

- [#12950](https://github.com/emqx/emqx/pull/12950) 添加了一个验证以防止配置 schema 验证时出现重复的主题。

  -->

- [#13018](https://github.com/emqx/emqx/pull/13018) 在连接断开时减少了 Postgres/Timescale/Matrix 连接器的日志垃圾。
- [#13118](https://github.com/emqx/emqx/pull/13118) 修复了规则引擎模板渲染中的性能问题。

#### 可观测性

- [#12765](https://github.com/emqx/emqx/pull/12765) 确保统计信息 `subscribers.count` 和 `subscribers.max` 包含共享订阅者。先前它只包含非共享订阅者。

#### 运维和管理

- [#12812](https://github.com/emqx/emqx/pull/12812) 将资源健康检查操作改为非阻塞操作。这意味着更新或删除资源等操作不会被长时间运行的健康检查阻塞。
- [#12830](https://github.com/emqx/emqx/pull/12830) 将通道（操作/源）健康检查操作改为非阻塞操作。这意味着更新或删除一个动作/ source 数据集成等操作不会被长时间运行的健康检查阻塞。

<!-- This is a fix for not new feature in this release

- [#12874](https://github.com/emqx/emqx/pull/12874) 在会话重新连接前修改订阅时，确保持久消息重新发送的一致性：

  - 持久会话保存接收到的 QoS2 消息的飞行数据包 ID。
  - 确保持久会话与非持久会话在重叠订阅方面的行为一致。
  - 在 REST API 中列出持久订阅。

    -->

- [#12993](https://github.com/emqx/emqx/pull/12993) 修复了处理未知 zone 时的监听器配置更新 API。

  在此修复之前，当使用未知 zone 更新监听器配置时，例如 `{"zone": "unknown"}`，更改会被接受，导致所有客户端连接时崩溃。 在此修复后，使用未知 zone 名称更新监听器将获得 “Bad request” 响应。

- [#13012](https://github.com/emqx/emqx/pull/13012) MQTT 监听器配置选项 `access_rules` 已通过以下方式进行改进：

  - 如果配置了无效的访问规则，监听器不再以难以理解的错误消息崩溃。而是生成配置错误。
  - 现在可以通过逗号分隔在单个字符串中添加多个规则（例如，“allow 10.0.1.0/24，deny all”）。

- [#13041](https://github.com/emqx/emqx/pull/13041) 改进了 HTTP 认证错误日志消息。如果 POST 方法缺少 HTTP 内容类型标头，它现在会发出一个有意义的错误消息，而不是一个不太可读的带有堆栈跟踪的异常。

- [#13077](https://github.com/emqx/emqx/pull/13077) 此修复使 EMQX 只在连接器启动/重新启动时从全局配置中读取动作配置，并在连接器中存储动作的最新配置。之前更新到动作配置有时不会生效，需要禁用并启用动作才能生效。这意味着，即使动作配置看起来已成功更新，动作有时也可能使用旧的（先前的）配置运行。

- [#13090](https://github.com/emqx/emqx/pull/13090) 如果动作或 source 的连接器被停用，尝试启动它们时将不再尝试启动连接器本身。

- [#12871](https://github.com/emqx/emqx/pull/12871) 修复了被疏散节点的启动过程。以前，如果一个节点被疏散并在没有停止疏散的情况下停止了节点，节点将无法重新启动。

- [#12888](https://github.com/emqx/emqx/pull/12888) 修复了在导入备份数据后丢失 License 相关配置的问题。

#### 网关

- [#12909](https://github.com/emqx/emqx/pull/12909) 修复了 UDP 监听器进程在出现错误或关闭时的处理。修复确保 UDP 监听器在需要时能够干净地停止和重新启动。
- [#13001](https://github.com/emqx/emqx/pull/13001) 修复了 syskeeper 转发器在连接丢失时永远不会重新连接的问题。
- [#13010](https://github.com/emqx/emqx/pull/13010) 修复了 JT/T 808 网关在请求注册服务进行身份验证失败时无法正确回复 REGISTER_ACK 消息的问题。

## 5.6.1

*发布日期：2024-04-18*

### 修复

- [#12759](https://github.com/emqx/emqx/pull/12759) EMQX 现在会自动删除由于 shcema 验证错误而上传失败的无效备份文件。此修复确保只显示和存储有效的配置文件，提升系统可靠性。

- [#12766](https://github.com/emqx/emqx/pull/12766) 将 `message_queue_too_long` 错误原因重命名为 `mailbox_overflow`，与对应的配置参数 `force_shutdown.max_mailbox_size` 保持一致。

- [#12773](https://github.com/emqx/emqx/pull/12773) 升级了 HTTP 客户端库。

  HTTP 客户端库（`gun-1.3`）在标准端口（`http` 为 80 端口，`https` 为 443 端口）错误地在 `Host` 标头添加了 `:portnumber` 后缀。这可能导致与执行严格 `Host` 标头检查的服务器或网关（例如，AWS Lambda、阿里云 HTTP 网关）的兼容性问题，从而引发 `InvalidCustomDomain.NotFound` 或 "指定的 CustomDomain 不存在" 等错误。

- [#12802](https://github.com/emqx/emqx/pull/12802) 改进了 EMQX 通过 `emqx ctl cluster leave` 命令处理节点从集群中移除的方式。之前，如果配置的集群 `discovery_strategy` 不是 `manual`，节点可能会无意中重新加入同一个集群（除非它被停止）。最新的更新中，执行 `cluster leave` 命令现在会自动禁用节点的集群节点发现功能，防止它重新加入。要重新启用集群节点发现，请使用 `emqx ctl discovery enable` 命令或简单地重启节点。

- [#12814](https://github.com/emqx/emqx/pull/12814) 改进了 EMQX 中 `/clients/{clientid}/mqueue_messages` 和 `/clients/{clientid}/inflight_messages` API 的错误处理。这些更新包括：

  - **内部超时**：如果 EMQX 在默认的 5 秒超时内无法检索到 Inflight 或 Mqueue 消息列表（这通常在系统负载较重时发生），API 将返回 500 错误，响应为 `{"code":"INTERNAL_ERROR","message":"timeout"}`，并记录额外的信息以便排错。
  - **客户端关闭**：如果在 API 调用期间客户端连接被终止，API 现在将返回 404 错误，响应为 `{"code": "CLIENT_SHUTDOWN", "message": "Client connection has been shutdown"}`。这确保了在客户端连接中断时提供更清晰的反馈。

- [#12824](https://github.com/emqx/emqx/pull/12824) 更新了统计指标 `subscribers.count` 和 `subscribers.max`，以包括共享订阅者。之前，这些指标仅计算非共享订阅者。

- [#12826](https://github.com/emqx/emqx/pull/12826) 修复了 EMQX 中与数据集成 Source 和保留消息的导入功能相关的问题。在此更新之前：

  - 备份文件中指定的数据集成 Source 未被导入。这包括 `sources.mqtt` 类别下的特定连接器和参数，如 QoS 和主题。
  - 不支持导入用于保留消息的 `mnesia` 表。

- [#12843](https://github.com/emqx/emqx/pull/12843) 修复了在执行 `emqx ctl cluster leave` 命令后，在复制节点上的 `cluster_rpc_commit` 事务 ID 清理程序。以前，未能适当清除这些事务 ID 阻碍了核心节点上的配置更新。

- [#12882](https://github.com/emqx/emqx/pull/12882) 修复了 EMQX 数据集成中 RocketMQ 动作的问题，确保消息正确路由到其配置的主题。之前，当多个动作共享单个 RocketMQ 连接器时，所有消息错误地发送到了第一个配置的主题。此修复为每个主题启动一组独立的 RocketMQ 工作进程，防止跨主题消息传递错误。

- [#12885](https://github.com/emqx/emqx/pull/12885) 修复了 EMQX 中用户无法在 Dashboard 的 "监控" 菜单下查看 "保留消息" 的问题。

  "保留消息" 后端 API 使用 `qlc` 库。这个问题是由于权限问题引起的，`qlc` 库的 `file_sorter` 功能试图使用不可写的目录 `/opt/emqx` 存储临时文件，这是由于 Docker 部署中目录所有权权限的最近更改所致。

  此更新修改了 `/opt/emqx` 目录的所有权设置为 `emqx:emqx`，确保所有必要的操作，包括保留消息检索，可以在没有访问错误的情况下进行。

## 5.6.0

*发布日期：2024-03-28*

### 增强

- [#12326](https://github.com/emqx/emqx/pull/12326) 通过使用注册历史增强了会话跟踪。EMQX 现在能够监控会话注册的历史，包括那些已过期的会话。通过配置 `broker.session_history_retain`，EMQX 保留了指定时间内过期会话的记录。

  - **会话计数 API**：使用 API `GET /api/v5/sessions_count?since=1705682238` 获取自给定 UNIX 纪元时间戳（精确到秒）以来保持活跃的集群内会话计数。这一增强有助于分析一段时间内的会话活动。

  - 使用集群会话指标进行扩展：添加了新的指标 `cluster_sessions`，以更好地跟踪集群内的会话数量。此指标也集成到 Prometheus 中以便于监控：

    ```
    # TYPE emqx_cluster_sessions_count gauge
    emqx_cluster_sessions_count 1234
    ```

    注意：请将此指标视为近似估计。由于数据收集和计算的异步性，精确度可能会有所不同。

- [#12398](https://github.com/emqx/emqx/pull/12398) 在 Dashboard 配置中暴露了 `swagger_support` 选项，允许启用或禁用 Swagger API 文档。

- [#12467](https://github.com/emqx/emqx/pull/12467) 开始支持使用 AAAA DNS 记录类型进行集群发现。

- [#12483](https://github.com/emqx/emqx/pull/12483) 将 `emqx ctl conf cluster_sync tnxid ID` 重命名为 `emqx ctl conf cluster_sync inspect ID`。为了向后兼容，保留了 `tnxid`，但将在 5.7 版本中废弃。

- [#12495](https://github.com/emqx/emqx/pull/12495) 新增了 AWS S3 连接器和动作。

- [#12499](https://github.com/emqx/emqx/pull/12499) 通过扩展规则增强了客户端封禁能力，包括：

  - 将 `clientid` 与指定的正则表达式进行匹配。
  - 将客户端的 `username` 与指定的正则表达式进行匹配。
  - 将客户端的对等地址与 CIDR 范围进行匹配。

  **重要提示**：实施大量广泛匹配规则（不特定于单个 clientid、username 或主机）可能会影响系统性能。建议谨慎使用这些扩展封禁规则以保持最佳系统效率。

- [#12509](https://github.com/emqx/emqx/pull/12509) 实现 API 重新排序所有认证器/授权源。

- [#12517](https://github.com/emqx/emqx/pull/12517) 升级了配置文件以适应多行字符串值，保留缩进以提高可读性和可维护性。这一改进使用 `"""~` 和 `~"""` 标记来引用缩进行，为定义复杂配置提供了一种结构化和清晰的方式。例如：

  ```
  rule_xlu4 {
    sql = """~
      SELECT
        *
      FROM
        "t/#"
    ~"""
  }
  ```

  有关详细信息，请参阅 [HOCON 0.42.0](https://github.com/emqx/hocon/releases/tag/0.42.0) 发行说明。

- [#12520](https://github.com/emqx/emqx/pull/12520) 实现了日志节流。该特性通过在配置的时间窗口内只保留事件第一次发生的记录，减少了可能泛滥系统的日志事件量。 对以下关键且容易重复的日志事件应用了日志节流：

  - `authentication_failure`
  - `authorization_permission_denied`
  - `cannot_publish_to_topic_due_to_not_authorized`
  - `cannot_publish_to_topic_due_to_quota_exceeded`
  - `connection_rejected_due_to_license_limit_reached`
  - `dropped_msg_due_to_mqueue_is_full`

- [#12561](https://github.com/emqx/emqx/pull/12561) 使用 HTTP API 获取客户端未确认消息和消息队列（mqueue）消息。这些 API 有助于深入了解和有效控制消息队列和未确认消息，确保消息处理和监控的效率。

  获取第一批数据：

  - `GET /clients/{clientid}/mqueue_messages?limit=100`
  - `GET /clients/{clientid}/inflight_messages?limit=100`

  或者，获取第一批数据而不指定起始位置：

  - `GET /clients/{clientid}/mqueue_messages?limit=100&position=none`
  - `GET /clients/{clientid}/inflight_messages?limit=100&position=none`

  获取下一批数据：

  - `GET /clients/{clientid}/mqueue_messages?limit=100&position={position}`
  - `GET /clients/{clientid}/inflight_messages?limit=100&position={position}`

  其中 `{position}` 是上一个响应中 `meta.position` 字段的值（不透明字符串令牌）。

  排序和优先级：

  - **Mqueue 消息**：这些消息基于它们在队列中的顺序（FIFO）进行排序和优先级划分，从高优先级到低优先级。默认情况下，mqueue 消息具有统一的优先级，为 0。
  - **未确认消息**：根据它们被插入未确认存储的时间戳进行排序，从最旧到最新。

- [#12590](https://github.com/emqx/emqx/pull/12590) 移除日志消息中的 `mfa` 元数据以提高清晰度。

- [#12641](https://github.com/emqx/emqx/pull/12641) 改进了文本日志格式化的字段顺序。新的字段顺序如下：

  `tag` > `clientid` > `msg` > `peername` > `username` > `topic` > [其他字段]

- [#12670](https://github.com/emqx/emqx/pull/12670) 在端点 `/monitor_current` 和 `/monitor_current/nodes/:node` 中添加了 `shared_subscriptions` 字段。

- [#12679](https://github.com/emqx/emqx/pull/12679) 将 Docker 镜像基础从 Debian 11 升级到 Debian 12。

- [#12700](https://github.com/emqx/emqx/pull/12700) 在 bytesize hocon 字段中开始支持 "b" 和 "B" 单位。

  例如，以下所有三个字段将具有 1024 字节的值：

  ```
  bytesize_field = "1024b"
  bytesize_field2 = "1024B"
  bytesize_field3 = 1024
  ```

- [#12719](https://github.com/emqx/emqx/pull/12719) `/clients` API 已升级，以同时容纳对多个 `clientid` 和 `username` 的查询，提供了一个更灵活、更强大的工具来监控客户端连接。此外，此更新引入了自定义 API 响应中包含哪些客户端信息字段的能力，为特定的监控需求进行了优化。

  多客户端/用户名查询示例：

  - 通过 ID 查询多个客户端：`/clients?clientid=client1&clientid=client2`
  - 查询多个用户：`/clients?username=user11&username=user2`
  - 在一个查询中组合多个客户端 ID 和用户名：`/clients?clientid=client1&clientid=client2&username=user1&username=user2`

  选择响应字段的示例：

  - 在响应中包含所有字段：`/clients?fields=all`（注意：省略 `fields` 参数默认返回所有字段。）
  - 只指定某些字段：`/clients?fields=clientid,username`

- [#12330](https://github.com/emqx/emqx/pull/12330) Cassandra 数据桥接已拆分为连接器和动作组件。它们与数据桥接 HTTP API 向后兼容。配置将自动升级。

- [#12353](https://github.com/emqx/emqx/pull/12353) OpenTSDB 数据桥接已拆分为连接器和动作组件。它们与数据桥接 HTTP API 向后兼容。配置将自动升级。

- [#12376](https://github.com/emqx/emqx/pull/12376) Kinesis 数据桥接已拆分为连接器和动作组件。它们与数据桥接 HTTP API 向后兼容。配置将自动升级。

- [#12386](https://github.com/emqx/emqx/pull/12386) GreptimeDB 数据桥接已拆分为连接器和动作组件。它们与数据桥接 HTTP API 向后兼容。配置将自动升级。

- [#12423](https://github.com/emqx/emqx/pull/12423) RabbitMQ 数据桥接已拆分为连接器、动作和 source 组件。它们与数据桥接 HTTP API 向后兼容。配置将自动升级。

- [#12425](https://github.com/emqx/emqx/pull/12425) ClickHouse 数据桥接已拆分为连接器和动作组件。它们与数据桥接 HTTP API 向后兼容。配置将自动升级。

- [#12439](https://github.com/emqx/emqx/pull/12439) Oracle 数据桥接已拆分为连接器和动作组件。它们与数据桥接 HTTP API 向后兼容。配置将自动升级。

- [#12449](https://github.com/emqx/emqx/pull/12449) TDEngine 数据桥接已拆分为连接器和动作组件。它们与数据桥接 HTTP API 向后兼容。配置将自动升级。

- [#12488](https://github.com/emqx/emqx/pull/12488) RocketMQ 数据桥接已拆分为连接器和动作组件。它们与数据桥接 HTTP API 向后兼容。配置将自动升级。

- [#12512](https://github.com/emqx/emqx/pull/12512) HStreamDB 数据桥接已拆分为连接器和动作组件。它们与数据桥接 HTTP API 向后兼容。配置将自动升级，但鉴于配置中添加了新字段，建议手动进行升级。

- [#12543](https://github.com/emqx/emqx/pull/12543) DynamoDB 数据桥接已拆分为连接器和动作组件。它们与数据桥接 HTTP API 向后兼容。配置将自动升级。

- [#12595](https://github.com/emqx/emqx/pull/12595) Kafka 消费者数据桥接已拆分为连接器和源组件。它们与数据桥接 HTTP API 向后兼容。配置将自动升级。

- [#12619](https://github.com/emqx/emqx/pull/12619) Microsoft SQL Server 数据桥接已拆分为连接器和动作组件。它们与数据桥接 HTTP API 向后兼容。配置将自动升级。

- [#12381](https://github.com/emqx/emqx/pull/12381) 新增 SQL 函数：`map_keys()`、`map_values()`、`map_to_entries()`、`join_to_string()`、`join_to_sql_values_string()`、`is_null_var()`、`is_not_null_var()`。

  有关函数及其使用的更多信息，请参阅文档：[内置 SQL 函数](../data-integration/rule-sql-builtin-functions)。

- [#12427](https://github.com/emqx/emqx/pull/12427) 实现了限制 Kafka 分区数量的功能以用于 Kafka 数据集成。

- [#12577](https://github.com/emqx/emqx/pull/12577) 更新了 GCP PubSub 生产者和消费者连接器的 `service_account_json` 字段，使其能够接受 JSON 编码的字符串。现在，可以将此字段设置为 JSON 编码字符串。仍然支持使用先前的格式（HOCON 映射），但不推荐。

- [#12581](https://github.com/emqx/emqx/pull/12581) 向模式注册中心添加了 JSON 架构。

  JSON Schema 支持 [Draft 03](http://tools.ietf.org/html/draft-zyp-json-schema-03)、[Draft 04](http://tools.ietf.org/html/draft-zyp-json-schema-04) 和 [Draft 06](https://datatracker.ietf.org/doc/html/draft-wright-json-schema-00)。

- [#12602](https://github.com/emqx/emqx/pull/12602) 使用其 `ping` API 而非仅检查现有套接字连接，增强了 IoTDB 连接器的健康检查。

- [#12336](https://github.com/emqx/emqx/pull/12336) 性能提升。创建了一个专门的异步任务处理池来处理客户端会话清理任务。

- [#12725](https://github.com/emqx/emqx/pull/12725) 添加了用于列出可用的 source 类型的 REST API。

- [#12746](https://github.com/emqx/emqx/pull/12746) 添加了 `username` 日志字段。如果 MQTT 客户端以非空用户名连接，日志和追踪将包含 `username` 字段。

- [#12785](https://github.com/emqx/emqx/pull/12785) 日志处理进程增加了一个新的配置选项 `timestamp_format`，用于自定义日志时间戳格式。该选项支持以下设置：

  - `auto`: 根据所使用的日志格式类型自动确定时间戳格式。对于文本格式类型，使用 `rfc3339` 格式；对于 JSON 格式类型，则使用 `epoch`格式。
  - `epoch`: 时间戳以微秒精度的 Unix 纪元时间格式表示。
  - `rfc3339`: 时间戳使用符合 RFC3339 标准的日期时间字符串格式，格式示例为 `2024-03-26T11:52:19.777087+00:00`。

### 修复

- [#11868](https://github.com/emqx/emqx/pull/11868) 修复了会话接管后未发布遗嘱消息的问题。

- [#12347](https://github.com/emqx/emqx/pull/12347) 对 MQTT 出口数据桥接的规则 SQL 处理的消息进行了更新，确保即使在数据不完整或缺少某些在桥接配置中定义的占位符的情况下，消息也始终被视为有效。此调整防止了之前发生的消息被错误地视为无效并随后被 MQTT 出口数据桥接丢弃的情况。

  当 `payload` 和 `topic` 模板中的变量未定义时，现在它们被渲染为空字符串，而不是字面量 `undefined` 字符串。

- [#12472](https://github.com/emqx/emqx/pull/12472) 修复了在滚动升级过程中，某些读取操作在 `/api/v5/actions/` 和 `/api/v5/sources/` 端点可能导致返回 `500` 错误码的问题。

- [#12492](https://github.com/emqx/emqx/pull/12492) EMQX 现在在 MQTT v5 客户端的 `CONNACK` 消息中返回 `Receive-Maximum` 属性。此实现考虑了客户端的 `Receive-Maximum` 设置和服务器的 `max_inflight` 配置的最小值作为允许的未确认（unacknowledged）消息数量的限制。之前，确定的值未在 `CONNACK` 消息中发送回客户端。


  注意：当前已知的问题是，由于包含了断开的会话，这些增强的 API 响应提供的总客户端计数可能超过实际的客户端数量。

- [#12505](https://github.com/emqx/emqx/pull/12505) 将 Kafka 生产者客户端 `wolff` 从版本 1.10.1 升级到 1.10.2。这个最新版本为每个连接器维持一个长期的元数据连接，通过减少为动作和连接器健康检查建立新连接的频率，优化了 EMQX 的性能。

- [#12513](https://github.com/emqx/emqx/pull/12513) 将几个可能导致日志泛滥的事件级别从 `warning` 改为 `info`。

- [#12530](https://github.com/emqx/emqx/pull/12530) 改进了 `frame_too_large` 事件和格式错误的 `CONNECT` 包解析失败的错误报告。这些更新现在提供了额外的信息，帮助故障排除。

- [#12541](https://github.com/emqx/emqx/pull/12541) 为基于 DNS 自动集群引入了新的配置验证步骤，以确保 `node.name` 和 `cluster.discover_strategy` 之间的兼容性。具体来说，当使用 `dns` 策略并带有 `a` 或 `aaaa` 记录类型时，所有节点必须使用（静态）IP 地址作为主机名。


- [#12566](https://github.com/emqx/emqx/pull/12566) 增强了 REST API 密钥的引导文件：

  - 文件中的空行现在将被跳过，消除了之前生成错误的行为。
  - 引导文件中指定的 API 密钥被赋予最高优先级。如果引导文件中的新密钥与现有密钥冲突，旧密钥将被自动删除，以确保引导密钥无问题地生效。

- [#12646](https://github.com/emqx/emqx/pull/12646) 修复了规则引擎日期时间字符串解析器的问题。之前，时区调整仅对以秒级精度指定的日期时间字符串有效。

- [#12652](https://github.com/emqx/emqx/pull/12652) 修复了文档中有描述但实际实现中缺失的带有 4 和 5 个参数的 subbits 函数的问题。这些函数现已被添加。

- [#12663](https://github.com/emqx/emqx/pull/12663) 确保通过 Prometheus 端点 `/prometheus/stats` 访问的 `emqx_vm_cpu_use` 和 `emqx_vm_cpu_idle` 指标准确反映当前的 CPU 使用和空闲情况，而不是自操作系统启动以来的平均 CPU 使用率，为监控目的提供更相关和及时的数据。

- [#12668](https://github.com/emqx/emqx/pull/12668) 使用 `calendar:datetime_to_gregorian_seconds/1` 重构了 SQL 函数 `date_to_unix_ts()`。此更改还为输入日期格式添加了验证。

- [#12672](https://github.com/emqx/emqx/pull/12672) 在生成节点启动配置的过程中加载 `{data_dir}/configs/cluster.hocon`。之前，通过Dashboard 进行的日志配置更改保存在 `{data_dir}/configs/cluster.hocon` 中，仅在使用 `etc/emqx.conf` 生成初始启动配置后应用，这可能导致在之后的重新配置中会丢失一些日志段文件。

  现在，创建启动配置时 `{data_dir}/configs/cluster.hocon` 和 `etc/emqx.conf` 同时加载，且 `emqx.conf` 中的设置优先。

- [#12696](https://github.com/emqx/emqx/pull/12696) 修复了尝试重新连接动作或 source 可能导致在 HTTP API 中返回错误的错误消息的问题。

- [#12714](https://github.com/emqx/emqx/pull/12714) 修复了 Prometheus API `/prometheus/stats` 端点报告的几个指标不准确的问题。更正适用于以下指标：

  - `emqx_cluster_sessions_count`
  - `emqx_cluster_sessions_max`
  - `emqx_cluster_nodes_running`
  - `emqx_cluster_nodes_stopped`
  - `emqx_subscriptions_shared_count`
  - `emqx_subscriptions_shared_max`

  此外，此修复纠正了 `/stats` 端点中 `subscriptions.shared.count` 和 `subscriptions.shared.max` 字段的问题。之前，这些值在客户端断开连接或取消订阅共享订阅后未能及时更新。

- [#12390](https://github.com/emqx/emqx/pull/12390) 修复了在集群加入过程中 `/license` API 请求可能崩溃的问题。

- [#12411](https://github.com/emqx/emqx/pull/12411) 纠正了 Cassandra 数据集成中 `null` 值会以 `1853189228` 被错误地插入到 `int` 列。

- [#12522](https://github.com/emqx/emqx/pull/12522) 改进了 Kafka 引导主机的解析过程，以排除逗号后的空格，解决了由于主机条目格式错误导致的连接超时和 DNS 解析失败。

- [#12656](https://github.com/emqx/emqx/pull/12656) 在创建 GCP PubSub 生产者动作时实现了主题验证步骤，确保当主题不存在或提供的凭据权限不足时通知失败。

- [#12678](https://github.com/emqx/emqx/pull/12678) 对 DynamoDB 连接器进行了改进，现在能够明确指出连接失败的具体原因，显著提升了对错误原因的识别，从而解决了以往在错误诊断方面的不足。

- [#12681](https://github.com/emqx/emqx/pull/12681) 解决了在启用 debug 级别日志的情况下，向 RocketMQ 桥接/动作发送消息可能导致敏感信息在 debug 日志中泄露的问题。

- [#12715](https://github.com/emqx/emqx/pull/12715) 解决了当入口数据集成 source 的连接器存在活动通道时，进行配置更新可能导致系统崩溃的问题。


- [#12767](https://github.com/emqx/emqx/pull/12767) 修复了从 5.0.1 升级到 5.5.1 期间遇到的问题，特别是与 Kafka 生产者配置相关的问题，这些问题导致了升级失败。此修正确保 Kafka 生产者配置准确转换为 EMQX 版本 5.5.1 及更高版本所需的动作和连接器配置的新格式。

- [#12768](https://github.com/emqx/emqx/pull/12768) 解决了 EMQX 5.4.0 及以后版本在启动时可能遇到的故障问题，特别是从 5.4.0 之前的版本进行滚动升级时。问题与当 v1 和 v2 路由表都为空时路由模式的初始化有关。

  现在，节点在启动时如果发现本地路由表为空，则会尝试从集群中检索正在使用的路由模式版本，而不是默认使用 v2 路由表。这种方法减少了潜在冲突的可能性，并降低了集群节点中路由存储模式分化的机会，尤其是在混合版本集群场景中。

  如果在运行中的集群检测到冲突，EMQX 将在日志中记录解决方法，作为级别为 `critical` 的错误消息的一部分。相同的错误消息和解决方法也会被记录在标准错误输出中，确保即使没有配置日志处理进程，消息也不会丢失。

- [#12786](https://github.com/emqx/emqx/pull/12786) 新增了一项严格的检查流程，防止复制节点连接到运行不同版本 EMQX 的核心节点。这项检查确保在滚动升级过程中，复制节点只有在至少有一个核心节点运行相同 EMQX 版本时才能工作。


## 5.5.1

*发布日期: 2024-03-06*

### 增强

- [#12497](https://github.com/emqx/emqx/pull/12497) 改进了 MongoDB 连接器的性能，使与数据库的交互更加高效。此增强功能通过改进 MongoDB Erlang 驱动得到支持（见 [mongodb-erlang PR](https://github.com/emqx/mongodb-erlang/pull/41)）。

### 修复

- [#12471](https://github.com/emqx/emqx/pull/12471) 修复了在从 EMQX 版本 5.0.2 升级到新版本期间，数据集成配置未能正确加载的问题。

- [#12598](https://github.com/emqx/emqx/pull/12598) 修复了用户无法通过 HTTP API 订阅或取消订阅共享主题过滤器的问题。

  受影响的 API 包括：

  - `/clients/:clientid/subscribe`
  - `/clients/:clientid/subscribe/bulk`
  - `/clients/:clientid/unsubscribe`
  - `/clients/:clientid/unsubscribe/bulk`

- [#12601](https://github.com/emqx/emqx/pull/12601) 修复了 LDAP 驱动的日志没有被捕获的问题。现在，所有日志都以 `info` 级别被记录。

- [#12606](https://github.com/emqx/emqx/pull/12606) 修复了一个问题， 即当指定的 SSL 证书文件在给定路径中不存在时，Prometheus API 会崩溃。现在，如果缺少 SSL 证书文件，`emqx_cert_expiry_at` 指标会报告一个值为 0，表示证书不存在。

- [#12608](https://github.com/emqx/emqx/pull/12608) 修复了在查询数据中缺少 `payload` 字段时，IoTDB 动作的 `function_clause` 错误。

- [#12610](https://github.com/emqx/emqx/pull/12610) 修复了与 LDAP 连接器的连接在一定时间后可能意外断开的问题。

- [#12620](https://github.com/emqx/emqx/pull/12620) 在 HTTP 服务连接器中对授权头中的敏感信息进行了编辑，从调试级别日志中排除了认证和授权信息，以减少潜在的安全风险。

- [#12632](https://github.com/emqx/emqx/pull/12632) 修复了一个问题，即在闰年的3月1日起，规则引擎的 SQL 内置函数 `date_to_unix_ts` 会产生不正确的时间戳结果。

## 5.5.0

*发布日期: 2024-02-01*

### 增强

- [#12085](https://github.com/emqx/emqx/pull/12085) EMQX 已升级，以利用 OTP 版本 26.1.2-2 的功能。注意：Docker 镜像仍然使用 OTP 25.3.2 构建。

- [#12189](https://github.com/emqx/emqx/pull/12189) 增强了 EMQX JWT 认证中的[权限列表](../access-control/authn/jwt.md#权限列表)声明格式，使其具有更高的灵活性。更新后的格式现在支持数组结构，更加符合基于文件的 ACL 规则。

  例如：

  ```json
  [
  {
    "permission": "allow",
    "action": "pub",
    "topic": "${username}/#",
    "qos": [0, 1],
    "retain": true
  },
  {
    "permission": "allow",
    "action": "sub",
    "topic": "eq ${username}/#",
    "qos": [0, 1]
  },
  {
    "permission": "deny",
    "action": "all",
    "topics": ["#"]
  }
  ]
  ```

  在这种新格式中，找不到匹配的规则不会自动导致操作被拒绝。如果在 JWT 权限列表中找不到匹配项，授权链可以将请求交由下一授权检查器继续检查。如果在整个链中都找不到匹配项，最终检查结果将根据 `authorization.no_match` 中设置的默认权限决定。

- [#12267](https://github.com/emqx/emqx/pull/12267) 为 `cluster/:node/invite` 接口增加了一个新的 `timeout` 参数，解决了默认超时问题。之前设置的 5 秒默认超时，往往会导致 HTTP API 调用超时，因为 EMQX 加入集群通常需要更多时间。

  此外，EMQX 还添加了一个新的 API `/cluster/:node/invite_async`，支持以异步方式邀请节点加入集群，并通过新增的 `cluster/invitation` API 检查加入状态。

- [#12272](https://github.com/emqx/emqx/pull/12272) 对 EMQX 中的 `retain` API 进行了更新：

  - 增加了一个新的 API `DELETE /retainer/messages`，用于清除所有保留的消息。
  - 在 `GET /retainer/messages` API 的查询字符串中增加了一个可选的主题过滤器参数 `topic`。例如，使用 `topic=t/1` 可以过滤特定主题的保留消息，提高消息检索的效率。

- [#12277](https://github.com/emqx/emqx/pull/12277) 新增了 `mqtt/delayed/messages/:topic` API，用于按主题名称删除延迟消息。

- [#12278](https://github.com/emqx/emqx/pull/12278) 将 REST API 中支持分页的 API 的最大分页大小从 `3000` 调整到 `10000`。

- [#12289](https://github.com/emqx/emqx/pull/12289) 授权缓存支持排除特定的主题列表。对于指定的主题和主题过滤器列表，EMQX 将不会生成授权缓存。列表可以通过 `authorization.cache.excludes` 配置项或在 Dashboard 上设置。对于这些特定的主题权限检查将会始终实时进行，而不是依赖于之前的缓存结果，从而确保了授权结果的及时性。

- [#12329](https://github.com/emqx/emqx/pull/12329) 新增了 `broker.routing.batch_sync` 配置项。这个配置启用了一个专门的进程池，能够批量地将订阅信息与全局路由表同步，从而减少了可能由于网络延迟导致的跨节点通信的减慢。通过集中处理多个订阅更新，它不仅加速了集群中副本节点和核心节点之间的同步，而且还减轻了代理池的负载，从而最大限度地减少了过载的风险。

- [#12333](https://github.com/emqx/emqx/pull/12333) 为动作和连接器添加了一个 `tags` 字段。与 `description` 字段（即自由文本注释）类似，`tags` 可用于为动作和连接器添加注释，便于过滤和分组。

- [#12072](https://github.com/emqx/emqx/pull/12072)  GreptimeDB 数据集成支持异步操作，以提供更好的性能。

- [#12194](https://github.com/emqx/emqx/pull/12194) 提高了 Kafka 生产者数据集成的性能，降低了所连接的 Kafka 服务器 CPU 占用。

- [#12247](https://github.com/emqx/emqx/pull/12247) 将 InfluxDB 的桥接分离出来，使其可以通过连接器和动作 APIs 使用。它们仍然与旧的桥接 API 兼容。

- [#12299](https://github.com/emqx/emqx/pull/12299) 公开了更多 EMQX 指标信息以提高可观测性：

  监控 API：

  - 在 `/api/v5/monitor_current` 中添加了 `retained_msg_count` 字段。
  - 在 `/api/v5/monitor_current` 中添加了 `license_quota` 字段。
  - 在 `/api/v5/monitor_current/nodes/{node}` 中添加了 `retained_msg_count` 和 `node_uptime` 字段。
  - 在 `/api/v5/monitor_current/nodes/{node}` 中添加了 `retained_msg_count`、`license_quota` 和 `node_uptime` 字段。

  Prometheus API：

  - 在 `/api/v5/prometheus/stats` 中添加了 `emqx_cert_expiry_at` 和 `emqx_license_expiry_at`，用于显示 TLS 监听器证书的过期时间和许可证的过期时间。
  - 添加了 `/api/v5/prometheus/auth` 端点，提供所有认证器和授权器的执行次数和运行状态等指标。
  - 添加了 `/api/v5/prometheus/data_integration` 端点，提供所有规则、动作和连接器的执行次数和状态等指标。

  限制： 

  Prometheus push gateway 仅支持 `/api/v5/prometheus/stats?mode=node` 中的内容。

  有关更多 API 详情和指标类型信息，请参阅 swagger api 文档。

- [#12196](https://github.com/emqx/emqx/pull/12196) 在路由清理过程中提高了网络效率。之前，当一个节点宕机时，所有其他存活节点之间必须交换针对该节点的每个路由的删除操作。在这次更改之后，所有存活节点之间只需交换一个 `match and delete`（匹配并删除）操作，这显著减少了所需的网络数据包数量并降低了集群间网络的负载。 这种优化对于地理分布式的 EMQX 部署尤为有用，在这些部署中网络延迟可能会非常高。

- [#12354](https://github.com/emqx/emqx/pull/12354) 支持并发创建和更新数据集成，大大提高了例如导入备份文件时的操作速度。

- [#12396](https://github.com/emqx/emqx/pull/12396) 增强了 `authentication/:id/import_users` 接口中的用户导入功能：

  - 新增 `?type=plain` 参数，用于更方便地导入使用明文密码的用户，补充现有的仅支持密码哈希的功能。
  - 增强了对 `content-type: application/json` 的支持，允许以 JSON 格式提交 HTTP Body。这扩展了当前仅支持 `multipart/form-data` 用于 CSV 文件的功能。


- [#11902](https://github.com/emqx/emqx/pull/11902) EMQX 消息桥接时支持 Nari Syskeeper 2000 单向隔离网闸穿透。
- [#12348](https://github.com/emqx/emqx/pull/12348) EMQX 支持与 Elasticsearch 的数据集成。

### 修复

- [#12232](https://github.com/emqx/emqx/pull/12232) 修复了节点被强制离开集群后集群提交日志表未被删除的问题。
- [#12243](https://github.com/emqx/emqx/pull/12243) 修复了一系列细微的竞争条件，这些条件可能导致全局路由状态不一致。
- [#12269](https://github.com/emqx/emqx/pull/12269) 改进了 `/clients` 接口的错误处理；现在在查询字符串验证失败时返回 400 状态和更详细的错误信息，而不是通用的 500。
- [#12285](https://github.com/emqx/emqx/pull/12285) 更新了 CoAP 网关，以支持短参数名，从而节省了数据报大小。例如，`clientid=bar` 可以写成 `c=bar`。
- [#12303](https://github.com/emqx/emqx/pull/12303) 修复了保留消息索引的问题。以前，具有通配符订阅的客户端可能会收到与其订阅主题不匹配的无关保留消息。
- [#12305](https://github.com/emqx/emqx/pull/12305) 修正了将不完整的客户端/连接信息传递到 `emqx_cm` 的问题，这可能导致内部不一致，并影响内存使用和节点疏散等操作。
- [#12306](https://github.com/emqx/emqx/pull/12306) 修复了通过 HTTP API 更新连接器密码参数后，连接器的连接测试无法正常工作的问题。
- [#12359](https://github.com/emqx/emqx/pull/12359) 修复了配置有某些类型数据桥接的节点重启时可能出现的错误消息问题。此外，这些桥接在节点重启时有进入失败状态的风险，需要手动重启以恢复功能。
- [#12404](https://github.com/emqx/emqx/pull/12404) 修复了一个问题，即在消息流量较大的情况下重启数据集成可能导致数据集成指标的收集停止。
- [#12282](https://github.com/emqx/emqx/pull/12282) 改善了 MySQL 桥接创建失败时 HTTP API 的错误响应。同时解决了在 SQL 中包含未定义列的 MySQL Sink 无法删除的问题。
- [#12291](https://github.com/emqx/emqx/pull/12291) 修复了 EMQX 在处理涉及敏感参数的配置更新时的不一致性，这以前导致集群配置文件中出现了错误的 `"******"` 字符串。
- [#12301](https://github.com/emqx/emqx/pull/12301) 修复了 InfluxDB 中的行协议问题，其中数值字面量被存储为字符串类型。
- [#12317](https://github.com/emqx/emqx/pull/12317) 从 MongoDB Action 架构中移除了尚未支持的 `resource_opts.batch_size` 字段。

## 5.4.1

*发布日期: 2024-01-09*

### 修复

- [#12234](https://github.com/emqx/emqx/pull/12234) 解决了 EMQX 5.4.0 之前版本在 `emqx.conf` 中定义的 Open Telemetry 配置的兼容性问题，确保最新 EMQX 发布版本能够平滑兼容旧版配置。
- [#12236](https://github.com/emqx/emqx/pull/12236) 修复了 MQTT 服务数据集成中客户端 ID 的生成方法，以符合 MQTT 3.1 规范的 23 字节限制。 客户端 ID 现在以用户分配的连接器名称为前缀，后跟节点名称的 SHA 哈希值和池成员 ID 的前 8 个字节。 如果生成的 ID 超过 23 字节，则会将 ID 重新使用 SHA 哈希，并取哈希的前 23 个字符以确保合规性。
- [#12238](https://github.com/emqx/emqx/pull/12238) 解决了EMQX 5.3.2 版本 HTTP Action 功能中引入的错误格式配置的兼容性问题。
- [#12240](https://github.com/emqx/emqx/pull/12240) 修改了 `/file_transfer` REST API，按照原始格式返回配置，避免将时间单位（如 "1h"）转换为秒，确保调用者接收到初始配置的值，此修改与其他 GET API 保持一致的数据格式。
- [#12241](https://github.com/emqx/emqx/pull/12241) 修复了配置额外的 S3 HTTP 请求头导致文件传输中断的问题，确保稳定且不间断的文件传输操作。
- [#12246](https://github.com/emqx/emqx/pull/12246) 停止在 Docker 中默认暴露不再使用的 11883 端口，并从 Helm  Chart 中移除。
- [#12249](https://github.com/emqx/emqx/pull/12249) 修复了 `/configs` API 中尝试修改只读配置值导致响应消息乱码的问题。
- [#12250](https://github.com/emqx/emqx/pull/12250) 解决了 `file_transfer` 配置的 `secret_access_key` 值错误更新为掩码星号 (`*****`) 的问题，确保原始密钥值保持不变以保证安全性。
- [#12256](https://github.com/emqx/emqx/pull/12256) 修复了没有密码就无法与 MySQL 资源建立连接的问题。
- [#12264](https://github.com/emqx/emqx/pull/12264) 修复5.4副本节点在滚动升级过程中无法加入运行早于 5.4 版本的核心节点所在集群的问题。

## 5.4.0

*发布日期: 2023-12-23*

### 增强

- [#11884](https://github.com/emqx/emqx/pull/11884) 对 Prometheus API 及其配置进行了以下改进：

  - 重构了配置部分，将相关设置分组，提高了可读性和可维护性。
  - 引入了 `enable_basic_auth` 配置项，用于 scrape API 端点的基本认证，增强了安全性。
  - 在重构代码的同时保持了向后兼容性，避免了破坏性的变更。

- [#11896](https://github.com/emqx/emqx/pull/11896) 引入了在桥接配置中设置敏感认证字段（如密码、令牌和密钥）的增强功能。此改进允许使用以文件形式存储在文件系统中的秘密信息。这些秘密信息可以通过在配置文件中使用特殊的 `file://` 前缀安全地引用，从而增强了桥接配置中敏感数据处理的安全性。

- [#11921](https://github.com/emqx/emqx/pull/11921) 引入了 Open Telemetry 日志处理进程，该进程允许按照 Open Telemetry 日志数据模型格式化日志事件。此处理程序便于将格式化的日志事件导出到配置的 Open Telemetry 收集器或后端，从而增强了日志管理和集成能力。

- [#11935](https://github.com/emqx/emqx/pull/11935) 默认切换到新的`v2`路由存储模式。新模式提升了订阅和路由性能，尤其是在具有共同通配符前缀的主题过滤器的并发订阅场景中更为显著。但这也会带来轻微的内存使用增加。该模式还消除了对单独索引的需求，从而解决了在以往版本中偶尔遇到的路由状态不一致问题。

  如果集群是从旧版本进行滚动升级，那么集群将继续使用`v1`存储模式，直到发生全集群（非滚动）重启。

  用户仍可以通过将`broker.routing.storage_schema` 配置选项设置为`v1`来选择以前的模式。但是，这也需要完整的非滚动集群重启才能生效。

- [#11984](https://github.com/emqx/emqx/pull/11984)  实现了 Open Telemetry 分布式追踪特性。

- [#12017](https://github.com/emqx/emqx/pull/12017) 实现了一个专用的 REST API，用于配置和用户数据的导入和导出。

- [#12040](https://github.com/emqx/emqx/pull/12040) 升级了 QUIC 协议栈。

- [#12201](https://github.com/emqx/emqx/pull/12201) 添加了对 TCP/SSL/WS/WSS MQTT 监听器配置的热更新支持。这个功能允许您在无需重新启动监听器和断开客户端连接的情况下修改大多数配置参数。然而，目前有一些限制：

  - 对于 TCP/SSL 监听器，仍然需要重新启动监听器并重新连接客户端才能更改以下参数：
    - `bind`
    - `tcp_options.backlog`
  - 对于 WS/WSS（WebSocket）监听器，修改与传输相关的参数（如下所示）将导致监听套接字被重新打开，但已建立的连接将保持不间断。
    - `bind`
    - `tcp_options.*`
    - `ssl_options.*`

- [#11608](https://github.com/emqx/emqx/pull/11608) 客户端认证 LDAP 数据源支持通过绑定操作进行认证，提供了更多灵活性和安全性的用户认证方式。

- [#11766](https://github.com/emqx/emqx/pull/11766) 为 REST API 实现了初步的基于角色的访问控制。在这个版本中，有三个预定义的角色：

  - 管理员：此角色可以访问所有资源。
  - 查看者：此角色只能查看资源和数据，对应于 REST API 中的所有 GET 请求。
  - 发布者：专门为 MQTT 消息发布定制，此角色仅限于访问与消息发布相关的端点。

- [#11773](https://github.com/emqx/emqx/pull/11773) Dashboard 中添加了审计日志管理页面，用户可以使用该页面查看对 EMQX 设备和数据进行的所有更改操作，例如踢出设备、创建/删除规则等。

- [#11778](https://github.com/emqx/emqx/pull/11778) Dashboard 单点登录中的 SAML 协议支持与 Azure Entra ID 进行集成。


- [#11811](https://github.com/emqx/emqx/pull/11811) 优化了 REST API 密钥引导文件的格式，以支持使用角色初始化密钥。

  新的格式为：`api_key:api_secret:role`。

  其中 `role` 是可选的，默认值为 `administrator`。

- [#11852](https://github.com/emqx/emqx/pull/11852) 新增了 GB/T 32960 协议网关，使车辆能够通过 GB/T 32960 车联网协议与 EMQX 连接。

- [#11883](https://github.com/emqx/emqx/pull/11883) 新增了 JT/T808 协议网关，使车辆能够通过 JT/T 808 车联网协议与 EMQX 连接。

- [#11885](https://github.com/emqx/emqx/pull/11885) 新增了 OCPP 网关，使电动车（EV）充电站能够通过 OCPP (Open Charge Point Protocol) 协议访问 EMQX。

- [#11971](https://github.com/emqx/emqx/pull/11971) 将 `/api/v5/load_rebalance/availability_check` 接口设为公共接口，即不再需要进行身份验证。这一变更简化了负载均衡器的设置。

  此外，它还改善了等待健康检查阶段的负载均衡重平衡/疏散过程的流畅性。现在，在此阶段，不会禁止连接到被标记为要疏散的节点。这个调整是因为无法确定负载均衡器是否已将所有这些节点标记为不健康。禁止连接到它们可能会导致多次不成功的重新连接尝试。
  
- [#12013](https://github.com/emqx/emqx/pull/12013) 调整数据桥接设计，将其拆分为连接器与动作（Sink）。连接用于管理数据集成与外部系统的连接，可以在多个动作之间重复使用，动作仅用于配置数据操作方式。这个设计能够提供更大的灵活性和更好的可扩展性，实现更清晰的数据集成配置与管理。

  已调整的数据桥接有包括 PostgreSQL, Timescale 和 Matrix，现在拆分为连接器和动作 API，不过它们仍然与旧的数据桥接 API 兼容。

- [#12016](https://github.com/emqx/emqx/pull/12016) 增强了许可证密钥管理。

  EMQX 现在可以从指定文件加载许可证密钥。通过将 `license.key` 配置设置为文件路径，并使用 `"file://"` 作为前缀来启用此功能。 还添加了通过设置 `license.key = default` 来恢复到默认试用许可证的功能。此选项简化了在需要时返回试用许可证的过程。
  
- [#12129](https://github.com/emqx/emqx/pull/12129) 续期默认的 License，替换了 2023 年 1 月发布的旧 License。与此同时还将 License 规格从 100 并发连接调整为 25 个并发连接。

### 修复

- [#10976](https://github.com/emqx/emqx/pull/10976) 修复共享订阅中的主题过滤器重复处理问题。 在之前的实现中，订阅选项的存储方法没有充分适配共享订阅，这导致在特定的主题和流程下，”订阅-取消订阅” 期间消息路由失败并且节点之间的路由表出现泄漏问题。
- [#12048](https://github.com/emqx/emqx/pull/12048) 修复 COAP 网关忽略订阅选项的错误。
- [#12078](https://github.com/emqx/emqx/pull/12078) 升级了 grpc-erl 到版本 0.6.12。此更新解决了潜在的死锁问题，其中 grpc 客户端延迟启动了依赖的应用程序。
- [#12081](https://github.com/emqx/emqx/pull/12081) 更新了 `gen_rpc` 库到版本 3.3.1。这个新版本包括了一些性能改进：
  - 在某些情况下避免为数据包在发送到网络之前分配额外的内存。
  - 对于本地调用，绕过了网络层。
  - 避免敏感信息打印到日志中 [#12202](https://github.com/emqx/emqx/pull/12202)。
- [#12111](https://github.com/emqx/emqx/pull/12111) 修复了一个问题，该问题导致 API 令牌因为竞态条件在登录后立即不可用。
- [#12121](https://github.com/emqx/emqx/pull/12121) 修复了在不同节点同时更新配置时，集群中的节点偶尔会返回旧视图的问题。
- [#12158](https://github.com/emqx/emqx/pull/12158) 修复规则引擎无法连接到 [Upstash](https://upstash.com/) Redis 的问题。修复前，在与 Redis 服务建立 TCP 连接之后，EMQX 的 Redis 驱动程序使用 [inline commands](https://redis.io/docs/reference/protocol-spec/#inline-commands) 来发送 AUTH 和 SELECT 命令。但 Upstash Redis 服务不支持 inline commands，导致 EMQX 无法连接到 Upstash Redis 服务。 修复后，EMQX 的 Redis 驱动使用 RESP (Redis Serialization Protocol) 来发送 AUTH 和 SELECT 命令。
- [#12176](https://github.com/emqx/emqx/pull/12176) 无论之前是否成功建立连接，始终向 MQTT-SN 客户端发送 "DISCONNECT" 数据包的确认。
- [#12180](https://github.com/emqx/emqx/pull/12180) 修复了 MQTT-SN 网关因 DTLS 相关配置兼容性问题导致监听器无法启动的问题。
- [#12219](https://github.com/emqx/emqx/pull/12219) 修复了从 Dashboard 更新文件传输 S3 配置时密钥反混淆的问题。

## 5.3.2

*发布日期: 2023-12-01*

### 增强

- [#11752](https://github.com/emqx/emqx/pull/11752) 将 core-replica 数据库同步的默认 RPC 驱动从 `gen_rpc` 更改为 `rpc`。

  这提升了核心副本数据复制的速度。

- [#11785](https://github.com/emqx/emqx/pull/11785) 拥有“查看者”角色的用户具有更改自己密码的权限，但无权更改其他用户密码。

- [#11787](https://github.com/emqx/emqx/pull/11787) 提升了 `emqx` 命令的性能。

- [#11790](https://github.com/emqx/emqx/pull/11790) 为 Redis 授权数据源中的 Redis 命令添加了验证功能。此外，此次改进优化了认证和授权过程中 Redis 命令的解析，现在的解析符合 `redis-cli` 兼容性标准，并支持引号参数。

- [#11541](https://github.com/emqx/emqx/pull/11541) 文件传输能力得到了增强。现在，客户端可以使用异步方式，通过 `$file-async/...` 主题进行文件传输，并通过 `$file-response/{clientId}` 主题订阅命令执行结果。这一改进简化了文件传输功能的使用，尤其适用于 MQTT v3.1/v3.1.1 或使用了 MQTT 桥接的客户端。 更多详情请参阅 [EIP-0021](https://github.com/emqx/eip)。

### 修复

- [#11757](https://github.com/emqx/emqx/pull/11757) 修复了下载不存在的追踪文件时返回的错误响应码。现在，响应码会返回 `404` 而不是 `500`。

- [#11762](https://github.com/emqx/emqx/pull/11762) 修复了 EMQX 中 `built_in_database` 授权数据源的一个问题。通过这次修复，现在在删除数据源时，所有 ACL 记录都会被彻底移除。这解决了数据库残留的记录在重新创建授权数据源时仍然存在的问题。

- [#11771](https://github.com/emqx/emqx/pull/11771) 修复了通过 API/Dashboard 进行身份验证管理时 Bcrypt 盐轮次(salt rounds)的验证问题。

- [#11780](https://github.com/emqx/emqx/pull/11780) 修复了 `pbkdf2` 密码哈希算法中 `iterations` 字段的验证问题。现在，`iterations` 必须是严格正数。之前，`iterations` 可以被设置为 0，这会导致验证器无法正常工作。

- [#11791](https://github.com/emqx/emqx/pull/11791) 修复了 EMQX CoAP 网关中的一个问题，即心跳没有有效地维持连接的活跃状态。此修复确保心跳机制正确维持 CoAP 网关连接的活跃状态。

- [#11797](https://github.com/emqx/emqx/pull/11797) 修改了管理 `built_in_database` 授权数据源的 HTTP API 行为。如果未将 `built_in_database` 设置为授权数据源，这些 API 现在将返回 `404` 状态码，替换了以前的 `20X` 响应。

- [#11965](https://github.com/emqx/emqx/pull/11965) 优化了 EMQX 服务的终止过程，确保即使在存在不可用的 MongoDB 资源的情况下，也能够实现优雅停止。

- [#11975](https://github.com/emqx/emqx/pull/11975) 此修复解决了由于对端和服务器同时关闭套接字时发生竞争条件导致的冗余错误日志问题。以前，由操作系统和 EMQX 触发的并发套接字关闭事件会导致不必要的错误记录。通过改进事件处理，本次修复消除了不必要的错误信息。

- [#11987](https://github.com/emqx/emqx/pull/11987) 修复了在尝试设置 TCP/SSL 套接字的 `active_n` 选项时连接崩溃的问题。

  在此修复之前，如果在连接过程中尝试设置 `active_n` 选项时套接字已经关闭，会导致 `case_clause` 崩溃。

- [#11731](https://github.com/emqx/emqx/pull/11731) 为文件传输功能添加了热配置支持。

- [#11754](https://github.com/emqx/emqx/pull/11754) 改进了 Postgres 桥接的日志格式化功能，针对驱动程序返回的错误消息中的 Unicode 字符进行了处理。

## 5.3.1

*发布日期: 2023-11-14*

### 增强

- [#11637](https://github.com/emqx/emqx/pull/11637) 增加了额外的诊断检查，以帮助调试当 Mnesia 因等待表而停滞时出现的问题。更新依赖库：`ekka` 已升级至 0.15.15 版本，`mria` 已升级至 0.6.4 版本。
- [#11581](https://github.com/emqx/emqx/pull/11581) 功能预告：计划在 EMQX v5.4.0 版本中，在数据桥接的基础上新增*连接*与*动作*概念，并逐步迁移现有数据桥接到连接与动作。连接用于管理数据集成与外部系统的连接，动作仅用于配置数据操作方式，连接可以在多个动作之间重复使用，以提供更大的灵活性和更好的可扩展性。目前 Kafka 生产者与 Azure Event Hub 生产者已经完成迁移。
- Dashboard 为规则引擎消息重发布动作提供了 MQTT 5.0 发布属性设置，允许用户更灵活的发布消息。

### 修复

- [#11565](https://github.com/emqx/emqx/pull/11565) 将 jq 库从 v0.3.10 升级至 v0.3.11。在此版本中，jq_port 程序将按需启动，除非 EMQX 中使用 jq 功能，否则不会出现在用户的进程中。此外，空闲的 jq_port 程序将在设定的一段时间后自动终止。注意：大多数运行 NIF 模式下的 EMQX 用户不会受到此更新的影响。

- [#11676](https://github.com/emqx/emqx/pull/11676) 隐藏 DEBUG 级别的日志中的部分敏感信息。

- [#11697](https://github.com/emqx/emqx/pull/11697) 在 EMQX 后端网络 (`gen_rpc`) 中禁用了过时的 TLS 版本和密码套件。增加了对后端网络的 tlsv1.3 支持，并引入了新的配置参数：`EMQX_RPC__TLS_VERSIONS` 和 `EMQX_RPC__CIPHERS`。

  对应的 `gen_rpc` PR: https://github.com/emqx/gen_rpc/pull/36

- [#11734](https://github.com/emqx/emqx/pull/11734) 修复了 IPv6 网络中集群配置的问题。新增了新的配置项 `rpc.listen_address` 和 `rpc.ipv6_only`，以允许 EMQX 集群的 RPC 服务和客户端使用 IPv6。

- [#11747](https://github.com/emqx/emqx/pull/11747) 更新 QUIC 到 msquic 2.2.3 版本。

- [#11796](https://github.com/emqx/emqx/pull/11796) 修复了 RPC schema，以确保客户端和服务器使用相同的传输驱动程序。

- [#11798](https://github.com/emqx/emqx/pull/11798) 修复了在执行 `./bin/emqx data import [FILE]` 后节点无法启动的问题。

  同时增强了 `apikey_key` 和 `apikey_name` 之间的关联以提高一致性和唯一标识性：

  - `apikey_key`：通过 Dashboard 生成 API 密钥时，`apikey_key` 现在会根据提供的易读性较强的 `apikey_name` 创建一个唯一值。
  - `apikey_name`：相反，当使用引导文件生成 API 密钥时，`apikey_name` 将基于关联的 `apikey_key` 生成为唯一值。

- [#11813](https://github.com/emqx/emqx/pull/11813) 修复了 schema，确保 RPC 客户端 SSL 端口与配置的服务器端口一致。此修复还确保了RPC 端口在 Helm 图表中的被正确打开。

- [#11819](https://github.com/emqx/emqx/pull/11819) 升级了 OpenTelemetry 库至 v1.3.1-emqx。该版本修复了在导出的指标中指标时间戳无效的问题。

- [#11861](https://github.com/emqx/emqx/pull/11861) 修复了 remote shell 中打印过多警告信息的问题。

- [#11722](https://github.com/emqx/emqx/pull/11722) 修复了同步请求模式下的 Kafka 生产者桥接在`正在连接`状态下无法缓存消息的问题。

- [#11724](https://github.com/emqx/emqx/pull/11724) 修复了一个与统计指标相关的问题，即消息发送到 Kafka 时，由于内部缓存、即使后来被成功传输，仍然被计为发送失败。

- [#11728](https://github.com/emqx/emqx/pull/11728) 改进了 LDAP 过滤字符串解析器，具体改进如下：
  - 自动转义过滤字符串中的特殊字符。
  - 修复了先前阻止使用 `dn` 作为过滤值的错误。
  
- [#11733](https://github.com/emqx/emqx/pull/11733) 解决了一个不兼容性问题，该问题导致在会话接管或通道驱逐时，如果会话位于运行 EMQX v5.2.x 或更早版本的远程节点上，可能会导致崩溃。

- [#11750](https://github.com/emqx/emqx/pull/11750) 日志不再输出使用 HTTP 服务进行认证和 HTTP 服务数据桥接的请求 Body。

- [#11760](https://github.com/emqx/emqx/pull/11760) 简化了用于 Cassandra 数据桥接健康检查的 CQL 查询，之前该查询在 Cassandra 服务器日志中生成了警告。

- [#11886](https://github.com/emqx/emqx/pull/11886) 修复了一个向后的插件兼容性的问题。

  目前，EMQX 对钩子挂载点名称进行验证，无效的钩子挂载点不能用于注册钩子。但是旧版本的插件模板使用了一些拼写错误的钩子挂载点，实际使用中的插件也可能存在这种问题，为了兼容以前的插件，我们允许使用旧的钩子挂载点来注册钩子，但会发出已被弃用的警告，这些钩子与以前一样不会被调用。

- [#11897](https://github.com/emqx/emqx/pull/11897) 修复了当集群节点几乎在同一时间启动时，节点间配置同步的时候等待循环竞争条件的问题。


## 5.3.0

*发布日期: 2023-09-29*

### 增强

- [#11597](https://github.com/emqx/emqx/pull/11597)  将 Ekka 升级到 0.15.13，包括以下增强：

  - 升级 Mria 到 0.6.2。
  - 可以通过配置设置初始化阶段数据同步批量大小，[Mria PR](https://github.com/emqx/mria/pull/159)。
  - 提升了 mria_membership 进程的健壮性，[Mria PR](https://github.com/emqx/mria/pull/156)。
  - 修复日志消息格式错误。
  - EMQX 配置中添加了 `node.default_bootstrap_batch_size` 选项。 增加此选项的值可以极大地减少复制节点的启动时间，特别是当 EMQX 集群互连网络延迟较高且 EMQX 内置数据库包含大量数据时，例如订阅数较多的情况。

- [#11620](https://github.com/emqx/emqx/pull/11620)  添加一个新的规则引擎 SQL 函数 `bytesize` 以获取字节字符串的大小。例如：`SELECT * FROM "t/#" WHERE bytesize(payload) > 10`。

- [#11642](https://github.com/emqx/emqx/pull/11642) 将 quicer 升级到版本 0.0.200，为启用 OpenSSL3 对 QUIC 传输的支持做准备。

- [#11610](https://github.com/emqx/emqx/pull/11610) 在 Dashboard 中实施了初步基于角色的访问控制。

  在此版本中，有两个预定义的角色：

  - 管理员：此角色可以访问所有资源。
  - 查看者：此角色只能查看资源和数据，对应于 REST API 中的所有 GET 请求。

- [#11631](https://github.com/emqx/emqx/pull/11631) 添加了单点登录（SSO）功能并与 LDAP 集成。
- [#11656](https://github.com/emqx/emqx/pull/11656) 集成了 SAML 2.0 支持以实现单点登录（SSO）。
- [#11599](https://github.com/emqx/emqx/pull/11599) 支持审计日志，会将来自 CLI、REST API 和 Dashboard 的操作记录在独立的日志文件中。

### 修复

- [#11682](https://github.com/emqx/emqx/pull/11682) 修复了在文件日志处理程序上将“轮换大小”设置为`infinity`时日志记录停止的问题。

- [#11567](https://github.com/emqx/emqx/pull/11567) 改进了 EMQX 的优雅关闭（`emqx stop` 命令）：
  
  - 将超时时间从1分钟增加到2分钟。
  - 如果 EMQX 无法在配置的超时时间内优雅地停止，则打印错误消息。
  - 在 EMQX 关闭过程中定期打印状态消息。
  
- [#11584](https://github.com/emqx/emqx/pull/11584) 修复了在 Windows 上当 os_mon 模块不可用时的遥测报告错误。

- [#11605](https://github.com/emqx/emqx/pull/11605) 降低了 CMD_overridden 的日志严重程度，从警告（warning）降至信息（info）。

- [#11622](https://github.com/emqx/emqx/pull/11622) 升级了 RPC 库 `gen_rpc` 从版本 2.8.1 到 3.1.0。

- [#11623](https://github.com/emqx/emqx/pull/11623) 将 `esockd` 库从版本 5.9.6 升级到 5.9.7。此次升级包括以下内容：

  - 对代理协议错误和超时进行了增强。[esockd pr#178](https://github.com/emqx/esockd/pull/178)
  - 将 `ssl_error` 异常的日志级别降低为信息级别。[esockd pr#180](https://github.com/emqx/esockd/pull/180)
  - 将异常 MQTT 数据包解析的日志级别从 `error` 降低为 `info`。
  - 在 `emqx ctl listeners` 命令输出中，当 TLS 握手失败（`ssl_error`）或 MQTT 数据包格式错误（`frame_error`）发生时，会增加 `shutdown_count` 计数器。

- [#11661](https://github.com/emqx/emqx/pull/11661) 修复了文件日志格式类型配置 `log.HANDLER.formatter` 设置为 `json` 时的问题。

  该 bug 在 v5.0.4 中引入，导致日志行不再是有效的 JSON，而是以时间戳字符串和级别名称作为前缀。

- [#11627](https://github.com/emqx/emqx/pull/11627) 修复了 HStreamDB 桥接中的资源清理问题。在此修复之前，HStreamDB 桥接在桥接配置更新期间可能会报告错误，因为 hstreamdb 客户端/生产者没有被正确停止。

## 5.2.1

*发布日期: 2023-09-20*

### 增强

- [#11487](https://github.com/emqx/emqx/pull/11487) 将认证功能中，基于 bcrypt 算法的密码加密的计算强度因子 (work factor) 限制在5-10的范围内，因为较高的值会消耗太多 CPU 资源。Bcrypt 库已更新以允许并行哈希计算。
- [#11568](https://github.com/emqx/emqx/pull/11568) 在消息重发布规则动作中，支持设置 MQTT 5.0 发布属性与用户属性。目前配置接口暂未完全集成到 Dashboard，将在后续版本中提供支持。
- [#11612](https://github.com/emqx/emqx/pull/11612) 在节点疏散期间，疏散所有断开连接的会话，而不仅仅是那些以 `clean_start` 设置为 `false` 开始的会话。
- [#11532](https://github.com/emqx/emqx/pull/11532) 改进了解析无效数据包时的错误消息，以提供更清晰的错误提示。

### 修复

- [#11493](https://github.com/emqx/emqx/pull/11493) 修复了REST API 示例文档中关于 `/api/v5/publish` 错误请求响应的描述。之前的文档示例指出错误请求的响应可以在响应体中返回一个列表，但实际情况并非如此。
- [#11499](https://github.com/emqx/emqx/pull/11499) 升级 Erlang/OTP 至 25.3.2-2，此版本从 mnesia_hook 日志消息中排除了敏感数据。
- [#11506](https://github.com/emqx/emqx/pull/11506) 此前尝试下载不存在的跟踪日志文件时，会下载一个空的文件。在实施此修复后，尝试使用 GET 请求 `/api/v5/trace/clientempty/download` 下载不存在的跟踪日志文件时，服务器现在将返回 404 状态码以及以下 JSON 消息：`{"code":"NOT_FOUND","message":"Trace is empty"}`。
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

## 5.2.0

*发布日期: 2023-09-07*

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

## 5.1.1

*发布日期: 2023-07-27*

### 增强

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

### 修复

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

- [#11150](https://github.com/emqx/emqx/pull/11150) 在启动 `emqx_psk` 应用程序时等待 Mria 表，确保即使没有初始化 PSK 文件，PSK 数据也能同步到 replicant 节点。

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

- [#11281](https://github.com/emqx/emqx/pull/11281) 恢复对特殊的共享订阅主题前缀 `$queue/` 的支持。

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

## 5.1.0

*发布日期: 2023-06-21*

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

## 5.0.4

*发布日期: 2023-05-26*

### 增强功能

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

### 错误修复

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
