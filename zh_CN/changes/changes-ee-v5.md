# EMQX 企业版 v5 版本

## 5.10.3

*发布日期：2026-01-28*

升级前请查看已知问题列表和不兼容变更列表。

### 增强

#### 部署

- [#16491](https://github.com/emqx/emqx/pull/16491) 开始为 macOS 15（Sequoia）发布安装包。

#### 可观测性

- [#16135](https://github.com/emqx/emqx/pull/16135) 为 `GET /monitor_current` HTTP API 新增了两个指标及其对应的速率：`rules_matched` 和 `actions_executed`。它们分别用于跟踪命中规则的数量，以及动作执行速率（即成功数 + 失败数）。
- [#16324](https://github.com/emqx/emqx/pull/16324) 新增对通过 HTTP API 发布消息的端到端链路追踪支持。

#### 安全

- [#16456](https://github.com/emqx/emqx/pull/16456) EMQX 现已支持使用无状态会话票据的 TLS 1.3 会话恢复机制，使客户端无需在服务器端存储会话状态即可恢复 TLS 会话。

  **配置**

  - **节点级别**：`node.tls_stateless_tickets_seed`

    用于生成 TLS 1.3 无状态会话票据的密钥种子。

  - **监听器级别**：`listeners.ssl.<name>.ssl_options.session_tickets`

    启用 TLS 1.3 会话恢复。支持的取值包括：

    - `disabled`（默认）
    - `stateless`
    - `stateless_with_cert`（在会话票据中包含证书信息）

  **说明**

  - 只有在配置了 `node.tls_stateless_tickets_seed`（非空），并且在监听器 SSL 选项中启用了 `session_tickets` 时，才会生成会话票据。
  - 如果启用了 `session_tickets`，但 `node.tls_stateless_tickets_seed` 为空，则不会生成会话票据，并且在启动监听器时会输出错误日志。

#### 网关

- [#16220](https://github.com/emqx/emqx/pull/16220) 新增 `jt808.frame.parse_unknown_message` 配置选项，允许 JT808 网关解析并透明转发消息 ID 未知的消息。
- [#16596](https://github.com/emqx/emqx/pull/16596) 新增对 JT/T 808 协议 2019 版本的支持。

#### 数据集成

- [#16511](https://github.com/emqx/emqx/pull/16511) 数据集成中新增对 IoTDB 表模型的支持。

### 修复

#### 核心 MQTT 功能

- [#16349](https://github.com/emqx/emqx/pull/16349) 修复了在处理 request-response-information 属性时，由于类型不匹配导致 MQTT v5 连接崩溃的问题。
- [#16514](https://github.com/emqx/emqx/pull/16514) 修复了当接收到大于客户端声明的 `Maximum-Packet-Size` 的 broker 消息时，WebSocket 连接可能崩溃的问题。

#### 规则引擎

- [#16489](https://github.com/emqx/emqx/pull/16489) 修复了以下规则函数始终返回 `undefined` 的问题：`msgid/0`、`qos/0`、`topic/0`、`topic/1`、`flags/0`、`flag/1`、`clientid/0`、`username/0`、`peerhost/0`、`payload/0`、`payload/1`。

  说明：这是针对 EMQX v4 的向后兼容修复。这些函数在 EMQX v5 及之后的版本中未在文档中说明。推荐的用法是直接从规则评估上下文中引用字段，例如使用 `SELECT clientid ...`，而不是 `SELECT clientid()`。

#### 数据集成

- [#16263](https://github.com/emqx/emqx/pull/16263) 健康检查现在仅验证分配给当前 EMQX 节点的分区的 leader 连通性，从而避免不必要的空闲连接和误报警。

  之前，Kafka 消费者连接器会检查所有分区的 leader 连通性。在集群部署中，每个节点只拥有部分分区，其余未分配分区的 leader 连接会保持空闲。由于 Kafka 会在超时后（默认 10 分钟）关闭空闲连接，这可能导致错误的连通性告警。

- [#16618](https://github.com/emqx/emqx/pull/16618) Kafka 的请求超时时间现在会自动设置为至少是元数据请求超时时间的两倍（最小为 30 秒）。当元数据请求耗时超出预期时，这可以减少不必要的重连和重试，尤其是在将元数据请求超时时间配置得较小的情况下，效果更为明显。

- [#16336](https://github.com/emqx/emqx/pull/16336) 修复了在通过控制台测试连通性或停止连接器时，可能因竞争条件导致超时的问题。

- [#16383](https://github.com/emqx/emqx/pull/16383) 改进了在使用 REST API 驱动时，IoTDB 连接器的健康检查机制。

  之前，健康检查过程中不会验证客户端凭据。现在，健康检查会发送一个轻量级的空操作（no-op）查询，从而能够尽早发现凭据配置错误的问题。

- [#16415](https://github.com/emqx/emqx/pull/16415) 将 Apache Pulsar 客户端升级至 2.1.2。

  当 Pulsar 生产者动作的 `batch_size` 配置为 `1` 时，producer 现在将对单条消息进行编码，而不是对仅包含一个元素的批次进行编码。这使得消费者可以使用 Key Share 策略进行负载共享。

- [#16507](https://github.com/emqx/emqx/pull/16507) 修复了 MQTT source 在其连接器重连后停止接收消息的问题。

  之前，当 MQTT source 的连接器从连接中断中恢复时，其订阅的主题不会被重新订阅，导致 source 在连接器重启之前无法继续工作。现在，source 会在重连后自动重新订阅。

- [#16585](https://github.com/emqx/emqx/pull/16585) 修复了 GreptimeDB TLS 连接失败的问题。

- [#16622](https://github.com/emqx/emqx/pull/16622) 修复了一个问题：当某个使用异步查询模式的动作在其连接器经历多次健康检查失败后发生断开时，可能会导致其回退动作被触发两次。

#### 集群

- [#16269](https://github.com/emqx/emqx/pull/16269) 修复了集群连接路由复制协议恢复流程中的一个问题：在远端仍然需要重新引导（re-bootstrap）的情况下，错误地跳过了该步骤。

- [#16317](https://github.com/emqx/emqx/pull/16317) 修复了集群连接垃圾回收逻辑中的一个问题，该问题可能在清理过期路由复制状态时，误将仍然有效的路由从内部路由表中移除。该问题仅在配置了多个相互独立的集群连接，并且其中部分连接长时间不可用时才会发生。

- [#16452](https://github.com/emqx/emqx/pull/16452) 将 `gen_rpc` 升级至 `3.5.1`。

  在升级 `gen_rpc` 之前，如果某个对端节点不可达，EMQX 可能会因连接超时而产生大量延迟出现的崩溃日志。新版 `gen_rpc` 不再存在这种长尾问题，并将崩溃日志转换为更易读的 `error` 日志，同时还对频繁出现的 `"failed_to_connect_server"` 日志进行了限流，以避免日志刷屏。

- [#16543](https://github.com/emqx/emqx/pull/16543) 提升了集群自动清理流程的健壮性。此前，如果在节点首次启动时禁用了自动清理功能，即使之后通过配置变更启用了该功能，自动清理也不会生效。

#### 安全

- [#16625](https://github.com/emqx/emqx/pull/16625) 为 SAML SSO 后端新增 `idp_signs_envelopes` 和 `idp_signs_assertions` 配置选项，用于控制签名校验，而此前该功能未能正常工作。为保持向后兼容，这两个选项默认值均为 `false`；当 IdP 对 SAML 响应进行签名时，用户需要显式将其设置为 `true`。

#### 访问控制

- [#16304](https://github.com/emqx/emqx/pull/16304) 修复了由于登录用户数据库记录不兼容，导致从 5.3.0 之前版本升级 EMQX 后无法启用多因素认证（MFA）的问题。
- [#16541](https://github.com/emqx/emqx/pull/16541) 修复了一个问题：在将 OIDC issuer URL 保存到配置文件时，系统会自动将其规范化为以斜杠（`/`）结尾，导致当 OIDC 提供方的 discovery 文档返回的 issuer 不带结尾斜杠时，出现 issuer 不匹配错误。

#### 可观测性

- [#16418](https://github.com/emqx/emqx/pull/16418) 减少了在发生资源异常（`resource_exception`）时生成的日志数量。这些日志现在已被限流，并且其中一些可能体积较大的字段内容已被脱敏处理。
- [#16535](https://github.com/emqx/emqx/pull/16535) 修复了在记录 `gen_rpc` 错误日志时格式化器崩溃的问题。此前，当 `gen_rpc` 记录某些错误消息（例如传输超时错误）时，EMQX 会出现 `"FORMATTER CRASH"` 错误并导致进程崩溃。现在，格式化器已能够正确处理这些错误消息，不再发生崩溃。

#### 网关

- [#16609](https://github.com/emqx/emqx/pull/16609) 修复了 JT/T 808 网关在处理参数设置（0x8103）和查询应答（0x0104）消息时，对 CAN 总线 ID 参数（0x0110~0x01FF）的处理问题。这些参数应在 JSON 中使用 Base64 编码的 BYTE[8] 数据类型，而不是字符串类型。

- [#16606](https://github.com/emqx/emqx/pull/16606) 修复了 CoAP 网关在基于 DTLS 的连接模式下无法正常工作的问题。

- [#16627](https://github.com/emqx/emqx/pull/16627) 为 JT/T 808 网关新增 GBK 字符编码支持。

  JT/T 808 协议规定 STRING 类型字段应使用 GBK 编码。为此新增了 `frame.string_encoding` 配置选项：

  - `utf8`（默认）：字符串按原样透传（向后兼容）。
  - `gbk`：将来自设备的 GBK 编码字符串转换为 UTF-8 用于 MQTT，将来自 MQTT 的 UTF-8 字符串转换为 GBK 用于设备。

  该配置会影响所有字符串字段，包括车牌号、驾驶员姓名、文本消息、区域名称以及客户端参数。无论该配置如何设置，MQTT 负载始终使用 UTF-8 编码。

## 5.10.2

*发布日期：2025-11-11*

升级前请查看已知问题列表和不兼容变更列表。

### 增强

#### 数据集成

- [#16183](https://github.com/emqx/emqx/pull/16183) EMQX 现在将有关过期消息被丢弃的日志（`buffer_worker_dropped_expired_messages`）以 warning 级别输出，并按resource ID 进行日志限流。
  这有助于识别哪些外部资源未能跟上消息输入速率，从而可能导致消息被丢弃。

- [#16206](https://github.com/emqx/emqx/pull/16206) 为 Kafka Producer 连接器新增了 `allow_auto_topic_creation` 配置项。
   启用该选项后，当客户端发送元数据请求（metadata fetch request）且目标主题不存在时，EMQX 将允许 Kafka 自动创建该主题。
   
- [#16209](https://github.com/emqx/emqx/pull/16209) GreptimeDB 连接器新增支持自定义时间戳列名，可通过配置参数 `ts_column` 指定。

#### 性能

- [#15949](https://github.com/emqx/emqx/pull/15949) 将监听器配置中的 `parse_unit` 选项默认值从 `chunk` 修改为 `frame`。当负载大小超过 socket 缓冲区（默认 4 KB）时，此更改可以显著降低 CPU 使用率。

  **注意：** 当 `parse_unit = frame` 时，如果 `PUBLISH` 报文超过允许的最大大小，EMQX 将关闭连接，而不是发送 `DISCONNECT` 报文。

- [#16165](https://github.com/emqx/emqx/pull/16165) 优化了 `GET /clients_v2` API 的性能。此前，在集群中连接客户端数量达到约 50,000 或以上时，调用该 API 获取客户端列表的响应速度可能非常慢，甚至会超时。

### 修复

#### 核心 MQTT 功能

- [#15884](https://github.com/emqx/emqx/pull/15884) 修复了一个问题：在极少数情况下，全局路由表可能会无限期保留已长时间离开集群的节点的路由信息。
- [#15518](https://github.com/emqx/emqx/pull/15518) 修复了一个竞争条件，该问题在大量共享订阅者同时断开连接时，可能导致集群中路由表和共享订阅状态持续出现不一致。

#### 访问控制

- [#16081](https://github.com/emqx/emqx/pull/16081) 修复了一个问题：使用扩展认证和内存会话的客户端可能因 `calling_self` 错误导致触发 `session_stepdown_request_exception` 异常并发生崩溃。

  <details> <summary>错误日志示例</summary>
  ```
  2025-09-24T07:13:08.973954+08:00 [error] clientid: someclientid, msg: session_stepdown_request_exception, peername: 127.0.0.1:41782, username: admin, error: exit, reason: calling_self, stacktrace: [{gen_server,call,3,[{file,"gen_server.erl"},{line,1222}]},{emqx_cm,request_stepdown,4,[{file,"emqx_cm.erl"},{line,427}]},{emqx_cm,do_takeover_begin,2,[{file,"emqx_cm.erl"},{line,398}]},{emqx_cm,takeover_session,2,[{file,"emqx_cm.erl"},{line,384}]},{emqx_cm,takeover_session_begin,2,[{file,"emqx_cm.erl"},{line,305}]},{emqx_session_mem,open,4,[{file,"emqx_session_mem.erl"},{line,210}]},{emqx_session,open,3,[{file,"emqx_session.erl"},{line,263}]},{emqx_cm,'-open_session/4-fun-1-',4,[{file,"emqx_cm.erl"},{line,290}]},{emqx_cm_locker,trans,2,[{file,"emqx_cm_locker.erl"},{line,32}]},{emqx_channel,post_process_connect,2,[{file,"emqx_channel.erl"},{line,575}]},{emqx_connection,with_channel,3,[{file,"emqx_connection.erl"},{line,852}]},{emqx_connection,process_msg,2,[{file,"emqx_connection.erl"},{line,470}]},{emqx_connection,process_msgs,2,[{file,"emqx_connection.erl"},{line,462}]},{emqx_connection,handle_recv,3,[{file,"emqx_connection.erl"},{line,406}]},{proc_lib,wake_up,3,[{file,"proc_lib.erl"},{line,340}]}], action: {takeover,'begin'}, ...
  ```
  
  </details>

#### 规则引擎

- [#16028](https://github.com/emqx/emqx/pull/16028) 修复了规则引擎中 `jq` 函数的内存泄漏问题。 此前，如果使用内置的 `jq` 函数 `index`（例如 `.key | index("name")`），会导致内存泄漏。

#### 数据集成

- [#16010](https://github.com/emqx/emqx/pull/16010) 修复了一个问题：如果原始规则的 SQL 未包含规则环境中的 `metadata` 字段，规则的备选动作可能会因 `function_clause` 错误而执行失败。

  错误日志示例：

  ```
  [error] tag: RESOURCE, msg: failed_to_trigger_fallback_action, reason: {error,function_clause}, fallback_kind: republish, primary_action_resource_id: <<"action:type:name:connector:type:name">>, republish_topic: <<"republish/topic">>
  ```

- [#16043](https://github.com/emqx/emqx/pull/16043) 优化了 Kafka 数据集成在发生 `not_all_kafka_partitions_connected` 事件时的日志信息。

- [#16046](https://github.com/emqx/emqx/pull/16046) 修复了一个潜在的内存溢出（OOM）崩溃问题：当加载或重启包含数百个动作的连接器配置时，可能导致崩溃。

- [#16138](https://github.com/emqx/emqx/pull/16138) 修复了一个 Redis 集群故障转移（failover）相关的问题，该问题可能导致连接器长时间停留在 “connecting” 状态。

  此前，EMQX 的 Redis 集群客户端仅在常规查询（如 `GET`）失败时才会刷新集群拓扑结构。然而，周期性发送的 `PING` 命令即使失败，也不会触发刷新操作。因此，在发生故障转移后，如果没有其他命令被发送，连接器可能会继续使用过时的拓扑信息，导致无法恢复连接。

  此次修复后，`PING` 命令失败也会触发集群拓扑刷新，确保连接器能够及时检测到故障转移并恢复正常工作。

- [#16212](https://github.com/emqx/emqx/pull/16212) 当缓冲队列处于 `memory` 模式时，移除了 Kafka 生产者的 linger time 设置。

#### 可观测性

- [#15963](https://github.com/emqx/emqx/pull/15963) 减少了在远程 shell（`remsh`）中进行循环评估时产生的过多审计日志。
- [#15967](https://github.com/emqx/emqx/pull/15967) 修复了一个问题：在清理大量审计日志时，Mnesia 事务阻塞可能导致内存迅速增长。

## 5.10.1

*发布日期：2025-09-18*

升级前请查看已知问题列表和不兼容变更列表。

### 增强

#### 性能

- [#15899](https://github.com/emqx/emqx/pull/15899) 通过确保在客户端断开时立即清除授权（authz）缓存来改进内存管理，减少不必要的内存消耗。
- [#15907](https://github.com/emqx/emqx/pull/15907) 优化了系统内存使用。当客户端 ID、用户名、密码和主题等字段长度超过 64 字节时，这些字段将被复制为新的二进制数据，而不再是原始报文的切片，以减少 Erlang 虚拟机中 “binary” 类型内存的占用。

#### 访问控制

- [#15294](https://github.com/emqx/emqx/pull/15294) 增强了 LDAP 认证和授权功能。LDAP 授权现在支持使用 JSON 格式的扩展 ACL 规则，除了现有的简单主题列表外，还可以在认证过程中基于客户端信息从 LDAP 获取 ACL 规则，并将其缓存在客户端的元数据中，以避免在授权过程中重复进行 LDAP 查询。
- [#15349](https://github.com/emqx/emqx/pull/15349) 优化了认证和授权的外部资源管理。此前，EMQX 在禁用认证或授权源的情况下，仍可能与配置的资源保持连接。

#### 数据集成

- [#15360](https://github.com/emqx/emqx/pull/15360) Amazon S3 Tables 动作现在支持以 Parquet 格式写入数据文件。
- [#15387](https://github.com/emqx/emqx/pull/15387) 为 Kinesis 生产者连接器和动作的健康检查增加了限速机制，以遵守 AWS API 限额并提升集群行为一致性：
  - 对 `ListStreams` 和 `DescribeStream` 接口的调用分别限制为每个连接器每秒 5 次和 10 次；
  - 集群中的核心节点协调分布式限速器，以确保限速一致。
  - 若健康检查被限速或超时，连接器或动作将保留原状态，而不是被标记为已断开。
  - 新增配置项 `resource_opts.health_check_interval_jitter`，在健康检查间隔基础上引入一个均匀随机延迟，减少同一连接器下多个动作同时发起健康检查的可能性。
- [#15542](https://github.com/emqx/emqx/pull/15542) 将 `erlcloud` 库升级到 `3.8.3.0`。升级后，如果 EMQX 运行的 EC2 实例具有正确的 IAM 权限来读写配置的 S3 存储桶，就可以在不指定访问密钥 ID 和私有访问密钥的情况下配置 S3 连接器。
- [#15845](https://github.com/emqx/emqx/pull/15845) MQTT 连接器的 `static_clientids` 配置项现支持为每个客户端 ID 分别指定用户名和密码，适用于如 Azure IoT Hub 等要求每个设备使用唯一凭证的场景。此增强提升了在集群部署中多节点连接的兼容性与稳定性。
- [#15911](https://github.com/emqx/emqx/pull/15911) HTTP 动作的 HTTP 请求超时时间现在可以通过 `resource_opts.request_ttl` 设置进行配置。此前，此超时时间固定为 30 秒且不可调整。

#### 可观测性

- [#15499](https://github.com/emqx/emqx/pull/15499) 添加了强制停用告警的 API 接口，允许管理员强制停用活动告警。
- [#15364](https://github.com/emqx/emqx/pull/15364) 为 OpenTelemetry 集成添加了 HTTP 头配置项，以适应带有 HTTP 认证的 collector。
- [#15944](https://github.com/emqx/emqx/pull/15944) 改进了以下连接器在资源被标记为 `disconnected` 状态时返回的信息：LDAP、Syskeeper、IoTDB、Snowflake（聚合模式）、JWKS 认证。
- [#15371](https://github.com/emqx/emqx/pull/15371) 为 `GET /actions_summary` 和 `GET /sources_summary` 接口的响应以及 `GET /actions/:id` 接口返回的备选动作添加了 `tags` 字段。

#### CLI

- [#15399](https://github.com/emqx/emqx/pull/15399) `node_dump` 工具现在导出当前系统配置为 HOCON 格式，并自动对敏感信息（如密码和密钥）进行脱敏处理，以确保安全。

### 修复

#### 核心 MQTT 功能

- [#15361](https://github.com/emqx/emqx/pull/15361) 修复了在解析格式错误的 `User-Property` 键值对时产生的 `function_clause` 错误，特别是当键值对的长度无效（过短）时。
- [#15396](https://github.com/emqx/emqx/pull/15396) 移除了已断开连接客户端的共享订阅的冗余清理操作。这些操作在高频断开情况下容易导致崩溃，并可能导致全局代理状态不一致。
- [#15416](https://github.com/emqx/emqx/pull/15416) 修复了 WebSocket 连接会话过期时偶尔出现的 warning 级别日志和崩溃问题。该问题由近期的 WebSocket 性能优化引入，虽然不会影响 Broker 的容量，但会在日志中产生如下错误信息：
  - `error: {function_clause,[{gen_tcp,send,[closed,[]],[{file,“gen_tcp.erl”},{line,966}]},{cowboy_websocket_linger,commands,3,[{file,“cowboy_websocket_linger.erl”},{line,665}]},...`
  - `message: {tcp,#Port<0.364>,<<136,130,...>>}, msg: emqx_session_mem_unknown_message`
- [#15872](https://github.com/emqx/emqx/pull/15872) 消除了在 CONNACK 后因非零原因代码断开连接时的 warning 日志 `unclean_terminate`。
- [#15518](https://github.com/emqx/emqx/pull/15518) 修复了一个竞争条件，该问题在大量共享订阅者同时断开连接时，可能导致集群中路由表和共享订阅状态持续出现不一致。

#### 部署

- [#15553](https://github.com/emqx/emqx/pull/15553) 修复了 EMQX Helm chart 的一个问题：在使用默认配置部署 EMQX 时，会启动多个副本，并导致除一个节点外其余节点全部崩溃。现在 Helm chart 默认改为单副本，因为集群部署需要商业 License。
- [#15712](https://github.com/emqx/emqx/pull/15712) 修复了从旧版本（5.9 之前）进行滚动升级时，节点启动失败的问题。在 EMQX 的早期版本中（5.9 之前），ZIP 时间戳编码器中的错误可能会在归档条目中存储无效的 "秒" 值（值对应于 DOS 时间格式中的第 30 或 31 个 2 秒槽）。
- [#15863](https://github.com/emqx/emqx/pull/15863) 修复了许可证配额报警文本。

#### 访问控制

- [#15818](https://github.com/emqx/emqx/pull/15818) 修正了 `{allow|deny, all}` ACL 规则的处理。以前，这些规则被内部转换为匹配 `#`，但由于 MQTT 规范的限制，未能正确匹配以 `$` 为前缀的主题（例如 `$testtopic/1`）。现在，使用了一个特殊的内部值，确保 `{allow|deny, all}` 规则能够正确匹配所有主题，包括以 `$` 为前缀的主题。

- [#15844](https://github.com/emqx/emqx/pull/15844) 添加了验证机制，禁止向内置数据库认证器添加空用户名。此类用户稍后无法通过 HTTP API 删除，因为它们会导致 API 路径混乱。
  如果您有此类用户并希望删除，请在 EMQX 控制台中运行以下命令：

  ```
  mria:transaction(emqx_authn_shard, fun() -> mnesia:delete(emqx_authn_mnesia, {'mqtt:global',<<>>}, write) end).
  ```

#### 集群

- [#15788](https://github.com/emqx/emqx/pull/15788) 修复了 etcd 集群发现问题。解决了使用共享 etcd 服务器时，EMQX 节点可能错误地加入到不同集群中的问题。此问题是由 etcd 客户端库中的 bug 引起的。

#### 智能数据中心

- [#15810](https://github.com/emqx/emqx/pull/15810) 引入了 `spb_{en,de}code` 函数来修正 `bytes_value` 指标的处理。修复了原始的 `sparkplug_{en,de}code` 函数的问题，因为它们没有根据 [Protobuf 规范](https://protobuf.dev/programming-guides/json/) 对 `bytes_value` 指标值进行 base64 编码/解码。为此，引入了新的 `spb_{en,de}code` 函数来正确编码/解码这些字段。旧的 `sparkplug_{en,de}code` 函数已被弃用，以保持向后兼容性。

#### 数据集成

- [#15394](https://github.com/emqx/emqx/pull/15394) 修复了一个罕见的竞争条件，导致动作指标因意外的异步回复而变得不一致。

- [#15603](https://github.com/emqx/emqx/pull/15603) 修复了 MQTT 桥接中的一个问题：过期的连接可能仍显示为 `Connected` 状态，且不会自动重连。

- [#15826](https://github.com/emqx/emqx/pull/15826) 改进了 Kafka 消费者连接器健康检查行为，尤其是在 ACL 限制的情况下。此前，若配置的用户缺少访问内部 `____emqx_consumer_probe` 消费者组的权限，则 Kafka 消费者连接器的健康检查可能会失败。通过此修复，如果 Kafka broker 返回 "ACL denied" 响应，EMQX 将视该连接为健康连接。

- [#15827](https://github.com/emqx/emqx/pull/15827) 修复了 GreptimeDB 驱动中的原子泄漏和进程泄漏问题。同时修复了在 GreptimeDB 动作中使用某些错误的写入语法时可能出现的 `function_clause` 错误。

- [#15836](https://github.com/emqx/emqx/pull/15836) 丰富了 Kafka 消费者源添加失败时的返回信息，例如因主题 ACL 被拒导致的失败。

- [#15866](https://github.com/emqx/emqx/pull/15866) 升级 Kafka 生产者库 `wolff` 到 4.0.12，以改善 Kafka 元数据响应中临时缺失分区的处理。
  在罕见的竞争条件下，Kafka 可能返回不完整的分区列表。此前，这种情况仅在主题被重新创建且分区较少时得到处理，而在分区临时缺失时未被处理。此修复解决了此问题，防止分区生产者挂起并无限期阻止关闭。
  
- [#15906](https://github.com/emqx/emqx/pull/15906) 将 Kafka 生产者库 Wolff 从 `4.0.12` 升级到 `4.0.13`，新增了处理 `ProduceResponse` 中 `record_list_too_large` 错误的功能。

- [#15902](https://github.com/emqx/emqx/pull/15902) 将 MQTT 客户端库升级至 1.13.8，提升了 MQTT 桥接的连接稳定性：

  - 当对端 Broker 未响应 PINGRESP 时，连接器将自动重连。
  - 若在等待 CONNACK 期间连接中断，基于 TLS 的桥接失败将更及时地被处理。

- [#15910](https://github.com/emqx/emqx/pull/15910) 修复了连接器中的一个问题：在较大的工作线程池中，若多个工作线程同时崩溃，可能导致连接器无法正常恢复。
  
  受影响并已修复的连接器包括：
  
  - MySQL
  
  - PostgreSQL
  - Oracle
  - SQLServer
  - TDEngine
  - Cassandra
  - Dynamo
  - HTTP
  - Couchbase
  - GCP PubSub
  - Snowflake
  
  同时将 `gun` 及相关依赖升级至 2.1.0。

#### API

- [#15547](https://github.com/emqx/emqx/pull/15547) 修复了 EMQX 在处理包含大体积请求体（例如 10MB）的 REST API 请求时可能失败的问题。
- [#15797](https://github.com/emqx/emqx/pull/15797) 为了提高与 EMQX 4.x 的兼容性，`batch publish` HTTP API (`/api/v5/publish/bulk`) 中的 `encoding` 参数已重新引入，并作为 `payload_encoding` 的别名。此更改解决了依赖于原始 `encoding` 参数的用户的迁移问题，确保现有的 EMQX v4 API 集成可以继续工作，无需软件级别的更改。

#### 速率限制

- [#15794](https://github.com/emqx/emqx/pull/15794) 改进了连接速率限制更新的行为，确保在监听器配置更新后，速率限制的更改（例如突发速率或速率阈值）会立即生效。此前，内部限速器状态未能正确刷新，可能导致速率限制比配置的严格。

#### 可观测性

- [#15785](https://github.com/emqx/emqx/pull/15785) 修复了在格式化网络拥塞告警消息时，若 MQTT 用户名包含非 ASCII 字符，可能导致崩溃的问题。

#### 网关

- [#15342](https://github.com/emqx/emqx/pull/15342) 修复了 NATS 网关中的崩溃问题，该问题由客户端信息覆盖模板引用了未定义的报文字段引起。系统现在会返回空二进制而非未定义的原子值。

## 5.9.2

*发布日期 2025-11-14*

升级前请查看已知问题列表和不兼容变更列表。

### 增强

#### 核心 MQTT 功能

- [#15773](https://github.com/emqx/emqx/pull/15773) 在客户端重连时增加了 Client ID 注册的节流机制。
  - 当之前的会话清理仍在进行中时，新连接使用相同 Client ID 将被节流，避免客户端在频繁重连时导致系统不稳定。
  - 受影响的客户端会在 `CONNACK` 中收到原因码 `137` (Server Busy)，并带有 Reason-String `"THROTTLED"`，应在会话清理完成后重试。
  - 修复了当另一个连接正在注册相同 Client ID 时返回的原因码，现在会正确返回 `137` 而不是 `133`。

#### 数据集成

- [#15542](https://github.com/emqx/emqx/pull/15542) 将 `erlcloud` 库升级到 `3.8.3.0`。升级后，如果 EMQX 运行的 EC2 实例具有正确的 IAM 权限来读写配置的 S3 存储桶，就可以在不指定访问密钥 ID 和私有访问密钥的情况下配置 S3 连接器。
- [#15585](https://github.com/emqx/emqx/pull/15585) 将 Kafka `brod` 客户端升级至 4.4.4，扩展了对更多 Kafka API 的支持，并解决了 `JoinGroups` API 版本 `v0` 和 `v1` 弃用的问题。
- [#15845](https://github.com/emqx/emqx/pull/15845) MQTT 连接器的 `static_clientids` 配置项现支持为每个客户端 ID 分别指定用户名和密码，适用于如 Azure IoT Hub 等要求每个设备使用唯一凭证的场景。此增强提升了在集群部署中多节点连接的兼容性与稳定性。
- [#15911](https://github.com/emqx/emqx/pull/15911) HTTP 动作的 HTTP 请求超时时间现在可以通过 `resource_opts.request_ttl` 设置进行配置。此前，此超时时间固定为 30 秒且不可调整。

#### 可观测性

- [#15499](https://github.com/emqx/emqx/pull/15499) 添加了强制停用告警的 API 接口，允许管理员强制停用当前告警。
- [#15944](https://github.com/emqx/emqx/pull/15944) 改进了以下连接器在资源被标记为 `disconnected` 状态时返回的信息：LDAP、Syskeeper、IoTDB、Snowflake（聚合模式）、JWKS 认证。

#### 性能

- [#15536](https://github.com/emqx/emqx/pull/15536) 默认禁用了 `node.global_gc_interval` 配置。该配置在启用时会引发 CPU 波动和消息延迟，而 Erlang 内置 GC 已足够应对大部分场景。禁用后整体性能更稳定。

- [#15539](https://github.com/emqx/emqx/pull/15539) 优化 Erlang VM 参数以提升性能与稳定性：
    - 增大分布式通道缓冲区至 32 MB（`+zdbbl 32768`），避免在高强度 Mnesia 操作中触发 `busy_dist_port` 报警。
    - 禁用调度器忙等待（`+sbwt none +sbwtdcpu none +sbwtdio none`），降低操作系统报告的 CPU 使用率。
    - 设置调度器绑定类型为 db（`+stbt db`），以降低消息延迟。

- [#15907](https://github.com/emqx/emqx/pull/15907) 优化了系统内存使用。当客户端 ID、用户名、密码和主题等字段长度超过 64 字节时，这些字段将被复制为新的二进制数据，而不再是原始报文的切片，以减少 Erlang 虚拟机中 "binary" 类型内存的占用。

- [#15899](https://github.com/emqx/emqx/pull/15899) 通过确保在客户端断开时立即清除授权（authz）缓存来改进内存管理，减少不必要的内存消耗。

- [#15949](https://github.com/emqx/emqx/pull/15949) 将监听器配置中的 `parse_unit` 选项默认值从 `chunk` 修改为 `frame`。当负载大小超过 socket 缓冲区（默认 4 KB）时，此更改可以显著降低 CPU 使用率。

  **注意：** 当 `parse_unit = frame` 时，如果 `PUBLISH` 报文超过允许的最大大小，EMQX 将关闭连接，而不是发送 `DISCONNECT` 报文。

- [#16165](https://github.com/emqx/emqx/pull/16165) 优化了 `GET /clients_v2` API 的性能。此前，在集群中连接客户端数量达到约 50,000 或以上时，调用该 API 获取客户端列表的响应速度可能非常慢，甚至会超时。

### 修复

#### 核心 MQTT 功能

- [#15884](https://github.com/emqx/emqx/pull/15884) 修复了一个问题：在极少数情况下，全局路由表可能会无限期保留已长时间离开集群的节点的路由信息。
- [#15518](https://github.com/emqx/emqx/pull/15518) 修复了一个竞争条件，该问题在大量共享订阅者同时断开连接时，可能导致集群中路由表和共享订阅状态持续出现不一致。
- [#15872](https://github.com/emqx/emqx/pull/15872) 消除了在 CONNACK 后因非零原因代码断开连接时的 warning 日志 `unclean_terminate`。

#### 部署

- [#15553](https://github.com/emqx/emqx/pull/15553) 修复了 EMQX Helm chart 的一个问题：在使用默认配置部署 EMQX 时，会启动多个副本，并导致除一个节点外其余节点全部崩溃。现在 Helm chart 默认改为单副本，因为集群部署需要商业 License。

- [#15580](https://github.com/emqx/emqx/pull/15580) 在 EMQX Enterprise Helm Chart 中新增变量 `emqxLicenseSecretRef`，可指定包含 EMQX License Key 的 Kubernetes Secret，使 License 自动生效。 该变量替代了无效的 `emqxLicenseSecretName`，后者仅创建并挂载 Secret 文件，却未将 License 应用于 EMQX。

- [#15712](https://github.com/emqx/emqx/pull/15712) 修复了从旧版本（5.9 之前）进行滚动升级时，节点启动失败的问题。在 EMQX 的早期版本中（5.9 之前），ZIP 时间戳编码器中的错误可能会在归档条目中存储无效的 "秒" 值（值对应于 DOS 时间格式中的第 30 或 31 个 2 秒槽）。

- [#15863](https://github.com/emqx/emqx/pull/15863) 修复了 License 配额报警文本。


#### 安全

- [#15581](https://github.com/emqx/emqx/pull/15581) 将 Erlang/OTP 从 26.2.5.2 升级至 26.2.5.14，包含两个与 TLS 相关的重要修复：
  - 修复了因证书更新过程中的竞争条件导致的 TLS 连接崩溃。
  - 现在可以正常使用 RSASSA-PSS 签名的 RSA 证书。此前，TLS 握手可能因 `bad_certificate / invalid_signature` 错误而失败。

- [#16237](https://github.com/emqx/emqx/pull/16237) 修复了禁用 OIDC SSO 后仍可能输出与 SSO 相关日志的问题。
- [#16217](https://github.com/emqx/emqx/pull/16217) 修复了在多节点集群环境下，OIDC 登录回调可能无法找到对应用户会话的问题。

#### 访问控制


- [#15818](https://github.com/emqx/emqx/pull/15818) 修正了 `{allow|deny, all}` ACL 规则的处理。以前，这些规则被内部转换为匹配 `#`，但由于 MQTT 规范的限制，未能正确匹配以 `$` 为前缀的主题（例如 `$testtopic/1`）。现在，使用了一个特殊的内部值，确保 `{allow|deny, all}` 规则能够正确匹配所有主题，包括以 `$` 为前缀的主题。

- [#15844](https://github.com/emqx/emqx/pull/15844) 添加了验证机制，禁止向内置数据库认证器添加空用户名。此类用户稍后无法通过 HTTP API 删除，因为它们会导致 API 路径混乱。 如果您有此类用户并希望删除，请在 EMQX 控制台中运行以下命令：

  ```erlang
  mria:transaction(emqx_authn_shard, fun() -> mnesia:delete(emqx_authn_mnesia, {'mqtt:global',<<>>}, write) end).
  ```

- [#16081](https://github.com/emqx/emqx/pull/16081) 修复了一个问题：使用扩展认证和内存会话的客户端可能因 `calling_self` 错误导致触发 `session_stepdown_request_exception` 异常并发生崩溃。

  <details> <summary>示例错误日志</summary>


  ```
  2025-09-24T07:13:08.973954+08:00 [error] clientid: someclientid, msg: session_stepdown_request_exception, peername: 127.0.0.1:41782, username: admin, error: exit, reason: calling_self, stacktrace: [{gen_server,call,3,[{file,"gen_server.erl"},{line,1222}]},{emqx_cm,request_stepdown,4,[{file,"emqx_cm.erl"},{line,427}]},{emqx_cm,do_takeover_begin,2,[{file,"emqx_cm.erl"},{line,398}]},{emqx_cm,takeover_session,2,[{file,"emqx_cm.erl"},{line,384}]},{emqx_cm,takeover_session_begin,2,[{file,"emqx_cm.erl"},{line,305}]},{emqx_session_mem,open,4,[{file,"emqx_session_mem.erl"},{line,210}]},{emqx_session,open,3,[{file,"emqx_session.erl"},{line,263}]},{emqx_cm,'-open_session/4-fun-1-',4,[{file,"emqx_cm.erl"},{line,290}]},{emqx_cm_locker,trans,2,[{file,"emqx_cm_locker.erl"},{line,32}]},{emqx_channel,post_process_connect,2,[{file,"emqx_channel.erl"},{line,575}]},{emqx_connection,with_channel,3,[{file,"emqx_connection.erl"},{line,852}]},{emqx_connection,process_msg,2,[{file,"emqx_connection.erl"},{line,470}]},{emqx_connection,process_msgs,2,[{file,"emqx_connection.erl"},{line,462}]},{emqx_connection,handle_recv,3,[{file,"emqx_connection.erl"},{line,406}]},{proc_lib,wake_up,3,[{file,"proc_lib.erl"},{line,340}]}], action: {takeover,'begin'}, ...
  ```

  </details>

#### 数据集成


- [#15616](https://github.com/emqx/emqx/pull/15616) 修复了 Kafka 健康检查逻辑。即使默认探测主题返回 `topic_authorization_failed` 错误，Kafka 连接也会被视为健康。

- [#15826](https://github.com/emqx/emqx/pull/15826) 改进了 Kafka 消费者连接器健康检查行为，尤其是在 ACL 限制的情况下。此前，若配置的用户缺少访问内部 `____emqx_consumer_probe` 消费者组的权限，则 Kafka 消费者连接器的健康检查可能会失败。通过此修复，如果 Kafka broker 返回 "ACL denied" 响应，EMQX 将视该连接为健康连接。

- [#15827](https://github.com/emqx/emqx/pull/15827) 修复了 GreptimeDB 驱动中的原子泄漏和进程泄漏问题。同时修复了在 GreptimeDB 动作中使用某些错误的写入语法时可能出现的 `function_clause` 错误。

- [#15836](https://github.com/emqx/emqx/pull/15836) 丰富了 Kafka 消费者源添加失败时的返回信息，例如因主题 ACL 被拒导致的失败。

- [#15850](https://github.com/emqx/emqx/pull/15850) 修复了一个问题：MQTT 桥接错误地将已失效的连接显示为已连接状态，并且未能重新建立连接。

- [#15866](https://github.com/emqx/emqx/pull/15866) 将 Kafka 生产者库 wollf 升级至 `4.0.12`，以改进对 Kafka 元数据响应中临时缺失分区的处理。

  在极少数竞争条件下，Kafka 可能返回不完整的分区列表。此前，仅在主题被重新创建且分区数量减少的情况下进行了处理，但未覆盖分区暂时缺失的情况。该缺陷可能导致分区生产者阻塞，并使节点在关闭时无限等待。

- [#15906](https://github.com/emqx/emqx/pull/15906) 将 Kafka 生产者库 Wolff 从 `4.0.12` 升级到 `4.0.13`，新增了处理 `ProduceResponse` 中 `record_list_too_large` 错误的功能。

- [#15902](https://github.com/emqx/emqx/pull/15902) 将 MQTT 客户端库升级至 1.13.8，提升了 MQTT 桥接的连接稳定性：

  - 当对端 Broker 未响应 PINGRESP 时，连接器将自动重连。
  - 若在等待 CONNACK 期间连接中断，基于 TLS 的桥接失败将更及时地被处理。

- [#15910](https://github.com/emqx/emqx/pull/15910) 修复了连接器中的一个问题：在较大的工作线程池中，若多个工作线程同时崩溃，可能导致连接器无法正常恢复。

  受影响并已修复的连接器包括：

  - MySQL
  - PostgreSQL
  - Oracle
  - SQLServer
  - TDEngine
  - Cassandra
  - Dynamo
  - HTTP
  - Couchbase
  - GCP PubSub
  - Snowflake

  同时将 `gun` 及相关依赖升级至 2.1.0。

- [#16010](https://github.com/emqx/emqx/pull/16010) 修复了一个问题：如果原始规则的 SQL 未包含规则环境中的 `metadata` 字段，规则的备选动作可能会因 `function_clause` 错误而执行失败。

  错误日志示例：

  ```
  [error] tag: RESOURCE, msg: failed_to_trigger_fallback_action, reason: {error,function_clause}, fallback_kind: republish, primary_action_resource_id: <<"action:type:name:connector:type:name">>, republish_topic: <<"republish/topic">>
  ```

- [#16043](https://github.com/emqx/emqx/pull/16043) 优化了 Kafka 数据集成在发生 `not_all_kafka_partitions_connected` 事件时的日志信息。

- [#16046](https://github.com/emqx/emqx/pull/16046) 修复了一个潜在的内存溢出（OOM）崩溃问题：当加载或重启包含数百个动作的连接器配置时，可能导致崩溃。

- [#16138](https://github.com/emqx/emqx/pull/16138) 修复了一个 Redis 集群故障转移（failover）相关的问题，该问题可能导致连接器长时间停留在 "connecting" 状态。

  此前，EMQX 的 Redis 集群客户端仅在常规查询（如 `GET`）失败时才会刷新集群拓扑结构。然而，周期性发送的 `PING` 命令即使失败，也不会触发刷新操作。因此，在发生故障转移后，如果没有其他命令被发送，连接器可能会继续使用过时的拓扑信息，导致无法恢复连接。

  此次修复后，`PING` 命令失败也会触发集群拓扑刷新，确保连接器能够及时检测到故障转移并恢复正常工作。


#### 规则引擎


- [#16028](https://github.com/emqx/emqx/pull/16028) 修复了规则引擎中 `jq` 函数的内存泄漏问题。

  此前，如果使用内置的 `jq` 函数 `index`（例如 `.key | index("name")`），会导致内存泄漏。

#### 数据智能中心


- [#15706](https://github.com/emqx/emqx/pull/15706) 修复了可能导致消息转换 和 Schema 验证表现不一致的索引问题。删除某个条目后可能破坏主题索引，导致后续条目即使被禁用仍然保持启用状态。
- [#15708](https://github.com/emqx/emqx/pull/15708) 修复了外部 Schema Registry 在节点重启后未能重新加载的问题。
- [#15810](https://github.com/emqx/emqx/pull/15810) 引入了 `spb_{en,de}code` 函数来修正 `bytes_value` 指标的处理。修复了原始的 `sparkplug_{en,de}code` 函数的问题，因为它们没有根据 [Protobuf 规范](https://protobuf.dev/programming-guides/json/) 对 `bytes_value` 指标值进行 base64 编码/解码。为此，引入了新的 `spb_{en,de}code` 函数来正确编码/解码这些字段。旧的 `sparkplug_{en,de}code` 函数已被弃用，以保持向后兼容性。

#### 可观测性

- [#15639](https://github.com/emqx/emqx/pull/15639) 修复了 `packets.subscribe.auth_error` 指标未在订阅认证失败时正确递增的问题。
- [#15785](https://github.com/emqx/emqx/pull/15785) 修复了在格式化网络拥塞告警消息时，若 MQTT 用户名包含非 ASCII 字符，可能导致崩溃的问题。
- [#15963](https://github.com/emqx/emqx/pull/15963) 减少了在远程 shell（`remsh`）中进行循环评估时产生的过多审计日志。
- [#15967](https://github.com/emqx/emqx/pull/15967) 修复了一个问题：在清理大量审计日志时，Mnesia 事务阻塞可能导致内存迅速增长。

#### 网关

- [#15679](https://github.com/emqx/emqx/pull/15679) 修复了 ExProto、JT/T 808、GB/T 32960 和 OCPP 网关的 global chain name 错误。这些网关的内置认证数据此前被错误地归类到 `unknown:global`，导致网关之间产生冲突。
- [#15699](https://github.com/emqx/emqx/pull/15699) 修复了当节点停止或重启时，网关（如 CoAP）的内置认证数据被错误删除的问题。
- [#15822](https://github.com/emqx/emqx/pull/15822) 修复了 OCPP 网关连接在发送一定数量的消息后会崩溃的问题。

#### 速率限制


- [#15794](https://github.com/emqx/emqx/pull/15794) 改进了连接速率限制更新的行为，确保在监听器配置更新后，速率限制的更改（例如突发速率或速率阈值）会立即生效。此前，内部限速器状态未能正确刷新，可能导致速率限制比配置的严格。

#### ExHook


- [#15683](https://github.com/emqx/emqx/pull/15683) 修复了 ExHook 的 TLS 选项，使 gRPC 客户端能够在 TLS 握手过程中正确验证服务器主机名。

## 5.10.0

*发布日期：2025-06-09*

升级前请查看已知问题列表和不兼容变更列表。

### 增强

#### 核心 MQTT 功能

- [#15118](https://github.com/emqx/emqx/pull/15118) 新增配置项 `mqtt.subscription_max_qos_rules`，用于控制每个客户端订阅所允许的最大 QoS 等级。管理员可以基于特定主题的匹配规则，限制客户端在 SUBSCRIBE 报文中请求的 QoS 等级。目前仅支持基于 SUBSCRIBE 报文中主题的少量匹配规则。
- [#15246](https://github.com/emqx/emqx/pull/15246) 提升了 WebSocket 连接的性能与资源效率：
  - 在模拟一对一 MQTT 消息交互的基准测试中，WebSocket 连接的 CPU 使用率降低约 20%，内存占用也有小幅优化。
  - 启用监听器级连接数限制时，WebSocket 连接的建立效率显著提升，尤其适用于管理大量连接的节点。

#### 安装部署

- [#14791](https://github.com/emqx/emqx/pull/14791) 在 EMQX 的 Helm Chart 中新增对 StatefulSet 自定义注解的支持，可用于在 ConfigMap 或 Secret 发生变更时自动重启 Pod。该功能提升了在 Kubernetes 上管理 EMQX 的自动化程度和可靠性。

#### 访问控制

- [#15250](https://github.com/emqx/emqx/pull/15250) 改进了 LDAP 绑定认证中的逻辑，支持正确从 LDAP 条目的属性中提取 `is_superuser` 标志。
   此前无论条目中是否包含 `isSuperuser` 属性，该值始终被错误地设置为 `false`。
- [#15249](https://github.com/emqx/emqx/pull/15249) 改进了 LDAP 认证与权限功能。
  - 新增对 LDAP `filter` 和 `base_dn` 配置项的校验。
  - 修复了多个变量插值相关的问题。

#### 规则引擎

- [#15001](https://github.com/emqx/emqx/pull/15001) 在规则引擎的 SQL 中新增 `ai_completion` 函数，可用于调用 AI 服务处理数据。

- [#15201](https://github.com/emqx/emqx/pull/15201) 在 AI 补全提供器配置中新增 `base_url` 选项。

- [#15188](https://github.com/emqx/emqx/pull/15188) 规则引擎的事件主题现在引入了命名空间。

  | 旧的时间主题                            | 新的事件主题                            |
  | :-------------------------------------- | :-------------------------------------- |
  | `$events/client_connected`              | `$events/client/connected`              |
  | `$events/client_disconnected`           | `$events/client/disconnected`           |
  | `$events/client_connack`                | `$events/client/connack`                |
  | `$events/client_check_authz_complete`   | `$events/auth/check_authz_complete`     |
  | `$events/client_check_authn_complete`   | `$events/auth/check_authn_complete`     |
  | `$events/session_subscribed`            | `$events/session/subscribed`            |
  | `$events/session_unsubscribed`          | `$events/session/unsubscribed`          |
  | `$events/message_delivered`             | `$events/message/delivered`             |
  | `$events/message_acked`                 | `$events/message/acked`                 |
  | `$events/message_dropped`               | `$events/message/dropped`               |
  | `$events/delivery_dropped`              | `$events/message/delivery_dropped`      |
  | `$events/message_transformation_failed` | `$events/message_transformation/failed` |
  | `$events/schema_validation_failed`      | `$events/schema_validation/failed`      |

  旧的事件主题仍保留，以保持兼容性。

- [#15175](https://github.com/emqx/emqx/pull/15175) 规则引擎现在支持使用通配符匹配事件主题。例如可使用 `$events/#`、`$events/sys/+` 等模式一次匹配多个事件。

#### 数据智能中心

- [#15174](https://github.com/emqx/emqx/pull/15174) 支持通过上传 Protobuf 源文件包的方式注册 Schema。

  例如，假设 Protobuf 源文件包位于 `/tmp/bundle.tar.gz`，其目录结构如下，其中 `a.proto` 是根 schema 文件：

  ```
  .
  ├── a.proto
  ├── c.proto
  └── nested
      └── b.proto
  ```

  可通过 HTTP API 使用该文件包创建一个新 schema，示例如下：

  ```sh
  curl -v http://127.0.0.1:18083/api/v5/schema_registry_protobuf/bundle \
    -XPOST \
    -H "Authorization: Bearer xxxx" \
    -F bundle=@/tmp/bundle.tar.gz \
    -F name=my_cool_schema \
    -F root_proto_file=a.proto
  ```

#### 数据集成

- [#15248](https://github.com/emqx/emqx/pull/15248) EMQX 新增与 [Doris](https://doris.apache.org/) 的集成，支持通过 SQL 语句进行数据写入 。

- [#15218](https://github.com/emqx/emqx/pull/15218) 在 Kafka 生产者和消费者连接器中支持使用 IAM 认证连接 Amazon MSK（托管版 Apache Kafka）。当 EMQX 部署在 AWS EC2 上时，可通过 AWS SDK 为 Kafka 客户端生成 OAuth Bearer 令牌。

- [#15157](https://github.com/emqx/emqx/pull/15157) Snowflake 连接器支持通过指定私钥文件路径进行身份验证，作为使用密码的替代方案。

  用户可选择使用密码、私钥，或在 `/etc/odbc.ini` 中配置其他身份验证参数。

- [#14983](https://github.com/emqx/emqx/pull/14983) EMQX 新增与 S3Tables 的数据集成。

  **当前限制：**

  - 仅支持 [S3Tables](https://docs.aws.amazon.com/AmazonS3/latest/userguide/s3-tables.html) 目录（表数据和元数据需存储在 S3 中）；
  - 仅支持 [Iceberg 表格式 v2](https://iceberg.apache.org/spec/#version-2-row-level-deletes)；
  - 仅支持以下分区转换函数：
    - `identity`
    - `void`
    - `bucket[N]`；
  - 数据文件仅支持写入为 [Avro 格式](https://avro.apache.org/docs/1.12.0/specification/)。

- [#15331](https://github.com/emqx/emqx/pull/15331) 修复了 InfluxDB 操作中的一个问题：当 `WriteSyntax` 中的 `timestamp` 留空且规则中也没有时间戳字段时，行协议转换会失败。现在改为使用系统当前的毫秒值作为时间戳，并强制使用毫秒级精度。

- [#15348](https://github.com/emqx/emqx/pull/15348) 允许为 SSL 客户端配置 `middlebox_comp_mode` 选项。此前，`middlebox_comp_mode` 在所有 TLS 1.3 连接中始终默认启用（`true`）。现在，该选项已支持用户自定义配置。为了保证在大多数网络环境中的兼容性，默认值仍为 `true`。

  在某些罕见情况下，如果 TLS 握手失败并出现如下错误：`unexpected_message, TLS client: In state hello_retry_middlebox_assert ...`，你可以尝试将 `middlebox_comp_mode` 设置为 `false` 以解决问题。


#### 多租户

- [#15253](https://github.com/emqx/emqx/pull/15253) 新增两个多租户相关的 API：`GET /mt/ns_list_details` 和 `GET /mt/ns_list_managed_details`。
   这两个接口与已有的对应接口功能类似，但会额外返回与命名空间相关的元数据信息，而不仅仅是名称。
- [#15160](https://github.com/emqx/emqx/pull/15160) 新增多租户管理接口 `DELETE /mt/bulk_delete_ns`，支持批量删除命名空间。

#### CLI

- [#15158](https://github.com/emqx/emqx/pull/15158) 新增命令 `emqx ctl conf remove x.y.z`，用于从现有配置中移除 `x.y.z` 这一配置路径。

#### 网关

- [#15138](https://github.com/emqx/emqx/pull/15138) 新增 **NATS 网关**，支持通过 TCP/TLS 和 WS/WSS 协议接收 NATS 客户端连接。

  例如，NATS 网关会将以下 NATS 消息转换为主题为 `sub/t`、payload 为 `hello` 的 MQTT 消息，并可无缝集成到 EMQX 的规则引擎、数据集成等功能中：

  ```
  PUB sub.t 5  
  hello
  ```


#### MQTT 会话持久化

- [#15043](https://github.com/emqx/emqx/pull/15043) 为 DS Raft 后端添加基础监控指标，用于观测集群状态、数据库概况、分片复制情况以及副本切换等信息。

### 修复

#### 访问控制

- [#15184](https://github.com/emqx/emqx/pull/15184) 修复了在创建新的黑名单列表记录失败时，错误信息格式不正确的问题。

#### 集群

- [#15304](https://github.com/emqx/emqx/pull/15304) 修复在使用 `static` 发现策略时，复制节点发现核心节点的问题。

  之前，复制节点可能会忽略未在 `static_seeds` 列表中显式列出的核心节点，这可能导致集群视图不一致和负载不均衡的问题。

- [#15180](https://github.com/emqx/emqx/pull/15180) 修复 `ekka_locker` 中未正确处理 RPC（`badrpc`）错误的问题。该问题会导致锁操作被错误地认为成功，进而引发集群中锁状态不一致和死锁风险。

#### 安全

- [#15159](https://github.com/emqx/emqx/pull/15159) 优化了 CRL 分发点（CDP）的处理机制：当某个 CRL 分发点 URL 连续刷新失败达到一定次数（默认 60 秒）后，该 URL 将被移除并停止刷新，以避免日志大量堆积。

#### 规则引擎

- [#15247](https://github.com/emqx/emqx/pull/15247) 修复使用命令 `emqx ctl conf remove dashboard.sso.<BACKEND_NAME>` 时出现 `function_clause` 错误日志的问题。

#### 数据智能中心

- [#15285](https://github.com/emqx/emqx/pull/15285) 为 External HTTP Schema 请求添加了 `content-type` 请求头。
- [#15224](https://github.com/emqx/emqx/pull/15224) 修复通过 Dashboard 更新 External Schema Registry 时，密码字段意外被修改为 `******` 的问题。
- [#15190](https://github.com/emqx/emqx/pull/15190) 支持在消息转换中设置固定的 QoS 和主题。

#### 数据集成

- [#15274](https://github.com/emqx/emqx/pull/15274) 现在，Postgres、Matrix 和 TimescaleDB 连接器在健康检查失败时会触发完整的重连。
   在此之前，部分情况下连接会变得不可用，但系统仍尝试继续使用它，可能导致请求阻塞，甚至引发内存溢出问题。

- [#15234](https://github.com/emqx/emqx/pull/15234) 为规则测试新增了追踪事件，当动作尚未安装或使用了 Republish Fallback 动作时，这些事件将显示在前端模拟测试中。

- [#15219](https://github.com/emqx/emqx/pull/15219) 减少 ClickHouse 连接器在健康检查超时时的日志输出量。
   同时，当出现超时时，连接器状态将标记为“连接中”（connecting）而非“已断开”，这意味着不会再因此触发完整重连。

- [#15154](https://github.com/emqx/emqx/pull/15154) 修复聚合模式下运行的动作（如 S3、Azure Blob Storage、Snowflake）中的一个罕见竞争条件，避免类似如下的崩溃日志：

  ```
  ** Reason for termination ==
  ** {function_clause,[{emqx_connector_aggregator,handle_close_buffer,[...], ...
  ```

- [#15147](https://github.com/emqx/emqx/pull/15147) 修复使用模拟输入数据进行规则测试时，某些动作未在渲染请求后发送追踪事件的问题。

  受影响的动作包括：

  - Couchbase
  - Snowflake
  - IoTDB（Thrift 驱动）

- [#15306](https://github.com/emqx/emqx/pull/15306) 修复了一个问题：连接器的健康检查返回后，无论依赖的 Action 和 Source 当前状态如何，都会无条件触发它们的健康检查。

#### 多租户

- [#15242](https://github.com/emqx/emqx/pull/15242) 修复在为多租户配置限流器后，节点重启时初始化限流器过程中日志中出现如下错误信息的问题：

  ```
  2025-05-15T16:45:13.276895+08:00 [error] clientid: ns3mqttx_620053b2_100, msg: hook_callback_exception, peername: 127.0.0.1:39364, username: ns3, reason: {limiter_group_not_found,{mt_tenant,<<"ns3">>}}, stacktrace: [{emqx_limiter,connect,1,[{file,"emqx_limiter.erl"},{line,134}]}
  ```

#### 可观测性

- [#15299](https://github.com/emqx/emqx/pull/15299) 修复导出 OpenTelemetry 指标时出现的 `badarg` 错误。

#### 遥测

- [#15216](https://github.com/emqx/emqx/pull/15216) 修复当插件被启用时，`emqx_telemetry` 进程崩溃的问题。

## 5.9.1

*发布日期 2025-07-02*

升级前请查看已知问题列表和不兼容变更列表。

### 增强

- [#15364](https://github.com/emqx/emqx/pull/15364) 支持在 OpenTelemetry 的 gRPC（基于 HTTP/2）集成中自定义 HTTP 请求头。此增强功能提升了对需要 HTTP 认证的 Collector 的兼容性。

- [#15160](https://github.com/emqx/emqx/pull/15160) 新增多租户管理接口 `DELETE /mt/bulk_delete_ns`，支持批量删除命名空间。

- [#15158](https://github.com/emqx/emqx/pull/15158) 新增 `emqx ctl conf remove x.y.z` 命令，用于从现有配置中移除配置键路径 `x.y.z`。

- [#15157](https://github.com/emqx/emqx/pull/15157) Snowflake Connector 支持通过指定私钥文件路径替代使用密码。用户可以选择使用密码、私钥二选一，或均不使用（在 `/etc/odbc.ini` 中配置参数）。

- [#15043](https://github.com/emqx/emqx/pull/15043) 为 DS Raft 后端添加基础指标采集，用于展示集群状态、数据库概览、分片复制及副本转换的监控数据。

### 修复

#### 数据集成

- [#15331](https://github.com/emqx/emqx/pull/15331) 修复了 InfluxDB 动作中的一个问题：当 `WriteSyntax` 中的 `timestamp` 留空，且规则中未提供时间戳字段时，行协议转换会失败。现在将默认使用系统当前的毫秒时间，并强制使用毫秒精度。
  
- [#15274](https://github.com/emqx/emqx/pull/15274) 在 Postgres、Matrix 和 TimescaleDB 连接器中，当健康检查失败时将触发完整重连，从而提升连接器的稳定性。此前，连接在健康检查失败后可能进入异常状态，导致操作阻塞，甚至引发内存溢出问题。

- [#15154](https://github.com/emqx/emqx/pull/15154) 修复了聚合模式下运行的动作（例如 S3、Azure Blob Storage、Snowflake）中一个罕见的竞争条件问题，可能会导致类似如下错误的崩溃：

  ```
  ** Reason for termination ==
  ** {function_clause,[{emqx_connector_aggregator,handle_close_buffer,[...], ...
  ```

- [#15147](https://github.com/emqx/emqx/pull/15147) 修复了某些动作在使用模拟输入数据进行规则测试时，即使已经生成了请求，也未能正确触发追踪事件的问题。

  受影响的动作包括：

  - Couchbase
  - Snowflake
  - IoTDB (Thrift 驱动)

- [#15383](https://github.com/emqx/emqx/pull/15383) 修复了 MQTT 桥接中可能存在的资源泄漏问题。当桥接启动失败时，主题索引表未被正确清理。此修复确保在启动失败时正确删除索引表，以防止资源泄漏。

#### 数据智能中心

- [#15224](https://github.com/emqx/emqx/pull/15224) 修复了通过 Dashboard 更新外部 Schema Registry 时，密码被意外覆盖为 `******` 的问题。现在在更新过程中，密码能够被正确保留。
- [#15190](https://github.com/emqx/emqx/pull/15190) 增强了消息转换功能，支持为 QoS 和主题设置硬编码值。

#### 可观测性

- [#15299](https://github.com/emqx/emqx/pull/15299) 修复了导出 OpenTelemetry 指标时出现的 `badarg` 错误。

#### 遥测

- [#15216](https://github.com/emqx/emqx/pull/15216) 修复了在插件启用时，`emqx_telemetry` 进程可能崩溃的问题。

#### 访问控制

- [#15184](https://github.com/emqx/emqx/pull/15184) 修复了创建黑名单失败时返回的错误信息格式问题。

#### 集群

- [#15180](https://github.com/emqx/emqx/pull/15180) 通过修复 `ekka_locker` 模块中对 `badrpc` 错误的不当处理，降低了通道注册过程中的死锁风险。此前的错误处理可能导致锁操作误判，从而引发集群状态异常甚至死锁。

#### 网络安全


- [#15159](https://github.com/emqx/emqx/pull/15159) 优化了对证书吊销列表（CRL）分发点 URL 的处理：在多次刷新失败后将停止重试（默认 60 秒），以避免因 URL 不可达而产生大量错误日志，从而提升系统稳定性。

## 5.9.0

*发布日期：2025-05-02*

升级前请查看已知问题列表和不兼容变更列表。

### 增强

#### 核心 MQTT 功能

- [#14721](https://github.com/emqx/emqx/pull/14721) 延迟发布间隔限制已从 4294967 秒（49.7 天）更改为 42949670 秒（497 天）。

- [#14595](https://github.com/emqx/emqx/pull/14595) 弃用 `retainer.enable` 标志。保留机制会根据区域配置中的 `mqtt.retain_available` 标志自动启动和停止。

#### 安装部署

- [#14930](https://github.com/emqx/emqx/pull/14930) 开始发布 macOS 15 (Sequoia) 版本的安装包。
- [#14590](https://github.com/emqx/emqx/pull/14590) 将试用 License 下运行的节点的最大运行时间限制为一个月。达到运行时间限制后，节点将拒绝新的连接。

#### 命名空间

- [#14261](https://github.com/emqx/emqx/pull/14261) 引入了增强功能，以便在 MQTT 客户端管理中支持命名空间。

  **新特性**：

  - 命名空间客户端识别：具有 `tns` 属性的 MQTT 客户端现在被视为命名空间客户端。
  - 命名空间索引：将 MQTT 客户端命名空间 (`tns`) 添加到客户端 ID 索引中，以支持多租户场景。

  **API**：

  - 列出命名空间：新增分页 API 用于检索命名空间： 端点：`/api/v5/mt/ns_list`
  - 列出命名空间中的客户端会话：新增分页 API 用于获取特定命名空间中的客户端会话： 端点：`/api/v5/mt/:ns/client_list`
  - 统计命名空间中的活跃客户端会话数：新 API 用于获取命名空间中活跃客户端会话的数量： 端点：`/api/v5/mt/:ns/client_count`

  **配置**：

  - 每个命名空间的会话限制：新增 `multi_tenancy.default_max_sessions` 配置，用于强制限制每个命名空间中允许的客户端会话数。

  注意：

  - 管理员命名空间（管理员用户组）功能不包括在此次提交中，仍在开发中。

- [#14884](https://github.com/emqx/emqx/pull/14884) 添加了 HTTP API 用于管理命名空间配置。
- [#14840](https://github.com/emqx/emqx/pull/14840) Added HTTP API endpoints to configure client and tenant rate limiters for multi-tenancy feature.

#### 认证与授权

- [#14584](https://github.com/emqx/emqx/pull/14584) 支持应用身份验证器通过 2FA（多因素认证）登录 Dashboard。

  [#14979](https://github.com/emqx/emqx/pull/14979) 在认证和授权模板中支持使用 `zone` 和 `listener`。 同时，将 `zone` 和 `listener` 添加到 ACL 规则中的 `who` 匹配条件。

  这使得可以实现基于监听器或区域的访问控制。 示例：

  - 使用如下模板向 HTTP 认证器的请求发送 `zone` 名称： `{"username": "${username}", "zone": "${zone}"}`
  - 在 `acl.conf` 中，仅当通过 SSL 监听器连接时，允许客户端订阅 `${username}/#`： `{allow, {listener, "ssl:default"}, subscribe, ["${username}/#"]}.`


- [#14976](https://github.com/emqx/emqx/pull/14976) 支持配置认证器调用条件。允许根据客户端信息选择性地调用认证器，避免不必要的认证请求。 例如，要仅为通过 `tcp:default` 连接的客户端触发 HTTP 认证器，并为通过 `ssl:default` 连接的客户端触发 Postgre 认证器，可以使用类似 `str_eq(listener, 'tcp:default')` 或 `str_eq(listener, 'ssl:default')` 的调用条件。
- [#14966](https://github.com/emqx/emqx/pull/14966) 增加了删除默认 Dashboard 管理员用户的功能。为此，必须至少存在一个其他的管理员用户。

- [#14358](https://github.com/emqx/emqx/pull/14358) 限制 LDAP 认证/授权模板中可用的变量，仅允许使用在其他认证/授权来源中支持的变量。所有不支持的变量将不会被渲染。

- [#14610](https://github.com/emqx/emqx/pull/14610) 支持处理从外部来源获取或存储在内置数据库中的授权规则中新增的字段。

  新增支持的字段包括：

  - `username_re`：用于按用户名进行规则过滤的正则表达式
  - `clientid_re`：用于按客户端 ID 进行规则过滤的正则表达式
  - `ipaddr`：用于按 IP 地址进行规则过滤的 IP 地址或掩码

  只有当所有过滤条件都满足时，规则才会生效。

- [#14329](https://github.com/emqx/emqx/pull/14329) 支持在认证和授权发出的外部请求模板中使用 `${peerport}` 变量。

- [#14286](https://github.com/emqx/emqx/pull/14286) 实现认证与授权的节点级缓存功能。

  某些认证和授权方法需要调用外部服务，这在客户端频繁重连的场景下可能对 EMQX 和外部服务带来较大压力。

  此功能允许为这些认证与授权方法启用节点级缓存。目前支持缓存的认证与授权后端包括：

  - HTTP
  - LDAP
  - MongoDB
  - MySQL
  - PostgreSQL
  - Redis

#### REST API

- [#14254](https://github.com/emqx/emqx/pull/14254) 在 `/status` HTTP 接口的返回结果中新增集群名称字段。
- [#14972](https://github.com/emqx/emqx/pull/14972) 实现了用于下载和上传单个插件配置的 API 接口。
- [#15013](https://github.com/emqx/emqx/pull/15013) 在规则引擎相关的 HTTP API 返回结果中新增 `action_details` 字段。该字段包含每条规则中引用的动作的类型、名称和状态信息。
- [#14735](https://github.com/emqx/emqx/pull/14735) 在规则引擎的 HTTP API 返回的规则信息中新增 `last_modified_at` 字段。

#### 集群

- [#14766](https://github.com/emqx/emqx/pull/14766) 为 `emqx ctl cluster leave` 命令添加了保护措施，防止负责持久存储数据复制的节点退出集群。

- [#14040](https://github.com/emqx/emqx/pull/14040) 在节点重新平衡过程中为内部 RPC 调用添加了超时机制。之前，如果节点未响应，重新平衡过程可能会挂起。

- [#14892](https://github.com/emqx/emqx/pull/14892) 增强了集群负载重平衡：

  - 修复了核心/副本集群中的负载不均衡问题。之前，在某些条件下，所有来自副本的事务可能会被发送到单个核心节点。

  - 为重新平衡副本节点与核心节点之间的负载添加了 CLI 命令：
    - `emqx_ctl cluster core rebalance plan`
      - `emqx_ctl cluster core rebalance status`
      - `emqx_ctl cluster core rebalance confirm`
      - `emqx_ctl cluster core rebalance abort`

- [#14907](https://github.com/emqx/emqx/pull/14907) 提升了节点迁移的稳定性。此前在某些情况下，迁移过程可能会进入死循环，需手动干预才能恢复。

#### 数据集成

- [#14118](https://github.com/emqx/emqx/pull/14118) 支持在 MySQL 动作中使用 `ON DUPLICATE KEY UPDATE`。

  现在，用户可以在 MySQL 动作中指定 `ON DUPLICATE KEY UPDATE`，例如：

  ```
  INSERT INTO t1 (a,b,c) VALUES (${id},${clientid},${qos}) ON DUPLICATE KEY UPDATE a=a;
  ```

  请注意，`ON DUPLICATE KEY UPDATE` 子句不支持占位符（`${var}`）。

- [#14629](https://github.com/emqx/emqx/pull/14629) 支持 S3 和 Azure Blob Storage 动作中使用 [JSON Lines](https://jsonlines.org/) 容器类型。

- [#14642](https://github.com/emqx/emqx/pull/14642) 添加了新的连接器和动作类型，允许将事件以 JSON Lines 格式记录到本地磁盘。

- [#14996](https://github.com/emqx/emqx/pull/14996) RabbitMQ 操作支持使用默认交换器。

- [#14901](https://github.com/emqx/emqx/pull/14901) 向 Schema Registry 添加了一种新的模式类型：`external_http`。使用这种新模式类型，可以设置一个外部 HTTP 服务器，对 payload 执行任意操作并返回结果，以便在规则中使用。

- [#14722](https://github.com/emqx/emqx/pull/14722) 在 MQTT 连接器中新增 `connect_timeout` 配置项。该参数用于控制连接过程中最长允许等待的秒数。适当调低该值可以更快反馈连接失败的问题。

- [#14615](https://github.com/emqx/emqx/pull/14615) 为多个使用 `ehttpc` HTTP 驱动的数据集成插件添加了对 `max_inactive` 参数的支持。

- [#14459](https://github.com/emqx/emqx/pull/14459) 新增对失败备选动作的支持。

  当消息在数据集成动作中处理失败时（例如因缓冲区溢出或到达生存时间而被丢弃），将触发配置的备选动作。所有数据集成动作均可配置备选动作。

  以下是 Kafka 动作的示例配置片段：

  ```
  actions.kafka_producer.my_action {
    fallback_actions = [
      {kind = reference, type = mqtt, name = mqtt_fallback_publisher},
      {kind = republish, args = {topic = "fallback/action/republish"}}
    ]
    # ...
  }
  ```

- [#14582](https://github.com/emqx/emqx/pull/14582) 在完全不必要的情况下，避免在序列化之前和反序列化之后对内部 JSON 表示进行预处理和后处理。

#### 运维管理

- [#14845](https://github.com/emqx/emqx/pull/14845) 在更改网关配置和监听器时，避免不必要的现有监听器重启。
- [#14773](https://github.com/emqx/emqx/pull/14773) 改进了速率限制功能（适用于 Zone 或监听器的 `bytes_rate`、`messages_rate`、`max_conn_rate` 配置项）：
  - 简化了速率限制算法，使其行为更加可预测。新的实现不会尝试对客户端施加回压，而是直接丢弃超出限制的消息。对于 QoS1/QoS2 消息，将返回相应的原因码。
  - 支持在运行时动态调整速率限制配置。
  - 速率限制配置现在支持显式指定窗口大小和每个窗口的限制，例如：`messages_rate = "300/5m"` 表示每 5 分钟最多允许 300 条消息（可有一定波动），而 `messages_rate = "10/10s"` 则表示每 10 秒最多 10 条消息。尽管两者每秒速率（RPS）相同，后者在波动控制上更严格。
  - 可通过 `messages_burst`、`bytes_burst` 和 `max_conn_burst` 等选项配置突发速率。例如：`messages_burst = 1000/h` 表示每小时允许额外发送 1000 条消息而不触发速率限制。
- [#14341](https://github.com/emqx/emqx/pull/14341) 增强了命名空间功能，支持为每个命名空间配置会话数量上限。当超过限制时，可通过 `client.authenticate` 钩子返回 `quota_exceeded` 错误码。
- [#14679](https://github.com/emqx/emqx/pull/14679) 将 `exhook.proto` 升级为 v3，以支持在 OnMessagePublish 回调中传递 `User-Property` 参数。
- [#14963](https://github.com/emqx/emqx/pull/14963) 插件支持实现新的 `on_health_check/1` 回调函数，用于上报健康状态。同时支持通过 HTTP API 和 CLI 查询插件的健康状态。

#### Dashboard

- [#14750](https://github.com/emqx/emqx/pull/14750) 修复了 Dashboard 中“共享订阅”数值的显示问题。此前可能会显示过期的旧数据。
- [#14638](https://github.com/emqx/emqx/pull/14638) 新增支持使用文件形式的密钥（如：`file://...`）来定义 Dashboard 的默认密码。
- [#14255](https://github.com/emqx/emqx/pull/14255) 为 Dashboard 用户引入密码过期机制。
- [#15014](https://github.com/emqx/emqx/pull/15014) 提升 Dashboard 安全性。在多次登录失败后，将暂时禁止登录。失败次数与锁定时长均可配置。

- [#15132](https://github.com/emqx/emqx/pull/15132) 修复 SAML 单点登录集成中的响应内容类型错误。
  当 EMQX Dashboard 配置为使用基于 SAML 的单点登录时，Assertion Consumer Service（ACS）的响应内容类型错误地设置为 `application/xml`，导致部分身份提供商（IdP）无法正常处理。此问题已修复，响应的内容类型现已更正为 `application/x-www-form-urlencoded`，以确保与 SAML 协议兼容。

#### 可观测性

- [#14794](https://github.com/emqx/emqx/pull/14794) 为 HTTP API 接口的日志追踪添加了 `payload_limit` 参数。之前，如果 payload 大小超过 1024 字节，payload 内容会被截断。现在，这个限制可以配置。

- [#14876](https://github.com/emqx/emqx/pull/14876) 为规则引擎添加了端到端追踪支持，包括以下条目的追踪：

  - 客户端发布的消息触发规则
  - 客户端事件和警报事件触发规则
  - Source 触发的规则
  - 规则执行的动作

  限制： 目前不支持备选动作追踪。

- [#14723](https://github.com/emqx/emqx/pull/14723) 为 Prometheus Push Gateway 配置新增 `method` 选项。此前默认使用 `post` 方法，现在默认值改为 `put`。

  `put` 方法用于替换 Pushgateway 中相同 job 的指标数据，从而避免在 EMQX 节点移除后，相关指标仍保留在 Pushgateway 中。

  更多信息可参考 [PUT method 文档](https://github.com/prometheus/pushgateway?tab=readme-ov-file#put-method)。

- [#14636](https://github.com/emqx/emqx/pull/14636) 弃用了原有的指标 `packets.publish.dropped`，并引入两个更具语义化的新指标：`messages.dropped.quota_exceeded` 和 `messages.dropped.receive_maximum`。

  - `messages.dropped.quota_exceeded`：当客户端达到配置的速率限制（例如 QoS 0 消息数量限制）时触发。
  - `messages.dropped.receive_maximum`：当会话达到其 QoS 2 消息的最大接收数量时触发。

  这两个指标提供了更准确的消息丢弃原因追踪。

- [#14540](https://github.com/emqx/emqx/pull/14540) 引入可配置的认证与鉴权延迟监测功能，相关指标以 Prometheus 直方图的形式暴露。

- [#14264](https://github.com/emqx/emqx/pull/14264) 为 crash_dump 文件添加时间戳，以防止它在下一次崩溃时被覆盖。

- [#15119](https://github.com/emqx/emqx/pull/15119) 新增会话注册表大小的高水位指标，用于反映系统中曾经同时存在的最大会话数量。该指标已集成到 Dashboard 的 Overview 页面，帮助用户监控会话资源的使用情况。

- [#15117](https://github.com/emqx/emqx/pull/15117) 优化了 cinfo 认证表达式评估失败的警告日志，使其更简洁，且不易被误认为崩溃。

  优化前的日志示例:

  ```
  2025-04-25T13:15:59.993395+00:00 [warning] tag: AUTHN, clientid: mqttx_a50058aa, msg: authenticator_error, peername: 127.0.0.1:60842, 
  reason: {case_clause,{error,#{error => #{reason => var_unbound,var_name => <<"cert_common_name">>},
  cause => "clientinfo_auth_expression_evaluation_error"}}}, 
  stacktrace: [{emqx_authn_cinfo,do_check,2,[{file,"emqx_authn_cinfo.erl"},{line,94}]},{emqx_authn_cinfo,check,2,[{file,"emqx_authn_cinfo.erl"},{line,82}]},{emqx_authn_chains,authenticate_with_provider,2,...
  ```

  优化后的日志示例:

  ```
  2025-04-25T15:46:50.748732+02:00 [warning] clientid: client1, 
  msg: clientinfo_auth_expression_evaluation_error, 
  peername: 127.0.0.1:53919, 
  reason: #{reason => var_unbound,var_name => <<"cert_common_name">>}
  ```

#### CLI

- [#14691](https://github.com/emqx/emqx/pull/14691) 为 CLI 命令 `emqx ctl data export` 添加了数据筛选功能。现在可以指定从 `cluster.hocon` 文件中导出哪些根键（root keys），以及导出哪些数据表集，行为与 `POST /data/export` 接口一致。

#### 配置文件

- [#14647](https://github.com/emqx/emqx/pull/14647)  `cluster.hocon` 文件的备份现在支持通过可配置的时间间隔进行。此前每次配置变更都会创建一次备份，现在改为收集多个更改后再进行一次备份，以减少不必要的备份频率。

#### 插件与扩展

- [#14957](https://github.com/emqx/emqx/pull/14957) 增强了插件配置更新的处理：
  - 增加了对插件 `on_config_changed` 回调响应的支持。确保在更新插件配置时，即使插件已停止，也能正确调用插件的回调来处理配置变化。
  - 引入了一种新的插件配置更新方法，该方法会根据 `on_config_changed` 回调的结果来进行配置更新。

#### 网关

- [#14017](https://github.com/emqx/emqx/pull/14017) 支持在 GB/T 32960 网关中解析自定义类型的 InfoReport 数据消息。

#### MQTT over QUIC

- [#14431](https://github.com/emqx/emqx/pull/14431) QUIC 协议栈更新至新版：`quicer 0.2.3`
  - 基于 `msquic 2.3.8`（附带补丁）
  - 增强资源管理能力
  - 为后续支持监听器动态配置做好准备

#### 系统升级

- [#14639](https://github.com/emqx/emqx/pull/14639) EMQX 现已支持 Erlang/OTP 27。

### 修复

#### 核心 MQTT 功能

- [#14707](https://github.com/emqx/emqx/pull/14707) 修复在启用 strict_mode 时，带有 DUP 标志的 QoS 2 PUBLISH 报文被错误地视为无效报文的问题。
- [#14192](https://github.com/emqx/emqx/pull/14192) 修复由于认证/授权过期导致断开的客户端无法发送遗嘱消息的问题。此前由于遗嘱发送发生在权限过期之后，导致消息无法通过授权校验而被阻止。
- [#14122](https://github.com/emqx/emqx/pull/14122) 修复当客户端错误地将 QoS 2 消息使用 PUBACK 响应，或将 QoS 1 消息使用 PUBREC/PUBCOMP 响应时，EMQX 未断开连接的问题。现在若客户端使用错误的应答流程，EMQX 将主动断开连接。
- [#15106](https://github.com/emqx/emqx/pull/15106) 修复了 `GET api/v5/clients_v2` API 返回重复 `clientid` 值的 bug。该问题是由于 `chaninfo` 事件意外复活，导致客户端数据重复。修复确保此类事件不会无意中影响客户端列表，从而解决了客户端页面上出现重复客户端的情况。
- [#14906](https://github.com/emqx/emqx/pull/14906) 将 Mria 更新为 0.8.12.1，以消除由意外退出信号引起的偶发警告。

  ```
  2025-01-10T20:00:00+00:00 [warning] clientid: C1, msg: emqx_session_mem_unknown_message, message: {'EXIT',<0.123456.0>,normal}
  ```

- [#15084](https://github.com/emqx/emqx/pull/15084) 客户端属性 `zone` 和 `listener` 可以作为各种字符串函数的输入。

  之前，像 `regex_match` 这样的函数会引发异常，因为 `zone` 和 `listener` 是内部的原子类型。

#### 安装部署

- [#14624](https://github.com/emqx/emqx/pull/14624) 修复 macOS 发行包的 OpenSSL 动态链接问题。此前，由于 quicer 模块链接了系统中的 OpenSSL 而非 EMQX 构建包，可能导致 zip 包无法启动。现在已禁用动态链接，与 OTP 中的配置保持一致。

#### REST API

- [#14771](https://github.com/emqx/emqx/pull/14771) 修复 `GET /clients_v2` 返回的客户端数量可能超过请求限制的问题。

  注意：滚动升级过程中，此 API 可能无法完整列出所有客户端，直到所有节点升级完毕。建议升级期间向旧节点发送请求以获取完整客户端列表。

- [#14182](https://github.com/emqx/emqx/pull/14182) 修复 `POST /publish` 接口发布延迟消息时响应码不一致的问题。此前会返回 202 和原因码 16（无匹配订阅者），现在返回 200 并附带消息 ID。

#### MQTT 会话持久化

- [#14674](https://github.com/emqx/emqx/pull/14674) 限制 EMQX 持久化存储中 RocksDB 日志文件的数量和大小，避免日志堆积。
- [#14498](https://github.com/emqx/emqx/pull/14498) 提升持久会话性能：
  - 空闲会话将不再消耗 CPU。
  - 修复 QoS 升级功能：开启该功能后，订阅者将不再接收到超过其订阅等级的消息。

- [#14933](https://github.com/emqx/emqx/pull/14933) 解决了一个罕见的边缘情况，即由 DS Raft 支持的持久存储可能被分配到早已离开集群的存储站点。

#### 认证与授权

- [#14777](https://github.com/emqx/emqx/pull/14777) 修复 JWT 授权配置更新时部分字段未正确更新的问题，尤其是使用外部 JWKS endpoint 时。
- [#14556](https://github.com/emqx/emqx/pull/14556) 修复在节点启动或关闭期间极少发生的认证误判（false positive）问题。
- [#15059](https://github.com/emqx/emqx/pull/15059) 修复 Redis 授权配置被更新为非法值时导致认证模块崩溃的问题。现在将返回错误提示并阻止更新。
- [#14303](https://github.com/emqx/emqx/pull/14303) 解决了 `scram:http` 认证的问题。发送到 HTTP 连接器的请求格式错误，导致认证失败。

#### 集群

- [#14778](https://github.com/emqx/emqx/pull/14778) 修复当某节点的 `data/certs` 或 `data/authz` 目录下存在损坏的符号链接时，其他节点无法加入集群的问题。
- [#14936](https://github.com/emqx/emqx/pull/14936) 修复在极少数情况下，全局路由表长时间保留已离开集群节点路由信息的问题。
- [#14977](https://github.com/emqx/emqx/pull/14977) 修复了 `emqx ctl conf cluster_sync status` 命令节点显示顺序的问题。之前，新旧配置的节点名称打印顺序是颠倒的。

#### 集群连接

- [#15067](https://github.com/emqx/emqx/pull/15067) 修复了集群连接路由复制中的几个问题。
  - 当集群连接在本地或远程配置错误时，复制过程可能会进入不稳定的重连循环，且一旦配置错误得到解决，复制仍然会受到阻碍。
  - 在尝试关闭不存在的 MQTT 客户端连接时，复制过程可能会崩溃。
  - 如果路由表中存在共享订阅，复制引导过程可能会崩溃。

#### 规则引擎

- [#14849](https://github.com/emqx/emqx/pull/14849) 从 `POST /rule_test` 响应中移除了一个多余的字段（`event_type`）。这个字段是一个内部字段，实际上在真实事件中并不存在，因此它出现在规则测试输出中可能会引起混淆。
- [#15056](https://github.com/emqx/emqx/pull/15056) 对于 payload 为 JSON 列表对象的 MQTT 消息，不再需要在 `foreach` 语句中显式解码 payload。

#### 数据智能中心

- [#14988](https://github.com/emqx/emqx/pull/14988) 修复了一个问题，之前在恢复备份时，Schema 验证或消息转换配置可能会在 Schema Registry 之前导入，导致验证错误。

#### 数据集成

- [#14716](https://github.com/emqx/emqx/pull/14716) 添加和移除数据动作/数据源的流程已异步化，不再与配置变更操作同步执行，从而避免因超时而导致资源状态与配置状态不一致的问题。

- [#14519](https://github.com/emqx/emqx/pull/14519) 修复节点启动时若已有配置数据源，在数据源刚启动便收到数据的情况下，可能输出警告日志的问题。

  示例日志：

  ```
  2025-01-08T07:48:36.421822+00:00 [warning] tag: RESOURCE, msg: handle_resource_metrics_failed, reason: {badkey,received}, stacktrace: ..., event: received, kind: error, hint: transient failures may occur when restarting a resource, resource_id: <<"source:mqtt:tset:connector:mqtt:test">>
  ```

- [#14992](https://github.com/emqx/emqx/pull/14992) 修复在连接器连接测试中因极少数竞争条件而可能造成的资源泄漏问题。

- [#15000](https://github.com/emqx/emqx/pull/15000) 修复通过 CLI 或 HTTP API 加载配置时可能引发连接器、动作或数据源不稳定的问题。

- [#15010](https://github.com/emqx/emqx/pull/15010) 修复关闭连接器时耗时较长的问题（5~10 秒），即便连接器处于健康状态。目前已优化该流程，但部分连接器由于存在动作或处于异常状态，关闭过程仍可能需要时间。

- [#15051](https://github.com/emqx/emqx/pull/15051) 通过添加参数验证并更新驱动版本，增强了 TDengine 连接器，以提供更清晰的错误信息。

- [#15012](https://github.com/emqx/emqx/pull/15012) 修复了 RabbitMQ 动作中 `publish_confirmation_timeout` 参数被乘以 1000 的问题。

- [#14989](https://github.com/emqx/emqx/pull/14989) 减少了 Kinesis 连接和动作在（重新）启动及健康检查过程中执行的 API 调用次数。

  之前，在（重新）启动连接器时，它会为连接池中的每个工作线程执行一个 `ListStreams` 请求。此外，每次周期性健康检查都会为每个工作线程执行 `ListStreams`。动作健康检查会为连接池中的每个工作线程执行 `DescribeStream`。

  现在，连接器在（重新）启动时不再执行初始的 `ListStreams` 请求。连接器和动作都会尝试检查至少一个工作线程是否从各自的 API 请求中得到健康响应：每个工作线程会依次尝试请求，一旦收到首次成功响应，连接器或动作即被视为“已连接”。因此，在最佳情况下，每个连接器和每个动作每次健康检查只会执行 1 次 API 请求，无论池的大小如何。最坏情况下，如果工作线程未能收到成功响应，池中的每个工作线程可能仍会执行一次请求。


- [#14767](https://github.com/emqx/emqx/pull/14767) Kafka 生产者现在能更顺利地处理 Kafka 主题重新创建并减少分区数量的问题。之前，丢失分区的生产者可能会滞后并重试，导致大量错误日志的产生。
- [#14121](https://github.com/emqx/emqx/pull/14121) 废弃了 Kafka 消费者连接器的 `health_check_topic` 配置，以避免进一步的混淆。该参数实际上从未被该连接器类型使用过。
- [#15116](https://github.com/emqx/emqx/pull/15116) Kafka 连接器的健康检查机制现在接受 `topic_authorization_failed` 作为可接受的返回码，解决了在启用 ACL 的 Kafka 部署中，默认 probe topic 无访问权限导致健康检查失败的问题。

#### 运维管理

- [#14931](https://github.com/emqx/emqx/pull/14931) `mqtt.max_qos_allowed` 现在会被用于决定订阅确认（SUBACK）中的 `reason code`，此前返回的 QoS 等级是订阅请求中的值，而非实际授予的 QoS。
- [#14975](https://github.com/emqx/emqx/pull/14975) 修复某些 TLS 监听器配置项无法热更新的问题。此前必须先禁用再启用监听器，才能使更改生效。
- [#15037](https://github.com/emqx/emqx/pull/15037) 修复动态创建的 Zone 无法应用速率限制的问题。此前如果 Zone 在节点启动后创建，其速率限制配置不会生效。

#### 配置文件

- [#15087](https://github.com/emqx/emqx/pull/15087) 修复了 `hocon` 库中的一个问题，如果配置文件中有任何以单独的反斜杠结尾的字符串单行字段，后续将无法正确解析该配置文件。

#### 插件与扩展

- [#15073](https://github.com/emqx/emqx/pull/15073) 为 `exhook` 配置中的服务器 URL 添加了验证器。这样可以确保只有有效的 URL 能够被保存。无效的 URL 将触发错误并阻止保存，从而避免在导入过程中出现问题，以前无效的 URL 可能会被接受。
- [#14774](https://github.com/emqx/emqx/pull/14774) 修复插件相关问题：启动插件时若集群节点上未存在插件配置文件，导致配置拉取失败的问题已解决。
- [#14826](https://github.com/emqx/emqx/pull/14826) 修复 Exhook 服务返回 "IGNORE" 不生效的问题。
- [#15018](https://github.com/emqx/emqx/pull/15018) 修复了 Exhook 的一个错误，该错误导致通过 CLI 尝试导入无效的 `exhook` 配置时发生崩溃，并显示 `badarg` 错误。
- [#15108](https://github.com/emqx/emqx/pull/15108) ExHook 现已内置 gRPC 健康检查机制，确保连接状态能够准确反映外部 Hook 服务端的实际可用性。

  该修复解决了在服务端长时间停止后，状态仍显示为“已连接”的问题。如果在配置中启用了自动重连，系统还将支持自动尝试重新连接。

#### MQTT over QUIC

- [#14775](https://github.com/emqx/emqx/pull/14775) 修复配置热重载后 QUIC 监听器未正确应用 zone 配置的问题。

## 5.8.9

*发布日期：2025-12-31*

### 增强

- [#16491](https://github.com/emqx/emqx/pull/16491) 开始为 macOS 15（Sequoia）发布安装包。
- [#15944](https://github.com/emqx/emqx/pull/15944) 改进了当资源被标记为 `disconnected` 时返回的信息，适用于以下连接器：LDAP、Syskeeper、IoTDB、Snowflake（聚合模式）、JWKS 认证。
- [#15911](https://github.com/emqx/emqx/pull/15911) 对于 HTTP 动作，HTTP 请求超时时间现在与 `resource_opts.request_ttl` 保持一致。此前该值为固定且不可配置的 30 秒。
- [#15845](https://github.com/emqx/emqx/pull/15845) 扩展了 MQTT 连接器的 `static_clientids` 配置，支持为每个 clientid 指定对应的用户名和密码。
  注意：此配置目前无法从 Dashboard 更新，Dashboard 支持将在 5.8.10 版本中添加。

### 修复

#### 核心 MQTT 功能

- [#16349](https://github.com/emqx/emqx/pull/16349) 修复了在处理 request-response-information 属性时，由类型不匹配导致 MQTT v5 连接崩溃的问题。

- [#16081](https://github.com/emqx/emqx/pull/16081) 修复了一个问题：当客户端使用扩展认证机制并启用内存会话（memory sessions）时，可能会因 `session_stepdown_request_exception` 错误并伴随 `calling_self` 原因而发生崩溃。

  例如：

  ```
  2025-09-24T07:13:08.973954+08:00 [error] clientid: someclientid, msg: session_stepdown_request_exception, peername: 127.0.0.1:41782, username: admin, error: exit, reason: calling_self, stacktrace: [{gen_server,call,3,[{file,"gen_server.erl"},{line,1222}]},{emqx_cm,request_stepdown,4,[{file,"emqx_cm.erl"},{line,427}]},{emqx_cm,do_takeover_begin,2,[{file,"emqx_cm.erl"},{line,398}]},{emqx_cm,takeover_session,2,[{file,"emqx_cm.erl"},{line,384}]},{emqx_cm,takeover_session_begin,2,[{file,"emqx_cm.erl"},{line,305}]},{emqx_session_mem,open,4,[{file,"emqx_session_mem.erl"},{line,210}]},{emqx_session,open,3,[{file,"emqx_session.erl"},{line,263}]},{emqx_cm,'-open_session/4-fun-1-',4,[{file,"emqx_cm.erl"},{line,290}]},{emqx_cm_locker,trans,2,[{file,"emqx_cm_locker.erl"},{line,32}]},{emqx_channel,post_process_connect,2,[{file,"emqx_channel.erl"},{line,575}]},{emqx_connection,with_channel,3,[{file,"emqx_connection.erl"},{line,852}]},{emqx_connection,process_msg,2,[{file,"emqx_connection.erl"},{line,470}]},{emqx_connection,process_msgs,2,[{file,"emqx_connection.erl"},{line,462}]},{emqx_connection,handle_recv,3,[{file,"emqx_connection.erl"},{line,406}]},{proc_lib,wake_up,3,[{file,"proc_lib.erl"},{line,340}]}], action: {takeover,'begin'}, ...
  ```

- [#15872](https://github.com/emqx/emqx/pull/15872) 消除了在发送带有非零原因码的 CONNACK 之后断开连接时出现的 `unclean_terminate` 警告日志。

- [#15902](https://github.com/emqx/emqx/pull/15902) 将 MQTT 客户端库升级至 1.13.8。

  该升级改进了 MQTT Bridge 的连接稳定性，包括：

  - 当对端 broker 未回复 PINGRESP 时，连接器会自动重连。
  - 当在等待 CONNACK 的过程中连接中断时，TLS Bridge 能够更及时地处理连接失败。

#### 集群

- [#16452](https://github.com/emqx/emqx/pull/16452) 将 `gen_rpc` 升级至 `3.5.1`。

  在升级 `gen_rpc` 之前，如果对端节点不可达，EMQX 可能会因连接超时而产生大量尾部崩溃日志。新版本的 `gen_rpc` 不再存在该问题，并将崩溃日志转换为更易读的 `error` 日志，同时还对频繁出现的 `"failed_to_connect_server"` 日志进行了限流，以避免日志刷屏。

#### 集群连接

- [#16317](https://github.com/emqx/emqx/pull/16317) 修复了集群连接垃圾回收逻辑中的一个问题，该问题在清理过期的路由复制状态时，可能会意外地从内部路由表中移除仍然存活的路由。该问题仅在配置了多个相互独立的集群连接，且其中部分连接长时间中断的情况下发生。
- [#16269](https://github.com/emqx/emqx/pull/16269) 修复了集群连接路由复制协议恢复流程中的一个问题：在远端仍然需要重新引导（re-bootstrap）的情况下，错误地跳过了该步骤。

#### 数据集成

- [#16415](https://github.com/emqx/emqx/pull/16415) 将 Apache Pulsar 客户端升级至 2.1.2。当 Pulsar 生产者动作的 `batch_size` 配置为 `1` 时，生产者现在会对单条消息进行编码，而不是使用单元素批次。这使得消费者可以通过 Key Share 策略来分担负载。

- [#16383](https://github.com/emqx/emqx/pull/16383) 在使用 REST API 驱动的 IoTDB 连接器时，之前在健康检查过程中不会校验凭据。现在，IoTDB 连接器的健康检查会发送一个 no-op 查询，从而能够及早发现客户端凭据配置错误的问题。

- [#16336](https://github.com/emqx/emqx/pull/16336) 修复了一个竞态条件问题，该问题可能会在通过 Dashboard 测试连接器连通性或停止连接器时导致超时。

- [#16263](https://github.com/emqx/emqx/pull/16263) 健康检查现在仅验证分配给当前 EMQX 节点的分区 leader 连通性，从而避免不必要的空闲连接和误报告警。

  之前，Kafka source 连接器会通过验证所有分区的 leader 连通性来进行健康检查在集群部署中，每个 EMQX 节点只会被分配到一部分分区，这会导致未分配分区的 leader 连接处于空闲状态。由于 Kafka 会在默认 10 分钟后关闭空闲连接，这种行为可能会触发错误的连接异常告警。

- [#16138](https://github.com/emqx/emqx/pull/16138) 修复了 Redis 集群故障切换问题。

  之前，EMQX 的 Redis 集群客户端仅在常规查询（例如 `GET`）失败时才会刷新集群拓扑，而周期性的 `PING` 命令在失败时不会触发刷新。这可能导致在发生故障切换后，如果没有新的查询请求，连接器会一直处于 “connecting” 状态，并继续使用过期的拓扑信息。

  通过本次修复，`PING` 响应失败时现在也会触发集群拓扑刷新，确保连接器管理能够在故障切换后及时恢复，并更新其对 Redis 集群的视图。

- [#16043](https://github.com/emqx/emqx/pull/16043) 修复了在发生 `"not_all_kafka_partitions_connected"` 情况时，Kafka 数据集成日志信息不完整的问题。

- [#15906](https://github.com/emqx/emqx/pull/15906) 将 Kafka 生产者库 Wolff 从 4.0.12 升级至 4.0.13，该版本新增了对 ProduceResponse 中 `record_list_too_large` 错误的处理。

- [#15866](https://github.com/emqx/emqx/pull/15866) 将 Kafka 生产者库 Wolff 升级至 4.0.12，以改进对 Kafka 元数据响应中分区临时缺失情况的处理。

  在少数竞态条件下，Kafka 可能会返回不完整的分区列表。之前仅在主题被重新创建且分区数减少时处理了该情况，但未覆盖分区临时缺失的场景。该缺口可能导致分区生产者停滞，并使关闭流程无限期阻塞。

- [#15836](https://github.com/emqx/emqx/pull/15836) 当 Kafka 消费者 source 因 topic ACL 被拒绝等原因无法添加时，增强了返回的错误信息。

- [#15826](https://github.com/emqx/emqx/pull/15826) 现在，当 Kafka broker 返回 ACL 拒绝响应时，连接会被视为健康。
   之前，如果 Kafka 消费者连接器中使用的用户没有权限读取用于健康检查的特殊组 `____emqx_consumer_probe`，健康检查会失败。

- [#15827](https://github.com/emqx/emqx/pull/15827) 修复了 GreptimeDB 驱动中的 atom 和进程泄漏问题。

  同时修复了在 GreptimeDB 动作中使用某些错误写入语法时可能出现的 `function_clause` 错误。

- [#15910](https://github.com/emqx/emqx/pull/15910) 修复了连接器中的一个问题：在大型 worker 池中，如果多个 worker 同时崩溃，worker 池可能无法从失败状态中恢复。

  受影响并已修复的连接器包括：

  - MySQL
  - PostgreSQL
  - Oracle
  - SQLServer
  - TDEngine
  - Cassandra
  - Dynamo
  - HTTP
  - Couchbase
  - GCP PubSub
  - Snowflake

  同时将 `gun` 及相关依赖升级至 2.1.0。

#### 安全与认证

- [#16237](https://github.com/emqx/emqx/pull/16237) 修复了在禁用 OIDC SSO 后，仍可能输出相关日志的问题。

- [#16217](https://github.com/emqx/emqx/pull/16217) 修复了在多节点集群中，OIDC 回调在登录过程中可能无法找到会话的问题。

- [#15844](https://github.com/emqx/emqx/pull/15844) 增加了校验，禁止在内置数据库认证器中添加空用户名由于空用户名会破坏 API 路径结构，这类用户此前无法通过 HTTP API 删除。

  如果已经存在此类用户并希望将其删除，可以在 EMQX 控制台中运行以下命令：

  ```
  mria:transaction(emqx_authn_shard, fun() ->
      mnesia:delete(emqx_authn_mnesia, {'mqtt:global',<<>>}, write)
  end).
  ```

- [#15818](https://github.com/emqx/emqx/pull/15818) 修正了 `{allow|deny, all}` ACL 规则的处理逻辑。之前，这些规则在内部被转换为匹配 `#`，由于 MQTT 规范限制，无法匹配以 `$` 开头的主题（例如 `$testtopic/1`）。现在使用了特殊的内部值，以确保 `{allow|deny, all}` 规则能够正确匹配所有主题，包括以 `$` 开头的主题。

- [#15899](https://github.com/emqx/emqx/pull/15899) 改进了内存使用方式：客户端断开连接时会立即清空授权（authz）缓存，从而减少不必要的内存消耗。

#### 规则引擎

- [#16028](https://github.com/emqx/emqx/pull/16028) 修复了规则引擎 `jq` 函数的内存泄漏问题。之前，如果使用 `jq` 内置函数 `index`（例如 `.key | index("name")`），会导致内存泄漏。

#### 可观测性

- [#15967](https://github.com/emqx/emqx/pull/15967) 防止了在清理大量审计日志时，由于 Mnesia 事务阻塞而导致内存快速增长的问题。
- [#15963](https://github.com/emqx/emqx/pull/15963) 避免了由远程控制台产生的过量审计日志。
- [#15863](https://github.com/emqx/emqx/pull/15863) 修复了 License 配额告警文本不正确的问题。

#### 耐用存储

- [#14674](https://github.com/emqx/emqx/pull/14674) 限制了 EMQX 持久化存储创建的 RocksDB info 日志文件的数量和大小。

## 5.8.8

*发布日期：2025-09-04*

### 增强

#### 部署

- [#15813](https://github.com/emqx/emqx/pull/15813) 新增 Debian 13 (Trixie) 安装包发布，并将 Docker 镜像的基础系统更新为 Debian 13。

#### 核心 MQTT 功能

- [#15773](https://github.com/emqx/emqx/pull/15773) 在客户端重连时增加了 Client ID 注册的节流机制。
  - 当之前的会话清理仍在进行中时，新连接使用相同 Client ID 将被节流，避免客户端在频繁重连时导致系统不稳定。
  - 受影响的客户端会在 `CONNACK` 中收到原因码 `137` (Server Busy)，并带有 Reason-String `"THROTTLED"`，应在会话清理完成后重试。
  - 修复了当另一个连接正在注册相同 Client ID 时返回的原因码，现在会正确返回 `137` 而不是 `133`。

#### 数据集成

- [#15542](https://github.com/emqx/emqx/pull/15542) 将 `erlcloud` 库升级至 3.8.3.0。升级后，如果 EC2 实例已具备访问目标 S3 bucket 的正确 IAM 权限，用户可在不指定访问密钥 Id 和私有访问密钥的情况下配置 S3 连接器。
- [#15585](https://github.com/emqx/emqx/pull/15585) 将 Kafka `brod` 客户端升级至 4.4.4，扩展了对更多 Kafka API 的支持，并解决了 `JoinGroups` API 版本 `v0` 和 `v1` 弃用的问题。

#### 可观测性

- [#15499](https://github.com/emqx/emqx/pull/15499) 新增强制停用告警的 API 端点，允许管理员强制停用正在触发的告警。

#### 性能

- [#15536](https://github.com/emqx/emqx/pull/15536) 默认禁用了 `node.global_gc_interval` 配置。该配置在启用时会引发 CPU 波动和消息延迟，而 Erlang 内置 GC 已足够应对大部分场景。禁用后整体性能更稳定。
- [#15539](https://github.com/emqx/emqx/pull/15539) 优化 Erlang VM 参数以提升性能与稳定性：
  - 增大分布式通道缓冲区至 32 MB（`+zdbbl 32768`），避免在高强度 Mnesia 操作中触发 `busy_dist_port` 报警。
  - 禁用调度器忙等待（`+sbwt none +sbwtdcpu none +sbwtdio none`），降低操作系统报告的 CPU 使用率。
  - 设置调度器绑定类型为 db（`+stbt db`），以降低消息延迟。

### 修复

#### 部署

- [#15580](https://github.com/emqx/emqx/pull/15580) 在 EMQX Enterprise Helm Chart 中新增变量 `emqxLicenseSecretRef`，可指定包含 EMQX License Key 的 Kubernetes Secret，使 License 自动生效。
  该变量替代了无效的 `emqxLicenseSecretName`，后者仅创建并挂载 Secret 文件，却未将 License 应用于 EMQX。

#### 集群

- [#14778](https://github.com/emqx/emqx/pull/14778) 修复了当一个运行节点的 `data/certs` 或 `data/authz` 目录中存在损坏的符号链接时，其他节点无法加入该集群的问题。

#### 安全

- [#15581](https://github.com/emqx/emqx/pull/15581) 将 Erlang/OTP 从 26.2.5.2 升级至 26.2.5.14，包含两个与 TLS 相关的重要修复：
  - 修复了因证书更新过程中的竞争条件导致的 TLS 连接崩溃。
  - 现在可以正常使用 RSASSA-PSS 签名的 RSA 证书。此前，TLS 握手可能因 `bad_certificate / invalid_signature` 错误而失败。

#### 数据集成

- [#15616](https://github.com/emqx/emqx/pull/15616) 修复了 Kafka 健康检查逻辑。即使默认探测主题返回 `topic_authorization_failed` 错误，Kafka 连接也会被视为健康。

#### 智能数据中心

- [#15706](https://github.com/emqx/emqx/pull/15706) 修复了可能导致消息转换 和 Schema 验证表现不一致的索引问题。删除某个条目后可能破坏主题索引，导致后续条目即使被禁用仍然保持启用状态。
- [#15708](https://github.com/emqx/emqx/pull/15708) 修复了外部 Schema Registry 在节点重启后未能重新加载的问题。

#### 可观测性

- [#15639](https://github.com/emqx/emqx/pull/15639) 修复了 `packets.subscribe.auth_error` 指标未在订阅认证失败时正确递增的问题。

#### 网关

- [#15679](https://github.com/emqx/emqx/pull/15679) 修复了 ExProto、JT/T 808、GB/T 32960 和 OCPP 网关的 global chain name 错误。这些网关的内置认证数据此前被错误地归类到 `unknown:global`，导致网关之间产生冲突。
- [#15699](https://github.com/emqx/emqx/pull/15699) 修复了当节点停止或重启时，网关（如 CoAP）的内置认证数据被错误删除的问题。
- [#15822](https://github.com/emqx/emqx/pull/15822) 修复了 OCPP 网关连接在发送一定数量的消息后会崩溃的问题。

#### ExHook

- [#15683](https://github.com/emqx/emqx/pull/15683) 修复了 ExHook 的 TLS 选项，使 gRPC 客户端能够在 TLS 握手过程中正确验证服务器主机名。

## 5.8.7

*发布日期：2025-07-02*

### 增强

- [#15364](https://github.com/emqx/emqx/pull/15364) 支持在 OpenTelemetry 的 gRPC（基于 HTTP/2）集成中自定义 HTTP 请求头。此增强功能提升了对需要 HTTP 认证的 Collector 的兼容性。

### 修复

- [#15383](https://github.com/emqx/emqx/pull/15383) 修复了 MQTT 桥接中可能存在的资源泄漏问题。当桥接启动失败时，主题索引表未被正确清理。此修复确保在启动失败时正确删除索引表，以防止资源泄漏。
- [#15331](https://github.com/emqx/emqx/pull/15331) 修复了 InfluxDB 动作中的一个问题：当 `WriteSyntax` 中的 `timestamp` 留空，且规则中未提供时间戳字段时，行协议转换会失败。现在，系统将使用当前时间的毫秒值作为默认时间戳，并强制采用毫秒精度。
- [#15274](https://github.com/emqx/emqx/pull/15274) 在 Postgres、Matrix 和 TimescaleDB 连接器中，当健康检查失败时将触发完整重连，从而提升连接器的稳定性。此前，连接在健康检查失败后可能进入异常状态，导致操作阻塞，甚至引发内存溢出问题。
- [#15224](https://github.com/emqx/emqx/pull/15224) 修复了通过 Dashboard 更新外部 Schema Registry 时，密码被意外覆盖为 `******` 的问题。现在在更新过程中，密码能够被正确保留。
- [#14989](https://github.com/emqx/emqx/pull/14989) 优化了 Kinesis 连接器和动作，在启动和健康检查过程中显著减少了对 AWS API 的调用次数。此改进有助于避免超出 AWS Kinesis 的 API 限速（如 `ListStreams` 和 `DescribeStream`），此前在使用较大连接池或多个连接器时，频繁触发限速导致健康检查频繁失败。
- [#15299](https://github.com/emqx/emqx/pull/15299) 修复了导出 OpenTelemetry 指标时出现的 `badarg` 错误。

## 5.8.6

*发布日期：2025-03-25*

升级前请查看已知问题列表和不兼容变更列表。

### 增强

- [#14869](https://github.com/emqx/emqx/pull/14869) 为 `$events/client_disconnected` 事件 payload 新增 `connected_at` 时间戳字段。此增强功能可用于追踪已断开连接的客户端原始的会话连接时间，防止旧的断开事件覆盖更新的连接状态。

  此前，在网络不稳定的情况下，客户端可能频繁重连，导致延迟的断开事件错误地影响会话状态跟踪。此次更新将 `connected_at` 字段纳入事件 payload ，使其行为与系统主题保持一致，确保会话状态的准确性。

- [#14855](https://github.com/emqx/emqx/pull/14855) 在 JT/T 808 网关中新增了配置项 `ignore_unsupported_frames`。该选项可防止设备因发送网关无法解析的消息而被断开连接。

- [#14858](https://github.com/emqx/emqx/pull/14858) EMQX 新增与 TDengine Cloud 的数据集成。TDengine Cloud 认证机制要求提供额外的 Token 参数，现已在 TDengine 连接器中完成适配与支持。

### 修复

#### 核心 MQTT 功能

- [#14815](https://github.com/emqx/emqx/pull/14815) 修复了 QoS 2 消息的报文 ID 释放问题。此前，如果客户端在达到配置的最大挂起 QoS 2 消息数量后仍未发送 PUBREL 并断开连接，即使超过了配置的最大等待 PUBREL 超时时间，报文 ID 仍未被释放，导致资源占用问题。

#### 安装部署

- [#14797](https://github.com/emqx/emqx/pull/14797) 修复 macOS 发行包因 OpenSSL 动态链接导致的启动问题（回溯 #14624）。

  此前，macOS 上的 EMQX ZIP 包可能因 `quicer` 应用动态链接到系统安装的 OpenSSL 而无法启动，而该 OpenSSL 在 EMQX 构建过程中未被签名。现在，我们已禁用 OpenSSL 的动态链接方式，使其与 macOS 上随附的 OTP 保持一致，从而确保 EMQX 在 macOS 13 及更高版本上能够稳定启动。

#### 认证

- [#14847](https://github.com/emqx/emqx/pull/14847) 修复了在使用通配符主机名的 HTTPS 端点时，JWKS 认证失败的问题。此前，JWKS 认证无法从使用通配符主机名的 HTTPS 端点获取密钥，导致认证失败。
- [#14786](https://github.com/emqx/emqx/pull/14786) 修复了使用外部 JWKS 端点时 JWT 认证配置更新的问题。之前，在更新 JWT 认证配置时，如果旧配置和新配置均启用了 JWKS（密钥服务器），部分配置项可能未正确生效。

#### REST API

- [#14834](https://github.com/emqx/emqx/pull/14834) 修复下载数据备份文件时 `Content-Type` 头字段错误的问题。之前，下载备份文件的响应头错误地使用了 `application/json`，而不是 `application/octet-stream`。
- [#14863](https://github.com/emqx/emqx/pull/14863) 修复 `cluster/:node/invite_async` REST API 的问题。之前，该 API 可能会尝试使用已下线的节点作为协调者。

#### 规则引擎

- [#14824](https://github.com/emqx/emqx/pull/14824) 修复 SQL 规则测试在处理告警事件 `details` 键时出现的 HTTP 500 错误。此前，在 SQL 规则测试中测试 `alarm_activated` 或 `alarm_deactivated` 事件时，`details` 键中的某些值由于嵌套 Map 键处理不当，可能会触发 HTTP 500 错误。

#### 数据集成

- [#14796](https://github.com/emqx/emqx/pull/14796) 修复 Pulsar 生产者的 Inflight 状态泄漏问题。在此修复之前，Pulsar 客户端的 Inflight 状态可能发生泄漏，导致连接器的 Inflight 计数器无法归零。此外，本次修复还针对 x86 平台优化了 Pulsar 和 Kafka 生产者的性能。

  同时，Pulsar Action 能够正确执行 `buffer.memory_overload_protection` 参数。此前，该参数未能生效，导致内存使用无法得到有效控制。

- [#14902](https://github.com/emqx/emqx/pull/14902) 改进了 SQL Server 动作在连接失败场景下的错误处理，将 `IMC0x` 类型的 SQLSTATE 错误视为可恢复错误。此改动可避免在外部 MSSQL 服务暂时不可用时丢失消息，确保消息能够被正确缓存以便后续重试。

  同时，还增强了连接的健康检查机制，能够更准确地识别断开的连接，并启动连接器的重连尝试，从而提升 SQL Server 连接器在网络不稳定环境下的可靠性。

- [#14833](https://github.com/emqx/emqx/pull/14833) Kafka 生产者现在能更顺利地处理 Kafka 主题重新创建并减少分区数量的问题。之前，丢失分区的生产者可能会滞后并重试，导致大量错误日志的产生。

#### 可观测性

- [#14800](https://github.com/emqx/emqx/pull/14800) 对 `warning` 级别日志 `dropped_qos0_msg` 进行限流处理。

- [#14793](https://github.com/emqx/emqx/pull/14793) 为 MQTT 连接中的 `protocol_error` 添加了追踪日志。
   之前，当客户端发送无效或意外的 MQTT 报文导致 `protocol_error` 时，EMQX 记录的日志信息较少，难以诊断具体问题。

  例如，如果客户端在已连接的状态下再次发送 `CONNECT` 报文，EMQX 仅会记录 `socket_force_closed` 并附带 `protocol_error`，但不会指明具体原因。

  现在，EMQX 会在 `socket_force_closed` 之前记录 `unexpected_connect_packet`，并附带 `conn_state=connected`，从而提供更清晰的上下文，方便排查协议违规问题。

- [#14813](https://github.com/emqx/emqx/pull/14813) 修复了发送至 WebSocket 客户端的消息未在端到端追踪中跟踪记录的问题。

- [#14880](https://github.com/emqx/emqx/pull/14880) 改进 SQL Server 连接器健康检查失败的日志记录。此次更新使日志能够提供更精确的故障原因，例如 `timeout errors` 或 `unexpected_SELECT_1_result`，并附带详细的诊断信息，以便更高效地排查问题。


#### 插件

- [#14802](https://github.com/emqx/emqx/pull/14802) 新增用于插件管理的 CLI 命令：

  ```
   emqx ctl plugins allow NAME-VSN
  ```

  在通过 HTTP API 或 Dashboard 安装插件之前，必须先执行此命令显式允许该插件包，以提升安全性并防止未经授权的安装。

#### 网关

- [#14756](https://github.com/emqx/emqx/pull/14756) 优化 JT/T 808 网关，使其在启用匿名认证时，注册响应携带默认认证码 `anonymous`。此改动可避免部分客户端因无法解析空认证码而出现问题。

## 5.8.5

*发布日期：2025-02-25*

升级前请查看已知问题列表和不兼容变更列表。

### 增强

#### MQTT 核心功能

- [#14454](https://github.com/emqx/emqx/pull/14454) 引入了 `max_publish_rate` 选项，用于控制每个节点发布保留消息的最大速率。超过此限制发布的消息将仍然被传送，但不会存储为保留消息。

  这个选项有助于限制已配置的保留消息存储的负载。


- [#14456](https://github.com/emqx/emqx/pull/14456) 引入了一个简单的防火墙脚本 `bin/emqx_fw`，用于保护 EMQX 监听器免受 SYN 洪泛攻击。此功能仅适用于 Linux 系统。
- [#14496](https://github.com/emqx/emqx/pull/14496) 为 `POST /data/export` API 中的 `root_keys` 参数添加了额外的验证。现在，非法的根密钥会导致错误，而不是被默默忽略。

#### 访问控制

- [#14494](https://github.com/emqx/emqx/pull/14494) 增强了 MongoDB 授权，支持复杂查询。
  - 在查询器 filter 配置中支持在第一层级中使用 `$orderby` 操作符指定排序字段和排序顺序，使得在授权检查时可以对查询结果进行排序。
  - 引入了 `skip` 和 `limit` 选项，增强了 MongoDB 授权中的分页功能以及对查询结果的控制。


- [#14570](https://github.com/emqx/emqx/pull/14570) 在 HTTP 认证和授权配置中支持使用占位符。

- [#14665](https://github.com/emqx/emqx/pull/14665) 支持客户端属性作为 ACL 规则前置条件。现在您可以根据客户端属性创建 ACL 规则，从而实现对访问的更精细控制。

  例如，以下规则允许 `"type"` 属性设置为 `"internal"` 的客户端发布或订阅所有主题：

  ```
  {allow, {client_attr, "type", "internal"}, all, ["#"]}.
  ```

  以下规则则拒绝 `"type"` 属性以 `"external-"` 开头的客户端发布任何消息：

  ```
  {deny, {client_attr, "type", {re, "external-.*"}}, publish, ["#"]}.
  ```

#### 规则引擎

- [#14627](https://github.com/emqx/emqx/pull/14627) 增加了两个新的规则引擎事件：`$events/sys/alarm_activated` 和 `$events/sys/alarm_deactivated`。这些事件会在系统告警激活和解除时被触发。

#### 数据集成

- [#14404](https://github.com/emqx/emqx/pull/14404) 支持对 MQTT 连接器指定静态客户端 ID。

- [#14450](https://github.com/emqx/emqx/pull/14450) 支持 MQTT Source 设置 `no-local` 标志。现在可以在 MQTT Source 设置中配置 `no-local` 标志，以防止客户端发布的消息被同一客户端接收。

- [#14507](https://github.com/emqx/emqx/pull/14507) 增加了两个新的轻量级 HTTP API：`GET /actions_summary` 和 `GET /sources_summary`。这两个新 API 提供了比现有的 `GET /actions` 和 `GET /sources` API 更简洁的动作和 Source 概览，而不是返回这些动作和 Source 的完整配置，能使查询速度更快，且消耗的资源更少。

- [#14524](https://github.com/emqx/emqx/pull/14524) 在 Couchbase 连接器健康检查失败时提供了更详细的错误信息。

- [#14572](https://github.com/emqx/emqx/pull/14572) 更新了 Kafka、Azure Event Hub 和 Confluent 生产者集成的默认设置，将 `parameters.buffer.memory_overload_protection` 的默认值更新为 `true`。

  此更改有助于防止内存超载，并减轻在连接的 Kafka 服务长时间宕机（例如几小时）的情况下发生内存不足（OOM）错误的风险。

- [#14626](https://github.com/emqx/emqx/pull/14626) 更改了 Kafka 和 Pulsar 动作的每分区默认缓冲区大小，调整为 256 MB。

#### 可观测性

- [#14437](https://github.com/emqx/emqx/pull/14437) 在 Prometheus 输出中增加了两个新的指标：`emqx_vm_mnesia_tm_mailbox_size` 和 `emqx_vm_broker_pool_max_mailbox_size`。这些指标跟踪内部 EMQX 进程的邮箱大小，能够指示系统是否超载。此外，当邮箱大小超过某些高水位时，会触发警报。


- [#14645](https://github.com/emqx/emqx/pull/14645) 增加了更多的日志信息，以帮助调试首次获取证书撤销列表（CRL）（在它们被缓存和自动刷新之前）。成功和失败的日志分别以 `debug` 和 `warning` 级别记录。

- [#14656](https://github.com/emqx/emqx/pull/14656) 增强了 Prometheus 推送，支持更多的指标，并允许将集群名称作为 Job 标签的变量名使用。

- [#14479](https://github.com/emqx/emqx/pull/14479) 为身份验证和授权后端在端到端追踪中增加了更多详细的追踪信息，以便更好地与 OpenTelemetry 集成。

- [#14644](https://github.com/emqx/emqx/pull/14644) 在端到端追踪中为 OpenTelemetry 集成增加了对客户端提供的 traceparent 的支持。

- [#14657](https://github.com/emqx/emqx/pull/14657) 使端到端追踪的白名单条目对 `broker.publish` span 生效。即，消息传递的 span。

  之前，为了踪订阅者接收消息，用户必须将消息发布者或主题添加到白名单中。这种方法也会追踪消息交付给其他订阅者，可能会生成不必要的 span。

  通过此更新，白名单条目现在也适用于消息传递过程。只需将订阅者的客户端 ID 添加到白名单，就能追踪 `broker.publish` span（即消息发布给特定订阅者）。

- [#14589](https://github.com/emqx/emqx/pull/14589) 和 [#14689](https://github.com/emqx/emqx/pull/14689) 对消息转换和消息验证功能的所有日志消息类型进行节流。

#### MQTT over QUIC

- [#14583](https://github.com/emqx/emqx/pull/14583) QUIC 监听器现在支持将 TLS 密钥秘密转储到 `SSLKEYLOGFILE` 环境变量中，允许像 Wireshark 这样的工具解密实时或捕获的 QUIC 流量。这使得在 QUIC 流量中解码 MQTT 数据包成为可能。

  示例配置： `EMQX_LISTENERS__QUIC__DEFAULT__SSLKEYLOGFILE=/tmp/EMQX_SSLKEYLOGFILE`

  注意：这是一个隐藏的配置，仅供故障排除使用。


- [#14597](https://github.com/emqx/emqx/pull/14597) 在连接终止过程中异步中止流读取。

  在会话被“接管”、“丢弃”或“踢出”的情况下，之前的连接终止过程涉及平滑的流关闭。如果旧客户端没有响应，这可能会导致最长达 3 秒的阻塞延迟。

  这个问题是因为平滑关闭依赖于两个端点之间的协作信号，确保 MQTT.DISCONNECT 数据包在关闭传输之前送达对端。如果对端没有响应，这种方法就会导致不必要的延迟。

  通过此改进，在终止过程中流现在被半关闭。读取（接收）操作会被中止，而写入（发送）操作保持打开。此调整确保 MQTT.DISCONNECT 数据包仍然会送达对端，正确地通知关闭过程，而不会造成不必要的延迟。

  **优势**：

  - 减少了在对端不可达或无响应时的阻塞时间。
  - 保证了关闭过程的正确通知，改善了整体连接关闭行为。
  - 减少了会话接管和清理启动场景中的延迟（例如，丢弃）。

### 修复

#### MQTT 核心功能

- [#14405](https://github.com/emqx/emqx/pull/14405) 将 `mqtt.max_packet_size` 中的 `256MB` 转换为 `268435455` 字节。

  以前，EMQX 允许为 `mqtt.max_packet_size` 配置设置 `256MB`，但实际上这是比协议规范允许的最大值多出一个字节。为了向后兼容，`mqtt.max_packet_size=256MB` 仍然可以从配置中使用，但会被自动转换为 `268435455` 字节。


- [#14508](https://github.com/emqx/emqx/pull/14508) 改进了当大量客户端重新连接时 EMQX 的性能。
- [#14608](https://github.com/emqx/emqx/pull/14608) 强制 MQTT 会话消息队列采用先进先出（FIFO）语义。现在，当 MQTT 会话消息队列达到其容量时，它将严格遵循 FIFO 语义。当队列满时，最旧的消息将首先被丢弃。
- [#14609](https://github.com/emqx/emqx/pull/14609) 修正了过载保护的高内存阈值，改为使用 `sysmon.os.sysmem_high_watermark`。现在，高内存阈值将在启动过程中或每次 `sysmon.os.sysmem_high_watermark` 发生变化时正确更新。这确保了内存过载保护阈值是动态的，并反映了系统内存设置的变化。
- [#14654](https://github.com/emqx/emqx/pull/14654) 即使达到了最大会话限制，只要之前的会话仍然有效（即未过期或被清理），客户端也可以成功重新连接。
- [#14588](https://github.com/emqx/emqx/pull/14588) 改进了在容器化环境中运行 EMQX 时内存使用报告的准确性。在像 Amazon Elastic Kubernetes Service（AWS EKS）这样的容器化环境中，内存使用读取的准确性可能会受到主机内核版本、cgroup 版本以及容器管理服务如何挂载 cgroupfs 等因素的影响。此更新改进了在 AWS EKS 中运行 EMQX 时内存使用报告的准确性，特别是解决了由容器环境引起的差异。

#### License

- [#14568](https://github.com/emqx/emqx/pull/14568) 增强了 License 限制，包括断开会话。以前，EMQX License 限制只计算已连接会话的数量。现在，限制适用于所有已连接和断开连接的会话，且启用了“会话保留”功能。当达到 License 限制时，将拒绝新的离线保留会话，从而确保更好的资源管理和 License 合规性。

#### 认证

- [#14585](https://github.com/emqx/emqx/pull/14585) 修复了一个问题，其中密码哈希比较区分大小写，这可能导致认证失败，特别是在与外部系统集成时，这些系统可能存储具有不同大小写约定的密码。现在，密码哈希将以不区分大小写的方式进行比较，从而提高了与外部源进行用户认证时的兼容性和可靠性。

#### 网关

- [#14484](https://github.com/emqx/emqx/pull/14484) 修复了 Exproto 网关不支持在服务器端点使用主机名的问题。


- [#14489](https://github.com/emqx/emqx/pull/14489) 修复了访问 `api/v5/gateways` 端点时，如果网关在集群中的节点上未启用，导致出现 500 错误的问题。现在，此类请求将返回更合适的响应，防止崩溃并提高 API 在这些场景中的稳定性。
- [#14501](https://github.com/emqx/emqx/pull/14501) 修复了网关客户端查询 HTTP API 总是返回 keepalive 为 `0` 值的问题。现在，HTTP API 将返回正确的 keepalive 值，且网关将遵守配置的空闲超时，正确反映客户端的心跳设置。
- [#14503](https://github.com/emqx/emqx/pull/14503) 如果网关没有监听器，返回空列表而不是 404 错误。以前，通过 API 访问网关的监听器页面（如 LwM2M）时，如果没有配置监听器，则会返回 404 错误。此修复将行为更改为当没有监听器时返回空列表。
- [#14511](https://github.com/emqx/emqx/pull/14511) 消除了 Stomp 网关在客户端认证失败时不必要的日志打印。
- [#14653](https://github.com/emqx/emqx/pull/14653) 修复了 Stomp 网关的 keepalive 行为。以前，如果心跳包在检查定时器稍后收到，STOMP 连接的心跳机制将无法保持连接存活。此更新引入了对轻微延迟的容忍，确保连接保持存活。平均而言，连接关闭现在发生在心跳间隔的约 1.5 倍时，提供了更可靠的 keepalive 功能。


#### 规则引擎

- [#14622](https://github.com/emqx/emqx/pull/14622) 修复了当超过 32 个消息转换或 schema 验证匹配一个主题时，消息转换和 schema 验证可能会以任意顺序执行的问题。

#### 数据集成

- [#14518](https://github.com/emqx/emqx/pull/14518) 此更新确保当从配置加载连接器时，无论是通过 CLI 还是 HTTP API，连接器现在都会异步启动。之前，如果连接器在启动过程中挂起，可能会导致整个配置导入过程超时。

  此外，当（重新）启动节点时，连接器现在也会异步启动，从而缩短启动时间。此版本还修复了一个潜在问题，即在连接器之前可以将 Source 添加到配置中，确保在配置导入过程中正确的初始化顺序。


- [#14545](https://github.com/emqx/emqx/pull/14545) 修复了一个问题，即当 RabbitMQ 变得无响应时，RabbitMQ 动作无法移除的问题。
- [#14550](https://github.com/emqx/emqx/pull/14550) 修复了一个问题，即在 MQTT 连接器的连接池中的 MQTT 客户端如果只有少数客户端断开连接时，无法自动重新连接。此修复确保当客户端断开连接时能够自动重新连接，从而提高连接的可靠性。
- [#14555](https://github.com/emqx/emqx/pull/14555) 修复了一个关于 MQTT Source 的问题，之前在移除或更新 Source 时，未能正确地取消订阅共享主题。
- [#14650](https://github.com/emqx/emqx/pull/14650) 将 `eredis_cluster` 库更新到版本 `0.8.8`，修复了一个问题，即在 Redis 集群模式下，Redis 主从故障转移后，EMQX 无法从 `no_connection` 错误中恢复。
- [#14671](https://github.com/emqx/emqx/pull/14671) 修复了 MQTT 动作中的一个问题。在修复之前，由于一个罕见的竞态条件，当 MQTT 连接器的连接关闭时，消息可能无法发送或重试。此更新确保 TCP 连接关闭（`tcp_closed`）和客户端断开连接被处理为可恢复的错误。
- [#14695](https://github.com/emqx/emqx/pull/14695) 改进了 HTTP API 错误信息，当尝试更新连接器并发生验证错误时，返回更详细的错误消息。
- [#14697](https://github.com/emqx/emqx/pull/14697) 修复了一个问题，当 Source 和动作共享相同名称并使用相同的连接器时，如果存在双重 Source /动作的规则依赖关系，无法删除动作或 Source。
- [#14427](https://github.com/emqx/emqx/pull/14427) 提供了关于 GCP PubSub 生产者连接器健康检查失败的更多详细错误消息。
- [#14451](https://github.com/emqx/emqx/pull/14451) 修复了 PostgreSQL 动作中的无效输入处理，该动作导致大规模的崩溃报告。现在，系统优雅地处理此类错误并记录简洁且详细的错误消息，使故障排除更简单清晰。
- [#14552](https://github.com/emqx/emqx/pull/14552) 修复了 Kafka 生产者在 Kafka 服务停止时由于缓冲区溢出而导致的 `unexpected_id` 崩溃问题。此错误在 EMQX 企业版 5.8.1 中引入。
- [#14560](https://github.com/emqx/emqx/pull/14560) 修复了 Oracle 动作由于复杂 SQL 模板问题导致健康检查失败的问题。
- [#14563](https://github.com/emqx/emqx/pull/14563) 修复了 Kafka 和 Pulsar 生产者中丢弃消息时的 "failed" 计数器问题。以前，当 Kafka 和 Pulsar 生产者动作因缓冲区溢出或请求超时而丢弃消息时，相应规则的 "failed" 计数器未正确递增。此修复确保规则指标准确更新，反映丢弃的消息，并解决了规则页面上的统计指示器不正确的问题。
- [#14567](https://github.com/emqx/emqx/pull/14567) 修复了在禁用或移除 S3 连接器后，S3 HTTP 池未停止的问题。
- [#14631](https://github.com/emqx/emqx/pull/14631) 增强了 Kafka、Azure Event Hub 和 Confluent 生产者动作的内存过载保护。系统现在在达到高内存水位时更加积极地丢弃缓冲数据，从而减少了内存不足状态的风险。
- [#14705](https://github.com/emqx/emqx/pull/14705) 改进了 Kafka 连接器的连接性检查，确保正确处理身份验证。之前，如果 Kafka 需要身份验证但未配置凭据或健康检查主题，连接性测试会错误地通过，从而导致潜在的动作创建失败。此修复引入了一个虚拟的健康检查主题 `emqx-connector-connectivity-probe`，确保连接器仅在 Kafka 返回有效的元数据或 `unknown_topic_or_partition` 响应时才被认为是健康的。

#### 集群

- [#14536](https://github.com/emqx/emqx/pull/14536) 修复了集群管理操作中的罕见竞态条件。修复前，竞态条件导致某些集群管理操作挂起，直到节点重启之前，集群变更无法进行。此问题通过加强全局锁定来保护 `mria:join/1` 操作得到解决。更严格的锁定防止了并发加入操作的相互干扰。


- [#14548](https://github.com/emqx/emqx/pull/14548) 修复了一个问题，即如果在节点关闭时有新节点加入集群，重启节点时会发生崩溃，导致出现 `** FATAL ** Failed to merge schema: {aborted,function_clause}` 错误。此修复确保节点现在可以平稳重启，而不需要重新加入集群。
- [#14662](https://github.com/emqx/emqx/pull/14662) 修复了一个问题，即当一个正在运行的副本节点在重新加入一个所有核心节点的内部数据库已被清空的集群时，无法参与某些远程过程调用（RPC）操作。

#### 管理

- [#14543](https://github.com/emqx/emqx/pull/14543) 修复了一个内部兼容性问题，导致通过 WS、WSS 或网关监听器连接的客户端触发某些 ExHooks 时崩溃。

#### 可观测性

- [#14544](https://github.com/emqx/emqx/pull/14544) 修复了一个问题，即禁用 TCP 或 TLS 监听器时，导致 Prometheus 度量收集过程崩溃。

- [#14466](https://github.com/emqx/emqx/pull/14466) 修复了一个问题，即当追踪事件采样比例设置为 `100%` 时，事件开关无法生效。
- [#14666](https://github.com/emqx/emqx/pull/14666) 暴露了配置项 `opentelemetry.traces.max_queue_size`，可通过 REST API 和 Dashboard 进行配置。之前，这个配置项只能通过配置文件或操作系统环境变量进行配置。

## 5.8.4

*发布日期：2024-12-26*

升级前请查看已知问题列表和不兼容变更列表。

### 增强

#### MQTT 核心功能

- [#13739](https://github.com/emqx/emqx/pull/13739) 新增支持清除整个集群的监控（统计）数据。现在可以通过向 `api/v5/monitor` 端点发送 `DELETE` 请求来清除所有收集到的监控指标。

- [#14247](https://github.com/emqx/emqx/pull/14247) 如果客户端元数据中存在 `tns` 属性，该属性将被记录到日志中。

  如果存在 `client_attrs.tns` 属性，它将被包含在日志元数据中。然而，如果客户端 ID 已经以 `tns` 值为前缀，则不会重复记录，以避免重复。

- [#14353](https://github.com/emqx/emqx/pull/14353) 提升了会话重平衡和疏散过程的稳健性。之前，在某些集群错误情况下，会话疏散过程可能会陷入死循环。

#### 规则引擎

- [#14369](https://github.com/emqx/emqx/pull/14369) 在规则引擎中新增了两个与数量相关的函数：
  - `is_empty`：判断一个 `Array` 或一个 `Map` 是否为空，则返回 `true`。
  - `map_size`：返回 `Map` 中 Key 的数量。

#### 数据集成

- [#14110](https://github.com/emqx/emqx/pull/14110) Pulsar 驱动支持报告指标，提升了可观察性。现在报告以下指标：
  - 排队消息数量
  - 正在处理的消息数量
  - 丢弃的消息数量
- [#14410](https://github.com/emqx/emqx/pull/14410) EMQX 支持与[阿里云表格存储 (Tablestore)](https://cn.aliyun.com/product/ots) 的数据集成。
- [#14370](https://github.com/emqx/emqx/pull/14370) 重构了 IoTDB 数据集成的实现，现在它有了更好的批量写入的处理能力。

#### 配置文件

- [#14269](https://github.com/emqx/emqx/pull/14269) 添加了 `etc/base.hocon` 配置文件。本次版本引入了新的配置文件 `etc/base.hocon`，以增强配置管理的清晰度和可维护性。

  之前，`emqx.conf` 是唯一的手动配置设置文件。然而，由于它位于配置覆盖层级的最上层，这导致了一些混淆。虽然在 `emqx.conf` 中设置的可变（非只读）配置可以通过 UI、API 或 CLI 修改并立即生效，但这些更改在节点重启后不会持久化，导致不一致的行为。

  为了解决这个问题，我们添加了 `etc/base.hocon` 作为基础配置层。更新后的配置优先级顺序（从上到下）如下：

  1. 环境变量
  2. `etc/emqx.conf`
  3. `data/configs/cluster.hocon`
  4. `etc/base.hocon`

  `etc/base.hocon` 文件作为配置的基础层。尽管此文件中的配置在节点启动后仍然可以修改，但它确保了行为的一致性，并保证配置的正确覆盖。

#### 可观测性

- [#14360](https://github.com/emqx/emqx/pull/14360) 在 Prometheus 指标中添加了按关闭原因分类的监听器关闭计数，计数器名为 `emqx_client_disconnected_reason`。示例输出：

  ```
  emqx_client_disconnected_reason{node="emqx@127.0.0.1",reason="takenover"} 1 
  emqx_client_disconnected_reason{node="emqx@127.0.0.1",reason="kicked"} 1
  ```

  目前，该功能仅适用于 TCP 和 TLS 监听器。

### 修复

#### MQTT 核心功能

- [#14248](https://github.com/emqx/emqx/pull/14248) 修复了集群节点间偶发的连接问题，这些问题可能导致集群范围的路由表状态部分丢失。此修复确保了集群间更好的一致性和可靠性。
- [#14272](https://github.com/emqx/emqx/pull/14272) 修复了通过 CLI 加载的 `auto_subscribe` 配置未生效的问题，尽管之前显示了成功消息。
- [#14424](https://github.com/emqx/emqx/pull/14424) 修复了与排它订阅相关的成员状态消息错误地被记录为 `unexpected_info` 警告的问题。

#### REST API

- [#14317](https://github.com/emqx/emqx/pull/14317) 修复了管理 API 中分页累积的问题，通过移除一个不正确但未激活的条件子句。此修复防止了可能导致返回空页面的问题，并避免了未来可能的内部 API 误用。

#### 数据集成

- [#14318](https://github.com/emqx/emqx/pull/14318) 修复了 HTTP 连接器状态初始化的问题。该错误可能发生在 HTTP 动作正在处理流入的消息，而其底层连接器正在重启的过程中。修复前，日志中可能会显示类似以下的错误信息：

  ```
  20:42:36.850 [error] msg: "resource_exception", info: #{error => {error, function_clause}, id => <<"action:http:a:connector:http:a">>, name => call_query, ...
  ```

- [#14319](https://github.com/emqx/emqx/pull/14319) 重构了资源管理的内部状态机，消除了多个竞态条件问题。例如，之前 HTTP 动作在处理传入流量时，如果遇到健康检查波动，可能会导致以下错误：

  ```
  2024-11-29T14:58:17.994119+00:00 [error] msg: action_not_found, connector: <<"connector:http:a">>, action_id: <<"action:http:a:connector:http:a">
  ```

- [#14362](https://github.com/emqx/emqx/pull/14362) 重构了资源管理器的状态机，防止了可能导致状态不一致的竞态条件。

- [#14429](https://github.com/emqx/emqx/pull/14429) 修复了在底层连接器被禁用时规则动作指标的处理问题。之前，每条消息的失败计数器会分别在 `unknown` 和 `out_of_service` 两个类别中各自增加一次。通过此修复，只有 `out_of_service` 计数器会增加，从而提供更准确的指标。

- [#14291](https://github.com/emqx/emqx/pull/14291) 该问题的修复升级了 Pulsar 生产者驱动，使其能够正确处理 `LookupType` （查找类型）为 `Redirect` 的响应。修复前，当 `LookupType` 响应类型为 `Redirect` 且生产者（重新）启动时，生产者会错误地尝试连接到返回的代理服务器节点，从而避免了无法发布消息的问题。以下是出现该问题时的示例日志：

  ```
  2024-11-25T20:40:54.140659+00:00 [error] [pulsar-producer][persistent://public/default/p3-partition-0] Response error:'ServiceNotReady', msg:"Namespace bundle for topic (persistent://public/default/p3-partition-0) not served by this instance. Please redo the lookup. Request is denied: namespace=public/default"
  ```

- [#14345](https://github.com/emqx/emqx/pull/14345) 修复了启动或重启一个已配置并启用 Kafka 消费者 source 的节点时可能导致崩溃日志的问题。该问题源于 Kafka 消费者监督进程启动失败，现已得到妥善处理。修复后，用户将不再在日志中看到如下错误信息：

  ```
  2024-12-03T17:11:31.861584+00:00 [warning] tag: CONNECTOR/KAFKA_CONSUMER, msg: add_channel_failed, reason: #{reason => {noproc,{gen_server,call,[emqx_bridge_sup,{start_child,#{id => emqx_bridge_kafka_consumer_sup,restart => permanent,shutdown => infinity,start => {emqx_bridge_kafka_consumer_sup,start_link,[]},type => supervisor,modules => [emqx_bridge_kafka_consumer_sup]}},infinity]}},stacktrace => [{gen_server,call,3,[{file,"gen_server.erl"},{line,419}]},{emqx_bridge_kafka_impl_consumer,ensure_consumer_supervisor_started,0,[{file,"src/emqx_bridge_kafka_impl_consumer.erl"},{line,395}]},{emqx_bridge_kafka_impl_consumer,start_consumer,5,[{file,"src/emqx_bridge_kafka_impl_consumer.erl"},{line,427}]},{emqx_bridge_kafka_impl_consumer,on_add_channel,4,[{file,"src/emqx_bridge_kafka_impl_consumer.erl"},{line,254}]},{emqx_resource,call_add_channel,5,[{file,"src/emqx_resource.erl"},{line,565}]},{emqx_resource_manager,add_channels_in_list,2,[{file,"src/emqx_resource_manager.erl"},{line,872}]},{emqx_resource_manager,channels_health_check,2,[{file,"src/emqx_resource_manager.erl"},{line,1304}]},{emqx_resource_manager,continue_resource_health_check_not_connected,2,[{file,"src/emqx_resource_manager.erl"},{line,1235}]},{gen_statem,loop_state_callback,11,[{file,"gen_statem.erl"},{line,1397}]},{proc_lib,init_p_do_apply,3,[{file,"proc_lib.erl"},{line,241}]}],exception => exit}, resource_id: <<"connector:kafka_consumer:a">>, channel_id: <<"source:kafka_consumer:b:connector:kafka_consumer:a">>
  ```

- [#14362](https://github.com/emqx/emqx/pull/14362) 更新了 MySQL 驱动程序，以解决崩溃问题并改善错误恢复。在此修复之前，用户可能会遇到系统在崩溃后无法恢复的问题，日志中可能会出现类似以下内容：

  ```
  crasher: initial call: mysql_conn:init/1, pid: <0.8940.0>, registered_name: [], exit: {timeout,[{gen_server,init_it,6,[{file,\"gen_server.erl\"},{line,961}]},{proc_lib,init_p_do_apply,3,[{file,\"proc_lib.erl\"},{line,241}]}]} Supervisor: {<0.8938.0>,ecpool_worker_sup}. Context: start_error. Reason: timeout. Offender: id={worker,1},pid=undefined. Generic server <0.23934.0> terminating. Reason: {{case_clause,{error,closed}},[{mysql_protocol,fetch_response,7,[{file,\"mysql_protocol.erl\"},{line,569}]},{mysql_conn,query,4,[{file,\"mysql_conn.erl\"},{line,648}]},{mysql_conn,handle_call,3,[{file,\"mysql_conn.erl\"},{line,316}]},{gen_server,try_handle_call,4,[{file,\"gen_server.erl\"},{line,1131}]},{gen_server,handle_msg,6,[{file,\"gen_server.erl\"},{line,1160}]},{proc_lib,init_p_do_apply,3,[{file,\"proc_lib.erl\"},{line,241}]}]} Supervisor: {<0.23913.0>,ecpool_worker_sup}. Context: shutdown. Reason: reached_max_restart_intensity. Offender: id={worker,8}
  ```

- [#14375](https://github.com/emqx/emqx/pull/14375) 增强了 Kafka 消费者 source 的模拟执行结果，添加了更详细的错误信息。此更新帮助用户在健康检查或模拟执行过程中，更好地诊断和调试与 Kafka 代理相关的问题。

#### 命令行

- [#14357](https://github.com/emqx/emqx/pull/14357) 修复了 `bin/emqx help` 命令的问题。此修复确保帮助命令现在显示正确的使用信息。现在，帮助命令显示了适当的详细信息，便于用户更容易理解如何使用该命令。

#### 配置文件

- [#14371](https://github.com/emqx/emqx/pull/14371) 修复了客户端 ID 覆盖表达式将 `undefined` 或 `null` 渲染为字面量字符串 `"undefined"` 或 `"null"` 的问题。现在，这些值会正确显示为空字符串，从而在变量未设置或没有值时提供更简洁和直观的输出。
- [#14376](https://github.com/emqx/emqx/pull/14376) 增强了配置导入功能，以处理不存在的日志文件目录。如果指定的日志文件目录不存在，系统将回退到默认日志目录 `"${EMQX_LOG_DIR}"`，确保操作更加顺畅且不会出现错误。

#### 可观测性

- [#14267](https://github.com/emqx/emqx/pull/14267) 修改了日志记录行为，避免在日志和 HTTP 响应中遮盖文件路径类型的秘密字符串（例如，`file:///path/to/the/secret`）。

- 修复了在获取永久 license `emqx_license_expiry_at`（表示 license 到期时间）相关的 Prometheus 指标值时出现的 `function_clause` 错误。

#### 网关

- [#14445](https://github.com/emqx/emqx/pull/14445) 修复了 JT/T 808 客户端在接收到无效下行控制消息时，连接进程崩溃的问题。

## 5.8.3

*发布日期：2024-12-05*

升级前请查看已知问题列表和不兼容变更列表。

### 增强

#### MQTT 核心功能

- [#14219](https://github.com/emqx/emqx/pull/14219) 强化了连接速率限制器，提升了系统的弹性。
  - **提升了高连接速率下的系统稳定性和响应性**：以前，当连接速率超出限制时，监听器接收器会忽略新的连接请求，这可能导致在大量客户端频繁连接或重新连接的情况下系统进入不可恢复的状态。现在，监听器会接受待处理的连接，但一旦达到连接速率限制，立即关闭这些连接。这减少了资源压力，并提高了高峰负载下的系统弹性。
  - **新增监听器选项 `nolinger`**：当设置为 `true` 时，在 socket 关闭时会立即发送 TCP-RST 数据包，有助于缓解 SYN 洪水攻击，并进一步提升连接处理效率。
  - **MQTT 监听器的 `max_connection` 配置现受系统限制**：MQTT 监听器的 `max_connection` 配置值现在受到系统限制（例如操作系统的 `ulimit` 和 `node.process_limit`）的约束。如果配置为 `infinity` 或超出系统限制的值，它将自动调整为与系统最大限制匹配的值。
  - **SSL 监听器的 `ssl_options` 配置现在在更改前会进行验证**：以前，非法的 SSL 配置（如不支持的 TLS 版本）可能会被接受，导致在监听器重新配置后客户端连接失败。此次更新后：
    - 如果监听器配置了无效的 SSL 选项，节点将无法启动。
    - 通过 Dashboard 或配置 API 提交无效的 SSL 选项请求将返回 `400` 状态码。

#### 认证和授权

- [#14147](https://github.com/emqx/emqx/pull/14147) 支持在 LDAP 可扩展匹配过滤器中使用 `memberOf` 语法，例如：`(&(objectClass=class)(memberOf:1.2.840.113556.1.4.1941:=CN=GroupName,OU=emqx,DC=WL,DC=com))`。

#### 数据集成

- [#14166](https://github.com/emqx/emqx/pull/14166) 支持在 RabbitMQ 生产者中将 `exchange` 和 `routing_key` 配置为模板值，实现基于消息 payload 动态路由。例如，可以根据 payload 的字段动态设置 `routing_key`，配置为 `${payload.akey}`。

  注意：在批处理模式下，`exchange` 和 `routing_key` 的模板值必须对批处理中的所有消息保持一致。这确保了路由的一致性，并避免了批处理过程中的冲突。

- [#14176](https://github.com/emqx/emqx/pull/14176) 在规则引擎中公开了更多 RabbitMQ source 动作的元数据，包括 `queue`、`exchange` 和 `routing_key`。这使得用户可以在规则中直接访问这些字段，从而增强处理和路由逻辑。

  示例：

  ```sql
  select *, 
     queue as payload.queue, exchange as payload.exchange, routing_key as payload.routing_key
  from 
     "$bridges/rabbitmq:test"
  ```

- [#14218](https://github.com/emqx/emqx/pull/14218) 引入了 vhost 风格的桶访问，并改进了对 S3 兼容存储提供商的重定向处理。这些改进现在可以在 S3 桥接和文件传输后台配置中使用。

#### 系统配置

- [#14195](https://github.com/emqx/emqx/pull/14195) 支持对客户端 ID 的覆盖。

  EMQX 现在通过允许使用 `mqtt.clientid_override={Expression}` 配置来提供更大的灵活性，支持自定义客户端 ID 覆盖。这引入了一种更动态的客户端 ID 管理方式。作为此更新的一部分，`use_userid_as_clientid` 和 `peer_cert_as_clientid` 选项已被弃用，但它们将保持兼容性，直到 6.0 版本。

#### MQTT over QUIC

- [#14283](https://github.com/emqx/emqx/pull/14283) 改进了 QUIC 传输，升级 `quicer` 到 0.1.9。
  - 在异常场景下，提前释放远程流资源。
  - 添加了更多故障排除 API。详情请见： https://github.com/emqx/quic/compare/0.1.6...0.1.9。

### 修复

#### MQTT 核心功能

- [#14201](https://github.com/emqx/emqx/pull/14201) 防止 WebSocket 连接遇到速率限制时出现 `check_gc` 警告。
- [#14215](https://github.com/emqx/emqx/pull/14215) 修复了当 retainer 被禁用时（通过 REST 或 CLI 调用）会抛出异常的问题。
- [#14223](https://github.com/emqx/emqx/pull/14223) 确保 WebSocket 关闭原因返回为原子类型，以避免崩溃。
- [#14260](https://github.com/emqx/emqx/pull/14260) 解决了一个罕见的竞态条件，如果 CONNECT 数据包在空闲超时（默认15秒）之前没有完全接收，可能导致连接进程崩溃。
- [#14268](https://github.com/emqx/emqx/pull/14268) 修复了另一个罕见的竞态条件，如果 CONNECT 数据包在空闲超时之前没有完全接收，可能导致 WebSocket 连接进程崩溃。
- [#14266](https://github.com/emqx/emqx/pull/14266) 将 `emqtt` 从版本 1.13.0 更新到 1.13.5。有关更多细节，请参阅 [emqtt 更新日志](https://github.com/emqx/emqtt/blob/1.13.5/changelog.md)。

#### 会话持久化

- [#14160](https://github.com/emqx/emqx/pull/14160) 确保持久会话订阅的主题匹配规则正确适用于以 `$` 符号开头的主题，以符合 MQTT 规范要求。
- [#14229](https://github.com/emqx/emqx/pull/14229) 修复了 Raft/RocksDB 后端实现中与持久存储相关的几个问题，这些问题可能在某些情况下影响持久共享订阅所使用的内部数据库的正确性和副本收敛性。
- [#14298](https://github.com/emqx/emqx/pull/14298) 改进了 DS Raft/RocksDB 后端对临时远程分片故障的容错能力，防止在拉取分片数据以获取更新时发生持久会话崩溃。

#### REST API

- [#14117](https://github.com/emqx/emqx/pull/14117) 修复了 REST API 文档中的一个问题，该问题错误地将 `Users` 端点标记为支持 `Basic` 认证。

#### 认证

- [#14314](https://github.com/emqx/emqx/pull/14314) 修复了 `scram:http` 认证功能，此前该功能无法正常工作。
- [#14305](https://github.com/emqx/emqx/pull/14305) 认证中移除了对哈希算法 `MD4`、`MD5` 和 `RIPEMD-160` 的支持，因为它们不符合 [NIST 安全哈希标准](https://www.nist.gov/publications/secure-hash-standard)。

#### 规则引擎

- [#14217](https://github.com/emqx/emqx/pull/14217) 修复了 schema registry 端点示例配置中的错误。

#### 数据集成

- [#14172](https://github.com/emqx/emqx/pull/14172) 解决了一个潜在的竞争条件问题，当使用 HTTP API 测试连接器时，如果 HTTP 请求超时，可能会留下未清理的资源。

- [#14178](https://github.com/emqx/emqx/pull/14178) 修复了一个问题，配置同步可能因在集群中不同节点上同时删除规则而导致某个节点上的同步进程卡住。

- [#14226](https://github.com/emqx/emqx/pull/14226) 缓解了在高负载情况下，节点可能丢失资源指标（如动作/ source）并且在没有重启的情况下无法恢复的场景。现在，当重新启动资源或重置其指标时，系统会尝试重建丢失的指标。

  此外，关于指标失败的警告日志（例如 `matched` 等 "热路径" 指标）现在会被限流，以防止日志过多。

- [#14265](https://github.com/emqx/emqx/pull/14265) 修复了一个问题，当 MQTT Source 动作未能成功完成对指定主题的订阅过程，停止连接器时会发生 `badkey` 错误。
- [#14296](https://github.com/emqx/emqx/pull/14296) 防止 `ecpool_sup` 被一个启动缓慢的 `ecpool_worker` 阻塞。
- [#14126](https://github.com/emqx/emqx/pull/14126) 修复了 Oracle 集成的预处语句问题。在此修复之前，当预处理语句无效（例如引用不存在的表列）时，可能导致动作应用最旧版本的预处理语句，从而导致不一致。
- [#14181](https://github.com/emqx/emqx/pull/14181) 使 Kafka 和 Pulsar 生产者在处理损坏的 COMMIT 文件时更加稳定。如果磁盘模式缓冲区中的 COMMIT 文件损坏，将被忽略。虽然这可能导致某些已发送的消息被重新发送，但生产者将不再崩溃。

#### 系统配置

- [#14180](https://github.com/emqx/emqx/pull/14180) 修复了当变量绑定为 `undefined` 或 `null` 时，variform 表达式返回 `'undefined'` 的问题。现在，返回空字符串代替。

- [#14289](https://github.com/emqx/emqx/pull/14289) 解决了在从不同环境导入配置时，日志文件路径不一致的问题。

  在 Docker 中，`EMQX_LOG_DIR` 环境变量设置为 `/opt/emqx/log`，但通过 RPM/DEB 包安装时则为 `/var/log/emqx/`。在此修复之前，导出的日志文件路径（默认文件日志处理进程和审计日志处理进程）会进行环境变量替换。这可能会导致在导入配置到不同环境时，如果目标环境中目录不存在，就会发生崩溃。

  通过此修复，导出时不再对日志文件路径进行环境变量替换。此外，如果旧版本的绝对日志目录路径在新环境中不存在，路径将被转换回环境变量。

- [#14313](https://github.com/emqx/emqx/pull/14313) 解决了在启动过程中，由于从副本节点读取 REST API 启动 API 密钥文件，可能导致 EMQX 启动过程停滞的问题。 现在，启动 API 密钥文件仅在核心节点上加载。

#### 网关

- [#14276](https://github.com/emqx/emqx/pull/14276) 改进了 JT/T808 消息解析失败的错误日志，提供了更详细的排错信息。

#### 插件与扩展

- [#14243](https://github.com/emqx/emqx/pull/14243) 修复了在某些网关中 `client.connect` 钩子未被触发的问题。

#### MQTT over QUIC

- [#14258](https://github.com/emqx/emqx/pull/14258) 缩短了 QUIC 连接的关闭超时。此前，QUIC 连接在优雅关闭时的超时为 5 秒。如果客户端未响应，EMQX 会记录如下警告：

  ```
  [warning] msg: session_stepdown_request_timeout, action: discard,
  ```

  或者可能会导致 Dashboard 上断开客户端时超时。现在，对于“踢出”操作，超时已缩短为 1 秒，对于其他情况，超时为 3 秒。

## 5.8.2

*发布日期：2024-11-12*

升级前请查看已知问题列表和不兼容变更列表。

### 增强

#### 核心 MQTT 功能

- [#14059](https://github.com/emqx/emqx/pull/14059) 为保留消息功能新增了一个配置选项，用于限制保留消息的过期时间间隔。如果存储空间不足，该选项可以使垃圾回收更早地清除消息。

- [#14072](https://github.com/emqx/emqx/pull/14072) 更新了虚拟机的可显示范围以支持 Unicode。此改进提升了消息中某些二进制数据的可读性。例如，之前显示为 `<<116,101,115,116,228,184,173,230,150,135>>` 的二进制数据现在将被格式化为 `<<"test中文"/utf8>>`，提供了更清晰的表示。

#### MQTT 会话持久化

- [#14130](https://github.com/emqx/emqx/pull/14130) 降低了空闲持久会话的 CPU 使用率。之前，空闲的持久会话需要定期唤醒以刷新 DS 流的列表。此更改使流发现改为基于事件触发，从而显著降低空闲期间的 CPU 消耗。此外，此更新减少了通知会话新流的延迟，有效地消除了端到端处理中的长尾延迟。

#### REST API

- [#13889](https://github.com/emqx/emqx/pull/13889) 提升了 `/api/v5/monitor_current` 和 `/api/v5/metrics` API 的性能。

  之前，这些 API 会按顺序逐个查询集群中的节点。现在，查询已改为并行发送，从而缩短了响应时间。目前，延迟主要取决于集群中最慢的节点。

  此外，`/api/v5/monitor_current` API 新增了 `node` 参数，使用户可以针对单个节点进行查询，而无需查询整个集群。例如，使用 `?aggregate=false&node=emqx@node1.domain.name` 将仅返回指定节点的数据。

#### EMQX 集群

- [#13903](https://github.com/emqx/emqx/pull/13903) 增加了日志，当副本节点找不到与其自身相同版本的核心节点时，会提示用户。

#### 安全

- [#13923](https://github.com/emqx/emqx/pull/13923) 在认证、授权和挂载点模板中添加了对 `zone` 的支持。

  此前，若需在认证或授权规则中引用客户端的 `zone`，用户必须通过 `client_attrs` 访问。现在，可以直接在这些模板中使用 `${zone}` 占位符，简化了规则创建，并支持基于 zone 的特定配置。

  例如，以下 ACL 规则使用 `${zone}` 根据客户端的分配 zone 动态应用权限：`{allow, all, all, ["${zone}/${username}/#"]}`。

- [#14102](https://github.com/emqx/emqx/pull/14102) 增加了从密钥文件读取 SSL 私钥密码的支持。

  如果将 `password` 配置为 `...ssl.password = "file://{path-to-secret-file}"`，EMQX 现在可以从密钥文件中读取密码。

#### 数据集成

- [#14065](https://github.com/emqx/emqx/pull/14065) 为数据集成添加了新的 `queuing_bytes` 指标。该指标显示特定动作在缓冲过程中消耗的 RAM 和/或磁盘资源。目前，Pulsar 生产者动作是唯一不支持该指标的动作。

- [#14044](https://github.com/emqx/emqx/pull/14044) 增强了 IoTDB Thrift 驱动，使其支持在 `server` 字段中配置多个地址。如果当前连接失败，驱动将尝试连接下一个地址。

- [#14048](https://github.com/emqx/emqx/pull/14048) 提高了 Kafka/Confluent/Azure Event Hub 生产者动作的稳定性。一旦这些动作成功创建并处于健康状态，在检测到未知主题时将不再被标记为不健康。相反，它们会在这种情况下继续将消息排入队列。

- [#14079](https://github.com/emqx/emqx/pull/14079) 为 Kafka 消费者 source 添加了 `max_wait_time` 设置选项，允许用户配置等待 Kafka broker 发送响应对象的最大时间。

- [#14110](https://github.com/emqx/emqx/pull/14110) 为 Pulsar 驱动添加了增强的指标报告，包括排队消息、传输中消息和丢弃消息的计数，从而提供更高的可观察性。

- [#14118](https://github.com/emqx/emqx/pull/14118) 为 MySQL 动作添加了对 `ON DUPLICATE KEY UPDATE` 子句的支持。

  用户现在可以在 `mysql` 动作中指定 `ON DUPLICATE KEY UPDATE`，例如：

  ```sql
  INSERT INTO t1 (a,b,c) VALUES (${id},${clientid},${qos}) ON DUPLICATE KEY UPDATE a=a;
  ```

  **Note:** `ON DUPLICATE KEY UPDATE` 子句不支持占位符（`${var}`）。

#### 可观测性

- [#14096](https://github.com/emqx/emqx/pull/14096) 将 `emqx_conf_sync_txid` 作为 Prometheus 指标公开，支持监控集群中每个节点的配置文件同步状态。

#### MQTT over QUIC

- [#13814](https://github.com/emqx/emqx/pull/13814) 为基于 QUIC 的多流 MQTT 引入了连接范围内的 Keepalive 机制：

  引入了一项新功能，当数据流保持活动状态时，即使控制流处于空闲状态，MQTT 连接也会保持存活。

  之前，客户端需要在空闲的控制流上发送 `MQTT.PINGREQ` 来保持连接。现在，每个连接的所有流共享一个状态，用于跟踪活动情况。该共享状态用于判断连接是否仍然存活，从而降低了因队头阻塞（Head-of-Line, HOL）而导致 Keepalive 超时的风险。

- [#14112](https://github.com/emqx/emqx/pull/14112) 在 QUIC 监听器中增加了对 `ssl_options.hibernate_after` 的支持，以减少 QUIC 传输的内存占用。

- [#13984](https://github.com/emqx/emqx/pull/13984) Quicer NIF 库现已链接到系统的 `libcrypto`，从而提升了安全性、性能和与系统 OpenSSL 更新的兼容性。

  **注意：** 此更改不适用于 RHEL 7/CentOS 7，因为这些系统仍在使用 OpenSSL 1.0.x。

### 修复

#### 核心 MQTT 功能

- [#13931](https://github.com/emqx/emqx/pull/13931) 将 `gen_rpc` 库更新至 3.4.1 版本，其中包含一个修复，防止客户端 socket 初始化错误在服务端升级到节点级别。
- [#13969](https://github.com/emqx/emqx/pull/13969) 优化了过期保留消息的定期清理，以确保资源的高效使用，尤其是在处理大量过期消息的情况下。
- [#14068](https://github.com/emqx/emqx/pull/14068) 为所有网关实现模块添加了 `handle_frame_error/2` 回调，用于处理消息解析错误。
- [#14037](https://github.com/emqx/emqx/pull/14037) 改进了内部数据库的引导过程，以更好地容忍节点临时不可用的情况，特别是在新节点加入现有集群时。
- [#14116](https://github.com/emqx/emqx/pull/14116) 修复了在加入集群后，保留消息的默认配置生成不正确的问题。

#### MQTT 会话持久化

- [#14042](https://github.com/emqx/emqx/pull/14042) 修复了在更新订阅参数（如 QoS、`no_local`、`upgrade_qos` 等）后持久会话崩溃的问题。
- [#14052](https://github.com/emqx/emqx/pull/14052) 修正了 cgroups 在启用时的内存使用报告。
- [#14055](https://github.com/emqx/emqx/pull/14055) 更新了 `/clients_v2` API，在查询具有持久会话的离线客户端时正确应用所有过滤参数。此前，只有 `username` 过滤器生效，其他过滤参数被忽略。
- [#14151](https://github.com/emqx/emqx/pull/14151) 修复了 `/clients_v2` API 中对具有持久会话的离线客户端的 `conn_state` 过滤器的处理问题。此前，这些客户端可能会被错误地筛选为 `conn_state=connected`。
- [#14057](https://github.com/emqx/emqx/pull/14057) 解决了一个兼容性问题，该问题导致 Messages DS 数据库因数据库配置模式的轻微差异而无法启动。当从 EMQX 5.7.x 版本升级到更高版本时，如果会话持久性功能被启用，就会出现这个兼容性问题。

#### REST API

- [#14023](https://github.com/emqx/emqx/pull/14023) 修复了 `GET /monitor` HTTP API 的一个问题，在某些时间窗口下，返回的值可能会比实际值更高。对于 1 小时内的数据点，此失真仅在 Dashboard 上呈现为视觉上的误差。然而，对于超过 1 小时的数据点，该数据失真则是永久性的。

  受影响的指标包括：

  - `disconnected_durable_sessions`
  - `subscriptions_durable`
  - `subscriptions`
  - `topics`
  - `connections`
  - `live_connections`

- [#14117](https://github.com/emqx/emqx/pull/14117) 修正了 REST API 文档中的一个错误，该错误错误地指示 `Users` 端点支持 `Basic` 认证。

#### EMQX 集群


- [#13996](https://github.com/emqx/emqx/pull/13996) 修复了在使用 `emqx conf fix` 解决配置差异时的间歇性崩溃问题，尤其是在某个节点缺少配置键的情况下。

#### 安全

- [#13922](https://github.com/emqx/emqx/pull/13922) 更新了 CRL（证书吊销列表）缓存，以完整的分发点（DP）URL 作为缓存键。此前仅使用了 URL 的路径部分，当多个 DP 共享相同路径时会导致冲突。
- [#13924](https://github.com/emqx/emqx/pull/13924) 修复了一个问题，该问题可能导致 JWT 认证失败时 JWK 密钥泄露到 debug 日志中。
- [#13998](https://github.com/emqx/emqx/pull/13998) 修复了一个问题，即如果 OIDC 配置不正确，会导致 SSO 功能崩溃。

#### 数据集成

- [#13916](https://github.com/emqx/emqx/pull/13916) 修复了一个问题，即当规则的 `failed.no_result` 或 `failed.exception` 指标更新时，父级指标 `failed` 未被相应地递增。
- [#14001](https://github.com/emqx/emqx/pull/14001) 解决了一个竞争条件问题，即资源（如连接器、动作、source、认证或授权）在短暂断开连接后可能错误地报告通道已连接且健康。此问题可能会导致大量 `action_not_found` 日志条目。
- [#13913](https://github.com/emqx/emqx/pull/13913) 修复了 actions 和 source HTTP API 的一个问题，如果在更新或删除资源时发生超时，将返回 500 状态码。
- [#14101](https://github.com/emqx/emqx/pull/14101) 解决了一个问题，即如果 source 和动作的名称相同，则删除其中任一项资源会失败。
- [#13901](https://github.com/emqx/emqx/pull/13901) 修复了 Postgres 集成的预处理语句问题。此前，如果在更新 Postgres 集成动作时使用了无效的预处理语句（例如引用了未知的表列），可能导致动作应用旧版本的预处理语句。


- [#14005](https://github.com/emqx/emqx/pull/14005) 修复了 IoTDB Thrift 驱动在启用 SSL 时无法正常工作的问题。

- [#14125](https://github.com/emqx/emqx/pull/14125) 对于 IoTDB，由于 Thrift 驱动从未支持过 `async` 模式，现在如果在配置中指定了 `async` 模式，将会生成一条错误日志。

- [#14008](https://github.com/emqx/emqx/pull/14008) 解决了具有聚合模式的动作（例如 S3、Azure Blob Storage、Snowflake）中的潜在竞争条件，该问题可能导致上传过程中跳过某个聚合批次。

- [#14015](https://github.com/emqx/emqx/pull/14015) 修复了 Kafka/Confluent/Azure Event Hub Producer 动作的一个问题。在启用了磁盘缓冲的情况下，重启后这些动作不会发送已排队的消息，直到新的消息到达。此修复适用于固定主题（即不包含占位符）的动作。此外，在 EMQX 5.7.2 之前，Kafka/Confluent/Azure Event Hub Producer 动作的磁盘缓冲消息存储在不同的目录结构中。现在，如果检测到旧的磁盘缓冲目录，EMQX 将自动重命名为当前结构，以防止数据丢失。

- [#14069](https://github.com/emqx/emqx/pull/14069) 修复了 Cassandra 集成的预处理语句问题。此前，当 EMQX 动作中的 SQL 模板被修改时，更新的语句无法为 Cassandra 预处理，导致写入失败。

- [#14079](https://github.com/emqx/emqx/pull/14079) 解决了 Kafka 消费者在多个分区共享同一分区 leader 时的延迟问题。此前，由于 Kafka 每个连接只能同时处理一个进行中的 fetch 请求，导致了队头阻塞。此修复确保每个分区消费者都建立自己的 TCP 连接到分区 leader，以防止分区共享 leader 时的延迟。

- [#14106](https://github.com/emqx/emqx/pull/14106) 增加了验证，防止单个 Kafka 消费者连接器包含具有重复 Kafka 主题的 source。

- [#14120](https://github.com/emqx/emqx/pull/14120) 改进了 Pulsar 连接器健康检查中的超时处理，以减少不必要的日志噪音。

#### 可观测性

- [#13909](https://github.com/emqx/emqx/pull/13909) 修复了日志格式问题，解决了无法将负载显示为可读 UTF-8 Unicode 字符的情况。

- [#14061](https://github.com/emqx/emqx/pull/14061) 改进了 `emqx_cm:request_stepdown/3` 失败时的日志信息。

  在某些场景下，当一个客户端通道需要终止另一个具有相同 ClientID 的通道时，可能会出现竞争条件，如果目标通道已经关闭或终止，则不会再生成无用的错误日志和堆栈信息。

- [#14070](https://github.com/emqx/emqx/pull/14070) 从错误和警告日志中移除了连接器的 `state` 信息，因为其长度可能过长。对于问题分析，现在可以通过 `emqx_resource:list_instances_verbose/0` 来获取连接器的状态。以下是变更前的日志示例：

  ```
  pid: <0.43914.0>, connector: connector:sqlserver:connector-05a2e105, reason: [Microsoft][ODBC Driver 17 for SQL Server][SQL Server]Argument data type varchar is invalid for argument 2 of dateadd function. SQLSTATE IS: 42000, state: {"resource_opts":{"start_timeout":5000,"start_after_created":true,"health_check_interval":15000},"pool_name":"connector:sqlserver:connector-05a2e105","installed_channels":{"action:sqlserver:action-4b033621:connector:sqlserver:connector-05a2e105":{"sql_templates":{"batch_insert_temp":{"send_message":{"batch_insert_tks":["{str,<<\" ( \">>}","{var,[<<\"messageId\">>]}","{str,<<\", \">>}","{var,[<<\"measurement\">>]}","{str,<<\", \">>}","{var,[<<\"Analog_IN_Fault_1\">>]}","{str,<<\", \">>}","{var,[<<\"Analog_IN_Fault_2\">>]}","{str,<<\", \">>}","{var,[<<\"Analog_IN_Fault_3\">>]}","{str,<<\", \">>}","{var,[<<\"Analog_IN_Fault_4\">>]}","{str,<<\", \">>}","{var,[<<\"Analog_IN_PV_1\">>]}","{str,<<\", \">>}","{var,[<<\"Analog_IN_PV_2\">>]}","{str,<<\", \">>}","{var,[<<\"Analog_IN_PV_3\">>]}","{str,<<\", \">>}","{var,[<<\"Analog_IN_PV_4\">>]}","{str,<<\", DATEADD(MS, \">>}","{var,[<<\"ms_shift\">>]}","{str,<<\", DATEADD(S, \">>}","{var,[<<\"s_shift\">>]}","{str,<<\", '19700101 00:00:00:000') ))\">>}"],"batch_insert_part":"insert into TransactionLog(MessageId, Measurement, Fault1, Fault2, Fault3, Fault4, Value1, Value2, Value3, Value4, DateStamp) \r\n"}}}}}},msg: invalid_request
  ```

- [#14099](https://github.com/emqx/emqx/pull/14099) 移除了在验证 MQTT 消息中的 UTF-8 字符串失败时触发的错误级日志。

- [#14091](https://github.com/emqx/emqx/pull/14091) 修复了 InfluxDB 数据集成中，用户提供不支持的写入语法时报告 `function_clause` 错误。

  不支持的语法示例：

  ```bash
  weather,location=us-midwest,season=summer temperature=82 ${timestamp}u 
  ```

  在此修复之前，错误日志中会包含 `function_clause` 错误，如下所示：

  ```
  pid: <0.558392.0>, info: {"stacktrace":["{emqx_bridge_influxdb_connector,parse_timestamp,[[1719350482910000000,<<\"u\">>]],[{file,\"emqx_bridge_influxdb_connector.erl\"},{line,692}]}", ...], ..., "error":"{error,function_clause}"}, tag: ERROR, msg: resource_exception
  ```

  该更改通过在语法错误情况下省略 `function_clause` 提升了日志的清晰度。

#### 审计日志

- [#14152](https://github.com/emqx/emqx/pull/14152) 实现了日志截断功能，以防止存储过长的审计日志内容。

#### 集群连接


- [#14004](https://github.com/emqx/emqx/pull/14004) 修复了集群连接中的一个问题，即在 `topics` 配置中，重叠的主题过滤器导致跨集群消息路由不一致且不完整。现在每个主题过滤器都会被单独处理；但需要注意的是，重叠的过滤器可能会增加跨集群路由的复杂性。
- [#13929](https://github.com/emqx/emqx/pull/13929) 修复了一个问题，即集群连接偶尔会卡住并停止工作，直到手动重启上游集群后才能恢复。

## 5.8.1

*发布日期：2024-10-14*

升级前请查看已知问题列表和不兼容变更列表。

### 重要变更

- [#13956](https://github.com/emqx/emqx/pull/13956) RPC 库 `gen_rpc` 升级到 3.4.1，修复一个竞争导致的节点崩溃问题。在此版本前，如果某个节点在 RPC 通道建立过程中关闭，可能会导集群中致对等节点崩溃。

### 增强

#### 核心 MQTT 功能

- [#13525](https://github.com/emqx/emqx/pull/13525) 新增了配置项 `shared_subscription_initial_sticky_pick`，用于在 `shared_subscription_strategy` 为 `sticky` 时第一次选取共享订阅客户端的策略。

- [#13942](https://github.com/emqx/emqx/pull/13942) HTTP 客户端现在在最新请求超时后，如果 10 秒内未检测到任何活动，将自动重新连接。此前，客户端会无限期等待服务器响应，导致服务器丢弃请求时发生超时。

  此更改影响以下组件：

  - HTTP 认证
  - HTTP 授权
  - Webhook（HTTP 连接器）
  - GCP PubSub 连接器（企业版）
  - S3 连接器（企业版）
  - InfluxDB 连接器（企业版）
  - Couchbase 连接器（企业版）
  - IoTDB 连接器（企业版）
  - Snowflake 连接器（企业版）

#### 认证与授权

- [#13863](https://github.com/emqx/emqx/pull/13863) EMQX 现在支持在原始 ACL 规则的主题名称模板中使用 `${cert_common_name}` 占位符。

- [#13864](https://github.com/emqx/emqx/pull/13864) 增加了在 LDAP 查询过滤器中使用 `memberOf` 语法的支持。

- [#13810](https://github.com/emqx/emqx/pull/13810) 新增了 client-info 认证。

  Client-info（类型为 `cinfo`）认证是一种轻量级的认证机制，它通过检查客户端属性和自定义规则来进行认证。这些规则使用 Variform 表达式定义匹配条件以及匹配后的认证结果。例如，为了快速屏蔽没有用户名的客户端，可以设置匹配条件为 `str_eq(username, '')`，并关联检查结果 `deny`。

- [#13792](https://github.com/emqx/emqx/pull/13792) 黑名单查询 API `GET /banned` 现支持使用以下参数对规则进行查找：

  - clientid
  - username
  - peerhost
  - like_clientid
  - like_username
  - like_peerhost
  - like_peerhost_net

  在新增黑名单记录时，对于未指定 `until`  参数的默认过期时间已从 1 年改为 `无限期`。

#### 规则引擎

- [#13773](https://github.com/emqx/emqx/pull/13773) 停用的规则动作现在不会触发 `out_of_service` 警告。

  之前，如果某个动作被停用，会出现带有 `msg: out_of_service` 的警告日志，并且该规则的 `actions.failed` 计数器会增加。

  经过此次优化后，停用的动作将记录 `debug` 级别日志，日志内容为 `msg: discarded`，并且新增的 `actions.discarded` 计数器将增加。

- [#13804](https://github.com/emqx/emqx/pull/13804) 支持使用 Confluent Schema Registry 作为外部 Schema 提供者与 EMQX 的 Schema Registry 集成。

#### 数据集成

- [#13716](https://github.com/emqx/emqx/pull/13716) 引入了 IoTDB 连接器的 Thrift 驱动。

- [#13745](https://github.com/emqx/emqx/pull/13745) EMQX 现已支持与 Snowflake 的数据集成。

- [#13783](https://github.com/emqx/emqx/pull/13783) 在 Kafka 生产者处于异步模式下运行时，降低了缓冲区内存的使用量。

- [#13861](https://github.com/emqx/emqx/pull/13861) 为部分数据集成动作添加了一个新的配置项 `undefined_vars_as_null`，用于确保当数据写入数据库时，SQL 模版中未定义的占位符变量将被替换为 `NULL`。

  以下 Sink 动作添加了此配置项：

    - MySQL
    - ClickHouse
    - SQLServer
    - TDengine
    - DynamoDB

#### MQTT over QUIC

- [#13814](https://github.com/emqx/emqx/pull/13814) 基于 QUIC 多流的连接范围保活功能：

  此更新引入了一项新功能，即使控制流处于空闲状态，但其他数据流仍活跃时，仍然可以保持基于 QUIC 多流的 MQTT 连接。

  之前，客户端需要在空闲的控制流上发送 `MQTT.PINGREQ` 来保持连接活跃。现在，每个连接都会维护一个共享状态，监控所有流的活动情况。这个共享状态有助于判断连接是否仍然活跃，减少由于队头阻塞（HOL blocking）导致的保活超时风险，并提高整体连接的稳定性。

#### MQTT 会话持久化

- [#13788](https://github.com/emqx/emqx/pull/13788) 当相应功能被禁用时，防止 DS 共享订阅应用程序执行完整的启动流程。这也避免了初始化内部数据库，进而防止占用大量磁盘空间。

#### 集群连接

- [#13835](https://github.com/emqx/emqx/pull/13835) 新增了 `PUT /cluster/links/link/:name/metrics/reset` HTTP API 端点，用于重置指定集群连接的指标。

#### Dashboard

- [#13873](https://github.com/emqx/emqx/pull/13873) 提升了 `/api/v5/monitor` 端点的性能。

  此更新改善了 Dashboard 监控页面的性能，特别是在拥有大量节点的集群中，之前常出现超时问题。

  主要改进包括：

  - 实现了并发的 RPC 调用，从集群中的各个节点检索指标数据。
  - 引入了数据降采样机制，根据查询的时间范围减少数据点密度：
    - 最近 `1h` 的数据间隔为 `10s`
    - 最近 `1d` 的数据间隔为 `1m`
    - 最近 `3d` 的数据间隔为 `5m`
    - 最近 `7d` 的数据间隔为 `10m`
  - 为 EMQX 停止运行的时段添加了虚拟数据点，确保在 Dashboard 的时间轴上清晰显示这些间隙。

#### 企业版 License

- [#13910](https://github.com/emqx/emqx/pull/13910) 优化了企业版 License 检查的性能。

### 修复

#### 核心 MQTT 功能

- [#13702](https://github.com/emqx/emqx/pull/13702) 修复节点宕机时清理该节点持有的排它订阅。

- [#13708](https://github.com/emqx/emqx/pull/13708) 修复了可能导致共享订阅的 “sticky” 策略降级为 “random” 的问题。

- [#13733](https://github.com/emqx/emqx/pull/13733) 在使用 `emqx ctl conf load` 命令配置 https 监听器时，允许 `cacertfile` 参数为可选项。

- [#13742](https://github.com/emqx/emqx/pull/13742) 修复了客户端在订阅主题 `#` 或 `+` 时，会收到以 `$` 开头的主题的保留消息的问题。

- [#13754](https://github.com/emqx/emqx/pull/13754) 修复了 websocket 连接会自行持续中断的问题。

- [#13756](https://github.com/emqx/emqx/pull/13756) 增加了分配给客户端 ID 的随机性。

- [#13790](https://github.com/emqx/emqx/pull/13790) 将 MQTT 连接器的默认心跳间隔从 300 秒减少到 160 秒。

  此更改有助于通过防止因负载均衡器或防火墙的空闲限制导致的超时来维护底层 TCP 连接，云服务提供商通常将这些限制设定在 3 到 5 分钟之间。

- [#13832](https://github.com/emqx/emqx/pull/13832) 修复了启用持久会话时，`Publish` 端点出现 500 错误的问题。

- [#13842](https://github.com/emqx/emqx/pull/13842) 修复了 UTF-8 字符串验证异常问题。

- [#13956](https://github.com/emqx/emqx/pull/13956) 将 `gen_rpc` 库更新至 3.4.1 版本，该版本包含一个修复，防止客户端 socket 初始化错误升级到服务端的节点级别。

#### 升级与迁移

- [#13731](https://github.com/emqx/emqx/pull/13731) 解决了运行在 EMQX 5.4.0 的集群无法升级到 EMQX 5.8.0 的问题。此修复引入了一个迁移过程，将 5.4.0 版本中创建的特定内部数据库表更新为符合新架构。

#### 认证

- [#13726](https://github.com/emqx/emqx/pull/13726) 升级了 Kerberos 认证库，改为使用 MEMORY 类型缓存，替代之前的 FILE 类型缓存，解决了在并发初始化认证请求时可能导致的失败问题。

#### 规则引擎

- [#13735](https://github.com/emqx/emqx/pull/13735) 改进了在解码无效 payload 时的消息转换错误提示。
- [#13769](https://github.com/emqx/emqx/pull/13769) 修复了使用带有 `extends` 属性的 JSON Schema 验证（draft 3）时，总是导致验证失败的问题。

#### 数据集成

- [#13851](https://github.com/emqx/emqx/pull/13851) 修复了使用 IoTDB Thrift 驱动时，`Test connectivity` 测试连接出现异常的问题。

- [#13724](https://github.com/emqx/emqx/pull/13724) 现在，在聚合模式下，Azure Blob Storage 和 S3 动作在达到最大记录数量后，会更快触发发送聚合数据。

- [#13734](https://github.com/emqx/emqx/pull/13734) 优化了 Azure Blob Storage 连接器配置时的错误提示，使其更加友好。

- [#13736](https://github.com/emqx/emqx/pull/13736) 升级了 Kafka 生产者，支持客户端重新认证。详见 [kafka_protocol#122](https://github.com/kafka4beam/kafka_protocol/pull/122)。

  同时修复了以下问题：

  - 修复了 [PR#13727](https://github.com/emqx/emqx/pull/13727) 和 [wolff#74](https://github.com/kafka4beam/wolff/pull/74) 中的 `unexpected_info` 错误日志。
  - 修复了 Kafka 连接中由于竞态条件导致的 `einval` 崩溃报告，详见 [kafka_protocol#124](https://github.com/kafka4beam/kafka_protocol/pull/124)。

- [#13896](https://github.com/emqx/emqx/pull/13896) 将 Pulsar 客户端从 `0.8.3` 升级至 `0.8.4`（详见 [pulsar#61](https://github.com/emqx/pulsar-client-erl/pull/61)）。

  在此修复之前，如果生产者客户端遇到 “socket error”（而非正常的“socket close”），它可能会继续向已关闭的 socket 发送数据且没有错误处理。用户在 EMQX Dashboard 上可能会观察到 “total” 计数器持续增加，但“success”、“failed” 和 “dropped” 计数器不增加。

- [#13897](https://github.com/emqx/emqx/pull/13897) Microsoft SQL Server 连接器现已兼容 Microsoft ODBC 18。

- [#13902](https://github.com/emqx/emqx/pull/13902) 修复了 MySQL 集成中预处理语句的问题。

  在此修复之前，如果在更新 MySQL 集成动作时使用了无效的预处理语句（例如引用了未知的表列名），可能会导致动作恢复为使用最早版本的预处理语句。

- [#13906](https://github.com/emqx/emqx/pull/13906) 修复了 PostgreSQL 集成中预处理语句的问题。

  在此修复之前，如果在更新 Postgres 集成动作时使用了无效的预处理语句（例如引用了未知的表列名），可能会导致动作恢复为使用最早版本的预处理语句。

- [#13921](https://github.com/emqx/emqx/pull/13921) 修复了 Pulsar 生产者动作中更改 `sync_timeout` 参数未按预期生效，未能正确影响请求超时的问题。

  此外，弃用了 Pulsar 生产者动作中的 `resource_opts.request_ttl` 配置，因为该配置未能如预期那样影响请求 TTL（实际由 `retention_period` 控制）。这一更改有助于防止用户产生混淆。

- [#13959](https://github.com/emqx/emqx/pull/13959) 将 Pulsar 客户端从 `0.8.4` 升级到 `0.8.5`（参见 [pulsar#62](https://github.com/emqx/pulsar-client-erl/pull/62)）。此更新修复了一个问题，在某些竞争条件下，生产者可能无法与客户端进程通信，导致客户端进程意外停止且不会自动重启。之前唯一的解决方法是手动重启该进程。
- [#13965](https://github.com/emqx/emqx/pull/13965) 修复了在 IoTDB Sink 中使用批量模式作为数据写入方式时出现的函数子句错误。
- [#13971](https://github.com/emqx/emqx/pull/13971) 修复了 EMQX Enterprise 5.8.0 中引入的 Kafka 生产者错误，生产者在初始化阶段如果未能获取元数据，可能会崩溃。
- [#13973](https://github.com/emqx/emqx/pull/13973) 修复了 Microsoft SQL Server 集成中的一个问题，EMQX 在每次与服务器的连接断开时会记录多条错误和警告日志。

#### 管理和运维

- [#13963](https://github.com/emqx/emqx/pull/13963) 修复了审计日志功能中的以下问题：
  - 审计日志功能与单点登录（SSO）功能不兼容，导致每个 SSO 事件都会引发异常。
  - 非法访问尝试（例如，对仅支持 `POST` 的端点发起 `GET` 请求）未被记录。

#### 集群连接

- [#13888](https://github.com/emqx/emqx/pull/13888) 修复了无法通过 HTTP API 更新没有 clientid 的集群连接的问题。
- [#13927](https://github.com/emqx/emqx/pull/13927) 修复了当本地集群中包含一个或多个高度拥挤的主题时，集群连接引导过程可能会崩溃的问题。

## 5.8.0

*发布日期：2024-08-28*

在升级之前，请先阅读 [EMQX 5.8 已知问题](./known-issues-5.8.md)。

### 增强

#### 集群连接

- [#13126](https://github.com/emqx/emqx/pull/13126) 新增了集群连接功能，使多个独立的 EMQX 集群可以互相连接和通信。此功能支持在地理位置分散的集群间实现高效的消息交换，提升 MQTT 部署的灵活性和扩展能力。

#### 核心 MQTT 功能

- [#13009](https://github.com/emqx/emqx/pull/13009) 将因速率限制导致消息接收暂停的日志级别从 `debug` 更新为 `warning`。为了避免过多日志输出，对日志消息 `socket_receive_paused_by_rate_limit` 进行了日志节流。

#### 认证与授权

- [#13350](https://github.com/emqx/emqx/pull/13350) 支持获取客户端连接的服务器名称，并将其存储在客户端信息中，作为 `peersni` 字段。

- [#12418](https://github.com/emqx/emqx/pull/12418) 增强了 JWT 认证功能，支持使用对象列表来进行声明验证：

  ```
  [
    {
      name = "claim_name",
      value = "${username}"
    },
    ...
  ]
  ```

  预期值现在作为模板处理，与其他认证器保持一致，允许使用 `${username}` 和 `${clientid}` 等任意表达式。之前，只支持插值固定的 `"${username}"` 和 `"${clientid}"` 值。

  改进了 `verify_claims` 参数的文档。

- [#13229](https://github.com/emqx/emqx/pull/13229) 在认证模板中添加了对 `${cert_pem}` 占位符的支持。

- [#13324](https://github.com/emqx/emqx/pull/13324) EMQX Dashboard 现在可以与支持 OIDC 协议的身份服务集成，例如 [Okta](https://www.okta.com/)，以实现基于 OIDC 的单点登录（SSO）。
- [#13534](https://github.com/emqx/emqx/pull/13534) 添加了日志追踪，用于记录超级用户绕过授权检查的情况。
- [#13601](https://github.com/emqx/emqx/pull/13601) EMQX 支持 Kerberos 认证，该认证使用 GSSAPI 机制（基于 Kerberos V5 的 SASL-GSSAPI）。此增强功能允许 MQTT 客户端和服务器使用 `GSSAPI-KERBEROS` 方法在不安全的网络上进行安全认证。

#### 数据集成

- [#13144](https://github.com/emqx/emqx/pull/13144) 修改了日志级别，将桥接缓冲区溢出且消息被丢弃时的日志消息 `data_bridge_buffer_overflow` 从 `info` 级别更改为 `warning` 级别，并增加了日志节流。之前，这些事件的日志记录级别为 `info`，在默认日志设置下不可见。

- [#13492](https://github.com/emqx/emqx/pull/13492) 增强了 `GET /connectors` 和 `GET /connectors/:id` 接口，现在这些接口返回使用特定连接器的动作和 source 的列表。此外，`GET /actions`、`GET /sources`、`GET /actions/:id` 和 `GET /sources/:id` 接口现在返回与特定动作或 source 关联的规则列表。

- [#13505](https://github.com/emqx/emqx/pull/13505) 在 HTTP API 中新增了根据数据集成动作或源的 ID 过滤规则的功能。

- [#13506](https://github.com/emqx/emqx/pull/13506) 在所有已包含 `peerhost` 字段的规则引擎事件中引入了 `peername` 字段。`peername` 字段是一个格式为 `IP:PORT` 的字符串。

- [#13516](https://github.com/emqx/emqx/pull/13516) 在 `republish` 动作中添加了 `direct_dispatch` 参数。

  当 `direct_dispatch` 设置为 `true`（或通过模板渲染为 `true`）时，消息将直接发送给订阅者，避免触发其他规则或重复激活相同的规则。

- [#13573](https://github.com/emqx/emqx/pull/13573) 在客户端连接事件和消息 `publish` 事件的 SQL 上下文中引入了 `client_attrs`。用户现在可以在规则 SQL 语句中访问客户端属性，例如 `SELECT client_attrs.attr1 AS attribute1`，并在数据集成动作中使用 `${attribute1}`。

- [#13640](https://github.com/emqx/emqx/pull/13640) 为规则添加了两个新的 SQL 函数：`coalesce/2` 和 `coalesce_ne/2`。

  这些函数简化了在规则 SQL 表达式中处理空值的过程。例如，之前需要使用以下表达式：

  ```sql
  SELECT
    CASE
      WHEN is_null(payload.path.to.value) THEN
        0
      ELSE
        payload.path.to.value
    END AS my_value
  ```

  现在可以使用更简洁的表达式：`SELECT coalesce(payload.path.to.value, 0) AS my_value`。

- [#12959](https://github.com/emqx/emqx/pull/12959) 为 Kafka 生产者连接器引入了一个新选项，可以配置专用于健康检查的主题。此功能能够更精确地检测与分区 leader 的连接问题，例如错误或缺失的凭据可能会阻止建立连接。

- [#12961](https://github.com/emqx/emqx/pull/12961) 添加了一个配置选项，可以提前自定义 Kafka Consumer source 的消费组 ID。

- [#13069](https://github.com/emqx/emqx/pull/13069) EMQX 支持与 Azure Blob Storage 的数据集成。

- [#13199](https://github.com/emqx/emqx/pull/13199) 新增了消息转换功能。该功能允许用户仅使用简单的 variform 语法即可转换和丰富传入的消息，而无需在规则引擎中定义 SQL 规则。

  **使用示例：** 假设您收到一条 Avro 格式编码的消息，您希望将其解码为 JSON 格式。解码后，您想在将消息发送到规则引擎处理之前，将一个 `tenant` 属性（从发布客户端的客户端属性中检索）添加到主题前面。通过此新功能，您可以使用以下配置来实现此转换：

  ```
  message_transformation {
    transformations = [
      {
        name = mytransformation
        failure_action = drop
        payload_decoder = {type = avro, schema = myschema}
        payload_encoder = {type = json}
        operations = [
          {key = "topic", value = "concat([client_attrs.tenant, '/', topic])"}
        ]
      }
    ]
  }
  ```

  此配置指定了一个名为 `mytransformation` 的转换，它：

  - 使用指定的 schema 将消息 payload 从 Avro 格式**解码**。
  - 将 payload **编码**为 JSON 格式。
  - 将客户端属性中的 `tenant` 属性与原始主题**连接**，从而在进一步处理之前修改主题。

- [#13415](https://github.com/emqx/emqx/pull/13415) EMQX 支持与 Couchbase 的数据集成。

- [#13463](https://github.com/emqx/emqx/pull/13463) 增强了 GCP PubSub 生产者动作，当从 PubSub 接收到 HTTP 状态代码 502（Bad Gateway）或 503（Service Unavailable）时，自动重试请求。重试将持续进行，直到请求成功或消息的生存时间（TTL）到达。

- [#13546](https://github.com/emqx/emqx/pull/13546) 为 Pulsar 生产者动作添加了一个可配置的查询模式选项，允许用户在数据发送到 Pulsar 服务之前自定义数据查询方式。

- [#13650](https://github.com/emqx/emqx/pull/13650) EMQX 支持与 DataLayers 的数据集成。

#### 运维

- [#13202](https://github.com/emqx/emqx/pull/13202) 引入了 `emqx ctl conf cluster_sync fix` 命令，用于解决集群配置不一致的问题。该命令将所有节点的配置与具有最高 `tnx_id` 的节点的配置进行同步，确保集群配置的一致性。

- [#13250](https://github.com/emqx/emqx/pull/13250) 为 `cluster.discovery_strategy` 添加了一个新的值：`singleton`。选择此选项后，将不会形成集群，并且节点将拒绝与其他节点之间的连接尝试。

- [#13370](https://github.com/emqx/emqx/pull/13370) 为持久存储添加了新版 `wildcard_optimized` 存储布局，提供了以下改进：
  - 新布局没有固有的延迟。
  - MQTT 消息被序列化为更节省空间的格式。
  
- [#13524](https://github.com/emqx/emqx/pull/13524) 添加了 `emqx ctl exclusive` CLI 界面以更有效地管理排它订阅主题。该功能使管理员能够更好地管理和排查排它主题订阅问题，确保订阅状态准确反映并防止意外故障。

- [#13597](https://github.com/emqx/emqx/pull/13597) 添加了轻量级的封装函数，供插件自行存储和管理证书文件。此修复措施防止插件证书被证书垃圾回收（GC）功能意外删除。

- [#13626](https://github.com/emqx/emqx/pull/13626) 添加了一个新命令 `emqx ctl listeners enable <Identifier> <Bool>` 用于启用/禁用监听器。

- [#13493](https://github.com/emqx/emqx/pull/13493) 将 RPC 库 `gen_rpc` 升级到 3.4.0 版本。此更新将默认的 RPC 服务器套接字选项从 `true` 更改为 `active-100`，在 RPC 服务器负载较重时对对等节点引入了反压机制。

- [#13665](https://github.com/emqx/emqx/pull/13665) 在 Prometheus 端点中新增了一个指标 `emqx_actions_count`。该指标包含所有规则添加的动作数量，包括重发布（Republish）动作和控制台输出（Console Output）动作。

- [#13434](https://github.com/emqx/emqx/pull/13434) 简化了 `rpc` 配置。新增配置 `rpc.server_port` 替代了 `rpc.tcp_server_port` 和 `rpc.ssl_server_port`。

  `rpc.tcp_client_num` 重命名为 `rpc.client_num`，因为该配置适用于 TCP 和 SSL。旧的配置名称保留为别名，以确保向后兼容。

### 修复

#### 核心 MQTT 功能

- [#12944](https://github.com/emqx/emqx/pull/12944) 修复了当客户端使用非 UTF-8 编码的 client ID 且 `strict_mode=false` 时导致连接崩溃的问题。
- [#13006](https://github.com/emqx/emqx/pull/13006) 改进了对保留消息、延迟消息和接管会话消息的验证，确保它们遵循通过正则表达式匹配实现的客户端 ID 封禁规则。此前，一些由于网络问题而延迟的消息或被其他会话接管的消息可能会绕过这些客户端 ID 封禁规则。

#### 认证与授权

- [#13024](https://github.com/emqx/emqx/pull/13024) 添加了默认 ACL 拒绝规则以拒绝订阅 `+/#` 主题模式。由于 EMQX 默认拒绝订阅 `#` 主题，为了完整性，它也应该拒绝 `+/#`。
- [#13040](https://github.com/emqx/emqx/pull/13040) 改进了 HTTP 认证：
  - 改进了在 HTTP 请求中缺少或无法识别 `Content-Type` 头时的错误日志记录，提供了更详细的诊断信息。
  - 修复了在认证 HTTP 请求中导致查询参数双重编码的问题。
  - 改进了在配置为使用 POST 方法和 JSON 内容类型的认证请求中，当 JSON 模板无法渲染为有效 JSON 时的错误提示。此类情况通常发生在模板包含类似 `${password}` 的占位符但收到非 UTF-8 编码的密码输入时，改进后提高了透明度并简化了调试过程。
- [#13196](https://github.com/emqx/emqx/pull/13196) 为内置授权数据库添加了限制，将每个客户端或用户的权限列表（ACL）规则数量限制为默认的 100 条。
- [#13584](https://github.com/emqx/emqx/pull/13584) 修复了创建 HTTP 授权时，当 HTTP 头列表为空时导致错误的问题。
- [#13618](https://github.com/emqx/emqx/pull/13618) 改进了 `authorization/sources` 端点的类型规范，以提供更清晰和简洁的错误消息。
- [#13624](https://github.com/emqx/emqx/pull/13624) 修复了内置数据库授权检查器中的一个问题，即在更新客户端或用户的规则时，可能导致规则总数超出 `max_rules` 限制。
- [#13678](https://github.com/emqx/emqx/pull/13678) 使删除链中认证器的操作幂等化，确保即使删除的认证器不存在，该操作也能成功。

#### 数据集成

- [#13207](https://github.com/emqx/emqx/pull/13207) 优化了 `republish` 规则引擎动作，以更准确地反映消息发布的成功和失败情况。此前，即使 `republish` 动作未能将消息传递给所有订阅者，成功指标仍然会增加。现在，如果检测到消息未能到达任意一个订阅者，失败指标将正确增加。
- [#13425](https://github.com/emqx/emqx/pull/13425) 改进了 MQTT 连接器的错误日志信息，提供了更清晰和更详细的信息。
- [#13589](https://github.com/emqx/emqx/pull/13589) 修复了一个问题，该问题允许通过 HTTP API 创建 ID 为字符串 `"null"` 的规则，这可能导致配置不一致。
- [#13414](https://github.com/emqx/emqx/pull/13414) 改进了 RabbitMQ 连接器错误日志信息，提供了更清晰和更详细的信息。

#### 文件传输

- [#12514](https://github.com/emqx/emqx/pull/12514) 修复了文件传输命令结果报告到 `$file-response/${clientid}` 通道的问题。此前，如果通道发出 `assemble` 命令后在组装过程完成之前断开连接，状态消息将丢失且不会发送到响应主题。现在，组装状态由专用进程监控，即使原始通道断开连接，也能确保状态消息可靠传递。

#### 运维

- [#13078](https://github.com/emqx/emqx/pull/13078) 改进了 EMQX 管理 API 中的验证和错误处理，以确保 JSON 请求体的请求包含 `Content-Type: application/json` 头。如果预期为 JSON 输入的 API 缺少此头，服务器现在会正确返回 `415 Unsupported Media Type` 状态码，而不是 `400 Bad Request`。
- [#13225](https://github.com/emqx/emqx/pull/13225) 增强了认证和授权 API 的安全性，通过屏蔽密码等敏感数据。此前，API 可能在响应中返回原始密码值。此次更新后，敏感信息将被替换为 `******`，以防止意外泄露并保护用户凭据。

#### 网关

- [#13607](https://github.com/emqx/emqx/pull/13607) 修复了通过 API 显示的 CoAP 订阅的 QoS 等级与实际使用的 QoS 等级不匹配的问题。这种不一致可能导致混淆，因为成功的订阅未能在 Dashboard上准确反映。

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

### 修复

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
- [#12844](https://github.com/emqx/emqx/pull/12844) 修复了 CPU 使用率和空闲统计值未正确保留精度的问题。现在，这些值始终以两位小数存储。此更改影响了 Prometheus 统计指标和 OpenTelemetry 管理指标。

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

- [#12883](https://github.com/emqx/emqx/pull/12883) 新增了用于持久化存储管理的 REST API 端点和 CLI 命令。

  新的 REST 端点：

  - `/ds/sites`
  - `/ds/sites/:site`
  - `/ds/storages`
  - `/ds/storages/:ds`
  - `/ds/storages/:ds/replicas`
  - `/ds/storages/:ds/replicas/:site`

  新的 CLI 命令：

  - `ds set_replicas`
  - `ds join`
  - `ds leave`

### 修复

#### 安全

- [#12887](https://github.com/emqx/emqx/pull/12887) 修复了使用 SASL SCRAM 的 MQTT 增强型身份验证。
- [#12962](https://github.com/emqx/emqx/pull/12962) TLS 客户端现在可以使用通配符证书验证服务器主机名。例如，如果证书是为主机 `*.example.com` 颁发的，则 TLS 客户端可以验证类似 `srv1.example.com` 的服务器主机名。

#### MQTT

- [#12996](https://github.com/emqx/emqx/pull/12996) 修复了 `emqx_retainer` 应用程序中的进程泄漏问题。以前，当接收到保留消息时客户端断开连接，可能会导致进程泄漏。
- [#12855](https://github.com/emqx/emqx/pull/12855) 修复了客户端订阅/取消订阅共享主题时，客户端订阅/取消订阅通知的系统主题消息未正确序列化的问题。同时，修复了 `/topics` 端点中 `$queue` 共享主题的格式错误。
- [#12976](https://github.com/emqx/emqx/pull/12976) 修复了一个问题，在接管一个 socket 已经断开连接的会话时，错误地触发 `client.disconnected` 事件。

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
- [#12880](https://github.com/emqx/emqx/pull/12880) 修复了 InfluxDB 动作配置中的一个问题，当 Tags 值包含字面量整数或浮点数时，序列化失败。现在，标签集值已正确地作为字符串处理。有关标签集的更多详情，请参阅 [Line Protocol - Tag Set](https://docs.influxdata.com/influxdb/v2/reference/syntax/line-protocol/#tag-set)。

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

- [#12902](https://github.com/emqx/emqx/pull/12902) 将 MQTT 消息的 Content-type 传递到 Stomp 消息中。
- [#12892](https://github.com/emqx/emqx/pull/12892) 修复了 OCPP 网关处理下行 BootNotification 时的错误。同时修复了 `gateways/ocpp/listeners` 端点，确保返回当前连接的正确数量。
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

- [#12417](https://github.com/emqx/emqx/pull/12417) 支持通过配置文件指定 MQTT 消息过期时间。有关更多详情，请参阅 `mqtt.conf.example` 文件中 `message_expiry_interval` 配置的描述。

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
- [#12388](https://github.com/emqx/emqx/pull/12388) QUIC 监听器现在显示每个监听器的连接数量，而不是全局连接数量。
- [#12325](https://github.com/emqx/emqx/pull/12325) QUIC 监听器支持在不干扰现有连接的情况下重新加载监听器绑定。

- [#12274](https://github.com/emqx/emqx/pull/12274) 启用 QUIC MQTT 监听器的动态 TLS 配置更新，且不会中断现有连接。实现了一个故障保护机制，在更新失败时回退到之前的 TLS 配置。

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

### 增强

- [#12261](https://github.com/emqx/emqx/pull/12261) IoTDB 的桥接功能已被拆分，可以通过连接器和动作 API 使用。它们仍然与旧的桥接 API 向后兼容。

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

- [#12114](https://github.com/emqx/emqx/pull/12114) ClientInfo 中新增了 peerport 字段。ExHook 中的 ClientInfo 和 ConnInfo 消息新增了 peerport 字段。

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
- [#12141](https://github.com/emqx/emqx/pull/12141) 修复了 API 端点 `/v5/topics` 在主题过滤器无效时返回 `InternalError` 和 HTTP 状态 500 的问题。
- [#12059](https://github.com/emqx/emqx/pull/12059) 将 `multi-time-warp` 设置为默认 time warp 模式。详情请参见：[time_correction_#multi-time-warp-mode](https://www.erlang.org/doc/apps/erts/time_correction#multi-time-warp-mode)。

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
