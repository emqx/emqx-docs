# EMQX 企业版 v6 版本

## 6.0.1

*发布日期: 2025-10-31*

在升级到 EMQX 6.0.1 之前，请务必查阅不兼容变更和已知问题。

### 增强

#### 消息队列

- [#16080](https://github.com/emqx/emqx/pull/16080) 新增用于禁用消息队列功能的配置选项。禁用消息队列可以略微降低集群的资源使用。当持久会话也被禁用时，EMQX 将避免维护持久存储，从而进一步降低管理开销并提升性能。
- [#16096](https://github.com/emqx/emqx/pull/16096) 新增支持：当客户端订阅不存在的 `$q/` 主题时自动创建消息队列。现在可以通过配置项分别为常规队列和最后值语义队列启用自动创建功能。
- [#16097](https://github.com/emqx/emqx/pull/16097) 优化了写入常规消息队列的性能。通过将事务追加操作替换为非事务（dirty）追加函数。对于 QoS 0 消息，现在使用异步追加操作。这些更改显著提升了写入常规队列的消息插入性能。
- [#16098](https://github.com/emqx/emqx/pull/16098) 新增配置项，用于限制系统中消息队列的总数量。
- [#16152](https://github.com/emqx/emqx/pull/16152) 引入每个队列的最大消息数量和消息总大小限制。同时新增了用于监控消息追加延迟的指标，有助于诊断性能问题或队列限制相关问题。

#### 数据集成

- [#16121](https://github.com/emqx/emqx/pull/16121) 将 GreptimeDB ingester 客户端升级至 [v0.2.3](https://github.com/GreptimeTeam/greptimedb-ingester-erl/releases/tag/v0.2.3)。此版本修复了若干问题，并引入了对基于行的 gRPC 协议的支持（原来的基于列的协议已被弃用）。此外，还将 CI 镜像更新为最新稳定版本的 GreptimeDB。
- [#16127](https://github.com/emqx/emqx/pull/16127) 修复了在 [#16121](https://github.com/emqx/emqx/pull/16121) 引入更改后，GreptimeDB 连接器中出现的无效字符串值问题。

#### 性能

- [#15949](https://github.com/emqx/emqx/pull/15949) 将监听器配置中的 `parse_unit` 选项默认值从 `chunk` 修改为 `frame`。当负载大小超过 socket 缓冲区（默认 4 KB）时，此更改可以显著降低 CPU 使用率。

   **注意：** 当 `parse_unit = frame` 时，如果 `PUBLISH` 报文超过允许的最大大小，EMQX 将关闭连接，而不是发送 `DISCONNECT` 报文。

- [#16165](https://github.com/emqx/emqx/pull/16165) 优化了 `GET /clients_v2` API 的性能。此前，在集群中连接客户端数量达到约 50,000 或以上时，调用该 API 获取客户端列表的响应速度可能非常慢，甚至会超时。

### 修复

#### 核心 MQTT 功能

- [#15884](https://github.com/emqx/emqx/pull/15884) 修复了一个问题：在极少数情况下，全局路由表可能会无限期保留已长时间离开集群的节点的路由信息。

- [#15518](https://github.com/emqx/emqx/pull/15518) 修复了一个竞争条件，该问题在大量共享订阅者同时断开连接时，可能导致集群中路由表和共享订阅状态持续出现不一致。

- [#16137](https://github.com/emqx/emqx/pull/16137) 修复了一个问题：当订阅者触发保留消息的派发速率限制时，可能无法收到全部保留消息。

  此前，在遍历保留消息主题期间，如果达到派发速率限制，派发进程会安排稍后重试。然而，对于启动时间过早的遍历操作，重试可能会被丢弃。此类遍历操作的生存时间（TTL）由 `retainer.dispatch_retry_ttl` 配置项控制，默认值为 10 分钟。

  此外，每个派发进程最多同时处理 1000 个保留消息派发请求（该限制不可配置）。EMQX 会为每个 Erlang 调度器启动一个派发进程，通常对应每个 CPU core。如果请求数量超过该限制，派发进程将丢弃队列中较早的请求。

  修复后，EMQX 现在会在触发速率限制后继续在后续重试中派发保留消息，从而确保更完整的消息投递。

#### 升级

- [#16047](https://github.com/emqx/emqx/pull/16047) 新增支持从 EMQX 企业版长期维护版本 5.8.0 及以上版本滚动升级至 6.0。在升级过程中，旧版本的配置会自动迁移为 6.0 所支持的新格式。具体来说，已废弃的 `bridges` 配置根节点将转换为新的 `connectors`、`sources` 和 `actions` 配置结构。

  不过，对于 GCP PubSub Consumer 和 Kafka Consumer 的 Source，仍然需要进行手动修改。如果配置中仍包含已废弃的 `topic_mapping` 字段，该字段必须被移除。随后，针对原先 `topic_mapping` 中的每一项，需手动创建一个对应的 “Source + Rule” 配对。

#### 安全

- [#16156](https://github.com/emqx/emqx/pull/16156) 修复了一个问题：与 EMQX 5.10 相比，某些依赖缺失了默认配置，可能导致 RSA 签名验证失败。缺失的默认配置可能导致错误，例如出现以下日志消息：

  ```
  {sign_unsupported,[[{rsa_padding,rsa_pkcs1_padding}]]}, [{jose_jwa_unsupported,verify,5,[{file,"src/jwa/jose_jwa_unsupported.erl"},{line,55}]}
  ```

- [#16175](https://github.com/emqx/emqx/pull/16175) 修复了周期性 TLS 证书垃圾回收的问题。此前，垃圾回收的执行过程错误地删除了在托管命名空间配置中仍在使用的证书文件。

#### 访问控制

- [#16081](https://github.com/emqx/emqx/pull/16081) 修复了一个问题：使用扩展认证和内存会话的客户端可能因 `calling_self` 错误导致触发 `session_stepdown_request_exception` 异常并发生崩溃。

  <details> <summary>错误日志示例</summary>


  ```
  2025-09-24T07:13:08.973954+08:00 [error] clientid: someclientid, msg: session_stepdown_request_exception, peername: 127.0.0.1:41782, username: admin, error: exit, reason: calling_self, stacktrace: [{gen_server,call,3,[{file,"gen_server.erl"},{line,1222}]},{emqx_cm,request_stepdown,4,[{file,"emqx_cm.erl"},{line,427}]},{emqx_cm,do_takeover_begin,2,[{file,"emqx_cm.erl"},{line,398}]},{emqx_cm,takeover_session,2,[{file,"emqx_cm.erl"},{line,384}]},{emqx_cm,takeover_session_begin,2,[{file,"emqx_cm.erl"},{line,305}]},{emqx_session_mem,open,4,[{file,"emqx_session_mem.erl"},{line,210}]},{emqx_session,open,3,[{file,"emqx_session.erl"},{line,263}]},{emqx_cm,'-open_session/4-fun-1-',4,[{file,"emqx_cm.erl"},{line,290}]},{emqx_cm_locker,trans,2,[{file,"emqx_cm_locker.erl"},{line,32}]},{emqx_channel,post_process_connect,2,[{file,"emqx_channel.erl"},{line,575}]},{emqx_connection,with_channel,3,[{file,"emqx_connection.erl"},{line,852}]},{emqx_connection,process_msg,2,[{file,"emqx_connection.erl"},{line,470}]},{emqx_connection,process_msgs,2,[{file,"emqx_connection.erl"},{line,462}]},{emqx_connection,handle_recv,3,[{file,"emqx_connection.erl"},{line,406}]},{proc_lib,wake_up,3,[{file,"proc_lib.erl"},{line,340}]}], action: {takeover,'begin'}, ...
  ```

  </details>

#### 集群

- [#16123](https://github.com/emqx/emqx/pull/16123) 修复了管理 Mria 复制的组件中的一个问题，该问题可能导致在核心-副本（core-replicant）集群中集群加入过程卡住或未完成。

  在涉及新增核心节点的集群变更过程中，这些新加入的核心节点有时无法正常启动副本节点所依赖的复制相关进程。结果，升级后的副本节点或新加入的副本节点在启动时可能会出现卡顿。

  在 Kubernetes 部署中，该问题常导致就绪探针（readiness probe）失败，从而使控制器不断重启受影响的副本节点 Pod。

  此问题通常会影响包含新增核心节点和副本节点的升级部署。例如，在一个已有 2 个核心节点和 2 个副本节点的集群中，新增 2 个运行更新版本 EMQX 的核心节点和 2 个副本节点时可能会遇到该问题。

#### 规则引擎

- [#16028](https://github.com/emqx/emqx/pull/16028) 修复了规则引擎中 `jq` 函数的内存泄漏问题。 此前，如果使用内置的 `jq` 函数 `index`（例如 `.key | index("name")`），会导致内存泄漏。

#### 数据集成

- [#16010](https://github.com/emqx/emqx/pull/16010) 修复了一个问题：如果原始规则的 SQL 未包含规则环境中的 `metadata` 字段，规则的备选动作可能会因 `function_clause` 错误而执行失败。

  错误日志示例：

  ```
  [error] tag: RESOURCE, msg: failed_to_trigger_fallback_action, reason: {error,function_clause}, fallback_kind: republish, primary_action_resource_id: <<"action:type:name:connector:type:name">>, republish_topic: <<"republish/topic">>
  ```

- [#16046](https://github.com/emqx/emqx/pull/16046) 修复了一个潜在的内存溢出（OOM）崩溃问题：当加载或重启包含数百个动作的连接器配置时，可能导致崩溃。

- [#16140](https://github.com/emqx/emqx/pull/16140) 修复了一个 Redis 集群故障转移（failover）相关的问题，该问题可能导致连接器长时间停留在 “connecting” 状态。

  此前，EMQX 的 Redis 集群客户端仅在常规查询（如 `GET`）失败时才会刷新集群拓扑结构。然而，周期性发送的 `PING` 命令即使失败，也不会触发刷新操作。因此，在发生故障转移后，如果没有其他命令被发送，连接器可能会继续使用过时的拓扑信息，导致无法恢复连接。

  此次修复后，`PING` 命令失败也会触发集群拓扑刷新，确保连接器能够及时检测到故障转移并恢复正常工作。

#### MQTT 会话持久化

- [#16105](https://github.com/emqx/emqx/pull/16105) 此修复优化了持久存储性能，尤其是减少了使用持久会话的客户端的 `CONNACK` 延迟。
- [#16129](https://github.com/emqx/emqx/pull/16129) 持久存储事务配置现在可以在运行时更改。以前，修改此配置需要重启节点。

#### 可观测性

- [#15963](https://github.com/emqx/emqx/pull/15963) 减少了在远程 shell（`remsh`）中进行循环评估时产生的过多审计日志。

- [#15967](https://github.com/emqx/emqx/pull/15967) 修复了一个问题：在清理大量审计日志时，Mnesia 事务阻塞可能导致内存迅速增长。

- [#16060](https://github.com/emqx/emqx/pull/16060) 修复了一个日志格式化器崩溃的问题，该问题可能发生在某些包含深度嵌套的非 ASCII 字符的调试级别日志消息中。

  <details> <summary>错误日志示例</summary>


  ```
  2025-09-29T06:55:34.120640+00:00 debug: FORMATTER CRASH: {report,#{request => #{messages => [#{role => <<"user">>,content => <<"{\"msg\": \"hello\"}">>}],system => <<"将输入的 JSON 数据中，值为数字的 value 相加起来，并输出，只需返回输出结果。"/utf8>>,model => <<"claude-3-haiku-20240307">>,max_tokens => 100},msg => emqx_ai_completion_request}}
  2025-09-29T06:55:34.120780+00:00 [debug] formatter_crashed: emqx_logger_textfmt, config: #{time_offset => [],chars_limit => unlimited,depth => 100,single_line => true,template => ["[",level,"] ",msg,"\n"],with_mfa => false,timestamp_format => auto,payload_encode => text}, log_event: #{meta => #{line => 44,pid => <0.281254.0>,time => 1759128934120640,file => "emqx_ai_completion_anthropic.erl",gl => <0.4317.0>,mfa => {emqx_ai_completion_anthropic,call_completion,3},report_cb => fun logger:format_otp_report/1,matched => <<"t/1">>,namespace => global,clientid => <<"c_emqx">>,trigger => <<"t/1">>,rule_id => <<"r1sczoo0">>,rule_trigger_ts => [1759128934120]},msg => {report,#{request => #{messages => [#{role => <<"user">>,content => <<"{\"msg\": \"hello\"}">>}],system => <<"将输入的 JSON 数据中，值为数字的 value 相加起来，并输出，只需返回输出结果。"/utf8>>,model => <<"claude-3-haiku-20240307">>,max_tokens => 100},msg => emqx_ai_completion_request}},level => debug}, reason: {error,badarg,[{erlang,iolist_to_binary,[["[",[["messages",": ",[[91,[[35,123,[["role"," => ",[60,60,"\"user\"",62,62]],44,["content"," => ",[60,60,"\"{\\\"msg\\\": \\\"hello\\\"}\"",62,62]]],125]],93]]],", ",["system",": ","将输入的 JSON 数据中，值为数字的 value 相加起来，并输出，只需返回输出结果。"],", ",["model",": ","claude-3-haiku-20240307"],", ",["max_tokens",": ","100"]],"]"]],[{error_info,#{module => erl_erts_errors}}]},{emqx_trace_formatter,format_term,2,[{file,"emqx_trace_formatter.erl"},{line,126}]},{emqx_logger_textfmt,format_term,2,[{file,"emqx_logger_textfmt.erl"},{line,230}]},{emqx_logger_textfmt,try_encode_meta,4,[{file,"emqx_logger_textfmt.erl"},{line,206}]},{lists,foldl_1,3,[{file,"lists.erl"},{line,2151}]},{emqx_logger_textfmt,enrich_report,3,[{file,"emqx_logger_textfmt.erl"},{line,102}]},{emqx_logger_textfmt,format,2,[{file,"emqx_logger_textfmt.erl"},{line,24}]}]}
  ```

  </details>

- [#16134](https://github.com/emqx/emqx/pull/16134) 修复了一个向后兼容性问题，该问题在某些情况下可能导致无法创建新的日志追踪。

#### 速率限制

- [#16160](https://github.com/emqx/emqx/pull/16160) 改进了针对单个客户端连接的速率限制算法。此前，客户端在刚连接后或经过一段时间不活动后，可能会短暂地超出其发布速率限制。此次更新使限速行为更加可预测且一致，确保从连接建立开始就能正确执行速率限制。

## 6.0.0

*发布日期: 2025-09-30*

在升级到 EMQX 6.0.0 之前，请务必查阅不兼容变更和已知问题。

### 功能亮点

EMQX Enterprise 6.0.0 是 EMQX 企业版 6 系列的首个发布版本，带来了重大的架构改进和全新能力。

#### 消息队列

原生的消息队列功能结合了实时 MQTT 发布/订阅与持久化异步队列。服务端缓存匹配主题过滤器的消息，即使订阅端离线也能保留，客户端通过 `$q/{topic}` 主题消费，实现更可靠的消息投递。消息队列支持离线消息存储、最后值保留和灵活的分发策略，使 MQTT 同时具备实时性与持久化能力。

#### 命名空间

命名空间功能进一步提升了多租户支持和可观测性。

- **命名空间角色**：在 Dashboard 中引入命名空间级别的角色控制，限制用户仅能访问本命名空间内的资源（如规则、动作和连接器），实现安全隔离。管理员可为不同命名空间分配更细颗粒度的权限（如管理员或查看者），通过 Dashboard、API 或 CLI 添加用户时，可直接创建和分配命名空间角色，简化了多租户场景下的运维管理。
- **会话数刷新优化**：改进了会话数刷新机制，连接数少于 1000 时按需更新，超过时每 5 秒更新一次。在从旧版本滚动升级时，会话数可能暂时不一致，升级完成后将恢复准确。

#### MQTT 会话持久化

通过将会话数据与 Broker 的其他元数据分离，优化了持久存储，显著降低了内存占用并提升了存储效率。

新增配置选项可对 RocksDB 的内存使用和性能进行更精细的控制。此外，存储消息的默认序列化方案已更新为 ASN.1，进一步提升了效率。

#### 新增数据集成

- Google BigQuery
- AWS AlloyDB
- CockroachDB
- AWS Redshift

#### 增强的数据集成

- **AWS**：
  - 在使用 S3 或 S3Tables 数据集成时，支持来自 EC2 实例的 Instance Metadata Service v2 API。这使得 EMQX 能够在无需手动配置 AWS 凭证的情况下无缝访问 S3 存储桶，并利用 IAM 角色提升安全性。
  - S3 Tables Action 新增 Parquet 格式支持。
- **RabbitMQ**：在 RabbitMQ Sink 中支持自定义 Headers 和 Properties 模板，以增强消息路由能力和与 RabbitMQ 的兼容性。
- **Snowflake**： Snowflake Action 新增 Snowpipe Streaming 上传模式（预览功能）。
- **RocketMQ**：在动作中新增了 `key` 和 `tag` 模板字段，并在消息 Produce Strategy 中增加了 `key_dispatch` 选项，使消息元数据的自定义更加灵活。

#### Elixir 支持

所有安装包现在均基于 Mix 构建系统 提供 Elixir 支持，为 Elixir 社区开放 EMQX，并通过 IEx 控制台提供更强大的工具链。

#### 增强的 LDAP 支持

LDAP 授权现在支持基于 JSON 格式的扩展 ACL 规则；LDAP 认证也可直接从 LDAP 获取 ACL 规则，并支持客户端缓存。

#### 改进的追踪功能

新增可配置的追踪数量上限（`trace.max_traces`）和追踪文件大小上限（`trace.max_file_size`）。
当达到 max_file_size 时，跟踪日志将轮转到新文件，而不是停止。

#### 集群管理

新增 `cluster.description` 配置项，允许用户在 EMQX Dashboard 中设置和显示自定义集群描述。

### 增强

#### 消息队列

- [#15789](https://github.com/emqx/emqx/pull/15789) 实现了消息队列，这是由 `topic_filter` 标识的消息集合。每个队列都有明确的生命周期，并在其生命周期内自动补充与队列主题过滤器匹配的已发布消息。客户端可以通过订阅特殊格式的主题 `$q/{topic}` 来协同消费队列中的消息。

#### 核心 MQTT 功能

- [#15805](https://github.com/emqx/emqx/pull/15805) 引入了一个专用的工作线程池，用于处理分片广播式消息分发。之前，broker 线程池同时处理订阅管理和消息分发，可能导致调度争用。此更改将广播式分发的工作负载单独划分到一个独立线程池中，以确保更均衡和高效地处理发布/订阅操作。

#### 访问控制

- [#15349](https://github.com/emqx/emqx/pull/15349) 优化了认证和授权的外部资源管理。此前，EMQX 在禁用认证或授权源的情况下，仍可能与配置的资源保持连接。
- [#15294](https://github.com/emqx/emqx/pull/15294) 增强了 LDAP 认证和授权功能。LDAP 授权现在支持使用 JSON 格式的扩展 ACL 规则。LDAP 认证现在可以从 LDAP 获取 ACL 规则。这些规则缓存在客户端的元数据中，因此无需额外的 LDAP 查询即可执行授权。
- [#15730](https://github.com/emqx/emqx/pull/15730) 新增支持根据认证结果覆盖客户端 ID。如果认证后端在成功认证后返回 `clientid_override` 属性，它将替换客户端原有的客户端 ID。
  以下认证后端现在支持 `clientid_override`：
  - HTTP
  - JWT
  - LDAP
  - MongoDB
  - MySQL
  - Postgres
  - Redis
- [#15820](https://github.com/emqx/emqx/pull/15820) 出于更安全的默认配置考量，将配置 `authorization.no_match` 的默认值从允许（`allow`） 更改为拒绝 （`deny`）。

#### 集群

- [#15600](https://github.com/emqx/emqx/pull/15600) 引入了一个新的配置选项 `cluster.description`，允许您为 EMQX 集群添加描述性标签。此描述可以通过 `PUT /cluster` 更新，并通过 `GET /cluster` API 检索。

#### 基于 LLM 的 MQTT 数据处理

- [#15467](https://github.com/emqx/emqx/pull/15467) AI 补全服务提供器现已支持传输层配置选项。用户可配置连接超时和最大连接数，从而在消息吞吐量较高、提供器负载较大时，减少 `checkout_timeout` 错误的发生。
- Flow 设计器支持与 Google Gemini 模型集成。
- [#15631](https://github.com/emqx/emqx/pull/15631) 添加了一个新的 API 端点，用于列出 AI 提供器可用的所有模型。
- [#15467](https://github.com/emqx/emqx/pull/15467) 为 AI 补全服务提供器开放了传输选项。这些选项允许配置连接超时和到 AI 补全服务提供器的最大连接数。
- [#15724](https://github.com/emqx/emqx/pull/15724) 为 AI 补全服务提供器和补全配置文件引入了 `openai_response` 类型，以使用 OpenAI 的 `response` API。

#### 数据集成

- [#15418](https://github.com/emqx/emqx/pull/15418) EMQX 新增与 BigQuery 的数据集成。
- [#15401](https://github.com/emqx/emqx/pull/15401) 在 Snowflake 动作中添加了对 Snowpipe Streaming 上传模式的支持。
  *注意：Snowpipe Streaming 目前是*[*预览功能*](https://docs.snowflake.com/en/release-notes/preview-features)*，仅适用于托管在 AWS 上的 Snowflake 帐户。*
- [#15387](https://github.com/emqx/emqx/pull/15387) 为 Kinesis 生产者连接器和动作的健康检查增加了限速机制，以遵守 AWS API 限额并提升集群行为一致性：
  - 对 `ListStreams` 和 `DescribeStream` 接口的调用分别限制为每个连接器每秒 5 次和 10 次。
  - 集群中的核心节点协调分布式限速器，以确保限速一致。
  - 若健康检查被限速或超时，连接器或动作将保留原状态，而不是被标记为已断开。
  - 新增配置项 `resource_opts.health_check_interval_jitter`，在健康检查间隔基础上引入一个均匀随机延迟，减少同一连接器下多个动作同时发起健康检查的可能性。
- [#15176](https://github.com/emqx/emqx/pull/15176) 升级了 GreptimeDB 连接器客户端，并支持一个可选的新参数 `ttl`，用于为自动创建的表设置默认的生存时间。
- [#15649](https://github.com/emqx/emqx/pull/15649) EMQX 新增与 AWS AlloyDB、CockroachDB 和 AWS Redshift 的数据集成。
- [#15635](https://github.com/emqx/emqx/pull/15635) 在 RocketMQ 动作中添加了新的 `key` 和 `tag` 模板字段，允许自定义消息的键和标签。此外，还为 `Produce Strategy` 字段引入了一个新的 `key_dispatch` 选项。
- [#15621](https://github.com/emqx/emqx/pull/15621) 现在，`access_key_id` 和 `secret_access_key` 是 S3 Tables 连接器的可选字段。如果省略，它们将从部署 EMQX 的 EC2 实例的实例元数据服务 v2 API 中获取。
- [#15628](https://github.com/emqx/emqx/pull/15628) 移除了 HStreamDB 数据集成。
- [#15544](https://github.com/emqx/emqx/pull/15544) 为 Datalayers 集成添加了 Arrow Flight SQL NIF 驱动支持。
- [#15637](https://github.com/emqx/emqx/pull/15637) 为 RabbitMQ 动作添加了消息头和属性的模板支持。
- [#15864](https://github.com/emqx/emqx/pull/15864) 移除了已弃用的“Bridges V1” API 和配置模式。`/bridges/*` 下的所有端点和 `bridges` 根键下的配置条目已不再可用，因为数据集成已完全迁移到“连接器/动作/Source”模型。
- [#15583](https://github.com/emqx/emqx/pull/15583) 将 Kafka `brod` 客户端升级至 4.4.4，扩展了对更多 Kafka API 的支持，并解决了 `JoinGroups` API 版本 `v0` 和 `v1` 弃用的问题。

#### 智能数据中心

- [#15525](https://github.com/emqx/emqx/pull/15525) 防止删除仍在使用的内部 schema。如果一个 schema 被 schema 验证或消息转换引用，它将不能再被移除，以避免运行时错误和配置不一致。

#### 持久存储

- [#15463](https://github.com/emqx/emqx/pull/15463) 改进了持久存储的 RAM 使用和存储效率。
  - 为持久存储引入了以下配置参数，以改进对 RocksDB 内存使用和存储性能的控制：
    - `durable_storage.messages.rocksdb.write_buffer_size`：每个分片的 RocksDB memtable 大小。
    - `durable_storage.messages.rocksdb.cache_size`：每个分片的 RocksDB 块大小。
    - `durable_storage.messages.rocksdb.max_open_files`：限制每个分片 RocksDB 使用的文件描述符数量。
    - `durable_storage.messages.layout.wildcard_thresholds`：允许为 `wildcard_optimized_v2` 存储布局调整通配符阈值。
  - 此外，存储消息的默认 `serialization_schema` 已更改为 `asn1`。

- [#16044](https://github.com/emqx/emqx/pull/16044) 持久会话的部分配置字段已被移除或重命名，旧值标记为已弃用：

    - `durable_sessions.heartbeat_interval` 已重命名为 `durable_sessions.checkpoint_interval`。
    - `durable_sessions.idle_poll_interval` 和 `durable_sessions.renew_streams_interval` 已被移除，因为会话现在完全基于事件驱动。
    - `durable_sessions.session_gc_interval` 和 `durable_sessions.session_gc_batch_size` 已作为过时配置被移除。

#### CLI

- [#15399](https://github.com/emqx/emqx/pull/15399) `node_dump` 工具现在导出当前系统配置为 HOCON 格式，并自动对敏感信息（如密码和密钥）进行脱敏处理，以确保安全。

#### 命名空间

- [#15841](https://github.com/emqx/emqx/pull/15841) 优化了命名空间会话数的刷新频率。
  
  - 当某个命名空间的连接数少于 1000 时，会话数将按需刷新；
  - 当连接数大于或等于 1000 时，会话数每 5 秒刷新一次。
  
  在从 6.0 之前版本进行滚动升级期间，由于内部跟踪表结构的变更，命名空间的会话数可能会出现不一致的情况。这属于预期行为：随着客户端逐步重新连接到已升级的节点，会话数将逐步趋于稳定，并在所有节点升级至 6.0 版本后恢复准确。

#### 可观测性

- [#15594](https://github.com/emqx/emqx/pull/15594) 引入了一个新的配置选项 `trace.max_traces`，用于控制集群范围内活动追踪的最大数量。此限制不适用于使用 `emqx ctl trace` 管理的节点本地 Trace。
  同时优化了 Trace 实现，消除了每个 Trace 可能导致的 atom 泄漏问题。
  
- [#15556](https://github.com/emqx/emqx/pull/15556) 引入了一个新的配置选项 `trace.max_file_size`，用于限制单个 Trace 的最大文件大小。

- [#15650](https://github.com/emqx/emqx/pull/15650) 实现了追踪日志自动轮转功能。
  
  当单个追踪日志文件大小超过 `trace.max_file_size` 限制时，EMQX 不再丢弃所有后续事件并向 `stderr` 输出难以理解的警告信息。取而代之的是，会优先丢弃最旧的部分事件，以保留最新的追踪数据。
  
  受此变更影响：
  
  - EMQX 现在为每个活动的追踪任务维护多个日志文件，追踪目录的结构也已相应调整。
  - Trace API 已更新以支持此行为，Log Stream API 也可能返回新的错误，例如在消费者处理过慢时，日志流变为过期状态。
  
- [#15904](https://github.com/emqx/emqx/pull/15904) 支持通过 Trace API 查看和更新追踪配置。

#### 性能

- [#15451](https://github.com/emqx/emqx/pull/15451) 为 TCP 监听器引入了一个实验性的 `socket` 后端，旨在提高消息处理延迟并减少计算资源使用。该功能可以通过新的 `tcp_backend` 监听器选项启用。

#### 构建和工具

- [#15484](https://github.com/emqx/emqx/pull/15484) 将构建系统切换到 [Elixir](https://elixir-lang.org/) 的 [Mix](https://hexdocs.pm/elixir/introduction-to-mix.html)，使所有软件包都包含原生 Elixir 支持。这一变化改进了开发人员工具，允许在需要时与 Elixir 依赖项集成，并能够使用 [IEx](https://hexdocs.pm/iex/IEx.html) shell 作为更强大的 EMQX 控制台。

#### License

- [#15921](https://github.com/emqx/emqx/pull/15921) 引入了 License 告警，用于监控集群范围内的最大 TPS（Transactions Per Second，每秒事务数）。
  - 每个节点的 TPS 计算方式为过去 10 秒内接收和发送的 MQTT 消息数的平均值。
  - 集群总 TPS 每 5 秒聚合一次。
  - 如果观测到的 TPS 超过了 License 限制，将触发告警。
  - 告警会一直保持，直到应用了具有更高 TPS 配额的 License 为止。

#### MQTT over QUIC

- [#15997](https://github.com/emqx/emqx/pull/15997) 添加了通过设置环境变量 `QUICER_SKIP_NIF_LOAD=1` 来禁用 QUIC 协议栈加载的支持。

### 修复

#### 核心 MQTT 功能

- [#15396](https://github.com/emqx/emqx/pull/15396) 移除了已断开客户端的共享订阅中冗余的清理操作。这些操作在高并发断开情况下容易导致崩溃，并可能引发全局路由状态不一致。
- [#15361](https://github.com/emqx/emqx/pull/15361) 修复了在解析格式错误的 `User-Property` 键值对时产生的 `function_clause` 错误，特别是当键值对的长度无效（过短）时。
- [#15783](https://github.com/emqx/emqx/pull/15783) 确保连接速率限制的配置修改在监听器更新完成后立即生效。
  之前部分内部限流器状态未能及时应用新配置，例如在提升突发速率 (`max_conn_burst`) 后，实际生效的限流可能比预期更严格。

#### 访问控制

- [#15489](https://github.com/emqx/emqx/pull/15489) 修复了单点登录（SSO）设置中的 OIDC issuer URL 验证。此前，带有端口号的 issuer URL（如
  `https://xxxxxxxx:8443/webman/sso/.well-known/openid-configuration`）会被错误地拒绝并报 `bad_port_number`。
- 现在支持这些 URL。

#### 规则引擎

- [#15569](https://github.com/emqx/emqx/pull/15569) 修复了当 `direct_dispatch` 模板为空或解析为非布尔值时，消息重发布规则动作可能失败的问题。在这些情况下，现在将使用默认值 `false`。

#### 数据集成

- [#15522](https://github.com/emqx/emqx/pull/15522) 修复了 Snowflake 连接器在未提供用户名时无法正常启动的问题。
- [#15476](https://github.com/emqx/emqx/pull/15476) 修复了 `emqx_connector_aggreg_delivery` 中遗漏的回调函数，导致在格式化聚合模式动作（如 Azure Blob Storage、Snowflake、S3 Tables）传输状态时发生崩溃。
   此问题发生在传输失败或调用 `gen_server:format_status/1` 检查传输状态时。现已修复，并增加了更详细的日志信息以便排查。
- [#15394](https://github.com/emqx/emqx/pull/15394) 修复了一个罕见的竞态条件，某些情况下由于收到意外的异步响应，导致动作指标统计出现不一致的问题。
- [#15647](https://github.com/emqx/emqx/pull/15647) 修复了 MongoDB 连接器被错误标记为 `Disconnected` 的问题。此前，如果配置的 MongoDB 账号缺少对某个集合执行 `find` 查询的权限，就会触发该问题。
- [#15603](https://github.com/emqx/emqx/pull/15603) 修复了 MQTT 桥接中的一个问题：过期的连接可能仍显示为 `Connected` 状态，且不会自动重连。
- [#15383](https://github.com/emqx/emqx/pull/15383) 修复了 MQTT 桥接中可能存在的资源泄漏问题。当桥接启动失败时，主题索引表未被正确清理。
- [#15786](https://github.com/emqx/emqx/pull/15786) 修复了探测 RocketMQ 连接器时可能存在的 atom 泄漏。
- [#15806](https://github.com/emqx/emqx/pull/15806) 改进了 Oracle 动作创建时的验证。以前，在极少数情况下，包含无效 SQL 语句的动作可能会被成功添加。
- [#15848](https://github.com/emqx/emqx/pull/15848) 改进了 Oracle 连接器的错误报告。当连接器断开连接时，其状态现在包含更具体的原因，使诊断更容易。
- [#15693](https://github.com/emqx/emqx/pull/15693) 修复了基于 Postgres 的桥接中的资源泄漏问题。在连接池初始化过程中，如果出现特定的竞争条件，删除连接器后，其连接池可能仍然残留。此问题已修复，确保连接池能够被正确清理。
- [#15543](https://github.com/emqx/emqx/pull/15543) 修复了 HTTP 服务数据集成在发送大消息 payload 时的问题。当 payload 大小达到 10 MB 或以上时，HTTP 请求可能会失败。

#### 数据智能中心

- [#15839](https://github.com/emqx/emqx/pull/15839) 修复了使用 `map<_, _>` 字段的 Protobuf schema 的编码问题。
  此前，包含 `map<string, string>` 字段的 schema 可能无法编码有效的 payload，导致隐晦的运行时错误。
  示例模式：
  
  ```protobuf
  syntax = "proto3";
  
  message test {
  map<string, string> args = 1;
  }
  ```
  示例规则：
  ```sql
  SELECT
  schema_encode('xxx', json_decode(payload), 'test') as protobuf_test
  FROM
  "t/#"
  ```
  无法编码的示例 payload：
  ```json
  {
  "args": {
  "env": "stag"
  }
  }
  ```
  此前的错误类似于：
  ```
  2025-06-17T06:59:22.725785+00:00 [warning] tag: RULE_SQL_EXEC, clientid: c_emqx, msg: SELECT_clause_exception, reason: {error,{gpb_type_error,{bad_unicode_string,[{value,env},{path,"test.args.key"}]}},[{'$schema_parser_xxx',mk_type_error,3,[{file,"$schema_parser_xxx.erl"},{line,437}]},{'$schema_parser_xxx','-v_map<string,string>/3-lc$^0/1-0-',3,[{file,"$schema_parser_xxx.erl"},{line,429}]},{'$schema_parser_xxx','v_map<string,string>',3,[{file,"$schema_parser_xxx.erl"},{line,429}]},{'$schema_parser_xxx',v_msg_test,3,[{file,"$schema_parser_xxx.erl"},{line,404}]},{'$schema_parser_xxx',encode_msg,3,[{file,"$schema_parser_xxx.erl"},{line,73}]},{emqx_schema_registry_serde,with_serde,2,[{file,"emqx_schema_registry_serde.erl"},{line,212}]}...
  ```

#### 可观测性

- [#15931](https://github.com/emqx/emqx/pull/15931) 修复了与 EMQX 告警系统相关的问题：在节点启动期间可能出现虚假但无害的错误日志的错误，例如：
  
  ```
  [error] Generic event handler emqx_alarm_handler crashed ...
  Reason: {aborted,{no_exists,[emqx_activated_alarm,runq_overload]}}
  ```

- [#15973](https://github.com/emqx/emqx/pull/15973) 修复了一个在某些条件下告警激活超时可能导致连接进程崩溃的错误。

#### MQTT over QUIC

- [#15614](https://github.com/emqx/emqx/pull/15614) QUIC 监听器：当启用 TLS 密钥日志记录（`SSLKEYLOGFILE`）时，即使握手失败，EMQX 现在也会转储 TLS 密钥。

#### 集群

- [#16021](https://github.com/emqx/emqx/pull/16021) 修复了 DS Raft 后端在某些情况下无法正常工作的问题。当已有节点加入新集群并随后成为 DS 副本集成员时，可能会触发该问题。

#### 集群连接

- [#15894](https://github.com/emqx/emqx/pull/15894) 以前，通过 `GET /cluster/links` 列出所有集群连接时，禁用的连接会以 `inconsistent` 状态返回。现在它们将以 `disconnected` 状态返回。

#### 性能

- [#15696](https://github.com/emqx/emqx/pull/15696) 为 WebSocket (WS) 和 WebSocket Secure (WSS) 监听器添加了连接速率限制支持。
  现在强制执行 `max_conn_rate` 和 `max_conn_burst` 配置选项：超过定义速率的传入连接在接受后会立即关闭，与现有的 TCP 监听器行为一致。
  此外，`max_connections` 的行为也已更新。当超过连接限制时，WS/WSS 监听器现在会在任何 HTTP 握手之前立即关闭连接，导致socket 突然关闭，而不是返回 HTTP 429 响应。
- [#15854](https://github.com/emqx/emqx/pull/15854) 将默认的 `active_n` 值从 `100` 减少到 `10`，以提高 MQTT 客户端的响应能力，特别是在高消息速率和消息 payload 较小的情况下。
  较低的 `active_n` 会在 TCP 层引入更强的背压机制，比默认的 `Receive-Maximum` of `32` 更严格，这在以下情况下有所帮助：
  - 客户端进程被外部授权检查阻塞
  - 数据集成操作延迟了消息处理
  - 系统负载过重或接近资源限制
- [#15981](https://github.com/emqx/emqx/pull/15981) 防止了因 Mnesia 事务阻塞在清理大量审计日志时导致的内存过度增长。这提高了在繁重的审计日志维护操作期间的系统稳定性和内存效率。
