# EMQX 企业版 v6 版本

## 6.2.0

*发布日期: 2026-03-31*

在升级到 EMQX 6.2.0 之前，请务必查阅不兼容变更和已知问题。

### 增强

#### AI 互操作性

- [#16840](https://github.com/emqx/emqx/pull/16840) 实现 Agent-to-Agent（A2A）Card Registry。该功能允许自主 AI 智能体通过标准化、事件驱动的 MQTT 5.0 机制进行发现与协作。

- [#16958](https://github.com/emqx/emqx/pull/16958) 新增 `/api-spec.md` 端点和 `/api-spec.html`，用于支持对 EMQX HTTP API 的分层发现，尤其适合 AI 智能体和其他工具按需获取特定 API 分片，而无需拉取完整的 API 规范文件。

#### 核心 MQTT 功能

- [#16612](https://github.com/emqx/emqx/pull/16612) 引入 `emqx_setopts` 应用，支持通过 `$SETOPTS` 主题对服务端选项进行动态更新，包括 keepalive 控制，以及对未知 `$SETOPTS/*` 发布的告警与抑制机制。

- [#16887](https://github.com/emqx/emqx/pull/16887) 新增可选的订阅消息过滤功能，由 `mqtt.subscription_message_filter` 配置项控制。

  启用后，客户端可以使用 `?` 后缀进行订阅，例如 `sensor/+/temperature?location=roomA&value>25`，EMQX 将仅投递 MQTT 5.0 User Properties 满足该过滤表达式的消息。禁用时，`?` 将作为主题过滤器文本的一部分，不执行额外过滤。

  因订阅过滤不匹配而被丢弃的消息将通过现有的 `delivery.dropped` 事件上报，原因为 `subscription_filter`，并由新增的 `delivery.dropped.filter` 指标进行统计。

- [#16929](https://github.com/emqx/emqx/pull/16929) 引入两种新的限速器类型：`delivery_messages` 和 `delivery_bytes`。与现有的 `messages` 和 `bytes` 限速器（限制单个客户端的发布速率）不同，新限速器用于限制单个客户端从任意来源接收消息的速率。当达到限制时，QoS 0 消息将被丢弃，QoS 1/2 消息将在内部排队并按配置的重试间隔调度重试。

  新限速器仅支持内存会话（`durable_sessions.enable = false`）。默认值为不限速，保持向后兼容。

- [#16779](https://github.com/emqx/emqx/pull/16779) 改进了对格式错误的首个报文的处理，将其归类为无效 CONNECT 报文，并在日志中提供更详细的协议提示信息。

#### 数据集成

- [#16589](https://github.com/emqx/emqx/pull/16589) 将规则引擎使用的 `jq` 库升级至 1.8.1 版本。

  注意：jq 1.8.1 相较于 1.6.1 版本引入了若干细微的不兼容变更：

  - 空字符串作为 jq 程序现在会报错，请改用 `"."`。（[jq#2790](https://github.com/jqlang/jq/pull/2790)）
  - 字符串函数 `indices/1`、`index/1` 和 `rindex/1` 现在使用码点（code point）索引而非字节索引；如需字节索引，请使用 `utf8bytelength/0`。（[jq#3065](https://github.com/jqlang/jq/pull/3065)）
  - `tonumber/0` 拒绝处理包含前导或尾部空白的数字字符串，请在调用前先使用 `trim/0`。（[jq#3055](https://github.com/jqlang/jq/pull/3055)、[jq#3195](https://github.com/jqlang/jq/pull/3195)）
  - `last(empty)` 现在不产生任何输出，与 `first(empty)` 行为保持一致。（[jq#3179](https://github.com/jqlang/jq/pull/3179)）
  - `limit/2` 在计数为负数时会报错，而不再静默接受。（[jq#3181](https://github.com/jqlang/jq/pull/3181)）
  - 现已支持 Tcl 风格的多行注释，可能对现有代码的解析产生细微影响。（[jq#2989](https://github.com/jqlang/jq/pull/2989)）
  - 十进制数字现在转换为 binary64（double）而非 decimal64。（[jq#2949](https://github.com/jqlang/jq/pull/2949)）
  - `nth/2` 在索引超出范围时返回空值而非报错。（[jq#2674](https://github.com/jqlang/jq/pull/2674)）
  - 字符串与 0 或小于 1 的数相乘，现在返回空字符串而非原字符串。（[jq#2142](https://github.com/jqlang/jq/pull/2142)）

- [#16634](https://github.com/emqx/emqx/pull/16634) 外部 HTTP Schema 校验现已支持 GET 请求。Schema 注册表条目可以指定 HTTP 方法，默认仍为 POST。

- [#16647](https://github.com/emqx/emqx/pull/16647) 在 GreptimeDB 和 EMQX Tables 动作中，不带 `i` 或 `u` 后缀的整数值现在会在发送至数据库前自动转换为 `float64` 类型。

  在 InfluxDB Write Syntax 中，浮点数是默认的数值类型，整数需要显式标注。此前，EMQX 遇到未标注的整数时，会将其解读为单字符字符串，导致目标列类型为 float 时写入失败。

- [#16707](https://github.com/emqx/emqx/pull/16707) 新增 Azure Event Grid 数据集成，支持从 Azure Event Grid 消费消息及向其发布消息。

- [#16750](https://github.com/emqx/emqx/pull/16750) GCP 连接器（GCP PubSub 生产者/消费者、BigQuery）现已通过 Service Account 模拟方式支持工作负载身份联合（Workload Identity Federation，WIF）认证。当前仅支持使用 Client Credentials 授权类型的 OIDC 工作负载身份池提供程序。

- [#16773](https://github.com/emqx/emqx/pull/16773) 使用 MQTT 连接器并启用 SSL 时，如果未设置服务器名称指示（SNI），现在将自动使用服务器主机名填充该字段。

- [#16893](https://github.com/emqx/emqx/pull/16893) 新增用于向 QuasarDB 追加写入数据的连接器和动作。

- [#16962](https://github.com/emqx/emqx/pull/16962) 改进了 Kafka Source 的轮询行为。当没有可用记录时，Fetch 请求现在会短暂等待数据到来，而不是立即返回空批次。这减少了不必要的轮询延迟，并有助于 Kafka 消费者更及时地接收新记录。

#### 访问控制

- [#16597](https://github.com/emqx/emqx/pull/16597) 改进了 MySQL 和 PostgreSQL 认证与授权中 SQL 模板对非法变量和带引号变量的处理方式。

- [#16616](https://github.com/emqx/emqx/pull/16616) 为 SSO OIDC 后端新增配置项，支持通过 `jq` 表达式在创建新 Dashboard 用户时提取所需的角色和命名空间。

- [#16759](https://github.com/emqx/emqx/pull/16759) 在 Variform 表达式中新增 `timestamp_s` 和 `timestamp_ms` 函数，分别用于获取当前系统时间（秒和毫秒），例如可在客户端连接阶段用于填充自定义客户端属性。

- [#16817](https://github.com/emqx/emqx/pull/16817) 新增重置认证和授权指标计数器的 REST API 端点：
  - `POST /authentication/:id/metrics/reset`：重置指定认证器的计数器。
  - `POST /authorization/sources/:type/metrics/reset`：重置指定授权源的计数器。

- [#16849](https://github.com/emqx/emqx/pull/16849) 为插件 API 端点新增基于 Cookie 的认证回退机制。Dashboard 提供的插件 UI iframe 现在可以在未携带 `Authorization` 请求头时，通过 `emqx_auth` Cookie 进行认证。此机制仅适用于 `/api/v5/plugin_api/...` 路径。

#### 管理

- [#16958](https://github.com/emqx/emqx/pull/16958) 新增 `emqx ctl api_keys` 命令，支持通过命令行对 API Key 进行列出、查看、添加、删除、启用和禁用操作。

#### 网关

- [#16734](https://github.com/emqx/emqx/pull/16734) 为 NATS 网关新增 `token`、`nkey` 和 `jwt` 内置认证方式，按顺序依次尝试，以缩小与 NATS Server 之间的认证功能差距。

#### 部署与安全

- [#16653](https://github.com/emqx/emqx/pull/16653) 支持通过 `node.dist_bind_address` 配置 Erlang 分布式监听器的绑定地址。

  例如：`node.dist_bind_address = "10.0.1.5"`。此前需要在 `vm.args` 中通过 `-kernel inet_dist_use_interface {10,0,1,5}` 进行配置。

- [#16888](https://github.com/emqx/emqx/pull/16888) 更新了 EMQX 安装包中用于本地开发和测试的默认 TLS 证书。新证书仅签发给 `localhost` 及回环地址（`localhost`、`127.0.0.1`、`::1`）。这些默认证书仅用于测试和本地部署场景，不得在生产环境中使用。

- [#16916](https://github.com/emqx/emqx/pull/16916) `emqx_cert_expiry_at` Prometheus 指标现在会同时考虑 MQTT 监听器中托管证书包所含证书的到期日期。

#### 性能

- [#16500](https://github.com/emqx/emqx/pull/16500) 优化了空闲内存占用，降低了维护基于速率的指标的开销。注意：5 分钟平均速率指标现在采用 EWMA（指数加权移动平均）计算，而非精确的滚动平均值。

- [#16547](https://github.com/emqx/emqx/pull/16547) 默认禁用 TLS 1.2 会话复用，以降低 TLS 握手开销。TLS 1.2 会话缓存上限为 1000 条且仅限单节点本地使用，在大规模集群中会话复用率极低。

- [#16794](https://github.com/emqx/emqx/pull/16794) 默认启用节点级认证与授权缓存。这减少了对后端的重复查询，在常见部署场景下提升了认证与授权性能。

- [#16829](https://github.com/emqx/emqx/pull/16829) 优化了 NATS 网关的发布热路径性能，降低了帧解析、主题处理、指标更新及 ACK/消息构建等环节的单消息开销。

- [#16911](https://github.com/emqx/emqx/pull/16911) 通过避免重复查询 Mria 统计信息，降低了 Prometheus 指标采集的开销。

- [#16550](https://github.com/emqx/emqx/pull/16550) 停止对订阅 ACL 检查结果进行缓存。MQTT 订阅通常在连接生命周期内只执行一次，缓存订阅 ACL 检查结果收益极低，反而浪费内存。

### 修复

#### 核心 MQTT 功能

- [#16721](https://github.com/emqx/emqx/pull/16721) 修复了 `await_rel_timeout` 超时后 QoS 2 重复消息处理不正确的问题。此前，当 Broker 的 PUBREL 等待状态过期（默认 300 秒）后，如果客户端以 `DUP=1` 重发 QoS 2 PUBLISH 报文，消息可能再次被投递给订阅者。现在，EMQX 会将此重传视为重复握手报文，返回 `PUBREC` 但不再重新投递应用消息。
- [#16725](https://github.com/emqx/emqx/pull/16725) 在默认 zone/全局配置中将 `conn_congestion.enable_alarm` 设置为 `false`，默认禁用 TCP 连接拥塞告警。
- [#16781](https://github.com/emqx/emqx/pull/16781) 修复了保留消息不可用时的 CONNECT 报文校验问题。当 `mqtt.retain_available` 设置为 `false` 时，携带 Will Retain 标志的 CONNECT 报文现在会被正确拒绝，并返回 CONNACK 原因码 `Retain not supported (0x9A)`。
- [#16783](https://github.com/emqx/emqx/pull/16783) 修复了 MQTT v5 SUBSCRIBE 报文对 `Subscription-Identifier` 上限的校验问题。现在可以正确接受 `268435455`（`0x0FFFFFFF`），即 MQTT 规范定义的最大有效 Subscription Identifier 值。
- [#16974](https://github.com/emqx/emqx/pull/16974) 恢复了恢复或接管会话时的原有保留消息行为。在 EMQX 6.1.1 中，如果会话已订阅含有保留消息的主题过滤器，在未重新订阅的情况下恢复或接管该会话时，会再次收到这些保留消息。现在，除非会话显式重新订阅该主题过滤器，否则保留消息的迭代投递将停止。
- [#16876](https://github.com/emqx/emqx/pull/16876) 将日志消息 `msg_publish_not_allowed` 重命名为 `msg_not_routed_to_subscribers`。

#### 数据集成

- [#16803](https://github.com/emqx/emqx/pull/16803) 改进了配置 MySQL 动作批量操作时的错误上报。
- [#16796](https://github.com/emqx/emqx/pull/16796) 修复了连接器动作中对多行 SQL 语句的处理问题。
- [#16936](https://github.com/emqx/emqx/pull/16936) 修复了 Azure Blob Storage 聚合模式动作在容器内 blob 数量过多时健康检查超时的问题。
- [#16955](https://github.com/emqx/emqx/pull/16955) 消除了 Kafka 生产者动作误产生的健康检查告警日志。此前，当 Kafka 生产者长时间空闲时，Kafka 可能会关闭连接（通常默认为 10 分钟），若此时恰好触发健康检查，可能产生内容为 `"not_all_kafka_partitions_connected"` 的误报告警日志。
- [#16972](https://github.com/emqx/emqx/pull/16972) 修复 HTTP 和 GCP PubSub 动作，将原因为 `closing` 的瞬态连接错误视为可恢复错误，减少日志噪音。
- [#16863](https://github.com/emqx/emqx/pull/16863) 新增告警日志：当异步动作中收到已过期请求的异步回复时，将记录 warning 级别日志。
- [#16847](https://github.com/emqx/emqx/pull/16847) 修复了消息转换表达式中使用非 ASCII Unicode 字符串时导致崩溃的问题。
- [#16979](https://github.com/emqx/emqx/pull/16979) MQTT Ingress Bridge 现已支持从远端消息队列 `$queue/{name}/{bind-filter}` 消费消息。

#### 访问控制

- [#16780](https://github.com/emqx/emqx/pull/16780) 修复了授权源校验中的问题：缺少 `type` 字段的请求可能触发内部错误。现在 EMQX 会针对此情况返回明确的 `BAD_REQUEST` 校验错误。
- [#16805](https://github.com/emqx/emqx/pull/16805) 新增支持：authz 钩子结果可选择不将结果写入授权缓存。
- [#16865](https://github.com/emqx/emqx/pull/16865) 为 `mqtt.client_attrs_init` 表达式新增 `cert_common_name` 和 `cert_subject` 别名，与现有的 `cn` 和 `dn` 变量并列使用。
- [#16868](https://github.com/emqx/emqx/pull/16868) 改进了面向程序化客户端的 REST API 认证错误提示信息。错误响应现在会说明 `api_key.bootstrap_file` 配置项和 `POST /api_key` 端点，用于创建持久化 API Key。
- [#16928](https://github.com/emqx/emqx/pull/16928) 通过 Dashboard 创建的 REST API Key 现在随机生成，不再基于 API Key 名称派生。
- [#16939](https://github.com/emqx/emqx/pull/16939) 修复了内置数据库认证器对于缺少但具有默认值的 bootstrap 文件，不再输出 warning 日志的问题。

#### 持久化存储

- [#16874](https://github.com/emqx/emqx/pull/16874) 修复了一个罕见问题：DS Raft 持久化存储在经历一系列快速 Leader 切换后，可能停止接受新消息，且需要重启节点才能恢复。

#### 集群

- [#16534](https://github.com/emqx/emqx/pull/16534) 将默认 `net_ticktime` 从 2 分钟降低至 1 分钟，以改善集群节点故障检测能力。

#### 插件

- [#16842](https://github.com/emqx/emqx/pull/16842) 减少了在没有 peer 节点持有插件配置时拉取插件配置产生的多余 warning 日志。此前，节点启动时从 peer 节点拉取插件配置，即使在无害的情况下（例如该插件首次被加载，没有任何 peer 持有配置），也会记录 warning 日志。此类情况现在改为 debug 级别日志，而真正的错误（如 RPC 失败、超时）仍保留为 warning。
- [#16843](https://github.com/emqx/emqx/pull/16843) 修复了 HTTP 头和查询字符串参数未被透传至插件 API 处理器的问题，导致插件收到空的请求头和缺失的查询参数。
- [#16904](https://github.com/emqx/emqx/pull/16904) 防止同一插件的多个版本同时被启用或运行。当启用更新版本时，已配置的旧版本现在将自动禁用。管理 API 操作也不再在另一版本仍处于活动状态时报告成功，而是返回明确的错误。

#### 网关

- [#16536](https://github.com/emqx/emqx/pull/16536) 修复了 CoAP 网关在 DTLS 连接模式下运行时存在的问题。

#### 可观测性

- [#16879](https://github.com/emqx/emqx/pull/16879) 新增 `log.audit.cache_size` 作为审计日志数据库缓存大小的主配置项，同时保留 `log.audit.max_filter_size` 以保持向后兼容。

#### 部署

- [#16901](https://github.com/emqx/emqx/pull/16901) 修复了 RHEL 9.6 LTS 的 RPM 安装包 OpenSSL 依赖问题：RHEL >= 9.7 固定依赖 `openssl >= 3.5.1`，旧版 RHEL 9 固定依赖 `openssl >= 3.0.7`。

#### ExHook

- [#16890](https://github.com/emqx/emqx/pull/16890) 修复了 ExHook 在重连成功重载后，相同服务器名称可能在运行列表中出现重复并触发重复回调分发的问题。

#### 许可证

- [#16764](https://github.com/emqx/emqx/pull/16764) 优化了许可证客户层级的执行逻辑：引入 `STANDARD` 和 `VIP` 两个层级，并将官方许可证 `STANDARD` 层级的到期宽限期从 90 天缩短至 15 天，超过宽限期后新会话将受到限制。

## 6.1.1

*发布日期: 2026-02-27*

在升级到 EMQX 6.1.1 之前，请务必查阅不兼容变更和已知问题。

### 增强

#### 核心 MQTT 功能

- [#16637](https://github.com/emqx/emqx/pull/16637) 改进了会话接管期间的保留消息分发。 此前，当通配符订阅的保留消息分发过程中发生会话接管时，分发进程会重新启动，从而导致消息重复。现在，EMQX 会从前一个会话最后确认的消息处恢复分发，显著减少了消息重复。

#### 持久化存储

- [#16704](https://github.com/emqx/emqx/pull/16704) 优化了 RocksDB 存储分片的磁盘空间预分配。此前，每个持久化存储分片默认会预分配大量磁盘空间。这导致了较高的存储消耗，特别是在默认 16 分片的配置下。现在，EMQX 阻止了激进的预分配行为，降低了持久化存储数据库的初始磁盘占用。

#### 消息队列和消息流

- [#16551](https://github.com/emqx/emqx/pull/16551), [#16714](https://github.com/emqx/emqx/pull/16714) 优化了消息流与消息队列的订阅接口。

  - **命名的流**：用户现在使用 `$stream` 前缀进行订阅时必须指定名称。订阅语法为 `SUBSCRIBE $stream/<name>/<topic_filter>`，如果流已存在，也可以简写为 `SUBSCRIBE $stream/<name>`。流消费的起始位置通过 `stream-offset` 用户订阅属性定义。
  - **命名的队列**：类似地，消息队列现在使用 `$queue` 前缀。订阅时需要指定名称，语法为 `SUBSCRIBE $queue/<name>/<topic_filter>`，对于已存在的队列可简写为 `SUBSCRIBE $queue/<name>`。
  - **命名规则**：名称仅限使用字母数字字符、下划线、连字符和点号。对于旧版本中创建的未命名实体，系统将自动采用其主题过滤器作为名称（并在前缀添加 `/`）。
  - **兼容性**：旧版接口 `$q`（v6.0.0）和 `$s`（v6.1.0）已被弃用，但为保持兼容性仍然可用。请注意，当启用消息队列功能时，`$queue` 前缀将不能再用于标准共享订阅。

- [#16820](https://github.com/emqx/emqx/pull/16820) 为消息队列和消息流管理 API 新增了更短的 API 路径别名 `/queues/*` 和 `/streams/*`。

  之前的 `/message_queues/*` 和 `/message_streams/*` 路径仍然可用，以保持向后兼容性，但已不再在 API 文档中展示。

#### Gateway

- [#16719](https://github.com/emqx/emqx/pull/16719) 为 CoAP 和 LwM2M 网关增加了分块传输（Block-Wise Transfer）支持。
  - 增加了分块设置：`enable`（启用）、`max_block_size`（最大分块大小）、`max_body_size`（最大消息体大小）和 `exchange_lifetime`（交换生命周期）。
  - 改进了 `POST /gateways/coap/clients/:clientid/request` 以及 LwM2M 对大分块消息的下行处理。
- [#16736](https://github.com/emqx/emqx/pull/16736) <!-- ported from PRs #16220, #16596, #16609, #16619, #16627, #16655 -->增强了 JT/T 808 网关功能及协议支持。
  - 增加了 `jt808.frame.parse_unknown_message` 选项，使 JT/T 808 网关能够透传未知消息。
  - 增加了对 JT/T 808 协议 2019 版的支持。
  - 为 JT/T 808 网关增加了 GBK 字符编码支持。JT/T 808 协议规定 STRING 类型字段使用 GBK 编码。新增了 `frame.string_encoding` 配置选项：
    - `utf8`（默认）：按原样透传字符串（保持向后兼容）。
    - `gbk`：将来自设备的 GBK 编码字符串转换为 UTF-8 供 MQTT 使用，并将来自 MQTT 的 UTF-8 字符串转换为 GBK 发送给设备。
    - 此项改动影响上行解析（GBK 转 UTF-8）和下行序列化（UTF-8 转 GBK），涉及的字符串字段包括车牌号、驾驶员姓名、文本消息、区域名称和终端参数等。无论此项如何设置，MQTT 载荷（Payload）始终使用 UTF-8 编码。
  - 在 JT/T 808 网关下行消息中增加了对自定义 `msg_sn` 的支持。当下行 MQTT 消息载荷的头部包含 `msg_sn` 字段时，网关将使用该值而非自动生成的通道序列号。这允许外部系统在特定用例下控制消息序列。
  - 修复了针对 CAN 总线 ID 参数（0x0110~0x01FF）的 JT/T 808 网关参数设置（0x8103）和查询响应（0x0104）的消息处理。这些参数在 JSON 中应使用 `BYTE[8]` 数据类型并进行 Base64 编码，而非字符串类型。
  - 修复了 JT/T 808 0x0702 驾驶员身份验证汇报消息的解析问题。

#### 安全

- [#16447](https://github.com/emqx/emqx/pull/16447) 为证书管理 API 添加了 `force_delete` 参数。全局证书和命名空间级证书端点的 `DELETE` 方法现在支持 `force_delete` 查询参数：

  - `DELETE /certs/global/name/:name`
  - `DELETE /certs/ns/:ns/name/:name`

  当该参数为 `false`（默认值）时，EMQX 会在所有命名空间范围内执行安全检查；如果证书当前被任何监听器或配置引用，则会阻止删除。将其设置为 `true` 时，将绕过这些检查并立即删除。

- [#16461](https://github.com/emqx/emqx/pull/16461) 支持 TLS 1.3 无状态会话恢复。EMQX 现已支持 TLS 1.3 的无状态会话票据，使客户端无需在服务器端存储会话状态即可恢复安全会话。这在客户端高频重连场景下可提升性能并降低内存开销。

  **配置：**

  - **全局密钥：** 设置 `node.tls_stateless_tickets_seed`，作为在节点范围内用于加密会话票据的密钥种子。
  - **监听器设置：** 配置 `listeners.ssl.<name>.ssl_options.session_tickets` 为以下选项之一：
    - `disabled`（默认）：禁用会话恢复。
    - `stateless`：启用基于无状态票据的会话恢复。
    - `stateless_with_cert`：启用会话恢复，并在票据中包含客户端证书信息。

  **重要说明：** 要生成会话票据，必须同时配置非空的全局密钥种子，并在监听器级别启用相关功能。如果启用了监听器级别的设置但未配置全局密钥种子，EMQX 将记录错误日志，并且不会签发会话票据。

#### 访问控制

- [#16504](https://github.com/emqx/emqx/pull/16504) 新增一个配置选项，用于在单点登录（SSO）用户创建过程中，指定使用 OIDC 数据源中的哪个字段（claim）来生成 EMQX Dashboard 用户名。
- [#16741](https://github.com/emqx/emqx/pull/16741) <!-- ported from PRs #16625, #16639 --> 为 SAML SSO 后端引入了 `idp_signs_envelopes` 和 `idp_signs_assertions` 选项，以精确控制签名验证行为。
  - 修复了由于未能从元数据中正确提取 IdP 证书指纹而导致 SAML 签名验证失败的问题。
  - 这两个选项默认值均为 `false`，以保持向后兼容。对于配置为对 SAML 响应进行签名的 IdP，用户应显式将这些选项设置为 `true`。
- [#16684](https://github.com/emqx/emqx/pull/16684) `mqtt.client_attrs_init` 表达式现已支持使用客户端密码。这允许在连接阶段通过函数（例如 `jwt_value`）处理密码，以初始化自定义客户端属性。
- [#16730](https://github.com/emqx/emqx/pull/16730) 为 Redis 授权引入了 `compatibility_mode` 设置，以支持来自 EMQX v4 的旧版数据模式。
  - **启用方式：** 设置 `compatibility_mode = v4` 以启用该模式。
  - **旧版映射：** 自动转换 `%u/%c` 占位符，并将旧版 ACL 访问值（`1`、`2`、`3`）映射为 `subscribe`、`publish` 和 `all`。
  - **注意：** 该模式默认禁用，以确保不会对现有 v5 配置产生影响。

#### 数据集成

- [#16511](https://github.com/emqx/emqx/pull/16511) IoTDB 数据集成现已支持 Table Model，从而可以将数据以更结构化的方式写入 Apache IoTDB。
- [#16516](https://github.com/emqx/emqx/pull/16516) 新增两个专用指标，用于跟踪聚合上传操作（兼容 S3、Azure Blob Storage、Snowflake 以及 S3 Tables）的性能：
  - `aggregated_upload.success`：在聚合投递成功时递增。
  - `aggregated_upload.failure`：在聚合投递失败时递增。
- [#16658](https://github.com/emqx/emqx/pull/16658) 更新了 EMQX Tables 连接器的默认配置和错误处理机制。
  - EMQX Tables 连接器的默认服务器端口已从 `80` 更改为 `4001`。
  - 改进了启用 SSL 的 EMQX Tables 连接器的错误提示信息。如果配置中缺少 `cacertfile`、`certfile` 或 `keyfile`，系统现在会返回更具描述性的错误信息，以便协助排查问题。

#### 规则引擎

- [#16524](https://github.com/emqx/emqx/pull/16524) 增强了规则引擎 SQL 中的 base64 编码与解码函数，新增对填充（padding）和 URL 安全选项的支持。

  `base64_encode` 和 `base64_decode` 函数现在支持可选参数，用于控制编码行为：

  - **`no_padding`**：在编码或解码时不使用填充字符（`=`）。当需要移除编码字符串中的填充字符，或解码不包含填充字符的字符串时非常有用。
  - **`urlsafe`**：使用 URL 安全的 base64 编码/解码。将 `+` 替换为 `-`，将 `/` 替换为 `_`，使编码后的字符串无需额外转义即可安全用于 URL。

  这些选项可以单独使用，也可以组合使用。组合使用时，参数顺序不影响结果。

  **规则 SQL 示例：**

  无填充编码：

  ```
  SELECT base64_encode(payload, 'no_padding') as encoded FROM "t/#"
  ```

  使用 URL 安全字符进行编码：

  ```
  SELECT base64_encode(payload, 'urlsafe') as encoded FROM "t/#"
  ```

  同时使用两种选项（无填充且 URL 安全）进行编码：

  ```
  SELECT base64_encode(payload, 'no_padding', 'urlsafe') as encoded FROM "t/#"
  ```

  解码 URL 安全的 base64：

  ```
  SELECT base64_decode(payload, 'urlsafe') as decoded FROM "t/#"
  ```

  解码无填充的 URL 安全 base64：

  ```
  SELECT base64_decode(payload, 'urlsafe', 'no_padding') as decoded FROM "t/#"
  ```

- [#16533](https://github.com/emqx/emqx/pull/16533) 为 Variform 表达式新增 `json_value` 和 `jwt_value` 辅助函数，用于通过以点号分隔的键路径，从 JSON 数据和 JWT 令牌中提取值。

  - **`json_value(json_string, path)`**：解析 JSON 编码的二进制字符串，并遍历嵌套结构以提取指定值。
    - *示例：* `json_value(username, 'shop.floor')` 从用户名字符串中的嵌套 `shop` 对象中提取 `floor` 字段。
  - **`jwt_value(jwt_string, path)`**：解码 JWT 令牌，并从其 payload 中获取指定的声明值。
    - *示例：* `jwt_value(password, 'client_attrs.unitid')` 从密码字段中提供的 JWT 中提取自定义的 `unitid` 声明。

- [#16539](https://github.com/emqx/emqx/pull/16539) 为 `spb_decode` 增加了对 Sparkplug B 指标别名跟踪的支持。规则引擎的 `spb_decode` 函数现在会基于 Sparkplug B Birth 证书自动跟踪并解析指标别名。

  - **动态映射**：当设备或边缘网络节点（EoN）发布 `DBIRTH` 或 `NBIRTH` 消息时，EMQX 会存储其中包含的别名到名称映射关系。
  - **自动解析**：随后通过 `spb_decode` 处理的 `DDATA` 或 `NDATA` 消息，将使用这些已存储的映射，在输出 payload 中填充原始指标名称。
  - **限制**: 在备选动作（fallback actions）的执行环境中，指标映射不可用。如果备选动作重新发布未解码的 payload，则指标名称字段将保持未填充状态。

- [#16581](https://github.com/emqx/emqx/pull/16581) 引入用于 Sparkplug B 数据规范化的 `spb_zip_kvs` 规则 SQL 函数。

  新增规则引擎函数 `spb_zip_kvs`，用于简化已解码 Sparkplug B 消息的结构。该函数将分离的 `keys` 和 `values` 数组合并为统一的键值映射（key-value map），从而使数据在下游集成处理中更加易于使用。

  **关键转换：**

  - `PropertySets`：递归地将 `keys` 和 `values` 字段“压缩合并（zip）”。原有数组将被移除，并替换为合并后的映射结构。
  - `PropertySetLists`：通过移除 `propertyset` 包装层并将其替换为已转换的 PropertySets 数组，从而扁平化结构。
  - `DataSets`：将 `columns` 和 `rows` 合并为单一对象。诸如 `types` 和 `num_of_columns` 等元数据字段将被移除，以提供更简洁的输出结构。
  - 非破坏性：所有其他字段和值保持不变。

  例如，给定如下已解码的 Sparkplug B 输入消息：

  ```json
  {
    "metrics": [
      {
        "properties": {
          "values": [
            {"int_value": 99},
            {
              "propertyset_value": {
                "values": [{"int_value": 999}],
                "keys": ["inner"]
              }
            },
            {
              "propertysets_value": {
                "propertyset": [
                  {
                    "values": [{"int_value": 1}],
                    "keys": ["inner1"]
                  },
                  {
                    "values": [{"int_value": 2}],
                    "keys": ["inner2"]
                  }
                ]
              }
            }
          ],
          "keys": [
            "leaf",
            "nested_prop",
            "nested_prop_list"
          ]
        }
      },
      {
        "dataset_value": {
          "num_of_columns": 2,
          "types": [7, 12],
          "rows": [
            {
              "elements": [
                {"int_value": 3},
                {"string_value": "3"}
              ]
            },
            {
              "elements": [
                {"int_value": 4},
                {"string_value": "4"}
              ]
            }
          ],
          "columns": ["col1", "col2"]
        }
      }
    ]
  }
  ```

  则 `spb_zip_kvs` 的输出将为：

  ```json
  {
    "metrics": [
      {
        "properties": {
          "nested_prop_list": {
            "propertysets_value": [
              {"inner1": {"int_value": 1}},
              {"inner2": {"int_value": 2}}
            ]
          },
          "nested_prop": {
            "propertyset_value": {"inner": {"int_value": 999}}
          },
          "leaf": {"int_value": 99}
        }
      },
      {
        "dataset_value": {
          "col2": {"elements": [{"int_value": 4}, {"string_value": "4"}]},
          "col1": {"elements": [{"int_value": 3}, {"string_value": "3"}]}
        }
      }
    ]
  }
  ```

#### REST API

- [#16718](https://github.com/emqx/emqx/pull/16718) 优化了 REST API 规范，以提升 Swagger UI 中的清晰度和可读性。

  之前，规范字段的摘要（summary）和描述（description）内容混合在一起。现在，摘要为简短、简单且不包含标点符号的表述，而描述则提供完整的详细信息。

- [#16735](https://github.com/emqx/emqx/pull/16735) EMQX 现已支持在 `/api/v5/plugin_api/{plugin}/...` 路径下由插件定义的 HTTP API 回调。

  这使插件开发者能够通过 Dashboard API 服务暴露插件特定的 API 端点，并保持一致的身份认证机制和 HTTP 错误处理方式。

#### 可观测性

- [#16656](https://github.com/emqx/emqx/pull/16656) 增强了系统监控报告（如 `busy_port` 和 `long_schedule`）的信息量，通过包含进程标签以便于故障排查。
- [#16744](https://github.com/emqx/emqx/pull/16744) <!-- ported from PR #16324 --> 支持对通过 HTTP API 发布的消息进行端到端追踪。

#### 性能

- [#16413](https://github.com/emqx/emqx/pull/16413) 提升了订阅处理性能。
- [#16492](https://github.com/emqx/emqx/pull/16492) 略微优化了系统空闲状态下的内存使用。
- [#16757](https://github.com/emqx/emqx/pull/16757) 将 `os_mon` 默认配置为仅收集系统级内存统计信息，从而减少对每个进程进行内存扫描所带来的开销。

### 修复

#### 核心 MQTT 功能

- [#16480](https://github.com/emqx/emqx/pull/16480) 修复了在对端关闭连接后 WebSocket 连接可能发生崩溃的问题，该问题通常在中等负载下出现。

  ```
  crasher: initial call: cowboy_tls:connection_process/4,
  error: {{case_clause,{error,closed}},[
  {cowboy_websocket_linger,websocket_send_close,2,[{file,"cowboy_websocket_linger.erl"},{line,752}]},
  {cowboy_websocket_linger,websocket_close,3,[{file,"cowboy_websocket_linger.erl"},{line,743}]},
  {proc_lib,wake_up,3,[{file,"proc_lib.erl"},{line,340}]}
  ]}
  messages: [
  {ssl,{sslsocket,{gen_tcp,#Port<...>,...},[...]},<<130,130,27,93,145,101,251,93>>},
  {ssl_closed,{sslsocket,{gen_tcp,#Port<...>,...},[...]}}
  ], ...
  ```

- [#16515](https://github.com/emqx/emqx/pull/16515) 修复了当接收到大于客户端声明的 `Maximum-Packet-Size` 的 Broker 消息时，WebSocket 连接可能发生崩溃的问题。

- [#16553](https://github.com/emqx/emqx/pull/16553) 修复了在达到分发速率限制时，部分保留消息在投递过程中被跳过的问题。现在，如果客户端在遍历保留主题时触发速率限制，进程将不再终止投递流程；而是采用指数退避策略（从最小 300 毫秒到最大 10 秒）重试遍历，直到所有消息发送完成。

  此外，本次更新还对 Retainer 的流控配置进行了调整：

  - `retainer.flow_control.batch_deliver_number` 配置项现已弃用。
  - `retainer.flow_control.batch_read_number` 不再支持设置为 `0`（此前表示批量大小不受限制）。如果该参数设置为 `0`，现在将默认使用 `1000` 条消息，以防止因大规模批量读取而导致潜在的系统不稳定问题。

- [#16569](https://github.com/emqx/emqx/pull/16569) 修复了在高系统负载下，用于抖动检测（flapping detection）的辅助进程 `emqx_flapping` 可能发生崩溃的罕见竞态条件问题。

- [#16651](https://github.com/emqx/emqx/pull/16651) 修复了在系统关闭期间，由于对已关闭 Socket 执行操作而导致连接进程崩溃的罕见问题，该问题通常发生在系统高压力场景下。
   之前，这类竞态条件通常会产生日志级别为 error 的日志，例如 `{badmatch,{ok,{sock_error,closed}...`。

- [#16675](https://github.com/emqx/emqx/pull/16675) 修复了在会话接管或丢弃场景下，`disconnected_at` 时间戳可能晚于 `connected_at` 的时间顺序问题。

  之前，`disconnected_at` 在 `ensure_disconnected` 阶段记录过晚，而此时新会话的 `connected_at` 已经设置完成。这会导致出现 `disconnected_at > connected_at` 的竞态情况，使外部系统难以准确追踪客户端在线状态。

  现在，系统会在会话接管开始时或接收到丢弃请求时立即记录 `disconnected_at` 时间戳。该调整确保断开事件始终发生在新会话连接时间之前，从而为外部状态追踪和分析提供可靠且有序的数据。

- [#16715](https://github.com/emqx/emqx/pull/16715) 修复了保留的 `$SYS` 消息（例如 Broker/节点标识相关主题）在存储时未设置过期时间的问题，这可能导致在 StatefulSet 轮换后，过期的节点标识仍然在 Dashboard 视图中可见。

  现在，新发布的保留 `$SYS` 消息将包含 `Message-Expiry-Interval = 3600`（1 小时）。

  对于在本次变更之前已存在的过期保留 `$SYS` 条目，可以通过向对应的过期主题发布一个空的保留消息进行手动清除：

  ```bash
  emqx eval 'emqx:publish(emqx_message:set_flag(retain, true, emqx_message:make(emqx_sys, <<"$SYS/brokers/emqx@127.0.0.1/sysdescr">>, <<>>))).'
  ```

  请将命令中的主题替换为需要移除的过期 `$SYS/...` 主题。

- [#16731](https://github.com/emqx/emqx/pull/16731) 修复了在存在共享订阅时 `emqx ctl subscriptions list` 命令可能触发崩溃的问题。

  在修复之前，列出订阅信息可能会针对某些客户端执行失败且不返回任何输出。修复后，`emqx ctl subscriptions list` 可以稳定地处理普通订阅和共享订阅。

- [#16782](https://github.com/emqx/emqx/pull/16782) 修复了 MQTT v5 协议中对无效 PUBLISH 属性的处理问题。如果客户端发送的 PUBLISH 报文包含 `Subscription-Identifier`，EMQX 现在将其视为协议错误，并断开该客户端连接。

#### 网关

- [#16603](https://github.com/emqx/emqx/pull/16603) 修复了 CoAP 网关在 DTLS 连接模式下运行时存在的问题。
- [#16670](https://github.com/emqx/emqx/pull/16670) NATS 网关现在会强制执行最大发布 payload 大小限制，并正确遵循 `echo` 选项以防止本地消息投递（回环）。此外，本次更新还改进了对发布和订阅主题的校验，并提供了更具描述性的错误信息。

#### 访问控制

- [#16423](https://github.com/emqx/emqx/pull/16423) 在 JWT 认证中新增对 `aud`（audience）声明的验证支持。

  当在 `verify_claims` 中配置了 `aud` 声明时，JWT 令牌必须包含有效的 `aud` 声明。验证同时支持字符串和数组两种格式：

  - 如果 `aud` 为字符串，则必须与期望值完全匹配。
  - 如果 `aud` 为数组，则数组中至少有一个元素必须与期望值匹配。
  - 空字符串或空数组将导致验证失败。
  - 当在 `verify_claims` 中配置了 `aud` 声明但令牌中缺少该声明时，将验证失败。

- [#16459](https://github.com/emqx/emqx/pull/16459) 修复了 SCRAM 认证 HTTP API 中的问题。此前，在用户创建 API 调用中返回的已创建用户 ID 不正确。

#### 数据集成

- [#16507](https://github.com/emqx/emqx/pull/16507) 修复了 MQTT Source 在其连接器重连后停止接收消息的问题。

  之前，当 MQTT Source 的连接器从连接丢失中恢复时，其订阅的主题未被重新订阅，导致该 Source 停止工作，直到连接器被重启。现在，Source 会在重连后自动重新订阅。

- [#16542](https://github.com/emqx/emqx/pull/16542) 修复了在 Kafka 负载过高时，Kafka 生产者连接可能过早断开的问题，该问题会导致过多的生产请求重试。

  现在，生产请求超时时间会自动设置为至少元数据请求超时时间的两倍，且最小值为 30 秒。当元数据请求耗时超过预期（尤其是在元数据请求超时时间被配置为较小值时），此调整可减少不必要的重连和重试。

- [#16622](https://github.com/emqx/emqx/pull/16622) 修复了当某个动作使用异步查询模式且其连接器在超过一次健康检查后断开连接时，其备选动作可能被触发两次的问题。

- [#16657](https://github.com/emqx/emqx/pull/16657) 修复了配置迁移过程中未执行必要模式转换的问题，该问题会导致从旧版本 EMQX 导入的配置在新版本中无法正常兼容。

  例如，在将带有静态 ClientID 的 MQTT 连接器从 v5.10.0 升级到 v6.0.0 时，与 ClientID 关联的认证信息（用户名和密码）在两个版本之间的内部表示方式发生了变化，但原有迁移逻辑未执行相应的数据结构转换，导致配置异常。

  本次修复确保所有导入的配置在迁移过程中都会经过正确的模式转换处理，从而保证升级后的功能一致性与完整性。

- [#16659](https://github.com/emqx/emqx/pull/16659) 修复了升级兼容性问题：从 v5.10.0 及更早版本迁移的 MQTT 连接器在使用静态 ClientID 时会忽略根级别凭据。此前，迁移逻辑未将根级别的用户名和密码字段传递到各个 ClientID 条目中，导致升级后与远程 Broker 建立连接失败。

  现在，如果在根连接器中存在用户名和/或密码字段，这些凭据将与每个 clientid 指定的凭据合并（后者优先生效）。

- [#16723](https://github.com/emqx/emqx/pull/16723) 解决了 RabbitMQ 连接器、动作和 Source 组件中的自愈问题。此前，如果底层连接或通道进程异常终止，组件将一直处于 “Disconnected” 状态，必须手动重启才能恢复功能。

- [#16742](https://github.com/emqx/emqx/pull/16742) <!-- ported from PR #16585 --> 修复了 GreptimeDB TLS 连接失败的问题。

#### 持久化存储

- [#16512](https://github.com/emqx/emqx/pull/16512) 改进了持久会话中对可恢复错误的处理。当由于网络问题导致创建持久存储迭代器失败时，持久会话现在会重试该操作；此前则会导致整个会话断开。

  修复了 `emqx_ds_client` 组件中重试机制的问题：此前对于可恢复错误的重试次数受到限制。

  修复了若干与共享订阅相关的问题：

  - 修复了节点重启后共享订阅 leader 未能启动的问题。
  - 共享订阅 leader 不再向客户端通告已回放完成的流。
  - 新增对共享订阅 leader 状态检查点事务选项的配置支持。

- [#16614](https://github.com/emqx/emqx/pull/16614) 对持久化存储功能进行了改进和缺陷修复：

  - 优化了节点间配置不一致时的处理机制。此前，如果各节点的初始持久化存储配置不一致，可能会导致副本无法收敛。本次改进确保在存储初始化及后续配置更新过程中，由分片 Leader 的配置统一下发并同步至所有副本，从而保证配置一致性。

    ::: warning 注意

    此变更**不向后兼容**。在滚动升级过程中，分片将暂停，直到大多数副本完成升级。一旦大多数副本升级完成，将无法再降级回之前的 EMQX 版本。

    :::

  - 修复了持久化存储订阅机制中的问题：当使用新迭代器创建订阅时，如果消息时间戳与迭代器时间戳精确匹配，可能会跳过消息。

- [#16770](https://github.com/emqx/emqx/pull/16770) 提升了持久会话在接管和垃圾回收过程中的稳定性。

#### 集群

- [#16393](https://github.com/emqx/emqx/pull/16393) 提升了在网络不稳定条件下集群连接路由复制的稳定性。

- [#16465](https://github.com/emqx/emqx/pull/16465) 将 `gen_rpc` 升级至 `3.5.1`。

  在升级 `gen_rpc` 之前，如果对端节点不可达，EMQX 可能会由于连接超时而产生大量延迟出现的崩溃日志。新的 `gen_rpc` 版本消除了这种长尾日志行为，并将崩溃日志转换为更易读的 `error` 日志，同时对频繁出现的 `"failed_to_connect_server"` 日志进行了限流处理，以避免日志刷屏。

- [#16544](https://github.com/emqx/emqx/pull/16544) 提升了集群自动清理流程的健壮性。此前，如果在节点初次启动时禁用了 autoclean 功能，后续即使修改配置也无法将其激活。

- [#16739](https://github.com/emqx/emqx/pull/16739) 改进了所有节点同时重启后集群的恢复时间。内置的 Mria 数据库管理系统不再等待用于生成事务同步事件的内部表完成全量同步。

#### 可观测性

- [#16537](https://github.com/emqx/emqx/pull/16537) 修复了由特定 `gen_rpc` 错误消息触发的日志格式化器崩溃问题。

  此前，当 `gen_rpc` 记录某些错误（例如传输超时）时，EMQX 可能会因 “FORMATTER CRASH” 错误而崩溃。现在，日志格式化器能够安全处理此类消息，不再发生崩溃。

- [#16661](https://github.com/emqx/emqx/pull/16661) 改进了在处理无效主题请求时 `topic_metrics` 和 `cluster_rpc` 的日志记录。

- [#16674](https://github.com/emqx/emqx/pull/16674) 更新了日志系统，确保在日志输出中将 Erlang 进程标识符（PID）明确作为结构化数据字段包含在内。

- [#16699](https://github.com/emqx/emqx/pull/16699) 改进了规则引擎指标工作进程（metrics worker）的错误处理和日志记录。此前，在某些竞态条件下，可能会打印如下冗长且难以理解的日志：

  ```
  2026-02-03T13:53:54.576326+00:00 [error] Generic server <0.11323236.0> terminating. Reason: {{badkey,'actions.success'},[{erlang,map_get,['actions.success',#{}],[{error_info,#{module => erl_erts_errors}}]},{emqx_metrics_worker,idx_metric,4,[{file,"emqx_metrics_worker.erl"},{line,683}]},{emqx_metrics_worker,inc,4,[{file,"emqx_metrics_worker.erl"},{line,322}]},{emqx_rule_runtime,do_eval_action_reply_t...
  ```

  现在，系统将输出更具可读性和可诊断性的错误信息，以便于问题排查。

#### 安全性

- [#16545](https://github.com/emqx/emqx/pull/16545) 修复了 `node.cookie` 对 `#` 字符的处理问题。此前，如果 cookie 中包含 `#`，则仅 `#` 之前的部分会生效。例如，若配置为 `abc#d`，实际使用的 cookie 仅为 `abc`。

  同时新增校验逻辑，拒绝包含反斜杠、单引号、双引号以及空格等问题字符的配置。

- [#16664](https://github.com/emqx/emqx/pull/16664) 之前可以上传与不存在的托管命名空间关联的托管证书文件。现在，在接受上传前会先检查命名空间是否存在。

- [#16692](https://github.com/emqx/emqx/pull/16692) 修复了 CRL 缓存的回归问题：`emqx_crl_cache:evict/1` 未能完全清除内部 URL 状态。

  在执行清除后，同一 CRL URL 在下次使用时将能够正确重新注册，并恢复其刷新定时器，同时避免在每个连接上重复发起 HTTP 拉取。

#### 插件

- [#16784](https://github.com/emqx/emqx/pull/16784) 减少了单节点部署场景下插件启动时的噪声告警。

  EMQX 在集群配置同步过程中不再尝试从本地节点获取插件配置，从而避免启动阶段反复出现 `config_not_found_on_node` 警告。

- [#16823](https://github.com/emqx/emqx/pull/16823) 修复了 Dashboard 中对预安装插件的管理问题。

  当插件包在节点启动前解压至 `plugins/` 目录时，通过 Dashboard 启动插件不再在插件配置页面出现 `Plugin Config Not Found` 错误。

#### 其他

- [#16620](https://github.com/emqx/emqx/pull/16620) 修复了在 aarch64 架构下 CRC32C 动态库加载失败的问题。

## 6.1.0

*发布日期: 2025-12-30*

在升级到 EMQX 6.1.0 之前，请务必查阅不兼容变更和已知问题。

### 功能亮点

EMQX 6.1.0 引入了 MQTT 消息流、增强的命名空间能力、新增数据集成以及集中式证书管理。

**MQTT 消息流**

MQTT 消息流提供了一种基于主题过滤器的持久化消息集合，并支持显式的生命周期管理。所有匹配消息流主题过滤器的消息都会自动追加到流中，从而支持具备顺序保证的消息消费，并允许多个消费者同时读取。

客户端可以通过特殊主题格式 `$s/<timestamp>/topic/filter` 订阅消息流，从指定的时间点开始消费历史消息。

**增强的命名空间能力**

- 命名空间相关配置与隔离设置现已在 Dashboard 中集中管理。
- 扩展了命名空间功能，支持命名空间级别的指标、认证和授权。
- 新增命名空间级别的指标监控，涵盖消息、会话以及数据集成操作，并通过 Prometheus 端点对外暴露。
- 内置的认证与授权后端现已支持命名空间级别的用户和规则，实现更完善的多租户隔离。
- 新增基于客户端命名空间作为挂载点的自动主题隔离机制。

**新增数据集成**

- AWS Timestream for InfluxDB 连接器
- EMQX Tables 连接器
- InfluxDB API v3（适用于 InfluxDB 与 AWS Timestream 连接器）
- Kafka 与 Confluent Producer 连接器支持 OAuth 认证
- 聚合模式下的 Azure Blob Storage 与 S3 Action 新增 Parquet 文件格式支持

**证书管理**

新增通过 HTTP API 实现的集中式证书管理功能，证书可独立进行管理，并在监听器和连接器的 SSL 配置中进行引用。

### 增强

#### 消息队列与 MQTT 消息流

- [#16326](https://github.com/emqx/emqx/pull/16326) 实现了消息流。MQTT 消息流是一种通过主题过滤器标识的持久化消息集合。消息流具有显式的生命周期管理，任何与消息流主题过滤器匹配的已发布消息都会自动追加到该消息流中。消息流支持具备顺序保证的消息消费，并且允许对同一消息流进行多次消费。

  客户端可以通过订阅特殊主题格式 `$s/<timestamp>/topic/filter` 来消费消息流中的消息，其中 `topic/filter` 指向一个已存在的消息流。通过在订阅中指定时间戳，客户端可以从特定时间点开始消费消息。时间戳可以是微秒级的 Unix 时间戳，或以下两个特殊值之一：`earliest` 或 `latest`。

- [#16454](https://github.com/emqx/emqx/pull/16454) 对于消息队列和消息流，重新配置的垃圾回收（GC）间隔现在会立即生效。此前，新配置的间隔仅会在下一次垃圾回收周期之后才生效。

#### 核心 MQTT 功能

- [#16099](https://github.com/emqx/emqx/pull/16099) 新增规则引擎事件：`$events/client/ping`。当客户端发送 `PINGREQ` 报文时触发该事件。

#### 访问控制

- [#16132](https://github.com/emqx/emqx/pull/16132) 新增用于集中管理证书的 HTTP API。
- [#16154](https://github.com/emqx/emqx/pull/16154) 新增支持在监听器和客户端的 SSL 配置选项中引用受管证书文件。
- [#16266](https://github.com/emqx/emqx/pull/16266) 新增 `authorization.include_mountpoint` 配置项。启用后，在进行授权校验之前，主题将自动加上监听器的挂载点前缀。
- [#16272](https://github.com/emqx/emqx/pull/16272) 在使用内置授权后端时，新增对指定命名空间规则的支持。现在，属于某个命名空间的 MQTT 客户端在进行授权时，仅会使用该命名空间下的规则。
- [#16345](https://github.com/emqx/emqx/pull/16345) 在使用内置认证后端时，新增对指定命名空间用户的支持。现在，属于某个命名空间的 MQTT 客户端在进行认证时，仅会使用其命名空间下的数据。

#### 数据集成

- [#15905](https://github.com/emqx/emqx/pull/15905) 对于 HTTP Action，HTTP 请求超时时间现在与 `resource_opts.request_ttl` 保持一致。此前，该值固定为 30 秒，且不可配置。
- [#16169](https://github.com/emqx/emqx/pull/16169) 更新 `parquer` 依赖，以支持将 `timestamp` 类型的 Iceberg 数据编码为 Parquet 文件。
- [#16179](https://github.com/emqx/emqx/pull/16179) 在 Azure Blob Storage 和 S3 Action 的聚合模式下，新增对写入 Parquet 文件的支持。
- [#16267](https://github.com/emqx/emqx/pull/16267) EMQX 新增与 AWS Timestream for InfluxDB 数据集成。
- [#16290](https://github.com/emqx/emqx/pull/16290) 在使用 Kafka 和 Confluent 生产者连接器时，新增对 OAuth 认证的支持。
- [#16316](https://github.com/emqx/emqx/pull/16316) 调整了多个动作的默认批处理大小和时间参数。之前支持批处理操作的动作，其默认配置已被提高，使批处理行为成为默认行为。
- [#16372](https://github.com/emqx/emqx/pull/16372) 为 InfluxDB 和 AWS Timestream 连接器新增对 InfluxDB API v3 的支持。
- [#16396](https://github.com/emqx/emqx/pull/16396) EMQX 新增与 EMQX Tables 数据集成。

#### 持久化存储

- [#16136](https://github.com/emqx/emqx/pull/16136) 改进了持久化存储的资源管理和性能。

  引入了持久化存储数据库组（durable storage database group）的概念。某些资源（例如 memtable 大小和磁盘使用配额）可以在同一组的成员之间共享。

  新增以下指标（按数据库组统计）：

  - `emqx_ds_disk_usage`：SST 文件的总大小
  - `emqx_ds_write_buffer_memory_usage`：RocksDB 的 memtable 大小
  - `emqx_ds_total_trash_size`：垃圾 SST 文件占用的磁盘空间

  新增以下数据库组配置项：

  - `durable_storage.db_groups.<group>.storage_quota`：SST 文件大小的软配额
  - `durable_storage.db_groups.<group>.write_buffer_size`：最大 memtable 大小
  - `durable_storage.db_groups.<group>.rocksdb_nthreads_high` 与 `durable_storage.db_groups.<group>.rocksdb_nthreads_low`：RocksDB 线程池大小

  新增告警 `db_storage_quota_exceeded:<DB>`，当存储配额被超出时触发。更多信息请参考文档中的“存储配额”章节。

  默认的会话检查点（checkpoint）间隔已更改为 15 秒。

- [#16286](https://github.com/emqx/emqx/pull/16286) 优化了默认的持久化存储配置以降低 CPU 负载。该 PR 禁用了未使用订阅功能的数据库的订阅支持。

#### 命名空间

- [#16211](https://github.com/emqx/emqx/pull/16211) 新增对命名空间级别指标的初始支持。

  - 接收的消息
    - 数量
    - 字节数
  - 发送的消息
    - 数量
    - 字节数
  - 会话数量
  - 数据集成
    - 触发的 Action 数量
  - 数据库记录数
  - AuthN 记录数
  - AuthZ 记录数

  位于受管命名空间中的客户端将更新上述命名空间指标，同时仍会更新全局指标。

  这些指标以 Prometheus 格式暴露，可通过 `GET /prometheus/ns/stats` 端点采集。通过指定查询参数 `ns=NAMESPACE`，仅返回指定命名空间的数据；若省略该参数，则返回所有命名空间的数据。命名空间将作为标签添加到指标中。

- [#16314](https://github.com/emqx/emqx/pull/16314) 全局管理员用户在列出命名空间资源（连接器 / Source / 动作 / 规则）时，默认可查看所有命名空间的资源。在执行 CRUD 操作时，可通过传递 `ns=NS` 查询参数聚焦于某一特定命名空间。若仅希望列出全局命名空间资源，可省略 `ns` 参数并传递 `only_global=true`。

  命名空间资源现在会返回 `namespace` 字段，用于标识资源所属的命名空间；对于全局资源，该字段为 `null`，以区别于可能存在的名为 `"global"` 的命名空间。

- [#16360](https://github.com/emqx/emqx/pull/16360) 新增 `GET /mt/ns/:ns/metrics` 接口，用于以 JSON 格式返回指定命名空间的指标数据。

- [#16472](https://github.com/emqx/emqx/pull/16472) 新增配置项 `namespace_as_mountpoint`，用于启用基于客户端命名空间的自动主题隔离。

  启用后，如果监听器未配置挂载点（mountpoint），EMQX 将使用客户端的命名空间（来自 `client_attrs.tns`）作为主题挂载点。

  对于 PUBLISH、SUBSCRIBE、UNSUBSCRIBE 以及 Will 消息，主题会自动加上命名空间前缀；在向客户端投递消息时，该前缀会被移除。

  如果监听器已配置挂载点，则该设置会被忽略，以确保现有配置优先生效。

#### 可观测性

- [#16135](https://github.com/emqx/emqx/pull/16135) 为 `GET /monitor_current` HTTP API 新增两个指标及其对应速率：`rules_matched` 和 `actions_executed`，分别用于统计规则匹配次数以及动作的执行速率（成功与失败之和）。
- [#16213](https://github.com/emqx/emqx/pull/16213) 将 MQTT 客户端 ID 添加为进程标签，使崩溃日志（包括最大堆内存和强制关闭错误）中包含客户端 ID，便于故障排查。

#### 性能

- [#16368](https://github.com/emqx/emqx/pull/16368) 将底层运行时系统从 Erlang/OTP 27 升级至 Erlang/OTP 28。
- [#16377](https://github.com/emqx/emqx/pull/16377) 减少了预分配的指标计数器数量，从而降低内存使用，尤其是在大量使用命名空间的集群中效果更明显。

#### MQTT over QUIC

- [#16133](https://github.com/emqx/emqx/pull/16133) MQTT over QUIC：新增基于数据报（datagram）的连接探测支持。

  EMQX 现在支持客户端发送零长度的数据报包以测试连接可达性。客户端也可以发送非零长度的数据报包，但这些数据报将被 EMQX 忽略。

### 修复

#### 核心 MQTT 功能

- [#16344](https://github.com/emqx/emqx/pull/16344) 修复了在处理 `request-response-information` 属性时，由于类型不匹配导致 MQTT v5 连接发生崩溃的问题。
- [#16354](https://github.com/emqx/emqx/pull/16354) 将 MQTT v5 `request-response-information` 模式类型修复回移植（backport）到 6.0.x 发布分支。

#### 访问控制

- [#16308](https://github.com/emqx/emqx/pull/16308) 修复了在从 5.3.0 之前版本升级 EMQX 后，由于登录用户数据库记录不兼容，导致无法启用多因素认证（MFA）的问题。
- [#16446](https://github.com/emqx/emqx/pull/16446) 修复了在使用 SCRAM 时认证器指标统计不正确的问题，其中每次认证尝试会将 “Total” 计数增加两次，而 “Success” 计数不会增加。

#### 数据集成

- [#16265](https://github.com/emqx/emqx/pull/16265) 健康检查现在仅验证分配给当前 EMQX 节点的分区的 leader 连接性，从而避免不必要的空闲连接和误报告警。

  之前，Kafka Source 连接器会对所有分区执行 leader 连接性检查。在集群部署中，每个节点仅拥有部分分区，未分配分区的 leader 连接会保持空闲。由于 Kafka 会在连接空闲一段时间后关闭连接（默认 10 分钟），这可能导致错误的连接性告警。

- [#16352](https://github.com/emqx/emqx/pull/16352) 将 Apache Pulsar 客户端升级至 2.1.2。当 Pulsar Producer Action 的 `batch_size` 配置为 `1` 时，生产者现在会对单条消息进行编码，而不是将其作为单元素批处理进行编码。这使得消费者可以使用 Key Share 策略进行负载分担。

- [#16383](https://github.com/emqx/emqx/pull/16383) 之前，在使用 IoTDB Connector 的 RestAPI 驱动时，健康检查过程中不会校验凭据。现在，在 IoTDB 连接器健康检查期间会发送一个空操作（no-op）查询，从而能够及早发现客户端凭据配置错误的问题。

#### 消息队列

- [#16270](https://github.com/emqx/emqx/pull/16270) 修复了 EMQX 消息队列消费者在关闭处理流程中的一个问题。

#### 集群

- [#16453](https://github.com/emqx/emqx/pull/16453) 将 `gen_rpc` 升级至 `3.5.1`。

  在升级 `gen_rpc` 之前，如果对端节点不可达，EMQX 可能会由于连接超时而产生大量延迟出现的崩溃日志。新的 `gen_rpc` 版本消除了这种长尾日志行为，并将崩溃日志转换为更易读的 `error` 日志，同时对频繁出现的 `"failed_to_connect_server"` 日志进行了限流处理，以避免日志刷屏。

#### 集群连接

- [#16269](https://github.com/emqx/emqx/pull/16269) 修复了集群连接路由复制协议恢复流程中的一个问题，该问题会在远端仍需要重新引导（re-bootstrap）的情况下错误地跳过该步骤。
- [#16317](https://github.com/emqx/emqx/pull/16317) 修复了集群连接垃圾回收逻辑中的一个问题，该问题在清理过期路由复制状态时，可能会意外地将仍然有效的路由从内部路由表中移除。该问题仅在配置了多个相互独立的集群连接，且其中部分连接长时间不可用时才会发生。

#### 可观测性

- [#16417](https://github.com/emqx/emqx/pull/16417) 减少了在发生资源异常（`resource_exception`）时生成的日志数量。这些日志现在会被限流处理，同时会对其中一些可能体量较大的字段进行脱敏。
- [#16434](https://github.com/emqx/emqx/pull/16434) 现在，清除某个告警名称将会在所有节点上同步清除该告警。此前，通过 HTTP API 强制停用告警时，并不会在所有节点上将其清除。

#### 网关

- [#16425](https://github.com/emqx/emqx/pull/16425) 改进了通过 HTTP API 创建或更新网关时返回的错误信息。

#### 其他

- [#16397](https://github.com/emqx/emqx/pull/16397) 在监听器启动前新增 TLS 证书校验。如果监听器配置了无效证书，将快速失败（fail-fast）。
- [#16311](https://github.com/emqx/emqx/pull/16311) 更新了错误码，将拼写错误的 `REST_FAILED` 更正为 `RESET_FAILED`。

## 6.0.2

*发布日期: 2026-01-16*

在升级到 EMQX 6.0.2 之前，请务必查阅不兼容变更和已知问题。

### 增强

#### 安全

- [#16461](https://github.com/emqx/emqx/pull/16461) EMQX 现已支持通过无状态会话票据实现 TLS 1.3 会话恢复，使客户端无需在服务端保存会话状态即可恢复 TLS 连接。

  **配置说明**

  - **节点级配置**：`node.tls_stateless_tickets_seed`，用于生成 TLS 1.3 无状态会话票据的密钥种子。
  - **监听器级配置**：`listeners.ssl.<name>.ssl_options.session_tickets`，用于启用 TLS 1.3 会话恢复，支持以下取值：
    - `disabled`（默认）
    - `stateless`
    - `stateless_with_cert`（在会话票据中包含证书信息）

  **注意事项**

  - 只有在同时满足以下条件时才会生成会话票据：
    - 已配置 `node.tls_stateless_tickets_seed`（且值非空）
    - 在监听器的 SSL 选项中启用了 `session_tickets`
  - 如果启用了 `session_tickets`，但 `node.tls_stateless_tickets_seed` 为空，则不会生成会话票据，并且在监听器启动时会输出错误日志。

  此 PR 还修复了 TLS 1.2 会话恢复配置的问题：之前，SSL 监听器的 `reuse_sessions` 选项未生效，即 EMQX 总是尝试启用 TLS 1.2 会话恢复。现在可以将其关闭。请注意，从 6.2.0 版本开始，TLS 1.2 会话恢复将默认禁用。

#### 规则引擎

- [#16524](https://github.com/emqx/emqx/pull/16524) 增强了规则引擎 SQL 中的 Base64 编码与解码函数，新增对填充控制和 URL 安全模式的支持。

  `base64_encode` 和 `base64_decode` 函数现已支持可选参数，用于控制编码和解码行为：

  - **`no_padding`**：在编码或解码时不使用填充字符（`=`）。适用于需要移除 Base64 填充，或解码不包含填充字符的 Base64 字符串的场景。
  - **`urlsafe`**：使用 URL 安全的 Base64 编码/解码方式，将 `+` 替换为 `-`，`/` 替换为 `_`，使编码结果可直接用于 URL 而无需额外转义。

  这些选项可以单独使用，也可以任意组合使用，参数顺序不影响结果。

  **规则 SQL 示例：**

  不使用填充字符进行编码：

  ```sql
  SELECT base64_encode(payload, 'no_padding') AS encoded FROM "t/#"
  ```

  使用 URL 安全字符进行编码：

  ```sql
  SELECT base64_encode(payload, 'urlsafe') AS encoded FROM "t/#"
  ```

  同时使用无填充和 URL 安全选项进行编码：

  ```sql
  SELECT base64_encode(payload, 'no_padding', 'urlsafe') AS encoded FROM "t/#"
  ```

  解码 URL 安全的 Base64 字符串：

  ```sql
  SELECT base64_decode(payload, 'urlsafe') AS decoded FROM "t/#"
  ```

  解码不带填充的 URL 安全 Base64 字符串：

  ```sql
  SELECT base64_decode(payload, 'urlsafe', 'no_padding') AS decoded FROM "t/#"
  ```

- [#16533](https://github.com/emqx/emqx/pull/16533) 新增两个可变参数表达式辅助函数 `json_value` 和 `jwt_value`，用于通过点分隔的键路径从 JSON 数据和 JWT tokens 中提取值。

  - **`json_value`**：通过点分隔的键路径遍历嵌套结构，从 JSON 二进制字符串中提取字段值。
  - **`jwt_value`**：对 JWT 的 payload 进行解码，并使用相同的点分隔路径语法提取声明值。

  **示例：**

  - 如果 `username` 包含一个 JSON 对象，可以通过以下方式访问其中的嵌套字段：`json_value(username, 'shop.floor')`。
  - 如果 `password` 包含一个带有自定义声明的 JWT，可以通过以下方式提取嵌套值：`jwt_value(password, 'client_attrs.unitid')`。

- [#16539](https://github.com/emqx/emqx/pull/16539) 支持在规则引擎函数 `spb_decode` 中跟踪 Sparkplug B 指标别名（metric alias）。

  当设备或边缘节点（Edge of Network，EoN）发布其 `NBIRTH` 或 `DBIRTH` 消息后，EMQX 会记录这些消息中定义的指标别名与名称的映射关系。随后，对来自同一会话的 `NDATA` 或 `DDATA` 消息应用 `spb_decode` 时，系统会自动还原原始的指标名称，并将其包含在解码结果中。

  注意：在执行回退动作时，映射关系在运行环境中不可用。这意味着，如果回退动作将未解码的 `DDATA`/`NDATA` 负载重新发布到 Sparkplug B `DDATA`/`NDATA` 主题，指标 `name` 字段将不会通过别名映射填充。

#### 持久存储

- [#16136](https://github.com/emqx/emqx/pull/16136) 改进了持久存储的资源管理和性能。

  引入了持久存储数据库组（durable storage database group）的概念。某些资源（例如 memtable 大小和磁盘使用配额）可以在同一组的成员之间共享。

  新增以下指标（按数据库组统计）：

  - `emqx_ds_disk_usage`：SST 文件的总大小
  - `emqx_ds_write_buffer_memory_usage`：RocksDB 的 memtable 大小
  - `emqx_ds_total_trash_size`：垃圾 SST 文件占用的磁盘空间

  新增以下数据库组配置项：

  - `durable_storage.db_groups.<group>.storage_quota`：SST 文件大小的软配额
  - `durable_storage.db_groups.<group>.write_buffer_size`：最大 memtable 大小
  - `durable_storage.db_groups.<group>.rocksdb_nthreads_high` 与 `durable_storage.db_groups.<group>.rocksdb_nthreads_low`：RocksDB 线程池大小

  新增告警 `db_storage_quota_exceeded:<DB>`，当存储配额被超出时触发。更多信息请参考文档中的“存储配额”章节。

  默认的会话检查点（checkpoint）间隔已更改为 15 秒。

- [#16286](https://github.com/emqx/emqx/pull/16286) 优化了默认的持久化存储配置以降低 CPU 负载。该 PR 禁用了未使用订阅功能的数据库的订阅支持。

#### 性能

- [#16413](https://github.com/emqx/emqx/pull/16413) 通过减少对 MQTT 会话进程的冗余监控，提升了订阅处理性能。

### 修复

#### 核心 MQTT 功能

- [#16354](https://github.com/emqx/emqx/pull/16354) 修复了在处理 `request-response-information` 属性时，由于类型不匹配导致 MQTT v5 连接发生崩溃的问题。
- [#16515](https://github.com/emqx/emqx/pull/16515) 修复了当 Broker 发送的消息超过客户端声明的 `Maximum-Packet-Size` 时，WebSocket 连接可能发生崩溃的问题。
- [#16569](https://github.com/emqx/emqx/pull/16569) 修复了一个罕见的竞态条件，该问题可能导致在高系统负载下用于抖动检测的 `emqx_flapping` 进程崩溃。

#### 数据集成

- [#16265](https://github.com/emqx/emqx/pull/16265) 健康检查现在仅验证分配给当前 EMQX 节点的分区的 leader 连接性，从而避免不必要的空闲连接和误报告警。

  之前，Kafka Source 连接器会对所有分区执行 leader 连接性检查。在集群部署中，每个节点仅拥有部分分区，未分配分区的 leader 连接会保持空闲。由于 Kafka 会在连接空闲一段时间后关闭连接（默认 10 分钟），这可能导致错误的连接性告警。

- [#16542](https://github.com/emqx/emqx/pull/16542) 修复了当 Kafka 过载时 Kafka 生产者连接可能过早断开的问题，该问题会导致大量生产请求重试。

  现在，生产请求的超时时间会自动设置为至少为元数据请求超时时间的两倍，且最小值为 30 秒。这在元数据请求耗时超出预期时可以减少不必要的重连和重试，尤其是在元数据请求超时时间被配置为较小值的情况下。

- [#16352](https://github.com/emqx/emqx/pull/16352) 将 Apache Pulsar 客户端升级至 2.1.2。当 Pulsar Producer 动作的 `batch_size` 配置为 `1` 时，生产者现在会对单条消息进行编码，而不是将其作为单元素批处理进行编码。这使得消费者可以使用 Key Share 策略进行负载分担。

- [#16383](https://github.com/emqx/emqx/pull/16383) 改进了在使用 REST API 驱动时 IoTDB 连接器的健康检查。

  之前，健康检查过程中不会校验客户端凭据。现在，健康检查会发送一个轻量级的空操作（no-op）查询，从而可以及早发现客户端凭据配置错误的问题。

- [#16507](https://github.com/emqx/emqx/pull/16507) 修复了 MQTT Source 在其连接器重新连接后停止接收消息的问题。

  之前，当 MQTT Source 的连接器从连接丢失中恢复后，其订阅主题不会被重新订阅，导致 Source 在连接器重启之前无法继续工作。现在，Source 会在重新连接时自动重新订阅。

#### 集群

- [#16269](https://github.com/emqx/emqx/pull/16269) 修复了集群连接路由复制协议恢复流程中的一个问题，该问题会在远端仍需要重新引导（re-bootstrap）的情况下错误地跳过该步骤。

- [#16317](https://github.com/emqx/emqx/pull/16317) 修复了集群连接垃圾回收逻辑中的一个问题，该问题可能会在清理过期的路由复制状态时，错误地将仍然有效的路由从内部路由表中移除。

  该问题仅会出现在存在多个相互独立的集群连接部署场景中，并且其中部分连接长时间处于断开状态时。

- [#16465](https://github.com/emqx/emqx/pull/16465) 将 `gen_rpc` 升级至 `3.5.1`。

  在升级 `gen_rpc` 之前，如果对端节点不可达，EMQX 可能会由于连接超时而产生大量延迟出现的崩溃日志。新的 `gen_rpc` 版本消除了这种长尾日志行为，并将崩溃日志转换为更易读的 `error` 日志，同时对频繁出现的 `"failed_to_connect_server"` 日志进行了限流处理，以避免日志刷屏。

- [#16544](https://github.com/emqx/emqx/pull/16544) 提升了集群自动清理流程的健壮性。此前，如果在节点初次启动时禁用了 autoclean 功能，后续即使修改配置也无法将其激活。

#### 升级

- [#16308](https://github.com/emqx/emqx/pull/16308) 修复了一个问题：由于登录用户数据库记录不兼容，从早于 5.3.0 的版本升级 EMQX 后，无法启用多因素认证（MFA）。

#### 配置管理

- [#16397](https://github.com/emqx/emqx/pull/16397) 在监听器启动之前新增了 TLS 证书和私钥文件校验。

  在解析 SSL 监听器配置时新增了一些基本校验，如果发现无效的 PEM 文件，会输出错误级别的日志。例如：`invalid_pem_file_ignored` 和 `bad_keyfile_ignored`。这使得管理员能够在启动/重新配置时观察到错误，而不是在排查 TLS 握手失败时才发现问题。

#### 访问控制

- [#16423](https://github.com/emqx/emqx/pull/16423) 新增了在认证过程中校验 JWT `aud`（audience）声明的支持。

  当在 `verify_claims` 中配置了 `aud` 声明时，JWT 必须包含有效的 `aud` 值，且支持字符串和数组两种格式：

  - 如果 `aud` 为字符串，则必须与配置的值完全匹配。
  - 如果 `aud` 为数组，则数组中至少有一个元素与配置的值匹配。
  - 空字符串或空数组将导致校验失败。
  - 当在 `verify_claims` 中配置了 `aud`，但 JWT 中缺少该声明时，也会导致校验失败。

- [#16459](https://github.com/emqx/emqx/pull/16459) 修复了 SCRAM 认证 HTTP API 中的一个问题。此前，在用户创建 API 调用中，返回的已创建用户的用户 ID 不正确。

#### 可观测性

- [#16417](https://github.com/emqx/emqx/pull/16417) 降低了 `resource_exception` 事件产生的日志量。当发生资源异常时生成的日志现在会被限流，并且会对可能较大的项进行脱敏处理，以防止日志输出过多。

- [#16537](https://github.com/emqx/emqx/pull/16537) 修复了由某些 `gen_rpc` 错误消息触发的日志格式化器崩溃问题。

  之前，当 `gen_rpc` 记录特定错误（例如传输超时）时，EMQX 可能会因出现 “FORMATTER CRASH” 错误而崩溃。现在，日志格式化器可以安全地处理这些错误消息而不会导致崩溃。

## 6.0.1

*发布日期: 2025-11-11*

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
