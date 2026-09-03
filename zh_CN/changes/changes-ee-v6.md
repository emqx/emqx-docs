# EMQX 企业版 v6 版本

## 6.3.0

*发布日期: 2026-09-03*

在升级到 EMQX 6.3.0 之前，请务必查阅不兼容变更和已知问题。

### 增强

#### 核心 MQTT 功能

- [#16694](https://github.com/emqx/emqx/pull/16694) 支持从 QUIC 连接中提取对端证书，从而可在 QUIC mTLS 监听器上使用 `peer_cert_as_username`。

- [#17307](https://github.com/emqx/emqx/pull/17307) 为监听器新增按客户端限制 SUBSCRIBE 报文速率的功能，默认禁用。配置有限速率后，EMQX 会对超出限制的报文返回包含 Quota Exceeded 原因码的 SUBACK，且不处理这些报文。命名空间可配置独立速率，并覆盖监听器级别的速率。

- [#17546](https://github.com/emqx/emqx/pull/17546) [#18477](https://github.com/emqx/emqx/pull/18477) 新增 `mqtt.max_session_expiry_interval` 配置，用于限制 MQTT 5.0 客户端通过 `Session-Expiry-Interval` 属性请求的会话过期间隔。如果客户端请求的间隔超过该上限，服务端会静默截断，并在 CONNACK 中返回截断后的值。该配置默认值为 `infinity`（不限制），保留此前行为。此配置不影响 MQTT 3.1.1/3.1 客户端，其会话过期间隔仍完全由服务端通过 `mqtt.session_expiry_interval` 控制。

  此上限也适用于客户端在 DISCONNECT 报文中提供的 Session Expiry Interval，因此客户端断开连接时无法将会话过期间隔延长到配置的上限之外。

- [#17603](https://github.com/emqx/emqx/pull/17603) 支持通过 `mqtt.client_attrs_init` 中的 `cert_san.dns`、`cert_san.ip`、`cert_san.email` 和 `cert_san.uri`，从直接连接的 TLS 客户端证书中提取主题备用名称并写入 MQTT 客户端属性。

- [#17854](https://github.com/emqx/emqx/pull/17854) 为改善消息延迟和资源使用，Unix 系统上的 MQTT TCP 监听器现在默认使用 `socket` 作为 `tcp_backend`。仍可通过设置 `tcp_backend = gen_tcp` 使用 `gen_tcp` 后端；Windows 上仍默认使用 `gen_tcp`。

- [#17870](https://github.com/emqx/emqx/pull/17870) 改进内存会话向处理缓慢或发生拥塞的订阅者投递消息的行为。

  - EMQX 现在会跟踪连接发送队列的拥塞情况，并在需要时通过会话消息队列投递 QoS 0 消息，不再继续将消息直接推送到拥塞的连接。
  - 会话消息队列达到容量上限时，现在会优先淘汰较早的 QoS 0 消息，帮助 QoS 1 和 QoS 2 消息在 QoS 0 突发流量期间继续投递。
  - 修复会话投递达到速率限制时的投递顺序，避免后续消息越过先进入队列的消息。
  - 改进基于 socket 的连接对 `send_timeout` 的处理，仅在套接字队列超过水位线后启动超时计时。

#### 访问控制

- [#17145](https://github.com/emqx/emqx/pull/17145) 授权数据源现在支持基于 Variform 的前置条件。仅当表达式求值为 `true` 时，才会调用配置了前置条件的数据源。因此，可以根据客户端属性、操作和主题等客户端及请求上下文选择不同的授权后端。

- [#17487](https://github.com/emqx/emqx/pull/17487) 加强 Dashboard 管理员密码和 API 密钥 Secret 的哈希处理。

  Dashboard 管理员密码现在使用 PBKDF2-HMAC-SHA256（600,000 次迭代）和 16 字节随机盐进行哈希。API 密钥 Secret 使用相同的自描述存储格式和 16 字节随机盐，但不进行迭代拉伸，因此每次 HTTP API 认证仍可保持较低开销。这两类凭据均不再使用此前的方案（带 2 字节盐的单次 SHA-256）。

  现有哈希仍可用于认证。用户下次更改密码或重新创建 API 密钥时，哈希会改写为新格式。

- [#17671](https://github.com/emqx/emqx/pull/17671) 认证和授权拒绝日志现在会标明具体后端。

  认证器返回错误并导致 EMQX 拒绝客户端时，EMQX 会生成警告日志，标明作出拒绝的认证器 ID 和 Provider。授权数据源拒绝操作时，EMQX 会以警告级别记录拒绝信息（此前仅在客户端追踪中可见），包括数据源类型、模块、主题和操作。

  在配置多个认证器或授权数据源的部署中，无需先启用客户端追踪即可确定由哪个后端作出决定。新日志会按认证器和授权数据源分别限流，避免日志泛滥。

- [#18130](https://github.com/emqx/emqx/pull/18130) HTTP 认证器和授权器的 URL 主机现在支持模板变量，例如 `https://${client_attrs.tns}.auth.example.com/authn`，无需外部路由层即可使用按租户划分的认证和授权端点。

  `hostname_resolution` 配置用于控制 URL 主机的处理方式。默认值 `static` 保留此前行为：使用固定主机名，并在配置时建立持久连接池。设置为 `dynamic` 后（URL 主机包含模板占位符时必须使用此值），EMQX 会为每个请求解析主机名，通过按请求建立的连接发送检查，并向该主机应用已配置的 TLS 选项，包括对端验证。此时，`pool_size` 用于限制可保留以供复用的空闲连接数（`0` 表示不复用），流水线选项不适用。

  对于模板化主机，新增的 `allowed_hosts` 配置必须列出主机可能渲染成的主机名，可使用精确名称或 `*.suffix` 通配符模式。如果渲染后的主机名不在列表范围内，EMQX 不会发送请求，检查将失败。使用字面量主机名的 URL 仍与此前一样使用连接池。

- [#18239](https://github.com/emqx/emqx/pull/18239) [#18371](https://github.com/emqx/emqx/pull/18371) 除客户端 ID 外，抖动检测现在还可基于连接客户端的用户名和源 IP 地址执行。

  每个维度都有独立的检测窗口、连接尝试次数阈值和封禁时长，分别配置在 `flapping_detect.by_clientid`、`flapping_detect.by_username` 和 `flapping_detect.by_peerhost` 下，其中用户名和源 IP 检测默认禁用。客户端 ID 维度此前通过扁平的 `flapping_detect` 字段（`enable`、`window_time`、`max_count`、`ban_time`）配置；这些字段现已弃用，但仍可使用，并会自动映射到 `by_clientid`，因此现有配置无需更改。用户名或源 IP 地址在检测窗口内超过阈值后会被临时封禁：新的连接尝试会在认证前被拒绝，已连接的客户端不受影响。封禁条目会自动过期，也可通过 `/banned` REST API 查看或提前移除。每个条目包含封禁类型（`as` 为 `clientid`、`username` 或 `peerhost`）及 `by` = `flapping detector`，列表可按类型筛选。

  计数器按 Zone 和维度分别保存，因此配置不同检测窗口的 Zone 不会丢弃彼此仍有效的计数器。

  新增指标 `flapping.detected.clientid`、`flapping.detected.username` 和 `flapping.detected.peerhost`，分别统计各维度触发抖动检测的次数；`client.banned` 统计因存在有效封禁条目而被拒绝的连接尝试。

#### 多租户

- [#17454](https://github.com/emqx/emqx/pull/17454) `/api/v5/prometheus/data_integration` 返回的 Prometheus 数据现在限定在请求方所属的命名空间内。全局管理员可查看所有命名空间的数据，也可按命名空间筛选。

#### 数据集成

- [#17129](https://github.com/emqx/emqx/pull/17129) 为基于 GCP 的连接器（GCP PubSub Producer、GCP PubSub Consumer 和 BigQuery）新增 Attached Service Account 认证。当 EMQX 在附加了服务账号的 GCP VM 上运行时，可查询内部元数据端点，为这些连接器获取 Token。

- [#17222](https://github.com/emqx/emqx/pull/17222) EMQX 新增与 Bigtable 的数据集成，支持向 Bigtable 追加数据。

- [#17547](https://github.com/emqx/emqx/pull/17547) Kafka Producer 和 Consumer 连接器新增 AWS IAM Roles Anywhere 支持。可将连接器配置为使用凭据辅助进程公开的 HTTP 端点。

  凭据辅助进程必须处于运行状态，并且 EMQX 能够访问该进程。详情请参阅 [AWS IAM Roles Anywhere 凭据辅助工具文档](https://docs.aws.amazon.com/rolesanywhere/latest/userguide/credential-helper.html#credential-helper-serve)。

- [#17783](https://github.com/emqx/emqx/pull/17783) PostgreSQL 系列连接器新增 `application_name` 选项。默认值为 `emqx`，并作为 PostgreSQL 启动参数发送，以便在 PostgreSQL 活动视图和日志中识别连接器会话。该值的长度必须为 1 至 63 字节，且不能包含零字节。

- [#18119](https://github.com/emqx/emqx/pull/18119) 除按大小轮换外，Disk Log 连接器现在还支持按时间轮换文件。

  连接器配置新增可选的 `rotation` 设置：

  - `rotation.period`：可选值为 `none`（默认值）、`day` 或 `hour`。设置为 `day` 或 `hour` 后，连接器会在每个周期边界启动一组独立的日志文件，并将周期日期戳（`YYYYMMDDHH`）编码到文件名中。例如，按日轮换 `mqtt-trace.log` 时，文件名为 `mqtt-trace-2026062400.log.1`。每个周期内仍会按大小轮换（`max_file_size` / `max_file_number`）。
  - `rotation.retention_period`：保留此前周期文件的时长，例如 `30d`。每次周期轮换后，会自动删除日期戳超出保留期的文件。默认值为 `infinity`，即无限期保留文件。
  - `rotation.timezone`：用于确定周期边界的时区，可选值为 `UTC`（默认值）、`local` 或 `+02:00` 等固定偏移。

  未配置 `rotation` 或设置 `rotation.period = none` 时，默认行为保持不变。

- [#18319](https://github.com/emqx/emqx/pull/18319) BigQuery 动作现在支持指定动作级别的项目 ID。

- [#18624](https://github.com/emqx/emqx/pull/18624) 新增 `emqx ctl actions show` 和 `emqx ctl actions status` 命令。无需 REST API 凭据或网络调用，即可以 JSON 格式报告本地节点上的动作状态。

  `status` 输出由 `{"<type>:<name>": "<status>"}` 条目组成的紧凑 JSON 数组；`show` 输出与 `GET /api/v5/actions/{id}` 相同的信息，但仅针对本地节点，并会隐去连接器 Secret。两个命令均支持使用 `--name <type:name>` 选择单个动作，使用 `--ns <namespace>` 选择命名空间；默认选择全局命名空间中的所有动作。

  这适用于按节点执行的就绪探测，因为 REST API 聚合整个集群的 `status` 字段，无法反映本地节点上的动作是否已准备好接收流量。

#### 可观测性

- [#17493](https://github.com/emqx/emqx/pull/17493) 新增会话缓冲区可观测能力：客户端 API 现在会报告 `total_payload_bytes`；`sysmon.session.total_payload_bytes_high_watermark` 可针对 Payload 字节数超过阈值的会话发出限流警告；`emqx ctl session-top` 可按 `total_payload_bytes` 或 `mqueue_length` 导出集群中排名靠前的会话。

- [#17582](https://github.com/emqx/emqx/pull/17582) Prometheus 指标采集升级到 `prometheus.erl` 6.1.2，提高性能和可扩展性。

  `vm_dist`、`vm_statistics`、`vm_system_info` 和 `vm_memory` 采集器现在默认启用。Prometheus 输出中已移除废弃的 `emqx_vm_process_messages_in_queues` 指标。

- [#17607](https://github.com/emqx/emqx/pull/17607) [#17998](https://github.com/emqx/emqx/pull/17998) 新增 v2 主题指标功能，支持命名集合、通配符主题过滤器、命名空间归属、REST CRUD 操作和 Prometheus 抓取端点。

  - `/api/v5/mqtt/topic_metrics2/:name` 下的新路由允许运维人员使用自定义名称（`my-pressure`、`vehicle-events` 等）注册主题指标集合，不再使用主题本身作为标识符。主题过滤器现在支持通配符（`t/#`、`sensor/+/temp`），一条发布消息可以匹配多个集合并递增其计数。
  - 集合按命名空间划分：命名空间管理员创建的集合仅统计 `client_attrs.tns` 匹配的发布客户端；非命名空间管理员创建的全局集合会统计所有发布客户端。命名空间管理员只能查看和修改自己的集合。全局管理员可在单个集合端点（`GET`、`DELETE`、`PUT .../reset`）中传递 `ns` 查询参数，以访问指定命名空间的集合。命名空间管理员传递其他命名空间名称时会收到 `403 Forbidden`；省略 `ns` 时仍使用请求方自己的命名空间。
  - 计数器以 Prometheus exposition 格式通过 `/api/v5/prometheus/topic_metrics` 公开，并带有 `name`、`topic_filter` 和 `namespace` 标签。可使用 Prometheus `rate()` 计算速率。
  - v1 API（`/api/v5/mqtt/topic_metrics` 和 `/api/v5/mqtt/topic_metrics/:topic`）保持不变并可继续使用；其 Swagger 规范中已将其标记为弃用，集成应改用 v2。

- [#18148](https://github.com/emqx/emqx/pull/18148) 支持通过 OpenTelemetry 集成 Dynatrace。支持的信号包括 Traces 和 Logs，集成使用 OAuth2 Token 认证。

#### 部署与安全

- [#17381](https://github.com/emqx/emqx/pull/17381) OpenAPI 规范端点现在默认要求认证，包括 `GET /api-docs/swagger.json`、`GET /api-spec.json`、`GET /api-spec.md` 和 `GET /api-spec/:tag[/:name]`。

  未认证请求会收到 401 响应，其中包含 `WWW-Authenticate` 响应头和一个最小但有效的 OpenAPI 文档（`/api-spec.md` 返回对应的 Markdown）。该文档列出支持的安全方案和公开的引导端点（`POST /api/v5/login`、`GET /api/v5/status`），使调用方无需让 Dashboard 匿名公开完整 API 接口面即可了解如何认证。

  Dashboard 的 `api-spec.html` 浏览器仍可匿名加载，并使用现有会话 Cookie 或 Token 获取规范。

- [#17407](https://github.com/emqx/emqx/pull/17407) [#17808](https://github.com/emqx/emqx/pull/17808) 新增 Feature Gates。

  支持通过 `EMQX_FEATURES` 环境变量指定有限的功能集来启动 EMQX。无效的预设或功能名称会阻止节点启动；依赖功能会自动启用。

  提供以下两个预设：

  - `FULL`：默认值，使用所有可用功能启动 EMQX。
  - `ESSENTIAL`：使用最小功能集启动 EMQX，包括核心 MQTT Broker、认证和授权。

  可用功能如下：

  - `dashboard`：Dashboard UI（包括 SSO 和 RBAC）、REST API。
  - `data_integration`：连接器、动作、Source 和规则引擎。
  - `message_transformation`：消息转换。
  - `schema_validation`：Schema 验证。
  - `schema_registry`：Schema Registry。
  - `gateways`：网关协议。
  - `cluster_link`：集群连接。
  - `multi_tenancy`：多租户和命名空间。
  - `ai`：AI 功能（A2A Registry、AI Completion）。
  - `metrics`：导出 Prometheus 指标。
  - `mqtt_extensions`：MQTT 扩展，包括延迟发布、主题重写、自动订阅、慢订阅、消息队列和消息流。
  - `plugins`：用于安装和管理第三方插件的插件框架。

  以下功能不能单独启用，仅在使用完整预设时启用：

  - `file_transfer`：MQTT 文件传输扩展。
  - `exhook`：外部 gRPC Hook。
  - `opentelemetry`：OpenTelemetry Exporter。

- [#17768](https://github.com/emqx/emqx/pull/17768) 支持通过 `file://` URL 从文件读取 `node.cookie`。

  运维人员现在可设置 `node.cookie = "file:///path/to/cookie"`，或让 `EMQX_NODE__COOKIE` 环境变量指向 `file://` URL，避免在配置中以明文存储集群 Secret。引用路径可以是常规文件或 FIFO（命名管道），并在节点启动时读取一次。使用 FIFO 时，编排系统必须在每次启动时、调用其他任何 `emqx` 命令（例如 `emqx ctl`）之前将 Cookie 写入其中，因为后续命令会从已运行的节点获取 Cookie，不会重新读取文件。

  解析后的 Cookie 现在会直接传递给 Erlang VM，不再写入生成的 `data/configs/vm.*.args` 文件，因此启动期间不会将该 Secret 持久化到磁盘。

- [#17803](https://github.com/emqx/emqx/pull/17803) 使用 `EMQX_FEATURES=ESSENTIAL` 启动 EMQX 时，Erlang 代码加载模式现在默认为 `interactive`，禁用功能的 `.beam` 文件会在需要时加载，而不是在启动时全部加载。由于被跳过功能的模块不会驻留在内存中，此变更可显著降低 Essential 模式节点的常驻内存占用。仍可通过显式设置 `CODE_LOADING_MODE` 覆盖该模式。

- [#18451](https://github.com/emqx/emqx/pull/18451) 支持从 `etc/emqx.env`（RPM 和 DEB 安装中为 `/etc/emqx/emqx.env`，Docker 镜像中为 `/opt/emqx/etc/emqx.env`）读取启动环境变量。

  该文件以注释形式列出 `EMQX_FEATURES` 和 `EMQX_SECURITY_PROFILE` 的默认值，并说明其用途。这些变量在解析 `emqx.conf` 前读取，因此不能在 `emqx.conf` 中设置。`emqx` 命令每次调用时都会加载该文件，因此服务启动、前台启动和 `emqx ctl` 使用相同的值。文件中的值会覆盖继承的环境变量，升级安装包时会保留对该文件的修改。

- [#18452](https://github.com/emqx/emqx/pull/18452) `GET /nodes` 和 `GET /nodes/{node}` 返回的节点信息新增 `security_profile` 和 `feature_preset`。

  `security_profile` 的值为 `legacy` 或 `hardened`；`feature_preset` 的值为 `full`、`essential` 或 `custom`。两个值在节点启动时确定，因此列表视图可显示集群节点是否使用了不同设置。已停止的节点不报告这些字段。

- [#18453](https://github.com/emqx/emqx/pull/18453) 新增 `security_profile_divergence` 告警。

  运行 `hardened` 安全配置（`EMQX_SECURITY_PROFILE`）的节点会定期检查集群中其他运行节点的安全配置；如果有其他节点运行 `legacy` 安全配置，则触发告警。运行 `legacy` 安全配置的节点不会执行该检查；不支持安全配置的旧版 EMQX 节点按 `legacy` 处理。告警消息会列出 `legacy` 节点，告警处于活动状态时，其详情会持续更新当前节点列表。当所有运行节点均使用 `hardened`，或最后一个 `legacy` 节点离开集群后，告警会自动清除。

  在更改安全配置的滚动升级期间，短时间出现此告警属于预期行为。告警持续存在表示有节点未使用新的 `EMQX_SECURITY_PROFILE` 值重启。

- [#18471](https://github.com/emqx/emqx/pull/18471) 如果启动环境变量文件 `etc/emqx.env` 存在，`node_dump` 诊断脚本现在会收集其中的 `EMQX_FEATURES` 和 `EMQX_SECURITY_PROFILE` 设置，不会收集文件中的其他变量。

- [#18557](https://github.com/emqx/emqx/pull/18557) [#18609](https://github.com/emqx/emqx/pull/18609) 新增 `node.default_listener_address` 配置。当 MQTT 监听器、网关监听器和 Dashboard HTTP 监听器的 `bind` 未显式指定地址（例如仅配置端口 `bind = 1883`）时，该配置用于设置监听地址。有效值包括：`loopback`（绑定到 127.0.0.1）、`nodename`（绑定到 Erlang 节点名称中主机部分的地址；如果该值不是 IP 地址，则先进行解析）、`all`（绑定到 0.0.0.0）、IPv4/IPv6 字面量地址，或在启动时解析的主机名。未设置该选项时，仍由安全配置决定默认地址。显式配置的 `IP:port` bind 始终优先。也可通过 `EMQX_NODE__DEFAULT_LISTENER_ADDRESS` 环境变量设置此选项。

  官方 Docker 镜像会设置 `EMQX_NODE__DEFAULT_LISTENER_ADDRESS=all`，因此无论采用哪种安全配置，使用默认地址的监听器均可通过已发布的容器端口访问。

  监听器视图现在会报告 `resolved_address`，即监听器在其运行节点上实际绑定的 IP 地址，并同时保留现有的 `bind` 字段。`bind` 继续显示包括端口在内的配置值；`resolved_address` 显示安全配置或 `node.default_listener_address` 生效后 `bind` 解析到的不含端口的 IP 地址。对于 `bind = 1883` 等仅指定端口的 bind，该地址可能与 `bind` 中的地址不同。

  新增的 `resolved_address_from` 字段用于说明 `resolved_address` 取值的来源：监听器自身的 `bind` 已显式设置地址时为 `bind`；所有接口为 `0.0.0.0`；环回地址为 `127.0.0.1`；也可能为 `nodename`，或主机名/IP 字面量形式的 `node.default_listener_address` 值。

  两个字段均为节点本地值：`GET /api/v5/listeners/:id` 报告处理请求的节点上的值；`GET /api/v5/listeners` 在 `node_status` 下按节点报告。集群中相同 ID 的监听器在不同节点上可能解析为不同地址，例如 `node.default_listener_address` 设置为 `nodename` 时。

  `emqx ctl listeners` 会在现有 `listen_on` 字段旁输出这两个字段。

- [#18628](https://github.com/emqx/emqx/pull/18628) 数据备份导出现在会记录执行导出的节点所使用的安全配置。

  将在 `legacy` 安全配置下导出的备份恢复到运行 `hardened` 安全配置的节点时，可能会带入恢复后行为不同的数据和配置：

  - 仅指定端口的 MQTT、网关和 Dashboard HTTP 监听器 bind 会解析为环回地址，而不是所有接口。
  - 留空或禁用的认证器链会开始拒绝所有客户端，而不是允许客户端连接。
  - 恢复后仍使用默认密码的 Dashboard 账户无法登录。
  - 此前会被忽略的认证和授权后端故障现在会拒绝操作。

  导入此类备份时，现在必须使用 `--allow-security-profile-mismatch` CLI 标志或 `allow_security_profile_mismatch` API 参数，使运维人员有机会检查这些差异，而不是等到客户端停止连接或无法登录后才发现。此变更前生成的备份按在 `legacy` 安全配置下导出的备份处理。恢复到运行 `legacy` 的节点始终不受影响。

#### 插件

- [#18455](https://github.com/emqx/emqx/pull/18455) 在 `hardened` 安全配置下，`emqx ctl plugins allow <Name-Vsn>` 现在必须提供 `sha256:<hex>` 参数。授权会将插件包绑定到指定的 SHA-256 摘要，只有字节内容与摘要匹配的上传包才能安装。未提供摘要的授权会被拒绝，并显示所需命令。运行 `hardened` 安全配置的节点也会拒绝集群对端发送的无摘要授权。

  `legacy` 安全配置保持不变，`sha256:<hex>` 参数仍为可选。

#### 软件包

- [#17335](https://github.com/emqx/emqx/pull/17335) 通过 RPM 或 DEB 包安装后，系统会创建 `/opt/emqx` 目录，并在其中生成便捷符号链接（`bin`、`data`、`etc`、`lib`、`log`、`plugins`、`releases`、`erts-*`），指向安装包采用的各个 FHS 路径。无论采用何种方式安装 EMQX，运维人员现在都可以使用与官方 Docker 镜像相同的 `/opt/emqx/...` 路径。

#### 性能

- [#17583](https://github.com/emqx/emqx/pull/17583) 提高 JSON 编码和解码性能。

  此变更还使 JSON 输出中的浮点数格式与 Erlang/OTP 标准保持一致，可能与此前版本略有不同，例如会稍早改用科学计数法。

- [#18033](https://github.com/emqx/emqx/pull/18033) 在 `ESSENTIAL` 功能模式下，以及任何禁用了 Dashboard/管理客户端信息 API 的部署中，EMQX 不再定期报告仅供 `GET /clients` 端点使用的逐连接统计信息，从而降低高连接数场景下的开销。

- [#18424](https://github.com/emqx/emqx/pull/18424) 每次验证插件配置时，现在会从已安装的插件包中读取插件配置 Schema（`config_schema.avsc`），不再为每个已安装插件将其保存在内存中。

  如果已安装插件的 Schema 文件缺失或不可读，插件配置验证现在会报告文件错误。

- [#18688](https://github.com/emqx/emqx/pull/18688) 现在可通过 `node.dirty_io_schedulers` 配置 Dirty I/O 调度器线程数（`+SDio`）。当 `node.schedulers` 的解析结果大于 2 时，默认值 `auto` 保持原有固定值 8；在较小的节点上（例如 2 vCPU 的容器或 cgroup），该值为 4。这样既可降低小型节点启动时的内存占用，又能保留足够的线程来并发执行阻塞 I/O 操作。

### 缺陷修复

#### 核心 MQTT 功能

- [#18010](https://github.com/emqx/emqx/pull/18010) 客户端发送的畸形 MQTT 报文不再记录为 Broker 错误。

  此类无效输入现在计入监听器的连接关闭计数器，可减少端口扫描器、协议模糊测试工具和异常客户端产生的告警噪声。包含报文特定详情的解析错误共用一个 `frame_error` 计数器，因此畸形报文无法再创建新的计数器名称。

  解析错误详情（包括导致错误的字节）通过追踪报告。可针对客户端 ID、IP 地址或主题启动追踪进行查看。

- [#18027](https://github.com/emqx/emqx/pull/18027) 更改共享订阅处理方式：禁用共享订阅时，如果客户端尝试使用共享订阅，EMQX 会断开其连接。

  当 `mqtt.shared_subscription` 设置为 `false`，且 SUBSCRIBE 包含共享主题过滤器（`$share/...` 或 `$queue/...`）时，EMQX 现在会按照 MQTT 规范对协议错误的要求关闭网络连接。MQTT 5.0 客户端会先收到原因码为 `0x9E`（Shared Subscriptions not supported）的 DISCONNECT；MQTT 3.1/3.1.1 客户端的连接会直接关闭。此前，EMQX 会在 SUBACK 中返回失败原因码并保持连接。

- [#18116](https://github.com/emqx/emqx/pull/18116) 启用 `strict_mode`（默认行为）后，如果 MQTT v5 报文多次包含不可重复的属性，例如 CONNECT 中包含两个 `Session-Expiry-Interval`，EMQX 会将其作为协议错误拒绝，不再静默使用最后一个值。允许重复的 `User-Property` 不受影响。可在监听器配置中设置 `strict_mode = false` 恢复此前的宽松行为。

- [#18438](https://github.com/emqx/emqx/pull/18438) 修复出站报文大小检查中的差一错误。序列化大小恰好等于客户端 `Maximum Packet Size` 的报文此前会被丢弃并记录为 `frame_is_too_large`。现在只丢弃超过限制的报文，符合 MQTT 5.0 的要求。

- [#18470](https://github.com/emqx/emqx/pull/18470) 修复以下问题：发布客户端在原始 QoS 2 PUBLISH 被接收前断开连接后，EMQX 会确认重传的报文，但不进行投递。

  此修复部分回退了 [#16721](https://github.com/emqx/emqx/pull/16721) 引入的 QoS 2 重复报文处理变更。awaiting-PUBREL 状态过期后，重传的 QoS 2 PUBLISH 会再次被视为新的 QoS 2 交互，因此可能多次投递给订阅者。从 EMQX 7.0.0 开始，awaiting-PUBREL 状态过期将默认禁用，从而避免此类重复投递。

- [#18487](https://github.com/emqx/emqx/pull/18487) 减少携带未知报文标识符的 PUBACK、PUBREC、PUBREL 和 PUBCOMP 所产生的日志量。此类事件现在以 debug 级别记录，仍可通过客户端追踪查看。

- [#18523](https://github.com/emqx/emqx/pull/18523) 调整关闭顺序，使 EMQX 先停止 MQTT 监听器，再停止应用。此前，发布路径上的应用停止后，监听器仍会继续处理客户端流量，可能触发大量 `hook_callback_exception` 错误。现在应用关闭期间不会处理客户端流量。节点开始关闭后，`GET /status` 也会立即将节点报告为未运行，使负载均衡器停止向其路由新连接。

- [#18585](https://github.com/emqx/emqx/pull/18585) 修复会话接管行为：另一个连接使用相同客户端 ID 接管连接时，EMQX 会按照 MQTT 规范结束生命周期不超过其连接的会话。此修复适用于 Session Expiry Interval 为 0 的 MQTT 5.0 客户端，以及 Clean Session 为 1 的 MQTT 3.1.1 客户端。

  修复前，新连接可能继承旧会话的订阅和队列消息，Will Delay Interval 大于零的遗嘱消息会被静默丢弃。现在，新连接会启动全新会话（CONNACK Session Present 0），旧连接会收到原因码为 0x8E（Session taken over）的 DISCONNECT；其遗嘱消息会在接管时发布。

#### 访问控制

- [#18246](https://github.com/emqx/emqx/pull/18246) 在加固安全配置中新增延迟消息授权。

  - 重放延迟消息时会重新授权。来自 MQTT 和网关客户端的延迟消息携带受限授权上下文；EMQX 会在重放前检查当前发布授权规则和封禁记录。
  - 升级前创建的待处理延迟消息不包含授权上下文，因此在加固安全配置下重放时会被丢弃；传统安全配置仍会重放。
  - 修复多个网关对挂载点的处理。网关统一将未挂载的逻辑主题传递给授权模块。`authorization.include_mountpoint = false` 时检查逻辑主题；设置为 `true` 时为授权检查应用一次挂载点。发布或订阅前均只应用一次挂载点。
  - GBT 32960、JT/T 808、LwM2M、NATS 和 STOMP 网关不再将预先应用挂载点的主题传递给发布授权，避免重复应用挂载点。
  - GBT 32960 `dnstream`、JT/T 808 `proto.dn_topic` 和 LwM2M 命令自动订阅不再在授权前应用挂载点。
  - JT/T 808 `proto.up_topic` 和 `proto.dn_topic` 现在相对于网关挂载点，默认值从 `jt808/${clientid}/${phone}/up` 和 `jt808/${clientid}/${phone}/dn` 改为 `${phone}/up` 和 `${phone}/dn`。
  - MQTT-SN 空闲状态下的 QoS -1 发布和遗嘱消息现在会应用配置的挂载点。
  - NATS 发布授权会先检查由 NATS Subject 转换而来的 MQTT 主题，再应用挂载点。NATS JWT 权限和 EMQX 授权不再检查预先应用挂载点的主题。
  - 修复 Bridge、Retainer 和 Schema Validator 等系统重复处理延迟消息的问题。EMQX 仅在实际重放时处理延迟消息。内部发布方必须调用 `message.ingress` Hook 才能调度延迟消息；直接调用 `emqx:publish/1` 或通过管理 API 向 `$delayed/...` 发布无法调度延迟消息。

- [#18458](https://github.com/emqx/emqx/pull/18458) 将 `oidcc` 升级到 `3.2.3`。

  此升级修复了 Provider 配置 Worker 忙于刷新缓存配置时，Dashboard SSO (OIDC) 登录超时（`INTERNAL_ERROR: exit,{timeout,{gen_server,call,[...]}}`）的问题。

- [#18576](https://github.com/emqx/emqx/pull/18576) 未配置客户端 JWKS 时，OIDC SSO 配置 API（`GET /api/v5/sso/oidc`）现在会将 `client_jwks` 返回为 `none`，与 CLI 输出一致。已配置的客户端 JWKS 仍会显示为 `******`。

- [#18580](https://github.com/emqx/emqx/pull/18580) 对 `bin/node_dump` 生成的 `conf.hocon` 中的敏感配置值进行脱敏。Schema 中标记为敏感的值（例如 `dashboard.default_password` 和 `license.key`）现在写为 `******`。此前，脚本只对固定的键名列表进行脱敏，因此这些值会以明文写入。

#### 多租户

- [#18423](https://github.com/emqx/emqx/pull/18423) 命名空间管理员导入数据备份时，现在只应用其所属命名空间的配置。认证、授权、ExHook 或监听器等集群级设置会被跳过并记录警告，不再写入全局配置。

- [#18466](https://github.com/emqx/emqx/pull/18466) 修复名称包含特殊字符的命名空间无法列出备份文件的问题。此前，如果命名空间名称包含 `*`、`?`、`{`、`}`、`[` 或 `]` 等字符，即使备份文件存在于磁盘，列表仍为空。现在会将命名空间名称视为字面量目录名。

- [#18539](https://github.com/emqx/emqx/pull/18539) 修复多租户客户端列表不随持久会话在其他命名空间重新连接而更新的问题。此前，客户端更改命名空间后恢复现有会话（`clean_start=false`）时，`GET /api/v5/mt/ns/{ns}/client_list` 仍会在旧命名空间下列出该客户端，新命名空间的列表中则没有该客户端。客户端列表和各命名空间的客户端数量现在始终反映客户端连接时使用的命名空间。此变更还修复了恢复持久会话后客户端从列表中消失的问题。

#### 数据集成

- [#18300](https://github.com/emqx/emqx/pull/18300) 无论 `verify` 模式为何，连接器 TLS 设置中的空证书文件字段现在都视为未配置。此前，`verify_peer` 模式会拒绝空的客户端证书字段，尽管对端验证并不要求客户端证书。
- [#18392](https://github.com/emqx/emqx/pull/18392) 修复不同命名空间中同名的聚合动作（S3、S3Tables、Azure Blob Storage、Snowflake Aggregated）共用临时文件工作目录的问题。
- [#18449](https://github.com/emqx/emqx/pull/18449) 修复 PostgreSQL 动作写入数据时将罕见的 `sock_closed` 竞态错误错误地视为不可恢复的问题。EMQX 现在将其视为可恢复。
- [#18767](https://github.com/emqx/emqx/pull/18767) 修复 RocketMQ 连接器被报告为属于其实际并不归属的命名空间的问题。

  RocketMQ 连接器自身带有一个 `namespace` 配置字段，用于保存 RocketMQ 实例命名空间。连接器 API 响应此前会将该值放在与 EMQX 命名空间相同的 JSON 字段中，导致 Dashboard 将该连接器视为属于这个名称的命名空间，并显示“Only the administrator of namespace <name> can perform operations on the connector”；打开连接器时还会失败并提示“Managed namespace not found”。

  现在，连接器 API 响应中的 `namespace` 字段始终表示 EMQX 命名空间。RocketMQ 实例命名空间不再返回，并且在更新连接器时如果未提供该字段，其值会保持不变。

#### 规则引擎

- [#18527](https://github.com/emqx/emqx/pull/18527) 修复消息发布时，如果 Schema 验证、消息转换或规则引擎主题索引表不可用，日志中会反复出现 `badarg` 的问题。发布现在会按没有验证、转换或规则匹配主题的情况继续执行；Broker 会记录限流的 `topic_index_table_missing` 消息，不再为每次发布记录一条错误。索引表可在其 Owner 进程重启后继续存在；应用关闭时会先移除 Hook，再移除表，从而消除发布操作遇到表缺失的已知时间窗口。

#### 集群

- [#18409](https://github.com/emqx/emqx/pull/18409) 修复集群连接的 `server` 字段列出多个地址时无法工作的问题。现在每个连接会依次优先使用其中一个地址，并在无法连接时故障切换到其他地址。此前，该集群连接无法建立，并且在节点重启前无法创建、更新或删除任何集群连接。
- [#18447](https://github.com/emqx/emqx/pull/18447) 修复节点从其他集群成员同步配置后，`base.hocon` 更改可能被忽略的问题。配置同步不再将对端的 `base.hocon` 值持久化到 `cluster.hocon`，因此除非集群配置显式覆盖，否则本地 `base.hocon` 更改会在重启后生效。
- [#18537](https://github.com/emqx/emqx/pull/18537) 修复集群连接未将临时消息转发连接错误归类为可恢复错误的问题。受临时网络中断影响的消息现在会被缓冲并重试。

#### 网关

- [#18312](https://github.com/emqx/emqx/pull/18312) 修复启用连接模式的明文 CoAP UDP 监听器中，来自其他源且被拒绝的请求可能重定向后续下行消息的问题。
- [#18436](https://github.com/emqx/emqx/pull/18436) 修复 NATS 网关内部 JWT 认证未强制检查账户 JWT `exp`/`nbf` 声明和账户级用户撤销的问题。认证期间会拒绝已过期或尚未生效的账户 JWT，以及被账户撤销的用户 JWT；现有连接会在用户 JWT 或账户 JWT 中较早的过期时间到达时断开。格式错误且由 Resolver 预加载的账户 JWT 会在网关配置验证期间被拒绝。
- [#18494](https://github.com/emqx/emqx/pull/18494) 修复 CoAP 网关客户端报告内部 Keepalive 检查间隔而不是已配置心跳间隔的问题。网关 API 和 `emqx ctl gateway-clients list coap` 现在会以秒为单位报告配置的心跳值。
- [#18504](https://github.com/emqx/emqx/pull/18504) 修复 STOMP 帧对转义请求头字符和 CRLF 行尾的解析。网关现在会按照 STOMP 1.2 的要求，在请求头名称和值中解码 `\c`、`\r`、`\n` 和 `\\`。CONNECT 和 CONNECTED 帧除外：为兼容 STOMP 1.0，这两种帧的请求头（包括含冒号或反斜杠的密码）会原样传递。其他帧中未定义的转义序列会作为帧错误被拒绝。网关现在也支持 CRLF（`\r\n`）行尾和 CRLF 心跳；此前使用 CRLF 行尾的客户端无法连接。
- [#18700](https://github.com/emqx/emqx/pull/18700) 修复从 Dashboard 保存已有 NATS 网关配置时，认证凭据可能被表单中显示的掩码值替换，导致 NATS 客户端认证失败的问题。

  当用户更新其他网关设置而不更改认证配置时，现在会保留认证凭据。

  NATS 网关认证配置现在会拒绝重复的认证方式和凭据条目，包括重复的 NKey 和 JWT 账户条目，以避免认证行为不明确。

#### 插件

- [#18188](https://github.com/emqx/emqx/pull/18188) 加固插件框架的软件包和运行时完整性检查。
  - 必须先运行 `emqx ctl plugins allow`，再运行 `emqx ctl plugins install`。
  - 插件 API 回调响应仅允许使用安全响应头白名单；浏览器敏感响应头（例如 `set-cookie`、`location`、`access-control-*`、`content-security-policy` 及其他认证或安全策略响应头）和不带 `x-plugin-` 前缀的自定义响应头会被移除。
  - 新增 `plugins.package_limits`，用于限制 `max_package_size`（默认 `10MB`）、`max_decompressed_size`（默认 `50MB`）、`max_file_count`（默认 `10000`）、`max_path_depth`（默认 `32`）和 `max_extraction_time_ms`（默认 `60s`，也用作集群复制的 RPC 超时）。违反限制或包含路径遍历条目的软件包会被拒绝。
- [#18468](https://github.com/emqx/emqx/pull/18468) 热升级（relup）插件现在会在修改文件前验证目标版本字符串和升级路径兼容性。不兼容或格式错误的升级包会被拒绝，且不会删除或覆盖已安装 Release。
- [#18540](https://github.com/emqx/emqx/pull/18540) `emqx_relup` 插件包现在包含默认配置文件 `priv/config.hocon`，安装时不再反复记录 `failed_to_copy_plugin_default_hocon_config` 警告。

#### ExHook

- [#18464](https://github.com/emqx/emqx/pull/18464) 修复配置更新期间 ExHook Server 变为不健康时，ExHook Manager 可能发生的罕见崩溃。Server 重新连接期间，Manager 会保持配置顺序并继续处理配置更改。
- [#18473](https://github.com/emqx/emqx/pull/18473) 修复没有回调 Server 运行时 ExHook 的认证和授权行为。传统安全配置现在遵循 `failed_action`；加固安全配置仍保持故障关闭。

#### 可观测性

- [#17602](https://github.com/emqx/emqx/pull/17602) 新增由配置支持的 `emqx ctl log outputs` 命令，使 CLI 对日志输出的更改与 HTTP API 和 Dashboard 管理的日志配置一致。
- [#17912](https://github.com/emqx/emqx/pull/17912) 修复 OpenTelemetry W3C Baggage 请求头提取中的安全漏洞（GHSA-64w2-whjg-q7q7）。此前，传入的 `baggage` 请求头在解码时不限制字节数或条目数，格式错误的键值对会导致进程崩溃。提取上限现为 8192 字节和 180 个条目，格式错误的键值对会被跳过。
- [#18521](https://github.com/emqx/emqx/pull/18521) 连接超过强制关闭限制（`force_shutdown.max_mailbox_size` 或 `force_shutdown.max_heap_size`）时生成的报告现在包含 `label`。已建立连接使用客户端 ID；CONNECT 完成前关闭的连接使用监听器名称和对端地址。此前报告只包含限制值和测量值，无法识别受影响的连接。
- [#18696](https://github.com/emqx/emqx/pull/18696) 修复查询审计日志时，SSO 认证用户创建的记录可能导致查询返回错误的问题。

#### 管理

- [#18289](https://github.com/emqx/emqx/pull/18289) 修复带引号的 HOCON 字符串和键中与 JSON 兼容的 Unicode 转义序列未被解码的问题。
- [#18403](https://github.com/emqx/emqx/pull/18403) 修复在没有 UTF-8 Locale 的 Shell 中运行时（例如通过非交互式 SSH、cron 或未设置 `LANG`），`emqx ctl` 将非 ASCII 字符输出为 `\x{...}` 转义或无效字节的问题。由 `emqx` 脚本启动的 Erlang VM，包括节点、`emqx ctl`、`emqx eval`、`emqx remote_console` 和 `emqx escript`，现在始终以 UTF-8 读写标准输入输出。
- [#18444](https://github.com/emqx/emqx/pull/18444) 修复配置文件中的字节大小单位 `b` 和 `B` 必须加引号的问题。`max_packet_size = 1MB` 可以正常解析，但 `max_packet_size = 1B` 此前会失败，必须写为 `"1B"`。现在所有字节大小单位均可不加引号。
- [#18509](https://github.com/emqx/emqx/pull/18509) 修复 `GET /clients/{clientid}/mqueue_messages` 和 `GET /clients/{clientid}/inflight_messages` 中的消息分页问题。`max_payload_bytes` 截断响应页时，`meta.position` 此前会指向被省略消息之后的位置，导致下一页跳过这些消息，并可能出现 `mqueue_len` 大于 API 返回消息总数的现象。现在，`meta.position` 指向最后一条已返回消息，下一页会从第一条被省略的消息继续。
- [#18544](https://github.com/emqx/emqx/pull/18544) 修复 `GET /clients_v2` 在返回所有内存会话客户端后仍返回 Cursor 的问题。
- [#18558](https://github.com/emqx/emqx/pull/18558) 修复 `GET /clients_v2` 忽略 `fields` 查询参数的问题。
- [#18590](https://github.com/emqx/emqx/pull/18590) 修复节点未运行时 `emqx stop` 的输出。此前，该命令会两次报告 `Node <name> not responding to pings.`，随后以 `Graceful shutdown failed PID=[]` 失败。现在只报告一次节点不可达，不会针对无法找到的节点输出关闭失败。退出码保持不变。
- [#18619](https://github.com/emqx/emqx/pull/18619) 修复目标节点在活跃状态检查与获取信息的 RPC 之间变为不可达时，`GET /nodes/{node}` 返回 500 而不是 400 的问题。

#### 部署与安全

- [#17921](https://github.com/emqx/emqx/pull/17921) 将 `protobuf` 依赖升级到 v0.17.0。该依赖仅用于生成 SBOM，不属于 EMQX 运行时。升级包含深度嵌套消息解码时无限递归拒绝服务问题的修复（GHSA-rv48-qqj5-crxg）、Elixir 1.19/1.20 编译器警告修复，并将此前固定的开发版本引用替换为正式 Release。

- [#18706](https://github.com/emqx/emqx/pull/18706) 调试模式下不再输出敏感信息。

  使用 `DEBUG=1` 或 `DEBUG=2` 运行 `bin/emqx` 命令时，Shell 跟踪输出不再打印 Erlang Cookie 或 License Key。

## 6.2.3

*发布日期: 2026-08-21*

在升级到 EMQX 6.2.3 之前，请务必查阅不兼容变更和已知问题。

### 增强

#### 访问控制

- [#17813](https://github.com/emqx/emqx/pull/17813) Dashboard 用户和 API 密钥端点现在会拒绝将特权 scope（`system`、`user_management`、`api_key_management`、`sso_management`）与其他 scope 混合的 scope 列表。这四个特权 scope 在效果上都等同于管理员权限，因此将它们与受限 scope 列表组合并不能真正限制账户权限。请根据账户是否需要管理员等效能力，仅使用特权 scope 列表，或仅使用非特权 scope 的 scope 列表。已存在的混合 scope 记录在下一次更新前仍可继续使用；下一次更新必须拆分 scope 列表才能成功。
- [#17980](https://github.com/emqx/emqx/pull/17980) 在加固安全配置下，EMQX 现在会对服务端发起的订阅执行主题校验、授权和 MQTT 能力检查，并触发 client subscribe hook。
- [#18002](https://github.com/emqx/emqx/pull/18002) 在加固安全配置下默认启用 SAML Response 和 Assertion 签名验证。
- [#18296](https://github.com/emqx/emqx/pull/18296) 新增 `is_jwt(value)` 函数，可用于认证器 `precondition` 表达式，也可用于任何接受 Variform 表达式的位置。仅当该值在结构上是 JWT（JWS Compact 格式）时，该函数才返回 true，并且不会验证签名。对于同时服务 JWT 客户端和旧版凭据客户端的认证链，这可以让 JWT 认证器在客户端提供非 JWT 密码时被干净地跳过。尤其是在加固安全配置下，非 JWT 凭据原本会被 JWT 认证器拒绝。

#### 多租户

- [#17732](https://github.com/emqx/emqx/pull/17732) API 密钥创建和更新端点新增 `namespace` 字段，因此运维人员不再需要将命名空间编码到 `role` 字符串中。现有的 `ns:<namespace>::<role>` 形式仍继续可用。当两种形式同时提供时，二者必须一致。
- [#17855](https://github.com/emqx/emqx/pull/17855) 命名空间范围内的 Dashboard 管理员现在可以在自己的命名空间内创建、列出、读取、更新和删除 API 密钥。他们不能创建全局 API 密钥，也不能创建其他命名空间中的 API 密钥；其命名空间之外的 API 密钥对他们不可见。

#### 数据集成

- [#17933](https://github.com/emqx/emqx/pull/17933) RabbitMQ 连接器支持多节点 `servers` 列表（例如 `rmq1:5672,rmq2:5672`），并支持连接时故障切换和轮换连接池启动偏移。当 `servers` 未设置时，旧版 `server`/`port` 配置仍继续保留。

- [#17944](https://github.com/emqx/emqx/pull/17944) 为 HTTP 连接器以及 HTTP 认证/授权新增 OAuth2 Client Credentials 认证。启用后，EMQX 会从配置的 Token 端点获取并刷新访问令牌，并将其作为 Bearer Authorization 头添加到出站请求中。

  当无法获取 Token 时，连接器健康检查会报告 `disconnected`。同时启用 OAuth2 并提供 `Authorization` 头的配置会被拒绝。

  EMQX 会在 Token 请求体中以表单字段发送 Client ID 和 Client Secret。不支持在 HTTP Basic `Authorization` 头中发送这些凭据。

- [#18014](https://github.com/emqx/emqx/pull/18014) Datalayers Arrow Flight 连接器现在启用预处理语句的自动重建。当服务端丢失预处理语句时（例如重启后），客户端会自动重新创建该语句并重试写入操作，避免写入失败。

- [#18042](https://github.com/emqx/emqx/pull/18042) 为 DynamoDB 连接器新增 AWS IAM role 凭据支持。

  当访问密钥 ID 和 Secret Access Key 均省略时，EMQX 会从 ECS task role 或 EC2 实例元数据获取临时凭据，并在凭据过期前刷新。

- [#18081](https://github.com/emqx/emqx/pull/18081) 提升 Snowflake Streaming 动作的弹性。在追加行时遇到某些错误类型，尤其是 channel 内部状态不同步时，该动作会重试失败的行，并尝试重新打开 channel，无需人工干预。

- [#18085](https://github.com/emqx/emqx/pull/18085) 为 Kafka、Confluent 和 Azure Event Hubs 生产者新增以下配置项：

  - `max_batch_age`（动作）：丢弃在生产者缓冲区中停留超过该时长的消息，而不是发送这些消息；计入 `dropped.expired` 指标。默认值：`infinity`（永不丢弃）。
  - `max_retries`（动作）：消息批次在失败重试达到该次数后会被丢弃；计入 `failed` 指标。只有当 Kafka 明确返回错误码时，重试计数器才会递增；连接丢失后的重新发送不会递增该计数器。默认值：`infinity`（永久重试）。
  - `reconnect_delay`（动作）：生产者在连接丢失后重新连接前的延迟。默认值：`2s`（此前为硬编码值）。
  - `request_timeout`（连接器）：等待 Kafka 响应的时长，超时后连接会被视为过期并重新建立。默认值：`30s`。

  此外，Kafka 客户端库升级到 wolff 4.2.1 后，恢复了内存模式缓冲区对 `max_linger_time` 的支持：未填满的批次现在会最多等待 `max_linger_time` 以收集更多消息，从而降低 Produce 请求速率；已满批次会立即发送。

- [#18110](https://github.com/emqx/emqx/pull/18110) Schema Registry 新增对 JSON Schema draft 2019-09 和 2020-12 的支持。

- [#18137](https://github.com/emqx/emqx/pull/18137) GCP Pub/Sub 生产者和消费者现在可在主题配置中接受完整主题路径（`projects/<project-id>/topics/<topic-name>`），从而可以向与服务账号自身项目不同的 GCP 项目中的主题发布或从中消费。不带项目前缀的主题名仍像以前一样解析到服务账号所属项目。对于消费者，订阅仍创建在服务账号所属项目中；只有主题引用可以指向另一个项目。

#### 规则引擎

- [#18253](https://github.com/emqx/emqx/pull/18253) 新增两个规则引擎 SQL 函数：`map_to_range(value, min, max)` 和 `hash_to_range(value, min, max)`。它们会将值（或其哈希值）映射到一个闭合整数区间，可用于分片或分桶。例如，可通过从主题片段派生分片索引，将大量设备分散到多条规则中：`hash_to_range(nth(2, tokens(topic, '/')), 0, 3)`。
- [#18306](https://github.com/emqx/emqx/pull/18306) 新增用于 LZ4 Frame 压缩和解压缩的 `lz4_compress` 和 `lz4_uncompress` 规则函数。

#### 插件

- [#18012](https://github.com/emqx/emqx/pull/18012) 新增 `emqx_sync_request` 插件，用于通过 EMQX REST API 实现同步 MQTT 请求/响应流程。该插件还提供节点本地 CLI 诊断能力，用于查看请求计数器和当前 pending 状态。

- [#18353](https://github.com/emqx/emqx/pull/18353) 新增 `emqx_maptabs` 插件，为规则 SQL 提供命名映射表。

  表由 JSON 文件初始化并保存在内存中，以便规则引擎热路径快速查找。新的 `maptab_lookup(Table, Key)`、`maptab_lookup(Table, Key, Field)` 和 `maptab_lookup(Table, Key, Field, Default)` 规则 SQL 函数可将冗长的 `CASE ... WHEN ... THEN` 语句转换为一次表查询；查询到的字段可直接驱动内置 `subbits` 函数解码二进制 Payload。

  表通过 `emqx ctl maptabs` CLI 管理：在一个节点上加载或删除表会将变更复制到集群中的每个节点；如果某个节点在更新期间离线，它会在重新加入集群后自动追上更新。

  插件配置提供安全限制：`max_tables`（默认 100）、`max_rows_per_table`（默认 10000）和 `max_table_file_bytes`（默认 10000000）。

#### 安装包

- [#18037](https://github.com/emqx/emqx/pull/18037) 新增 Enterprise Linux 10（EL10）安装包，适用于 Red Hat Enterprise Linux 10、Rocky Linux 10 及兼容发行版。
- [#18127](https://github.com/emqx/emqx/pull/18127) 开始发布 macOS 26（Tahoe）安装包。

#### 性能

- [#18185](https://github.com/emqx/emqx/pull/18185) 改进订阅 HTTP API 的深分页查询，通过在每个目标节点上累积内存中的订阅行，避免每个分页批次都执行一次 RPC。
- [#18229](https://github.com/emqx/emqx/pull/18229) 降低数据集成发送路径上的 CPU 开销。Broker 不再为通过既不是动作也不是 Source 的资源路由的每条消息（例如集群连接消息转发）构造格式化错误字符串，这在高消息量下此前可能触发长调度器告警。

### 修复

#### 核心 MQTT 功能

- [#17895](https://github.com/emqx/emqx/pull/17895) [#18062](https://github.com/emqx/emqx/pull/18062) 将 TLS/WSS 监听器从托管证书包切回基于文件的证书时，现在即使引用的证书包已被删除也能成功，包括请求通过将 `managed_certs` 发送为 `null`（Dashboard 的行为）来清除该字段的情况。

- [#17911](https://github.com/emqx/emqx/pull/17911) 当运行时 OTP ssl 应用支持 `ECDHE-PSK-CHACHA20-POLY1305` 密码套件时，允许 DTLS 监听器校验该密码套件。

- [#18102](https://github.com/emqx/emqx/pull/18102) 修复启用投递速率限制时，MQTT 客户端可能乱序接收 QoS 1 和 QoS 2 消息的问题。现在，EMQX 会将后续消息保持在队列中，直到被阻塞的消息可以发送。

  受影响版本：6.1.2、6.1.3，以及 6.2.0 到 6.2.2。仅配置了投递速率限制（`delivery_messages_rate` 或 `delivery_bytes_rate`）的会话受影响；默认不配置投递速率限制。

- [#18108](https://github.com/emqx/emqx/pull/18108) 删除仍被某些配置引用的托管证书包（或其中的单个文件）现在总是会失败，并返回清晰的错误，列出引用该证书包的配置；`force_delete` 查询参数不再绕过此检查，并已从 API schema 中移除。

  此外，当某个监听器引用的证书包在磁盘上缺失时，Prometheus stats 端点不再整体失败；受影响的监听器会在证书过期指标中被跳过，并记录警告日志。

- [#18111](https://github.com/emqx/emqx/pull/18111) 当启用 `mqtt.strict_mode` 时，设置 password 标志但未设置 username 标志的 MQTT v3.1 CONNECT 报文现在会被拒绝，与 MQTT v3.1.1 的现有行为保持一致。MQTT v3.1 规范规定，不能在没有用户名的情况下提供密码。

  同时改进了连接日志可读性：未提供密码时，CONNECT 报文追踪现在打印 `Password=undefined`（此前无法与空密码区分），日志中的 `peername` 字段现在始终渲染为普通字符串，例如 `10.0.0.1:54123`。

- [#18181](https://github.com/emqx/emqx/pull/18181) 修复 burst 值配置为 `0` 的速率限制器仍可能额外允许一次突发流量的问题。这会使 MQTT 投递消息速率限制等限制不如配置严格。

- [#18236](https://github.com/emqx/emqx/pull/18236) 修复使用 socket-backed TCP 监听器的客户端在高负载下可能意外断开的问题，原因是偶发的就绪信号到达了尚未就绪的 socket。

  ```
  [error] crasher: initial call: emqx_socket_connection:init/4, ..., error: {{case_clause,{select,{select_info,recv,#Ref<...>}}},[{emqx_socket_connection,handle_msg,2,[{file,"emqx_socket_connection.erl"},{line,827}]}, ...
  ```

- [#18293](https://github.com/emqx/emqx/pull/18293) 将 QUIC 协议栈升级到 quicer-0.4.8（msquic 2.5.7）。

- [#18357](https://github.com/emqx/emqx/pull/18357) [#18375](https://github.com/emqx/emqx/pull/18375) 节点启动完成前，MQTT 连接现在会被拒绝，因此监听器不会在认证、授权和插件 hook 激活前处理流量。

  `GET /status` API 现在会在启动完成前返回 HTTP 503，因此负载均衡器可以将新连接路由到集群中的其他节点。

  发往尚未完成启动节点的集群加入请求现在会被拒绝，并返回提示稍后重试的消息。

#### 持久存储

- [#18143](https://github.com/emqx/emqx/pull/18143) 修复持久共享订阅在订阅者连接到其他节点时，可能无法与共享订阅 leader 通信的问题。该问题可能导致 CPU 使用率出现无法解释的峰值。

#### 规则引擎

- [#17957](https://github.com/emqx/emqx/pull/17957) 修复当 `rule_engine.limit_selects_in_namespace = true` 时，多个规则事件（例如 `$events/client/connack`）无法触发全局命名空间中规则的问题。
- [#18049](https://github.com/emqx/emqx/pull/18049) 修复设置 `rule_engine.limit_selects_in_namespace = true` 后，告警激活/解除触发的全局规则无法触发的问题。
- [#18388](https://github.com/emqx/emqx/pull/18388) 修复规则引擎 `republish` 动作处理命名空间规则时的问题。当启用 `rule_engine.limit_selects_in_namespace`（默认启用）时，重新发布的消息现在会发布到该规则的命名空间下（`<namespace>/<topic>`）。这使 `republish` 动作遵循与规则本身相同的命名空间边界。渲染后的主题如果已经以 `<namespace>/` 开头，则会原样发布，因此自行添加该前缀的 republish 模板仍可继续工作。此前，该消息会发布到渲染后的主题，且不会添加命名空间前缀。设置 `rule_engine.limit_selects_in_namespace = false` 会保留此前行为。

#### 数据集成

- [#17859](https://github.com/emqx/emqx/pull/17859) 修复 MQTT 连接器，使其可以连接到 IPv6 Broker。

  此前，将 MQTT 连接器配置为连接 IPv6 Broker 会以两种方式失败：`[::1]:1883` 这样的 IPv6 字面量会在保存时因 `bad_host_port` 校验错误被拒绝；仅解析为 IPv6（`AAAA`）地址的主机名会因为连接默认使用 IPv4 而连接失败，并显示 "Could not resolve host" 错误。

  服务器地址解析器现在接受带方括号的 IPv6 字面量（例如 `[::1]`、`[::1]:1883` 和 `mqtt://[::1]:1883`），MQTT 连接器现在会在连接时启用 IPv6 探测，因此可以访问仅支持 IPv6 的 Broker。

  MQTT 连接器和集群连接的 `server` 地址现在接受官方 MQTT URI scheme `mqtt`（普通 TCP）和 `mqtts`（TLS），例如 `mqtt://broker:1883` 和 `mqtts://broker:8883`。不带 scheme 的 `host:port` 仍被接受。任何其他 scheme 现在都会以 `unsupported_scheme` 校验错误被拒绝。

- [#17947](https://github.com/emqx/emqx/pull/17947) 修复更新 HTTP 连接器后，其动作 buffer worker 可能在连接器重建后保持阻塞，导致消息持续排队直到下一次重试间隔的问题。

- [#17955](https://github.com/emqx/emqx/pull/17955) 修复低写入速率下，GreptimeDB 异步批次在健康检查后可能无法刷新的问题。

- [#17961](https://github.com/emqx/emqx/pull/17961) 修复 Kafka 或 Pulsar 连接器在健康检查超时时会进入 `disconnected` 状态，并可能重建其内部队列的问题。现在，它们会进入 `connecting` 状态。

- [#17970](https://github.com/emqx/emqx/pull/17970) 启用 SSRF 防护后，已有连接器的地址即使被策略阻止，也不再干扰连接器管理操作。

  此前，在创建连接器后启用 SSRF 防护（或扩展其拒绝列表）可能导致无关连接器操作因内部错误失败；删除受影响连接器时，也可能在其动作和规则已被删除后仍留下连接器本身。

  SSRF 防护现在适用于 HTTP 和 MQTT 连接器，并在创建或更新连接器时强制执行：使用被阻止地址创建或更新此类连接器会被拒绝。启用、禁用和删除连接器永远不会被阻止，其他连接器类型不受该策略约束。

- [#17973](https://github.com/emqx/emqx/pull/17973) 修复 Kafka 生产者动作重试指标。动作指标中的 `retried`、`retried.success` 和 `retried.failed` 计数器现在会反映内部缓冲区在 Broker 重连后重新发送的消息，因此运维人员可以判断重试消息最终是成功还是失败。此前，无论发生多少次内部重试，这些计数器都保持为 `0`。`success` 和 `failed` 计数器不受影响，也不会被重复计数。

- [#17982](https://github.com/emqx/emqx/pull/17982) GCP PubSub Consumer 现在使用 HTTP/2，并在拉取请求超时时取消该请求。这样可以更清晰地通知 GCP 服务端该请求已结束，使消息可以分配给后续拉取请求，从而降低尾延迟。

- [#18055](https://github.com/emqx/emqx/pull/18055) 修复集群中不同节点上的 Snowflake Streaming 动作开始失败并出现以下错误的问题：

  ```
  {unrecoverable_error,#{body => <<"{\"code\":\"STALE_CONTINUATION_TOKEN_SEQUENCER\",\"message\":\"Channel sequencer in the continuation token is stale. Please reopen the channel\"}">>,...
  ```

- [#18110](https://github.com/emqx/emqx/pull/18110) 修复在 Schema Registry 中使用 draft-06 JSON Schema 的 `examples` 注解时，有效数据会被错误拒绝为无效的问题。

- [#18174](https://github.com/emqx/emqx/pull/18174) 当服务器地址 scheme 与 SSL 设置不一致时，MQTT 连接器现在会报告清晰的错误消息，例如在 SSL 禁用时使用 `mqtts://`（TLS）地址。

  此前，此类配置会因内部错误失败并产生噪声日志，因为连接器尝试对 TLS 端口发起普通 TCP 连接，且无法解释服务端响应。从服务端收到非 MQTT 数据的连接尝试（例如端口预期 TLS）现在也会生成清晰说明，而不是内部错误。

- [#18193](https://github.com/emqx/emqx/pull/18193) 修复对 GCP Pub/Sub Consumer 连接器或 Source 使用 "Test Connection" 后，正在运行的 GCP Pub/Sub Consumer Source 可能显示为 `disconnected`（原因：`timeout`），并保持该状态直到手动禁用再重新启用的问题。

  受影响版本：6.1.3 和 6.2.2。

  为连接测试创建的临时 worker pool 与运行中 Source 的 pool 共用了健康状态记录，因此清理测试 pool 时也会丢弃运行中 Source 的健康状态。现在，每个 pool 都保留自己的记录，测试连接不再影响运行中的 Source。该修复包含热升级 hook，因此旧版本启动的消费者会被重启，以使用新的记录方式。

- [#18198](https://github.com/emqx/emqx/pull/18198) 修复两个 JSON Schema Registry 问题：

  - 包含非 ASCII 字符的 schema（例如中文属性名或示例值）现在可以通过 HTTP API 注册。此前，注册会因内部 `badarg` 错误失败。
  - 指向包含非 ASCII 字符定义名的 `$ref` 引用现在可以在校验和解码期间正确解析，包括百分号编码形式（例如 `#/definitions/%E5%A7%93%E5%90%8D%E7%B1%BB%E5%9E%8B`）和原始 UTF-8 形式。此前，此类引用无法解析，解码会因内部 `badmatch` 错误失败。

  此外，Payload 不符合其 JSON schema 时，现在会在规则引擎解码期间产生清晰的 schema 校验错误，而不是内部错误。

- [#18242](https://github.com/emqx/emqx/pull/18242) 修复 Datalayers 连接器在数据库或凭据留空时因 `function_clause` 失败的问题。现在会报告清晰的配置错误。

- [#18270](https://github.com/emqx/emqx/pull/18270) 修复 GreptimeDB 连接器在 worker 被强制停止后残留陈旧 gRPC channel 时可能无法重启的问题。

- [#18274](https://github.com/emqx/emqx/pull/18274) 修复 Tablestore 连接器健康检查每次都会列出所有时序表的问题。健康检查现在会针对配置的 `probe_table_name` 使用 `DescribeTimeseriesTable` 探测；当该项未设置时，回退为列出所有时序表。

- [#18299](https://github.com/emqx/emqx/pull/18299) 修复 Snowflake 连接器已配置的 TLS（`ssl`）设置未应用到出站 HTTPS 连接（Streaming 和 Aggregated 两种模式）的问题。`verify`、`cacertfile`、客户端证书和 `server_name_indication` 等设置此前会被接受并显示，但对实际连接没有影响。现在，配置值会生效。未自定义 `ssl` 设置的连接器保持原有连接行为。

- [#18302](https://github.com/emqx/emqx/pull/18302) Elasticsearch 动作的 `index` 和 `id` 值现在会在构造请求路径时进行 URL 编码，因此模板值中的 `#` 或 `/` 等字符会被视为单个路径段内的字面文本，而不是改变请求目标。JSON 请求体不受影响。

- [#18303](https://github.com/emqx/emqx/pull/18303) Sparkplug B alias 与名称的映射现在仅为 MQTT 客户端直接发布的消息维护。通过桥接或其他内部路径到达的消息不再共享 alias 映射，从而防止某个发布方的映射被应用到另一个发布方的解码指标。因此，对于通过 MQTT 桥接接入的数据消息，`spb_decode` 不再将 alias 解析为指标名。

#### 集群

- [#17995](https://github.com/emqx/emqx/pull/17995) 修复节点加入集群时，如果持久化的 `mqtt.max_packet_size` 与本地配置不同，节点可能终止的问题。EMQX 现在会在监听器启动前跳过监听器刷新副作用，并在 EMQX 应用启动时根据同步后的配置创建监听器。

- [#17999](https://github.com/emqx/emqx/pull/17999) 修复使用社区版（单节点）License 的节点加入到持有集群能力 License 的对等节点所在集群时，可能出现启动崩溃循环的问题。

  此前，如果在对等节点的 License 复制到加入节点之前已建立集群成员关系，该节点会因 `SINGLE_NODE_LICENSE` 错误拒绝启动，并在自动重启 supervisor 下持续崩溃重启。现在，节点会在启动前等待一段有界宽限期，以便集群 License 同步。如果集群中始终没有节点获得集群 License，仍会在宽限期结束后被拒绝。

- [#18077](https://github.com/emqx/emqx/pull/18077) 修复节点在完全启动前收到 `cluster join` 请求（CLI 或 API）时发生崩溃的问题：加入操作会在应用仍在启动时重启内部数据库，可能导致整个节点宕机。现在，此类请求会被拒绝并返回清晰错误消息；请在节点完全启动后重试。

- [#18277](https://github.com/emqx/emqx/pull/18277) 提升配置变更持久化到 `cluster.hocon` 的可靠性：更新现在会先写入并同步到磁盘，然后再原子替换文件；读取先前文件用于备份失败时，也不再阻止新配置保存。

- [#18287](https://github.com/emqx/emqx/pull/18287) 提升集群节点不可达或在处理请求时失败时 REST API 的弹性。当到对等节点的 RPC 未完成时，多个端点此前会返回不透明的 500 错误，少数情况下甚至会在部分工作失败时报告成功。现在，这些端点会返回描述性错误响应，集群范围读取会优雅降级为来自可达节点的结果。

  受影响区域包括：列出和描述插件、列出客户端（v2）、流式传输和下载追踪日志、从指定节点读取 HOCON 格式配置、删除指定节点上的延迟消息、重置主题指标、导入数据备份、按节点执行动作/Source 操作、列出规则、文件传输下载以及删除消息队列。保留消息重建索引和会话接管现在也能容忍对等节点不可达，而不会中止。

- [#18347](https://github.com/emqx/emqx/pull/18347) 修复 Mnesia RocksDB 后端的问题：当某个 core 节点离线期间删除 key 时，可能导致 core 节点上的表不一致。

  从 EMQX 角度看，该问题可能导致 Dashboard 登录锁延迟释放，也可能因为旧 schema 的删除被遗漏而造成 EMQX Schema Registry 浪费磁盘空间。

- [#18383](https://github.com/emqx/emqx/pull/18383) 修复通过 `PUT /configs` 提交包含非法 Unicode 转义序列的配置时返回内部错误的问题。此类请求现在会返回校验错误，并指出非法的转义序列。

#### 访问控制

- [#17806](https://github.com/emqx/emqx/pull/17806) 使数据备份导入和导出端点符合最小权限原则：Dashboard 用户的 scope 集合如果不同时包含 `user_management` 和 `api_key_management`，则不能导入或导出包含 `dashboard_users` 或 `api_keys` 表集的归档。全局管理员和具有必要 scope 的 API 密钥调用方不受影响。
- [#17853](https://github.com/emqx/emqx/pull/17853) 改进连接器调试日志中敏感 HTTP 请求头的脱敏处理。`x-api-key`、`x-auth-token`、`api-key` 和 `cookie` 头现在会作为 secret 存储在连接器状态中（与现有的 `Authorization` / `Proxy-Authorization` 行为一致），因此在 trace / debug 级别输出连接器状态时不会打印其值。此外，共享的头部脱敏辅助函数现在可以识别以 iolist 形式存储的头名称（连接器模板解析器会产生这种形态），此前这种形态会绕过敏感性检查。
- [#17871](https://github.com/emqx/emqx/pull/17871) 通过批量导入或 bootstrap 文件导入内置数据库用户时，现在会拒绝在非全局命名空间中创建超级用户，与单用户管理 API 保持一致。此类行会报告为失败，并且不会被存储。
- [#17974](https://github.com/emqx/emqx/pull/17974) 连接日志中的原始 MQTT 报文数据现在默认脱敏；可按监听器为诊断目的配置受信任的客户端 IP 地址允许列表。
- [#18005](https://github.com/emqx/emqx/pull/18005) 修复 CLI 审计日志可能存储敏感命令参数的问题。
- [#18009](https://github.com/emqx/emqx/pull/18009) 统一使用角色隐式默认 scope（显示为 `unset`）的管理员和 API 密钥记录的 scope 处理。读写操作现在接受等同于 unset 的 scope 列表，并且此类记录会保留向前兼容的隐式 scope，而不是冻结为显式列表，因此可自动获得未来版本新增的 scope。
  - 仅通过 Dashboard 用户 API 编辑默认管理员的备注（description）不再失败；用户 API 现在会将匹配角色隐式完整集合的 scope 列表（以及 `unset` 值）视为“未显式设置 scope”。
  - [#18196](https://github.com/emqx/emqx/pull/18196) API 密钥创建和更新请求接受同样等同于 unset 的 scope 列表，因此重新提交读取返回的值不再失败。
  - [#18221](https://github.com/emqx/emqx/pull/18221) 默认管理员在启动时不再使用显式 scope 列表创建；已有携带显式列表的默认管理员记录会在启动时更新为隐式形式。
- [#18146](https://github.com/emqx/emqx/pull/18146) 加固 Dashboard 和管理 API 的基于 scope 的授权，确保访问控制检查在等效请求路径之间一致应用。
- [#18177](https://github.com/emqx/emqx/pull/18177) 修复当客户端未被 `allow_log_packet_data_from` 允许时，`frame_parse_error` 日志可能在 `received_prefix` 中暴露报文数据的问题。
- [#18204](https://github.com/emqx/emqx/pull/18204) 加强数据备份归档导入期间的校验，确保备份文件内容只会恢复到其目标表中。
- [#18225](https://github.com/emqx/emqx/pull/18225) 改进 API 密钥 bootstrap 文件条目中包含会在加载时被丢弃的 scope 时记录的警告。警告现在会按丢弃原因对 scope 名称分组：未知 scope 名称、发布者角色不允许的 scope，或不能与其他 scope 组合的特权 scope。此前，每个被丢弃的 scope 都会报告为未知 scope 名称。
- [#18314](https://github.com/emqx/emqx/pull/18314) 通过 HTTP API 读取使用服务账号 JSON 认证的 GCP 连接器（GCP PubSub Producer/Consumer、BigQuery）时，服务账号 JSON 值现在会被脱敏。
- [#18330](https://github.com/emqx/emqx/pull/18330) 为只读 REST 端点增加更多 secret 脱敏处理，包括监听器、ExHook 和审计日志端点。
- [#18344](https://github.com/emqx/emqx/pull/18344) 将 HOCON 升级到 0.46.3。该版本会将数组类型配置字段中的敏感值渲染为 `******`，并且不再在配置校验错误日志中打印敏感字段值。
- [#18386](https://github.com/emqx/emqx/pull/18386) 修复使用 query-string 认证的 InfluxDB v1 连接器（包括 Datalayers 连接器）在日志中泄露密码的问题。该密码此前会作为客户端 `path` 和 `auth_path` 字段的一部分以明文记录。
- [#18391](https://github.com/emqx/emqx/pull/18391) 修复认证缓存 key 碰撞问题。两个不同凭据的字段值在拼接后生成相同字节序列时，可能共享同一个缓存条目，导致一个客户端在缓存 TTL 内复用另一个客户端的缓存认证结果。

#### 多租户

- [#17807](https://github.com/emqx/emqx/pull/17807) 命名空间管理员现在拥有隔离的数据备份空间。他们通过数据备份端点（`/data/export`、`/data/import`、`/data/files`、`/data/files/:filename`）执行的导出、上传、列表、下载、导入和删除操作只会作用于其自身命名空间的备份。命名空间管理员不能再查看、下载或删除全局备份或其他命名空间的备份。

  全局管理员默认继续管理全局备份（包括此变更前创建的备份），也可以在 `GET`/`DELETE /data/files` 和 `GET /data/files/:filename` 中额外传入 `namespace` 查询参数，以检查或清理指定命名空间的备份。

- [#17975](https://github.com/emqx/emqx/pull/17975) `/tracing` 配置端点（`PUT /api/v5/tracing`）现在仅限全局管理员使用。命名空间 Dashboard 管理员和 API 密钥不能再修改全局 `[trace]` 配置；此类请求会被 HTTP 403 拒绝。

- [#18008](https://github.com/emqx/emqx/pull/18008) 数据备份：全局管理员现在可以通过传入 `namespace` 查询参数导入或上传命名空间备份，与列表和下载行为保持一致。此前，直接导入命名空间备份会失败，而先上传该备份会将其静默移动到全局范围并成功。现在，两种操作行为一致。命名空间管理员在所有操作中仍限制在自己的命名空间内。

- [#18117](https://github.com/emqx/emqx/pull/18117) 删除命名空间现在也会删除该命名空间的内置数据库认证用户（包括基于密码和 SCRAM 的用户）以及授权规则。此前，这些记录会在命名空间删除后保留，并在创建同名命名空间时重新出现。

  此外，新增 `emqx ctl mt purge_ns <namespace>` CLI 命令，用于删除命名空间并清除其所有数据。该命令是幂等的，不要求命名空间必须存在，因此可作为最后手段清理由中断的命名空间删除操作留下的数据。

- [#18164](https://github.com/emqx/emqx/pull/18164) 改进在命名空间中执行备份导入时的反馈。导入不属于目标命名空间的归档（例如从其他命名空间导出的归档或全局备份）现在会返回清晰错误，而不是看似成功但没有导入任何内容。全局管理员仍可使用 `namespace` 查询参数恢复指定命名空间的备份。

  全局备份现在是完整的集群快照：全局导出也会包含每个命名空间的配置，全局导入会将每个命名空间的配置恢复到其自身命名空间。没有命名空间的集群仍会生成和读取与以前完全相同的归档。

- [#18222](https://github.com/emqx/emqx/pull/18222) 命名空间管理员 API 密钥现在获得与命名空间 Dashboard 用户相同的默认 scope 列表。特别是，默认值不再包含 `publish` scope，因为 publish API 是全局专用的，命名空间 API 密钥无法使用。创建新的命名空间 API 密钥时显式 scope 列表包含 `publish`，或修改现有命名空间 API 密钥 scope 列表时包含 `publish`，现在都会返回校验错误。现有 API 密钥不受影响：此前存储的 scope 列表（包括包含 `publish` 的列表）会原样保留并继续按以前方式工作。

- [#18227](https://github.com/emqx/emqx/pull/18227) 修复已删除托管命名空间的客户端在异步踢除进行期间，可能临时不受命名空间速率限制而发布消息的问题。

- [#18339](https://github.com/emqx/emqx/pull/18339) 修复数据备份导入隔离问题：上传的归档可能删除或写入属于其他命名空间的备份文件。现在，导入会在调用方自身命名空间目录内解压和清理。包含符号链接或硬链接成员的备份归档现在会被拒绝。

- [#18372](https://github.com/emqx/emqx/pull/18372) [#18378](https://github.com/emqx/emqx/pull/18378) 确保命名空间的备份文件和托管证书包操作始终限制在该命名空间自己的目录内。对于名称不能用作目录名的命名空间，例如 `.`、`..`、空名称或包含路径分隔符的名称，这些操作不可用。

#### 网关

- [#17796](https://github.com/emqx/emqx/pull/17796) 修复 MQTT-SN 网关在新设备从最近被已断开设备使用过的 UDP 源端口连接时发生崩溃的问题（常见于 loopback 或 NAT 后场景，操作系统或 NAT 设备会重新分配同一端口）。现在，旧 channel 会被正常关闭，新连接会作为新会话处理。

- [#17805](https://github.com/emqx/emqx/pull/17805) 修复网关重新加载时，如果上一次加载尝试中途失败（例如配置无效或监听端口繁忙），后续重新加载可能因 `already_started` 错误失败的问题。失败尝试遗留的 locker 进程现在会自动回收，因此下一次 `load`（或运维人员重试）会从干净状态开始。

- [#17815](https://github.com/emqx/emqx/pull/17815) 修复 UDP 源元组变化或复用时的 MQTT-SN UDP 会话路由。

  MQTT-SN UDP 监听器现在会通过 `esockd_udp_proxy` 按报文中解析出的 ClientId 路由报文，使休眠会话可以从不同 UDP 源元组恢复，同时防止复用的 UDP 源元组将另一个 ClientId 的报文投递给旧会话。

- [#17888](https://github.com/emqx/emqx/pull/17888) 修复 LwM2M 网关可能在注册/更新 MQTT 报告中包含敏感 REGISTER 查询字段的问题，例如 `password`、`secret`、`private_key` 和 `access_token`。

- [#18051](https://github.com/emqx/emqx/pull/18051) 修复 CoAP debug 日志泄露敏感 URI query 值的问题。

#### 插件

- [#17861](https://github.com/emqx/emqx/pull/17861) 恢复此前的插件启动行为：节点启动或重新加入集群时，不再删除集群插件配置中缺失的本地插件包。

- [#17884](https://github.com/emqx/emqx/pull/17884) 修复插件管理 HTTP API，使其忽略不在集群插件配置中且本地未运行的过期解包插件目录。

  此类过期包不再出现在插件列表、详情、配置或 schema 响应中，不能通过插件操作 API 操作，也不再阻止通过 HTTP 安装 API 重新安装同一软件包。已配置的预安装插件仍可见，并继续遵循已记录的预安装流程。

  当某个插件包已解包但既未在 `plugins.states` 中启用也未禁用时，EMQX 现在会在启动和访问 HTTP API 时记录错误。

- [#17932](https://github.com/emqx/emqx/pull/17932) 修复通过 CLI 安装插件时产生噪声 `failed_to_get_plugin_config_from_cluster` 警告的问题。

  `emqx ctl plugins install` 命令现在以 `fresh_install` 模式安装插件（与 HTTP API 行为一致），会跳过新安装插件的集群配置查询，避免每个节点反复出现 `config_not_found_on_node` 警告。

  新增 `--cluster` 参数用于集群范围安装。指定后，插件包会通过单个命令分发并安装到所有运行中的节点。

- [#18018](https://github.com/emqx/emqx/pull/18018) 修复插件包安装过程中，在校验包的应用声明、配置 schema 和默认配置前加载代码的问题。

- [#18153](https://github.com/emqx/emqx/pull/18153) [#18172](https://github.com/emqx/emqx/pull/18172) 修复插件配置 API 在根 JSON 值类型错误时返回可读校验错误，而不是返回 `500 INTERNAL_ERROR`。

- [#18304](https://github.com/emqx/emqx/pull/18304) UNS Governance 插件现在会在每次向受治理主题发布时校验消息 Payload。此前，当授权缓存仍有效时，对同一主题的重复发布可能会跳过 Payload 校验。

- [#18333](https://github.com/emqx/emqx/pull/18333) 修复声明 `emqx_plugins` 为应用依赖的插件在节点重启后无法启动的问题。

  插件会在插件子系统自身启动期间启动。如果某个插件声明 `emqx_plugins` 为依赖项，该插件自身的启动会等待插件子系统启动，导致插件启动超时，并在每次节点重启后处于已启用但未运行状态。现在，EMQX 会忽略该依赖声明，并记录警告，要求插件作者移除该声明。

  当插件因超时启动失败时，错误日志现在会列出当时尚未运行的已声明依赖应用。

- [#18337](https://github.com/emqx/emqx/pull/18337) 在所有 EMQX 应用启动完成后再启动插件。插件现在可以在其 `applications` 列表中声明任意 EMQX 应用。此前，如果插件声明了启动序列中较晚启动的应用（例如 `emqx_management`），节点重启后该插件会启动失败。

#### 可观测性

- [#17886](https://github.com/emqx/emqx/pull/17886) 通过 Prometheus 暴露发布配额超限报文指标 `emqx_packets_publish_quota_exceeded`。

- [#18114](https://github.com/emqx/emqx/pull/18114) 修复节点加入集群期间，Dashboard 指标 API（`GET /api/v5/monitor_current` 和 `GET /api/v5/monitor`）返回 `500 INTERNAL_ERROR` 的问题。

  加入中的节点正在重启其应用时，采样其指标会失败；现在会容忍此类失败：API 返回其余可达节点的聚合结果并记录警告，而不是让整个请求失败。

  同时修复了每次成功执行 `DELETE /api/v5/monitor` 请求时都会记录的虚假 `clear_monitor_metrics_rpc_errors` 警告。

- [#18183](https://github.com/emqx/emqx/pull/18183) 修复未启用多租户功能时，Prometheus 指标采集可能反复失败（每次抓取都记录错误）的问题。当对应功能未激活时，按命名空间划分的会话、认证和授权指标现在会直接省略。

- [#18292](https://github.com/emqx/emqx/pull/18292) 修复 `/prometheus/namespaced_stats` 端点会为不存在的命名空间报告零值指标的问题。当请求的命名空间未知时，其指标现在会从输出中省略，与采集所有命名空间指标的行为保持一致。

#### 文件传输

- [#18069](https://github.com/emqx/emqx/pull/18069) 修复文件传输文件 API（`GET /api/v5/file_transfer/files`）在列出名称包含非 ASCII 字符的文件时返回 500 错误的问题。
- [#18315](https://github.com/emqx/emqx/pull/18315) MQTT 文件传输文件列表和下载 REST 端点现在仅限全局（非命名空间）Dashboard 用户和 API 密钥使用。命名空间用户和 API 密钥不能再读取其命名空间之外客户端上传的文件。

#### 部署

- [#17877](https://github.com/emqx/emqx/pull/17877) 修复 `emqx-enterprise` Helm chart 在节点主机名中硬编码 `svc.cluster.local` 的问题。在 DNS 域不是 `cluster.local` 的 Kubernetes 集群上，节点会使用无法解析的 FQDN 为自身命名，导致 Erlang distribution 无法启动，节点无法组成集群。现在，主机名会遵循 chart 的 `clusterDomain` 值，该值此前已经用于 DNS 和 Kubernetes discovery 设置。

#### AI 互操作性

- [#17936](https://github.com/emqx/emqx/pull/17936) 修复 HTTP API 中属于全局命名空间的 A2A cards 的格式。此前，它们会显示为字符串 `"global"`。现在，它们会格式化为 `null`，以区别于具体命名空间。

## 6.2.2

*发布日期: 2026-07-02*

在升级到 EMQX 6.2.2 之前，请务必查阅不兼容变更和已知问题。

### 增强

#### 集群

- [#17530](https://github.com/emqx/emqx/pull/17530) 集群连接现在需要非社区版 License。在默认社区版 License 下，已配置的连接会保持非活动状态（不转发消息，也不复制路由），REST API 在尝试启用连接时会拒绝请求，并明确提示需要加载非社区版 License。禁用和删除连接仍可使用，以便清理遗留配置。升级 License 后，可通过 Dashboard 或 REST API 启用连接，无需重启节点。
- [#17549](https://github.com/emqx/emqx/pull/17549) 新增 EMQX Backup Sync 插件，通过数据备份 API 定期将选定配置从主集群同步到备集群。该插件支持为对主集群的 HTTPS 调用配置 TLS 选项。
- [#17620](https://github.com/emqx/emqx/pull/17620) 新增面向运维人员的诊断模块 `emqx_router_tool`，用于检查和修复路由表。该模块可通过 `emqx ctl eval` 运行，提供三个辅助函数：

  - `cluster_schema_view/0` 用于报告每个集群节点正在使用的路由存储 schema。
  - `scan_missing_routes/0,1` 会流式扫描本地订阅表，并报告本节点缺少对应路由条目的主题。该扫描会执行两遍、限流运行，并可容忍并发订阅和取消订阅。
  - `reconcile_missing_routes/0,1` 会通过现有 `emqx_router:add_route/2` API 重新添加缺失的路由。

  该模块不依赖特定 schema，可安全用于运行中的集群。

#### 多租户

- [#17665](https://github.com/emqx/emqx/pull/17665) 为多租户应用新增按命名空间统计的消息丢弃计数器和投递丢弃计数器。这些计数器通过 `/api/v5/prometheus/namespaced_stats` 暴露，并带有 `namespace` 标签，与现有按命名空间划分的指标族一起提供。运维人员现在可以直接通过 Prometheus 按租户诊断丢弃率，而无需依赖日志排查。

  已知限制：QoS 2 PUBREL 等待超时导致的丢弃目前还无法按命名空间归因，因为该丢弃路径只递增全局计数器，且不会触发 `message.dropped` hook。

- [#17711](https://github.com/emqx/emqx/pull/17711) 统一了内置数据库认证用户 HTTP API 中的命名空间选择方式，并允许清理已删除命名空间遗留的记录。

  此前，只有创建用户时支持在请求体中传入 `namespace` 字段；更新和删除用户时只能通过 `ns` 查询参数指定目标命名空间。现在，更新和删除端点也支持在请求体中传入 `namespace` 字段。当二者同时提供时，`ns` 查询参数优先。用户列表仍继续使用 `ns` 查询参数。

#### 访问控制

- [#17564](https://github.com/emqx/emqx/pull/17564) 为加固安全配置下的授权后端故障新增失败关闭（fail-closed）行为。在加固模式下，后端故障和无效的后端响应现在会拒绝访问；旧模式保留现有的忽略和回退行为。
- [#17589](https://github.com/emqx/emqx/pull/17589) 为加固安全配置下的访问控制钩子回调故障新增失败关闭（fail-closed）处理。认证或授权钩子回调崩溃时，现在会拒绝访问，而不是被忽略。
- [#17674](https://github.com/emqx/emqx/pull/17674) 在加固安全配置下，当认证后端发生故障或返回格式错误的响应时，认证后端现在会采用失败即拒绝行为。可通过 `authentication_settings.ignore_backend_failures` 保留旧行为。
- [#17696](https://github.com/emqx/emqx/pull/17696) 加固使用 JWKS 的 JWT 认证：在加固安全配置下，默认验证 JWKS 端点的 TLS 证书；当 JWKS 密钥不可用时拒绝提交的 JWT；在加固模式下拒绝缺失 JWT 凭据的连接。

#### 数据集成

- [#17481](https://github.com/emqx/emqx/pull/17481) 为 MQTT Bridge 入口（Source）订阅新增 `retain_as_published` 选项。当 Bridge 使用 MQTT 5.0 连接到远端 Broker 且 `retain_as_published = true` 时，转发消息会保留原始 `retain` 标志，而不是清除该标志，从而可以如实重新发布来自上游的保留消息。默认值为 `false`，以保持现有行为。当 `proto_ver` 为 `v3` 或 `v4` 时，该选项不生效。

  此外，当同时配置 `bridge_mode = true` 和 `proto_ver = v5` 时，连接器现在会输出一条警告日志，因为旧版 bridge-mode 标志在 MQTT 5.0 下不生效；请改为在单个订阅上设置 `retain_as_published`。

- [#17508](https://github.com/emqx/emqx/pull/17508) 为 PostgreSQL 和 TimescaleDB 连接器连接设置 PostgreSQL `application_name` 启动参数为 `emqx`。

  这使得 EMQX 数据库会话更容易在 PostgreSQL 日志和 `pg_stat_activity` 等视图中识别。

- [#17576](https://github.com/emqx/emqx/pull/17576) 通过现有 `ssl.ciphers` 字段为 GreptimeDB 连接器新增 TLS 密码套件配置支持。指定密码套件列表后，TLS 协商将被限制为这些套件。不支持的密码套件会在连接器启动时被拒绝。

- [#17594](https://github.com/emqx/emqx/pull/17594) 支持为 Google Cloud Pub/Sub 和 BigQuery 连接器的 `service_account_json` 配置 `file://` 密钥文件，从而可以从外部文件注入服务账号凭证。

- [#17717](https://github.com/emqx/emqx/pull/17717) 为 Confluent Producer 连接器新增启用 TLS 对端验证的选项。

- [#17718](https://github.com/emqx/emqx/pull/17718) 为 GCP PubSub Producer/Consumer 和 BigQuery 连接器新增启用 TLS 对端验证的选项。

#### 可观测性

- [#17558](https://github.com/emqx/emqx/pull/17558) 在 `GET /monitor_current` HTTP API 中新增两个指标及其对应速率：`rules_matched` 和 `actions_executed`。它们分别跟踪规则匹配情况和动作执行速率，其中动作执行速率包括执行成功和执行失败的情况。

  此更新还修复了非批处理模式（`batch_size = 1`）下 `actions.executed` 计数偏低的问题：该计数器现在会在每次动作回调调用时递增一次，不再依赖 buffer worker 遥测刷新窗口。

- [#17712](https://github.com/emqx/emqx/pull/17712) 新增 `emqx_session_tool` 诊断模块，运维人员可通过远程控制台调用。使用 `emqx_session_tool:top_by(mqueue_len)`，可在连接数较多的集群中按 gauge 或 counter 值查找 top-K 会话。还支持其他会话指标，例如 `mqueue_dropped` 和 `inflight_cnt`。这有助于运维人员定位最繁忙的会话，而无需手动翻阅客户端列表。

  该扫描会流式遍历 channel registry，仅保留有界的 top-K 结果，并读取缓存的单会话指标，而不会向连接进程发送消息。`emqx_session_tool:cluster_top_by/1` 会汇总所有集群节点上的结果。

- [#17758](https://github.com/emqx/emqx/pull/17758) Prometheus `emqx_messages_retained` 计数器现在会报告实际的保留消息写入次数。此前该指标虽已暴露，但从未递增，因此始终为 0。现在，每次成功存储保留消息都会递增该计数器。

### 修复

#### 核心 MQTT 功能

- [#17529](https://github.com/emqx/emqx/pull/17529) 修复了通过消息队列订阅投递的 QoS 0 消息可能在内部保持未确认状态的问题。该问题会导致队列订阅者在达到本地 inflight 限制后停止接收更多消息。
- [#17540](https://github.com/emqx/emqx/pull/17540) 修复在 SSL 监听器上设置 `password = "file://..."` 时，如果 keyfile 已加密，配置校验会因 `bad_password_or_invalid_keyfile` 失败的问题。现在，`file://` 引用会在校验期间解析，而不只是在运行时解析。
- [#17569](https://github.com/emqx/emqx/pull/17569) 将 MQTT v5 User Property 解析成本从平方复杂度降低为线性复杂度。

  此前，当 CONNECT、PUBLISH 或 SUBSCRIBE 报文携带大量 User Property 时，每个解析出的属性都会追加到累积列表末尾，导致拥有该连接的进程出现超线性的调度耗时。现在，解析会在保留属性顺序的同时按条目数量线性扩展。

- [#17731](https://github.com/emqx/emqx/pull/17731) 修复更新 WS 或 WSS 监听器选项时可能出现的临时性 "address already in use" 错误（例如轮换 TLS 证书时）。更新此类监听器会重新绑定端口，而操作系统可能尚未释放旧 socket；现在 EMQX 会短暂重试重新绑定，而不是直接让更新失败。

- [#17798](https://github.com/emqx/emqx/pull/17798) 修复保留消息可能使用原始发布 QoS 投递，而不是使用通配符订阅 QoS 上限投递的问题。

- [#17801](https://github.com/emqx/emqx/pull/17801) `ssl_opts.ciphers` 校验器现在接受 OpenSSL 或 IANA/RFC 命名格式的密码套件名称。此前，仅支持 OpenSSL 格式的名称，因此以 IANA 名称提供的有效 TLS 1.2 密码套件（例如 `TLS_ECDHE_ECDSA_WITH_AES_256_GCM_SHA384`）会被错误地拒绝为 `bad_ciphers`，即使 Erlang 的 `ssl` 模块本可以接受该名称。TLS 1.3 密码套件不受影响，因为它们的 IANA 名称和 OpenSSL 名称相同。

#### 队列与流

- [#17515](https://github.com/emqx/emqx/pull/17515) 修复了使用 QoS 0 的消息队列订阅在队列订阅者本地 inflight 窗口满后可能停止接收消息的问题。
- [#17733](https://github.com/emqx/emqx/pull/17733) 修复消息队列消费者在持久存储订阅恢复后，可能无法恢复空流缓冲区的问题。

#### 规则引擎

- [#17725](https://github.com/emqx/emqx/pull/17725) 修复 6.0.3、6.1.2 和 6.2.1 中引入的问题：当发布客户端携带租户命名空间（`client_attrs.tns`）时，全局规则可能无法再匹配其 `FROM` 主题上的消息。

  当启用 `rule_engine.limit_selects_in_namespace`（默认启用）时，全局规则现在会保留系统范围可见性，并匹配来自任意命名空间的消息。在命名空间内创建的规则仍隔离在各自命名空间内。若运维人员希望完全禁用命名空间限制，仍可设置 `rule_engine.limit_selects_in_namespace = false`。

#### 数据集成

- [#17568](https://github.com/emqx/emqx/pull/17568) 将 Kafka 客户端库 `brod` 升级到 4.5.5。

  消费者组：当 join 响应携带 `member_id_required` 错误码（由不支持静态成员实例 ID 的旧版 Kafka Broker 返回，例如 2.2.0）时，尊重 Broker 分配的 member ID。此前，错误返回中的 member ID 会被丢弃，导致重试无法成功。

- [#17579](https://github.com/emqx/emqx/pull/17579) 修复 Redis Sentinel 连接器，使其为每个资源使用独立的 Sentinel 管理器，并在资源停止时清理这些管理器，避免连接器之间共享 Sentinel 状态。

- [#17584](https://github.com/emqx/emqx/pull/17584) 限制了 Snowflake 聚合连接器健康检查期间返回的数据量。仅当已有 schema 列表非常大时才有可观察影响，此时健康检查的执行时间将大幅缩短。

- [#17588](https://github.com/emqx/emqx/pull/17588) 限制了 Kinesis 集成的连接器和动作健康检查期间返回的数据量。仅当已有 schema 列表非常大时才有可观察影响，此时健康检查的执行时间将大幅缩短。

- [#17595](https://github.com/emqx/emqx/pull/17595) 限制了 S3 和 S3 Tables 集成的连接器健康检查期间返回的数据量。仅当已有桶（bucket）列表非常大时才有可观察影响，此时健康检查的执行时间将大幅缩短。

- [#17598](https://github.com/emqx/emqx/pull/17598) 修复 MongoDB 8.0+ 在需要认证时的连接失败问题。此前，驱动会在认证前查询 `buildInfo` 以选择认证机制；MongoDB 8.0 将该命令限制为只有已认证调用方可用。现在驱动会跳过该探测，并直接使用所有受支持 MongoDB 版本都接受的 SCRAM-SHA-1。

- [#17605](https://github.com/emqx/emqx/pull/17605) 修复 Oracle 动作的 prepare/status 检查，使其在不执行 SQL 的情况下解析动作 SQL，并拒绝不受支持的顶层 DDL/DCL/TCL 语句。同时改进了对超过 4000 字节文本 Payload 的支持，适用于 Payload 占位符是最后一个绑定参数的场景。

- [#17625](https://github.com/emqx/emqx/pull/17625) 修复 GCP PubSub Consumer Source 的问题：如果 Source 初始创建时使用的服务账号缺少为配置主题创建订阅所需的权限，即使后来向该服务账号授予了权限，该 Source 也无法变为 `connected`。

- [#17633](https://github.com/emqx/emqx/pull/17633) 修复 6.1.2 和 6.2.1 中引入的问题：MQTT Bridge 和集群连接的 TLS 连接在短时间流量后可能停滞。受影响节点会反复记录来自 `emqtt` 客户端的类似 `unexpected_event ... ssl_passive ...` 的错误消息。EMQX 现在内置 `emqtt` 1.15.3，在 [#17617](https://github.com/emqx/emqx/issues/17617) 首次报告该问题后恢复了正常流量传输。

- [#17649](https://github.com/emqx/emqx/pull/17649) 提升启动和停止 GCP PubSub Consumer 连接器的响应性。此前，如果连接较慢或繁忙，可能出现超时，导致连接器仍在运行且状态与配置不一致。

- [#17681](https://github.com/emqx/emqx/pull/17681) 修复禁用预处理语句时 PostgreSQL 连接器的批量写入问题。

  此前，同一连接上的并发批次可能会交错执行原始 SQL 解析，并导致 PostgreSQL 协议错误。表存在性检查现在也会通过连接器 worker 串行执行，以避免与批量执行交错。

- [#17701](https://github.com/emqx/emqx/pull/17701) 修复 PostgreSQL 动作在批处理中使用会返回结果行的 SQL 模板（例如 `SELECT ...`）时，出现含义不清的 `badarith` 错误的问题。

  PostgreSQL 动作批处理不支持返回结果行的 SQL。现在 EMQX 会返回明确的不支持 SQL 错误，而不是让批处理结果处理器崩溃。

#### 集群

- [#17586](https://github.com/emqx/emqx/pull/17586) 定期清理全局会话注册表中的陈旧条目。

  此前，如果会话的属主进程在未正常注销的情况下退出，并且相同的客户端 ID 再也没有重新连接，注册表行可能会永久残留。例如，短暂网络分区导致注销操作未能复制，或在 down 事件清理期间某个 core 节点的一致性检查超时，都可能触发该问题。

  现在，每个 core 节点上都有一个受限流控制的后台清理任务来移除此类行。该任务限制为每个节点每秒最多 500 行，且运行间隔不短于 10 分钟，因此即使在持有数百万会话的注册表上也不会对 Broker 吞吐量产生可观测影响。

- [#17773](https://github.com/emqx/emqx/pull/17773) 修复配置更新命令（REST API 和 CLI）在底层集群 RPC 层意外中止时，可能触发 `function_clause` 崩溃报告的问题。例如，当节点启动或恢复期间集群 RPC 表尚不可用时，可能会出现 `{no_exists, cluster_rpc_mfa}`。现在，此类失败会作为结构化错误返回给调用方。

- [#17764](https://github.com/emqx/emqx/pull/17764) 修复某节点离线期间插件已从集群卸载时，该节点重新加入集群后本地仍可能残留过期插件条目的问题。现在，EMQX 会在插件启动过程中移除本地不再存在于集群插件配置中的插件包。

#### 访问控制

- [#17575](https://github.com/emqx/emqx/pull/17575) 修复 `emqx_username_quota` 插件中的竞争条件。该问题可能导致按用户名统计的会话计数器与实际跟踪的客户端记录数量不一致。计数器可能被递减到零以下，随后被删除；与此同时，并发会话注册又递增该计数器，导致该增量永久丢失。

- [#17644](https://github.com/emqx/emqx/pull/17644) 修复 `plain` 密码哈希算法在认证时接受仅大小写不同的密码的问题。

- [#17646](https://github.com/emqx/emqx/pull/17646) 修复 JWT 认证中 JWKS 获取客户端的 HTTP/1.1 协议合规性问题。早期版本由于 Erlang/OTP `inets` HTTP 客户端中的长期默认行为（已在 inets 9.4.2 / OTP 28.1 中修复），会发送空值 `TE:` 头。一些身份提供商（尤其是 PingFederate）会拒绝此类请求。EMQX 现在在获取 JWKS 时发送显式且有效的 `TE: trailers` 头。

- [#17653](https://github.com/emqx/emqx/pull/17653) 修复 Prometheus 配置 API 在响应中返回 Pushgateway 头部里已存储的 `Authorization` 头值的安全问题。现在 API 会在响应中对这些值进行脱敏。

- [#17654](https://github.com/emqx/emqx/pull/17654) 修复通过 `POST /authentication` 创建认证器时，返回的新认证器配置未对提供方密钥进行脱敏的问题，这些密钥包括 JWT HMAC 密钥、HTTP `Authorization` 头以及请求体密码等。创建响应现在会应用与 list 和 get 端点相同的脱敏处理。

- [#17657](https://github.com/emqx/emqx/pull/17657) 修复原始 `authorization` 和 `cookie` 头被转发到插件 API 回调的安全问题。现在这些包含凭据的头在到达插件代码前会被脱敏。

- [#17711](https://github.com/emqx/emqx/pull/17711) 当创建或更新内置数据库用户时，如果目标命名空间不是已知托管命名空间，现在会以 "Managed namespace not found" 失败。此前，当命名空间在请求体中提供时，即使该命名空间不存在，也可能创建用户。

  此外，全局管理员现在可以删除属于已删除命名空间的内置数据库用户，而不再收到 "Managed namespace not found" 错误。

- [#17736](https://github.com/emqx/emqx/pull/17736) 限制 JWT 认证器只能使用与配置密钥类型一致的 JWS 算法验证令牌。基于 HMAC 的认证器现在只接受 `HS256`、`HS384` 和 `HS512`。公钥和 JWKS 认证器接受 `RS*`、`PS*`、`ES*` 和 `EdDSA` 算法。`alg` 头与配置密钥类型不匹配的令牌（包括 `alg=none`）都会被拒绝。

- [#17739](https://github.com/emqx/emqx/pull/17739) 改进了日志、追踪和审计记录中敏感数据的脱敏处理。

- [#17787](https://github.com/emqx/emqx/pull/17787) 防止当 `ehttpc` worker 在请求过程中被终止时，HTTP 连接器错误日志中包含请求头。

  此前，如果 HTTP 连接器的 `ehttpc` worker 在请求尚未返回时被终止（例如在请求返回前删除对应 Source），生成的 EXIT reason 会携带原始 `gen_server:call` 参数。由于这些参数包含请求头，请求头会被原样写入错误日志。现在，EMQX 会在记录日志前从 reason 中移除这些调用参数。

- [#17790](https://github.com/emqx/emqx/pull/17790) 停止将 TOTP 共享密钥写入 `dashboard_login_failed` 服务器日志。此前，在首次设置 MFA 期间，该密钥会包含在此日志条目中。

- [#17791](https://github.com/emqx/emqx/pull/17791) 改进日志脱敏，避免 JWT HMAC 密钥字节出现在配置更新期间输出的 `cluster_rpc_apply_result` 和 `cluster_rpc_apply_ok` 调试日志中。

  脱敏器现在可以识别内部 JWK record 结构，并在记录日志前将其替换为占位符，同时也会将 `jwk` 字段视为敏感字段。

#### 多租户

- [#17715](https://github.com/emqx/emqx/pull/17715) 修复一个多租户准入检查缺口。此前，当配置了 `multi_tenancy.post_auth_tns_expression` 且表达式求值为空字符串或错误时，命名空间准入检查（`allow_only_managed_namespaces` 强制检查、会话配额等）会被跳过，从而允许客户端通过。

  空字符串和错误结果现在会被视为 "no namespace assigned"，并与认证前未提供命名空间的客户端一样经过同一准入检查。当 `allow_only_managed_namespaces = true` 时客户端会被拒绝；当其为 `false` 时，客户端会在不带命名空间的情况下被接受。在这种情况下，认证前 `client_attrs.tns` 中携带的任何命名空间值也会被清除，因此当表达式拒绝分配命名空间时，该值不会被保留。

- [#17757](https://github.com/emqx/emqx/pull/17757) 修复 `/prometheus/namespaced_stats`，使命名空间管理员和 API 密钥只能查看其所属命名空间的数据。全局管理员和 API 密钥仍可查看所有命名空间的数据。

#### 网关

- [#17556](https://github.com/emqx/emqx/pull/17556) 修复 OCPP 网关未将监听器 `enable_authn` 选项传递给共享认证流程的问题。该问题是由于该选项存储在拼写错误的 client-info key 下导致的。
- [#17581](https://github.com/emqx/emqx/pull/17581) 修复 JT/T 808 网关，使其使用认证期间接受的手机号作为连接身份，拒绝不匹配的注册码认证尝试以及手机号不同的后续上行帧。
- [#17604](https://github.com/emqx/emqx/pull/17604) 修复 GBT32960 网关路由：车辆对下行命令（参数查询、参数设置、终端控制）的响应现在会正确发布到 `upstream/response`，而不是 `upstream/transparent`。
- [#17528](https://github.com/emqx/emqx/pull/17528) 修复多个网关发布和订阅流程中缺少授权检查的问题。现在，以下操作会在发布或订阅前执行授权检查：MQTT-SN Will 消息发布；JT/T 808 上行发布和自动下行订阅；GBT32960 上行发布和自动下行订阅；以及 OCPP 上行发布和自动下行订阅。

#### 可观测性

- [#17497](https://github.com/emqx/emqx/pull/17497) 修复非批处理模式（`batch_size = 1`）下配置的动作中，`actions.executed` 指标低于 `actions.messages` 的问题。

  此前实现会在每次 buffer-worker 遥测刷新时递增一次 `actions.executed`，一次刷新可能聚合多个单独完成事件，因此即使未配置批处理，`actions.executed` 也会落后于 `actions.messages`。

  现在，这两个指标会在独立调用点递增：`actions.executed` 按动作回调调用次数递增（批处理模式下每批一次，单条模式下每条消息一次），`actions.messages` 按处理的消息数递增。

- [#17513](https://github.com/emqx/emqx/pull/17513) 修复 Prometheus 匹配授权允许/拒绝指标，使其反映实际匹配到的授权决策。

- [#17536](https://github.com/emqx/emqx/pull/17536) 在 Dashboard 中为 SSL 监听器 `password` 以及其他 secret 类型配置字段（MQTT Bridge 密码、集群连接密码、Dashboard OIDC 客户端密钥、S3 Secret Access Key、AI Completion API Key、Pulsar/RocketMQ 凭据等）的工具提示补充 `file://` 选项说明。通用 secret 类型描述已提到这一约定，但字段级描述会覆盖该说明，导致用户误以为这些字段只接受字面值。

- [#17708](https://github.com/emqx/emqx/pull/17708) 修复 logger JSON formatter 崩溃可能导致部分 debug 级别追踪事件被替换为 `FORMATTER CRASH` 行的问题。

## 6.2.1

*发布日期: 2026-06-11*

在升级到 EMQX 6.2.1 之前，请务必查阅不兼容变更和已知问题。

### 增强

#### 安全加固

- [#17040](https://github.com/emqx/emqx/pull/17040) 限制 API 密钥访问 Dashboard 用户账户管理端点。

  此前，具有 `administrator` 角色的 API 密钥可通过 HTTP Basic 认证调用 Dashboard 用户管理端点 `POST/DELETE /users/:username/mfa` 和 `POST /users/:username/change_pwd`，这意味着 API 密钥可以重置或禁用其他 Dashboard 用户的 MFA，或修改其密码，绕过了人工 Dashboard 会话与机器 API 密钥之间的预期隔离。

  这些接口现在在通过 API 密钥访问时返回 `401 API_KEY_NOT_ALLOW`，与已阻止 API 密钥访问 `/users`、`/users/:username`、`/logout` 和 `/api_key` 的现有策略保持一致。Dashboard 用户仍可通过 Bearer Token（JWT）会话在 Dashboard UI 中管理自己的 MFA 和密码。

- [#17065](https://github.com/emqx/emqx/pull/17065) 为规则引擎可访问的连接器和 Bridge 配置添加 SSRF 防护。

  当 `rule_engine.ssrf.enable` 设置为 `true` 时，EMQX 对连接器、Bridge 和动作配置应用出站 SSRF 策略。策略对每个目标的评估流程如下：`rule_engine.ssrf.deny_hosts` 中的精确匹配项立即被拒绝；解析后的目标 IP 先经 `rule_engine.ssrf.allow_cidrs` 检查，再经 `rule_engine.ssrf.deny_cidrs` 检查。默认拒绝范围涵盖回环地址、链路本地地址（包括云实例元数据端点）、RFC 1918、ULA、未指定地址和多播地址。检查在配置更新时执行，覆盖所有连接器类型的 HTTP `url` 字段及 `server`、`servers`、`bootstrap_hosts` 字段。

  该功能默认禁用，以保持与连接器合法指向内部服务的部署的兼容性。建议在多租户或对外暴露的环境中启用此功能，并配合网络层出站防火墙一同使用。

- [#17173](https://github.com/emqx/emqx/pull/17173) 限制 API 密钥通过数据备份端点导出或导入 Dashboard 账户及 API 密钥。

  使用 API 密钥调用 `POST /data/export` 时，生成的归档文件中将静默省略 `dashboard_users` 和 `api_keys` Mnesia 表集。使用 API 密钥调用 `POST /data/import` 时，若上传的备份包含上述任一表集，将返回 `403 FORBIDDEN`。

  使用 Dashboard bearer-token（登录）调用不受影响，仍可备份和还原完整数据库，包括 Dashboard 用户和 API 密钥。

  此修复关闭了一个权限提升漏洞。现有的 `/users` 和 `/api_key` 端点已拒绝 API 密钥访问 Dashboard 登录凭据和 API 密钥记录，但 API 密钥持有者此前可通过数据备份端点绕过这些限制。

- [#17187](https://github.com/emqx/emqx/pull/17187) 从未经认证的 `GET /status?format=json` 响应中移除 EMQX 发行版本号（`rel_vsn`），避免向未认证调用方泄露 Broker 版本信息。版本信息仍可通过需要认证的节点信息 API 获取。

- [#17201](https://github.com/emqx/emqx/pull/17201) 加强插件安装端点对上传 tarball 中路径穿越的防护，并收紧安装白名单。

  - 安装路径现在拒绝解压任何条目会解析到插件安装目录以外的 tarball。
  - `emqx ctl plugins allow <name-vsn>` 条目在签发后 5 分钟过期，并可通过 `emqx ctl plugins allow <name-vsn> sha256:<HEX>` 固定到软件包的 SHA-256 哈希值。内容与固定哈希值不匹配的上传将被拒绝并返回 `403 Forbidden`。省略可选的 `sha256:` 参数时，保留原有的接受任何名为 `<name-vsn>.tar.gz` 的载荷的行为。
  - 通过 HTTP 插件安装端点（及其封装的 Dashboard 上传）成功安装后，白名单条目会立即在集群范围内撤销，防止同一授权被重复用于不同的 tarball。

- [#17252](https://github.com/emqx/emqx/pull/17252) 在官方下载站点的插件包旁发布 `.sha256` 校验和附件，允许用户验证下载的插件归档完整性。

- [#17271](https://github.com/emqx/emqx/pull/17271) 加固官方 EMQX Docker 镜像，清除镜像扫描器报告的问题：

  - 在运行时镜像构建期间应用 Debian 安全升级，使镜像获取最新修复版 `libssl3t64`。
  - 移除未使用的 `libgnutls30t64` 包。EMQX 通过 Erlang/OTP 使用 OpenSSL 进行 TLS 通信，从不链接 GnuTLS，该包仅作为 `curl` 的传递依赖存在并出现在扫描报告中。
  - 将 Debian `curl` 包替换为来自 [stunnel/static-curl](https://github.com/stunnel/static-curl) 的静态链接 `curl` 二进制文件（OpenSSL、HTTP/2、HTTP/3；无 RTMP，无 GnuTLS）。Debian 包会通过 `librtmp1` 重新引入 `libgnutls30t64`；静态二进制文件避免了这一问题，同时保持调用 `curl` 的容器健康检查正常工作。

- [#17309](https://github.com/emqx/emqx/pull/17309) 对 PROXY Protocol v2 SSL Common Name 和 Subject 字段进行净化处理，防止控制字符被带入客户端身份信息。

  当监听器配置了 `proxy_protocol = true` 时，Broker 现在会拒绝 PROXY Protocol SSL TLV 字节中包含 ASCII 控制字符的连接（与已应用于 MQTT 摄取的 `clientid`、`username` 和 `password` 的字节类检查相同）。这阻止了攻击者控制的字节通过 `${cert_common_name}` 和 `${cert_subject}` 模板到达出站 HTTP 认证、授权或规则引擎头部值。

  HTTP 认证和授权客户端现在也会在渲染后的请求头名称或值包含 CR、LF 或 NUL 字节时拒绝发送请求。

- [#17315](https://github.com/emqx/emqx/pull/17315) 将 MQTT clientid/username/password 的字节类检查扩展至其他填充 `ClientInfo` 和 HTTP 请求模板的字段：

  - `peersni`（TLS 服务器名称指示；也可从 PROXY Protocol v2 的 `authority` TLV 接受）现在在连接摄取边界进行验证。包含控制字符的连接会被拒绝并记录警告日志。
  - 由 `mqtt.client_attrs_init` Variform 表达式生成的客户端属性值，若包含控制字符则被丢弃（并记录警告），从而防止 `${client_attrs.tns}` 等模板将注入字节传播至下游。
  - HTTP 动作/Bridge 连接器渲染头部时，任何渲染后名称或值包含 NUL、CR 或 LF 的头部都会被丢弃。

- [#17440](https://github.com/emqx/emqx/pull/17440) 将 `GET /api/v5/data/files/<filename>`（备份文件下载）限制为全局 Dashboard 管理员。备份归档可能包含 Dashboard 账户（含密码哈希及 MFA/TOTP 状态）和 API 密钥记录，因此 API 密钥调用方、Dashboard 查看者和命名空间管理员不再被允许下载。列出备份目录（`GET /api/v5/data/files`）的权限对之前有访问权限的所有角色保持不变。

- [#17491](https://github.com/emqx/emqx/pull/17491) 修复了网关认证 API、错误路径和调试日志中密码和密钥被暴露的问题。网关认证 API 响应现在在保留原始配置结构的同时对密钥进行脱敏处理。以下日志路径不再打印原始密码或密钥：网关认证失败日志、监听器启动错误日志、ExProto 认证日志、CoAP 令牌必需日志和 LwM2M 无效注册日志。

- [#17501](https://github.com/emqx/emqx/pull/17501) 阻止命名空间 Dashboard 用户跨命名空间读取 MQTT 消息内容。

  - 以下接口对任何非全局调用方返回 `403 FORBIDDEN`，因为它们可能暴露调用方命名空间之外的 MQTT Payload。此前，命名空间用户可以读取或删除其他命名空间产生的消息。

    - `GET /clients/:clientid/mqueue_messages`
    - `GET /clients/:clientid/inflight_messages`
    - `GET|DELETE /mqtt/retainer/messages`
    - `GET|DELETE /mqtt/retainer/message/:topic`
    - `GET /mqtt/delayed/messages`
    - `GET|DELETE /mqtt/delayed/messages/:node/:msgid`
    - `DELETE /mqtt/delayed/messages/:topic`

  - Trace API 现已按命名空间隔离：`GET /trace` 仅列出由调用方命名空间创建的追踪。单个追踪的端点（`/trace/:name`、`/trace/:name/download`、`/trace/:name/log`、`/trace/:name/log_detail`、`/trace/:name/stop`）在追踪属于其他命名空间时返回 `404`，防止调用方发现其他命名空间的追踪记录。批量 `DELETE /trace` 仅限全局管理员使用，命名空间调用方将收到 `403`。命名空间管理员对自己的追踪仍拥有完整权限，包括创建、列出、下载、流式传输、停止和删除。

#### 集群

- [#17076](https://github.com/emqx/emqx/pull/17076) 引入新的路由表同步机制。路由表 schema 版本升级至 `v3`，并向下兼容 `v2`。

  在 schema v3 中，每个节点（核心节点或副本节点）对指向自身的路由表条目拥有完全所有权，其他节点只有只读访问权限。这提升了 EMQX 集群的分区容忍度，分区集群中的对等节点无法代表其他节点修改路由表，同时也降低了副本节点的 `SUBACK` 延迟。

  **向下兼容性：** 当支持 v3 的节点加入仅支持 v2 的集群时，它将继续使用 v2 以保持兼容。要将集群切换至 v3，请在升级后执行完整集群重启。若需阻止自动切换，请将 `broker.routing.storage_schema` 设置为 `v2`。

  **降级说明：** 集群切换至 v3 后，不支持滚动降级。

  查看节点当前路由 schema 版本：

  ```
  emqx eval 'emqx_router:get_schema_vsn()'
  ```

- [#17152](https://github.com/emqx/emqx/pull/17152) 支持为分发端口配置 Erlang inet 端口选项，默认 `buffer` 大小为 1 MB。

  此前，Erlang 分发端口使用极小的默认端口缓冲区（1460 字节，某些平台约为 9 KB），即使分发端口缓冲区（`+zdbbl`）配置了更大的值（如 32 MB），仍会导致性能瓶颈。该问题影响集群通信可靠性，可能表现为 `erpc timeout` 错误、Mnesia 事务拥塞以及多核节点支持下降。

#### 可观测性

- [#17018](https://github.com/emqx/emqx/pull/17018) 减少调用 Prometheus 采集 API 端点时对其他节点的请求次数，使 API 调用返回更快，并降低集群高负载时超时的概率。

  具体而言，`emqx_mria_lag` 指标（副本节点关注）现在每 10 秒定期刷新一次（默认值），而非每次 API 调用时按需刷新。

- [#17162](https://github.com/emqx/emqx/pull/17162) 通过 Prometheus 指标（`emqx_license_max_sessions`、`emqx_license_expiry_at`、`emqx_license_issued_at`）暴露每节点的 License 信息，无需对每个节点执行 CLI 检查即可对集群级 License 一致性进行告警。

  运维人员现在可以通过比较这些指标对集群节点间的 License 不一致进行告警。该实现通过单次 `emqx_license_checker:dump/0` gen_server 调用获取全部三个值，消除了每次 Prometheus 采集时的冗余往返。

- [#17176](https://github.com/emqx/emqx/pull/17176) 新增 `emqx_routes_count` 和 `emqx_routes_max` Prometheus 指标，用于导出每个节点的路由表条目数量。

- [#17329](https://github.com/emqx/emqx/pull/17329) 在 `/api/v5/prometheus/stats` 端点新增两个节点级 gauge 指标：

  - `emqx_vm_uptime_ms`：报告 EMQX 节点运行时间（毫秒）。
  - `emqx_vm_max_fds`：报告节点可用的最大文件描述符数量。

- [#17031](https://github.com/emqx/emqx/pull/17031) 新增 License 用量审计的会话高水位线历史记录功能。

  EMQX 现在记录每日峰值会话数，并保留至少 24 个月的历史数据。运维人员可通过 `emqx ctl license history` 命令查询这些数据，支持可选的 `--period daily|monthly` 和 `--json` 参数。新增 `license.high_watermark_timezone` 配置项，用于控制分桶时的日期边界。

#### 访问控制

- [#16942](https://github.com/emqx/emqx/pull/16942) [#17235](https://github.com/emqx/emqx/pull/17235) 为 API 密钥和 Dashboard 登录用户引入细粒度的基于 Scope 的访问控制。

  API 密钥现在可以使用源自 OpenAPI 标签的 scope 限制到特定 API 路径类别。没有 scope 的密钥保留完整访问权限（向后兼容）。scope 列表为空时拒绝所有受 scope 保护的 API 路径。`publisher` API 密钥角色现在仅限于 `[publish]` scope。

  Dashboard 登录用户现在也支持可选的 `scopes` 字段；设置后，请求将在现有基于角色的检查之上，与 API 密钥所用的路径到 scope 目录进行授权。四个新 scope（`user_management`、`mfa_management`、`sso_management`、`api_key_management`）覆盖仅限 Dashboard 的端点，除 `mfa_management`（任何角色均可持有以豁免强制 MFA）外，其余仅限管理员持有。API 密钥不能持有四个登录专用 scope 中的任何一个。两项检查均适用于 HTTP API 和引导文件加载（不兼容的 scope 会被丢弃并记录警告）。

  新增公开目录端点供 UI 使用：`GET /api_key_scopes` 和 `GET /user_scopes`，任何 bearer 认证调用方均可访问。`scopes` 字段也在 `GET /users`、`POST /users` 和 `PUT /users/:username` 响应中展示；未显式设置时，响应将投影角色默认的 scope 列表。

  新 scope 模型带来的其他行为变更：

  - `dashboard.default_username` 用户被保护为紧急访问账户，不可删除、不可降级（取消管理员角色）、不可设置 `scopes` 字段，仅允许修改其 `description`。这确保运维人员在其他管理员丢失或错误配置 scope 时始终保留管理员访问权限。
  - 用户对自身记录的自助服务操作现在受 scope 约束。只有专用的修改密码和 MFA 自助端点仍绕过 scope 检查；其他操作（如 `PUT /users/:self`）受用户的 scope 约束。
  - `PUT /users/:username` 和 `PUT /api_key/:name` 在请求体省略 `scopes` 字段时，会根据持久化的有效 scope 验证角色变更。若持久化 scope 与新角色不兼容，则拒绝降级用户或变更 API 密钥角色。
  - API 密钥引导文件支持可选的第四列 scope（`key:secret:role:scopes`）。未知或与角色不兼容的 scope 名称会被丢弃并记录警告，而非拒绝整个文件，因此现有的三列引导文件仍可正常加载。
  - SAML SP 元数据端点（`GET /sso/saml/metadata`）现在无需认证即可访问，与 `/sso/saml/acs` 保持一致。

- [#16943](https://github.com/emqx/emqx/pull/16943) 为 SSO（OIDC/SAML/LDAP）新增每后端 `force_mfa` 选项。

  启用后，无论身份提供商侧的 MFA 设置如何，SSO 用户在获得 Dashboard token 之前必须完成 TOTP MFA 设置或验证。支持三种 MFA 状态：`not_configured`（强制设置）、`enabled`（要求验证）和 `admin_disabled`（跳过 MFA）。新增 API 端点 `POST /sso/mfa/setup` 和 `POST /sso/mfa/verify` 处理 MFA 流程。

  管理员可以通过对 `/users/:username/mfa` 执行 DELETE/POST 操作对现有用户进行豁免或强制要求，该操作优先于实时后端策略，直到管理员再次修改。在 `force_mfa = true` 后端上禁用了自身 MFA 的 SSO 用户，下次登录时须重新设置 MFA；只有管理员发起的禁用操作才能豁免用户不受实时策略约束。

- [#17178](https://github.com/emqx/emqx/pull/17178) `emqx ctl api_keys add` CLI 命令现在支持 `--scopes <scope1,scope2,...>` 选项，与 REST API 已支持的基于 scope 的权限控制保持一致。

- [#17218](https://github.com/emqx/emqx/pull/17218) 新增 ACME 客户端插件（`emqx_acme`），可从任何符合 RFC 8555 的 ACME CA（如 Let's Encrypt）为 EMQX 托管证书包签发和续签 TLS 证书，并将配置的 SSL/WSS 和/或 Dashboard HTTPS 监听器切换为使用该证书包。

#### 多租户

- [#17053](https://github.com/emqx/emqx/pull/17053) 新增多租户配置选项 `multi_tenancy.post_auth_tns_expression`。

  配置后，它是一个在认证链完成后求值的 [Variform](https://docs.emqx.com/en/emqx/latest/configuration/configuration.html#variform-expressions) 表达式，渲染结果写入 `client_attrs.tns`（多租户配额和路由决策使用的租户命名空间键）。

  这使运维人员能够从认证响应属性（例如 HTTP 认证后端返回的 `tag` 字段）派生租户命名空间，而不必仅依赖认证前的 `mqtt.client_attrs_init`。示例表达式：`client_attrs.tag`，或带回退的写法 `coalesce(client_attrs.tag, username)`。

  表达式为空时（默认值），行为不变。

- [#17078](https://github.com/emqx/emqx/pull/17078) 在 `GET /api/v5/mt/managed_ns_list_details` 的响应中内联每个托管命名空间的配置（会话和限速器），使管理 UI 可以通过单次请求渲染命名空间列表及其配置，而无需为每个命名空间额外发起一次请求。

#### 网关

- [#17013](https://github.com/emqx/emqx/pull/17013) 为 GBT32960 网关新增 GBT32960-2025 协议支持。

  网关现在通过帧头（`##` 对应 2016 版，`$$` 对应 2025 版）自动检测协议版本，并处理版本相关的解析和序列化，包括：

  - 2025 版新信息类型：整车、驱动电机、燃料电池、发动机、位置、报警、动力电池电压/温度、燃料电池堆、超级电容、超级电容极值和数字签名。
  - 新命令：激活（0x09/0x0A）。
  - 参数查询/设置（0x02/0x03）中版本相关的参数大小（2025 版为 BYTE，2016 版为 WORD）。
  - 2025 版整车登录含 BMS 电池包编码字段。

#### 数据集成

- [#17011](https://github.com/emqx/emqx/pull/17011) 为 EMQX Tables（Rust NIF 驱动）连接器新增 `ts_column` 和 `ttl` 配置字段。

  - `ts_column`：为自动创建的表指定自定义时间戳列名（未设置时默认为 `ts`）。
  - `ttl`：为自动创建的表设置存活时间提示（如 `3 days`）。

  这两个字段在底层 `greptimedb-ingester-erlnif` 驱动（0.1.8 起）中已受支持，现在在 EMQX Tables 连接器配置中正式暴露。

- [#17025](https://github.com/emqx/emqx/pull/17025) 更改了 InfluxDB 数据库执行健康检查和凭据验证的方式。

  不再通过执行 `SHOW DATABASES` 进行检查，该操作可能被某些审计系统误报为系统渗透。

  另请参阅 [emqx/influxdb-client-erl#54](https://github.com/emqx/influxdb-client-erl/pull/54)。

- [#17027](https://github.com/emqx/emqx/pull/17027) A2A Registry HTTP API 现已感知命名空间。列出、删除和写入的 Agent Card 受限于 API 用户所属的命名空间。

- [#17046](https://github.com/emqx/emqx/pull/17046) 新增 `actions.messages` 指标（及 Dashboard 监控 API 中对应的 `actions_messages_rate`），统计规则引擎动作执行处理的消息总数。

  由于单次动作执行可能处理一批消息，`actions.messages` 大于或等于 `actions.executed`，`actions_messages_rate` 反映动作的真实每消息吞吐量。

- [#17089](https://github.com/emqx/emqx/pull/17089) MQTT 入口 Bridge 现在支持在远端 Broker 支持 MQTT 5 订阅标识符时，从以 `$queue/{name}/{bind-filter}` 形式暴露的远端消息队列中消费消息。当订阅标识符不可用时，队列订阅将被拒绝；若远端 Broker 不接受订阅标识符，普通主题订阅会自动重试（不带订阅标识符）。

- [#17104](https://github.com/emqx/emqx/pull/17104) 为聚合上传动作（Azure Blob Storage、Amazon S3、GCS、Snowflake、S3 Tables）的 Blob 名称模板新增日期部分占位符。占位符以聚合开始时间为基准渲染，默认使用 UTC。这支持 Hive 分区对象布局（如 `year=2025/month=04/day=22/hour=07/...`），可直接供 Spark、Databricks 和 Synapse 使用。

  支持的占位符：

  - `${datetime.YYYY}`
  - `${datetime.MM}`
  - `${datetime.DD}`
  - `${datetime.hh}`
  - `${datetime.mm}`
  - `${datetime.ss}`
  - `${datetime.DOY}`（年中第几天）

  每个占位符可添加显式时区前缀：

  - `utc`（默认）：如 `${datetime.utc.YYYY}`
  - `local`（EMQX 节点的系统时区）：如 `${datetime.local.YYYY}`

- [#17120](https://github.com/emqx/emqx/pull/17120) 为 `GET /clients_v2` 新增查询字符串过滤选项 `node`。指定后，将返回连接到该节点的在线客户端，以及上次连接到该节点的离线客户端。

- [#17136](https://github.com/emqx/emqx/pull/17136) 为 InfluxDB 连接器新增 `ping_with_auth` 选项。启用后，健康检查将包含配置的凭据，适用于要求认证健康检查请求的 InfluxDB 兼容服务。同时修复了 InfluxDB 连接器/动作在从 `write_syntax` 字面量或 MQTT 载荷写入值时的 Unicode 文本保留问题。

- [#17165](https://github.com/emqx/emqx/pull/17165) 为动作新增 `resource_opts.dispatch_strategy` 选项。

  新选项默认为 `per_clientid`，保持此前的缓冲工作器分发行为。设置为 `random` 时，没有显式 `pick_key` 的查询将使用随机分发键，有助于在少量客户端发布大量消息时将流量分散到多个缓冲工作器。

- [#17170](https://github.com/emqx/emqx/pull/17170) [#17282](https://github.com/emqx/emqx/pull/17282) [#17297](https://github.com/emqx/emqx/pull/17297) 为 MQTT Bridge 连接器和集群连接配置新增 `tcp_opts`（`nodelay`、`sndbuf`、`recbuf`、`buffer`、`keepalive`、`delay_send`、`active_n`），支持为每个连接调整出站 MQTT 客户端 TCP socket 参数。未设置的字段保持操作系统/`gen_tcp` 默认值。`delay_send`（默认关闭）合并小写入以提升吞吐量，代价是轻微的延迟增加。

#### 集群连接

- [#17221](https://github.com/emqx/emqx/pull/17221) 改进集群连接中 MQTT 消息转发的诊断信息。

  当消息转发连接出现连通性问题时，链路资源状态和相应告警现在会包含断开原因，使配置问题更易于识别。

#### 部署

- [#17079](https://github.com/emqx/emqx/pull/17079) 在 Helm chart 中新增 `service.wsEnabled` 选项，当 MQTT WebSocket 监听器被禁用时可省略 Service 中的 ws/wss 端口条目。默认值为 `true` 以保持现有行为。

### 修复

#### 核心 MQTT 功能

- [#17139](https://github.com/emqx/emqx/pull/17139) 恢复 `retainer.enable` 作为保留消息子系统的真实运行时开关。

  这使部署可以在保持 MQTT 保留消息协议支持的同时禁用保留消息存储，而无需依赖 `mqtt.retain_available`（后者会在协议层拒绝保留发布）。

- [#17172](https://github.com/emqx/emqx/pull/17172) 修复了客户端在断开连接前发送的 MQTT 包（如 PUBACK）可能丢失的问题（当连接进程邮箱中有待处理的出站消息时）。现在连接进程会在关闭前正确清空邮箱，确保入站包在 socket 关闭后也能被处理。

- [#17353](https://github.com/emqx/emqx/pull/17353) 修复了 `socket` TCP 后端中，当客户端连接反复遭遇发送拥塞时，出站 MQTT 包可能以错误顺序发送的问题。该场景在实际中极少发生。

- [#17383](https://github.com/emqx/emqx/pull/17383) 会话接管后，Dashboard 和 REST API 反映的 channel 信息（`mqueue_len`、`inflight_cnt`）现在在接管重放完成后立即更新，而不再等待下一次 15 秒的统计刷新周期。

#### MQTT Stream

- [#17175](https://github.com/emqx/emqx/pull/17175) 修复了从 Stream 分发的消息未应用订阅选项（如来自 stream 订阅的订阅标识符）的问题。

#### 规则引擎

- [#17211](https://github.com/emqx/emqx/pull/17211) 在 `$events/client/connack` 规则事件中补充了 `connected_at` 字段，该字段在文档中有说明但此前在实际数据中缺失。

#### 数据集成

- [#17001](https://github.com/emqx/emqx/pull/17001) 修复了当远端 Broker 启用消息队列（mq）功能时，MQTT Source 无法从 `$queue/` 订阅接收消息的问题。

  根本原因是 MQ 消息分发在 PUBLISH 包中未包含 MQTT v5 订阅标识符属性，而 MQTT Bridge 入口依赖该属性从队列订阅路由消息。

- [#17010](https://github.com/emqx/emqx/pull/17010) A2A Agent Card 中存在的 `a2a-status` 和 `a2a-status-source` 用户属性现在会被 EMQX 的存活信息覆盖，以避免重复属性。

- [#17068](https://github.com/emqx/emqx/pull/17068) 修复了当 `ssl.verify` 为 `verify_none` 且证书文件路径留空时，EMQX Tables TLS 连接器无法启动的问题，并对齐了 Rust NIF TLS verify 与连接器配置的传播行为。

- [#17084](https://github.com/emqx/emqx/pull/17084) 修复了 MQTT Source 的一个问题：若其连接器使用 `clean_start = false` 并重新连接到含有消息会话的 Broker，这些消息不会触发规则动作。

- [#17111](https://github.com/emqx/emqx/pull/17111) 修复了 PostgreSQL 连接器在禁用 prepared statements 模式下的查询执行问题。此前，并发查询可能相互交错并产生错误。

- [#17113](https://github.com/emqx/emqx/pull/17113) 修复了 RocketMQ 连接器隔离问题：配置错误或不可达的 RocketMQ 连接器不再影响同节点的其他 RocketMQ 连接器。此前，一个连接到不可达 Broker 的连接器可能导致共享客户端 supervisor 最多阻塞 60 秒，使同级连接器因 `resource_health_check_timed_out` 而反复抖动，Dashboard 对这些连接器的操作也会挂起。

  默认 TCP/TLS 连接超时也从 60 秒降至 10 秒，使配置错误的服务器快速显示为失败状态，而不是看起来卡住。

- [#17180](https://github.com/emqx/emqx/pull/17180) 修复了在高负载下对 MongoDB 进程调用超时会被当作不可恢复错误而不进行重试的问题。现在此类事件发生时会进行重试。

- [#17216](https://github.com/emqx/emqx/pull/17216) 修复了 Timescale/PostgreSQL 动作在将带引号的 JSON 数字字符串映射到 `FLOAT` 列时，会崩溃数据库连接进程而非报告结构化错误参数的问题。

- [#17250](https://github.com/emqx/emqx/pull/17250) 修复了 Redis Sentinel 连接器，支持对 Redis 数据节点和 Sentinel 节点分别配置认证。

- [#17293](https://github.com/emqx/emqx/pull/17293) 修复了写入 Parquet 文件时，对象包含必填键但值为 `undefined`/`null` 时写入损坏文件而非抛出错误的问题。

- [#17303](https://github.com/emqx/emqx/pull/17303) 升级 Kafka 客户端库：`brod` 从 4.5.2 升级至 4.5.4，`wolff` 从 4.1.9 升级至 4.1.10。

  上游修复的主要问题：

  - `brod`：修复 Kafka 连接重新认证时的竞争条件（via `kafka_protocol` 4.3.4）。
  - `wolff`：在高内存负载控制（`drop_if_highmem`）下保留最小缓冲区，防止生产者耗尽飞行中数据；仅丢弃超出保留量的字节。

- [#17343](https://github.com/emqx/emqx/pull/17343) 修复了集群配置复制的一个问题：导入包含 `file` 类型授权源的数据备份（或通过 `emqx ctl conf load`/`PUT /api/v5/configs` 加载 HOCON 配置）时，可能导致对等节点出现 `cluster_rpc_apply_failed`/`failed_to_read_acl_file` 错误并滞后。

  导入程序此前会在本地写入 ACL 文件并将内联 `rules` 替换为 `path`，然后将 path 形式的配置下发到集群。对等节点磁盘上没有该文件，因此无法应用变更。现在发送到集群的配置保持 `rules` 内联，每个对等节点从复制内容中写入自己的 ACL 文件副本。

- [#17347](https://github.com/emqx/emqx/pull/17347) 升级 RocketMQ 客户端依赖至 `v0.7.2`，修复异步生产者请求中的内存增长问题。

- [#17439](https://github.com/emqx/emqx/pull/17439) 修复了 Azure Blob Storage 连接器的健康检查在存储账户包含过多容器时可能超时或产生大量带宽消耗的问题。

- [#17450](https://github.com/emqx/emqx/pull/17450) 修复了使用 `mode=node` 时 `/prometheus/data_integration` Prometheus 端点可能返回 500 状态码的问题。该问题仅在动作和连接器配置被手动编辑且不一致（动作的连接器不存在）时才会出现。

- [#17474](https://github.com/emqx/emqx/pull/17474) 通过使用有界版本查询替代列出所有数据库的方式，降低了 IoTDB REST API 连接器健康检查的开销。

#### 集群

- [#17055](https://github.com/emqx/emqx/pull/17055) 修复了在滚动升级到 EMQX 6.2.0 版本过程中，内部 DS Raft 升级机制在特定情况下可能卡住，导致持久化存储（Durable Storage）暂时不可用直至核心节点重启的问题。

- [#17099](https://github.com/emqx/emqx/pull/17099) 修复了断开连接的核心节点重新连接时路由表出现不一致的问题。

- [#17132](https://github.com/emqx/emqx/pull/17132) 修复了在副本节点上，当其原始配置或运行时状态发生偏移时，添加或删除主题指标可能失败的问题，该问题会触发 `cluster_rpc_apply_failed` 告警并阻塞集群 RPC 复制。重复添加和删除不存在的操作现在仅在发起节点上被拒绝，副本节点以幂等方式应用变更。

- [#17182](https://github.com/emqx/emqx/pull/17182) 升级至 emqx-OTP 27.3.4.2-8（针对 mria）。

  未修复此问题前，EMQX 启动期间若未连接到集群，Mria 应用启动可能卡住。

- [#17198](https://github.com/emqx/emqx/pull/17198) 升级 OTP 版本至 28.4.1-3，构建器版本升级至 6.1-4。

  未修复此问题前，EMQX 启动期间若未连接到集群，Mria 应用启动可能卡住。

- [#17214](https://github.com/emqx/emqx/pull/17214) 移除了集群连接消息转发 MQTT 客户端断开事件的晦涩错误级日志，改为包含足够排查上下文的用户友好消息。类似以下的事件不应再出现在错误日志中：

  ```
  2026-05-06T03:00:48.738654+00:00 [error] [PoolWorker] unexpected info: {disconnected,141,#{}}
  ```

- [#17218](https://github.com/emqx/emqx/pull/17218) 避免 `bin/emqx` 和 `bin/emqx_ctl` 调用在运行中的 Broker 上触发 `nodeup`/`nodedown` 事件，这些事件此前在 Broker 日志中表现为误导性的 `cm_registry_node_down` 警告。这些脚本启动的临时辅助节点现在按预期注册为隐藏 Erlang 节点。

- [#17269](https://github.com/emqx/emqx/pull/17269) 改进了网络分区后的集群恢复能力。

  - 此前，连接到副本节点的部分客户端可能从全局注册表中丢失，导致接管时行为不一致以及 Dashboard 显示信息不正确。

    此修复新增了一个后台进程，在网络分区恢复后重新注册现有客户端。同时新增告警："Broker is recovering after a network partition"，在全局注册表重建期间触发。

  - 引入新的集群自愈算法，可自动恢复重叠网络分区。

- [#17342](https://github.com/emqx/emqx/pull/17342) 修复了当导出的 `cluster.hocon` 包含部分 `node` 节时，集群配置导入因 "required_field: node.cookie" schema 检查错误而失败的问题。只读根配置（`node`、`rpc`）本就不属于数据导入范围，现在在预检 schema 校验前从导入配置中删除，使验证使用运行节点自身的值。

- [#17348](https://github.com/emqx/emqx/pull/17348) 修复了当集群节点的有效配置相同但原始配置表示不同时，`emqx ctl conf cluster_sync status` 诊断输出嘈杂且具有误导性的问题。

  该命令现在会抑制不对应已检查配置变更的原始表示差异，同时在已检查配置不一致时仍会发出警告。当某个原始配置键在一个节点存在而在另一个节点缺失时，不再崩溃。

  同时忽略动作、Source、Bridge 和规则元数据中 `created_at` 和 `last_modified_at` 的纯时间戳元数据差异。数据导入或启动时配置加载可能仅在部分节点上刷新这些生成的时间戳，即使有效运行时配置完全相同。

- [#17349](https://github.com/emqx/emqx/pull/17349) 改进了集群连接在路由复制卡在连接到无响应目标集群时的响应速度。现在删除此类集群连接的速度会略有提升。

- [#17382](https://github.com/emqx/emqx/pull/17382) 修复了集群遭遇网络分区时可能发生的全局 channel 注册表损坏问题。

- [#17424](https://github.com/emqx/emqx/pull/17424) 修复了网络分区后 Mnesia 自愈可能导致同一客户端 ID 在全局会话注册表中留下重复或过期条目的泄漏问题。

  Discard 和 takeover-kick RPC 处理程序现在也会在目标进程不再存活时删除注册表行；连接路径上的注册限流现在能识别墓碑行（无本地 channel 状态）并清理它们，而不是无限期阻塞同一客户端 ID 的新连接。

- [#17432](https://github.com/emqx/emqx/pull/17432) 修复了并发集群连接 API 请求可能返回通用错误响应而非成功或未找到的问题。

- [#17469](https://github.com/emqx/emqx/pull/17469) 修复了启用或禁用活动集群连接时出现如下警告的问题：

  ```
  [warning] tag: RESOURCE, msg: handle_resource_metrics_failed, reason: {badkey, matched}, event: matched, ...
  ```

#### 访问控制

- [#17045](https://github.com/emqx/emqx/pull/17045) 修复了基于密码的认证后端在 CONNECT 包不含密码时会立即拒绝连接而非继续认证链的问题。

  此前，若客户端连接时不带密码，认证链中第一个基于密码的认证器（内置数据库、MySQL、PostgreSQL、MongoDB、Redis 或 LDAP）会返回错误，阻止后续认证器被尝试。

- [#17064](https://github.com/emqx/emqx/pull/17064) 修复了 `/authentication/:id/users` REST 端点的授权漏洞，命名空间管理员不再能通过省略 `ns` 查询参数或 `namespace` 请求体字段来列出或创建全局（或其他租户）命名空间中的用户。非全局命名空间的认证用户不再能被标记为 `is_superuser`；创建或更新此类用户的请求将被拒绝，确保始终对租户 MQTT 客户端强制执行显式 ACL 规则。

- [#17100](https://github.com/emqx/emqx/pull/17100) 修复了当身份提供商返回的 JWKS 响应 `Content-Type` 使用 `+json` 结构化语法后缀（如 `application/jwk-set+json; charset=utf-8`）时，OIDC SSO 登录失败并报 `provider_not_ready` 的问题。此类响应现在被接受为有效的 JWKS 内容。

- [#17122](https://github.com/emqx/emqx/pull/17122) 修复了 Dashboard RBAC 对含 URL 编码用户名（如电子邮件地址）的 SSO 用户的权限检查，确保 `force_mfa` 禁用时查看者的 MFA 自助禁用请求能正常工作。

- [#17140](https://github.com/emqx/emqx/pull/17140) 修复了 EMQX 通过 HTTP 获取证书吊销列表（CRL）时，服务器返回 DER 编码内容（`Content-Type: application/pkix-crl`，RFC 5280 §5 规定的格式）时的静默失败问题。

  此前，EMQX 仅解码 PEM 编码的 CRL 内容；DER 内容被静默视为零条 CRL 并缓存为空列表，导致 `enable_crl_check = true` 监听器上的每次 TLS 握手都以 `bad_crls, no_relevant_crls` 失败，且无日志说明原因。

  EMQX 现在同时解码 PEM 和 DER CRL 内容。当获取的内容两者都不是时，记录带有 URL 的警告日志，使配置错误可见。

- [#17171](https://github.com/emqx/emqx/pull/17171) 修复了阻止命名空间 Dashboard 管理员启用或禁用自身账户 MFA 的 RBAC 问题。

  命名空间管理员仍受限于无法管理其他 Dashboard 用户的 MFA 设置。

- [#17177](https://github.com/emqx/emqx/pull/17177) Dashboard 创建的 REST API 密钥现在随机生成，而非从 API 密钥名称派生。

- [#17223](https://github.com/emqx/emqx/pull/17223) 修复了在 SSL 监听器前放置 TCP 透传代理（如 GCP TCP Proxy NLB、AWS NLB）且配置 `proxy_protocol = true` 时，客户端证书丢失的问题。监听器处的 TLS 握手成功完成且客户端证书存在，但未暴露给认证或规则事件。依赖客户端证书（CN、subject、完整 PEM）的函数、ACL 规则和认证后端现在在此部署形态下可正常工作。

- [#17330](https://github.com/emqx/emqx/pull/17330) 加固了启用 `proxy_protocol` 的 TCP 和 SSL 监听器上的 PROXY Protocol v2 TLV 解析器。此前，TLV 声明长度超出缓冲区时会导致解析器静默截断 TLV 流，丢弃后续字段。解析器现在是严格模式：畸形 TLV 流会导致连接被拒绝并记录警告日志，而不是以部分解析的 PROXY 头部接受连接。

- [#17428](https://github.com/emqx/emqx/pull/17428) 修复了当提供商的 `.well-known/openid-configuration` 响应包含 `Cache-Control` 头（如 Kanidm 观察到的 `max-age=0`）时，Dashboard OIDC SSO 崩溃导致 EMQX 无法完成 OpenID 提供商发现的问题。该崩溃会导致 OIDC supervisor 在单次失败后耗尽重启预算，使 SSO 在不重新保存配置的情况下无法恢复。cache-control 解析器现在对这些值更具容错性，worker 不再因过期值错误而硬崩溃，OIDC supervisor 允许在一分钟内多次重启，使瞬时失败能自动重试。

#### 网关

- [#17141](https://github.com/emqx/emqx/pull/17141) 修复了 CoAP 连接模式的 token 接管问题，使重新连接的 UDP/DTLS 客户端可以使用有效 token 恢复，同时拒绝无效的 token/clientid 组合。同时确保在运行 CoAP 接管 connected hook 之前所需的连接信息字段已就绪。

- [#17258](https://github.com/emqx/emqx/pull/17258) 修复了 MQTT-SN 网关中，已连接客户端在同一会话上发送第二个 CONNECT 包会导致连接进程崩溃的问题。网关现在以 DISCONNECT 响应并优雅关闭会话。

- [#17287](https://github.com/emqx/emqx/pull/17287) 修复了 MQTT-SN 客户端因在意外连接或 Will 状态下收到包而崩溃的问题，包括连接设置期间的 `DISCONNECT`、Will 握手完成前的 `REGISTER`，以及 Will topic 不存在时的 `WILLMSGUPD`。

- [#17419](https://github.com/emqx/emqx/pull/17419) 修复了 CoAP 网关 observe 通知未遵守 `gateway.coap.notify_type` 设置的问题。

  Observe 通知现在使用每会话确认性飞行窗口为 1、所有 observe token 共享固定待处理队列（100 条）。当一条确认性通知正在飞行时，后续 observe 通知进入队列而非被静默丢弃。队列满时，最旧的待处理通知被丢弃，`delivery.dropped.queue_full` 递增，并记录经过限流的警告日志。

  取消 observe 关系现在也会移除该 observe token 对应的待处理通知，确保客户端取消 observe 后不会收到已排队的通知（包括通配符 observe 过滤器）。

- [#17507](https://github.com/emqx/emqx/pull/17507) 修复了多个网关在认证完成之前即可进入发布或订阅处理流程的问题。

  MQTT-SN QoS -1 发布现在使用固定的负 QoS 客户端身份，且必须通过网关认证和发布授权检查后才能分发。

  NATS 现在在未配置认证时遵守安全配置。在强化安全配置下，匿名发布、订阅和连接请求将被拒绝，除非监听器认证被显式禁用。

  STOMP 现在拒绝 CONNECT 完成前的 SEND 和 SUBSCRIBE 帧，包括事务性 SEND 帧。

  CoAP 无连接 `/ps` 发布和 observe 请求现在在进入发布/订阅处理前先进行认证。在强化安全配置下，未配置认证时此类请求将被拒绝，除非监听器认证被显式禁用。

#### 可观测性

- [#16956](https://github.com/emqx/emqx/pull/16956) 当连接终止原因为 `emsgsize`（接收的包超过 `mqtt.max_packet_size`）时，将客户端连接终止日志级别从 info 提升至 warning。

- [#17002](https://github.com/emqx/emqx/pull/17002) 将 `minirest` 库升级至 1.4.12 版本。该版本修复了一个导致 EMQX API 在返回 `204 No Content` 状态行时生成格式错误响应的问题：错误地附带了无效的 `content-length` 响应头。

- [#17024](https://github.com/emqx/emqx/pull/17024) Dashboard HTTP 监听器现在在绑定地址为 IPv6 地址时自动使用 IPv6，无需显式设置 `inet6 = true`。

- [#17054](https://github.com/emqx/emqx/pull/17054) 修复了设置 `Accept: application/json` 时 `GET /api/v5/configs?key=...` 返回不完整数据的问题。

  此前，JSON 响应忽略 `key` 查询参数，始终返回固定的根配置子集，不包含 `multi_tenancy` 等键。该端点现在与 hocon（text/plain）响应一致地处理 JSON 响应中的 `key` 参数。

- [#17118](https://github.com/emqx/emqx/pull/17118) 改进了多租户列表端点的分页功能（`/mt/ns_list`、`/mt/ns_list_details`、`/mt/managed_ns_list`、`/mt/managed_ns_list_details`、`/mt/ns/{ns}/client_list`）：

  - 新增符合 RFC 8288 的 `Link: <?...>; rel="next"` 响应头。当有更多页面时，该头部携带下一页的仅查询 URI 引用；缺失时表示当前响应是最后一页。这消除了此前需要额外请求才能区分整页（`len(results) == limit`）与精确边界"无更多数据"情况的歧义。
  - 在现有排他游标（`last_ns`、`last_clientid`）旁边新增包含性 keyset 游标查询参数（`first_ns`、`first_clientid`）。包含形式支持精确匹配查询（如 `?first_ns=foo&limit=1`），并在调用方选择使用时通过分页 Link 头传递。两种形式在单个请求中互斥；同时提供两者返回 HTTP 400。

- [#17134](https://github.com/emqx/emqx/pull/17134) 修复了禁止客户端列表 API 对 6.2.0 之前创建的客户端 ID 和用户名正则禁令返回 `invalid json term` 错误的问题。旧版本数据库中保留的已编译正则现在在序列化响应时被转换回原始模式字符串。

- [#17227](https://github.com/emqx/emqx/pull/17227) 集群配置文件保存错误现在会指明文件名和底层原因。

  当 `cluster.hocon`（或其目录）为只读、不可变或以其他方式不可写（如挂载为只读的容器）时，通过 Dashboard 或 REST API 修改配置此前会返回不透明的 HTTP 400，内容为 `{config_update_crashed,{badmatch,{error,ebusy}}}`，且只记录未指明文件名的 badmatch 崩溃日志。

  现在，错误将：

  - 记录 `failed_to_save_conf_file`，包含实际文件路径和原因（`eacces`、`eperm`、`ebusy` 等），以及列出常见运维侧原因的提示。
  - 返回结构化的 HTTP 400 响应体，同时指明文件和原因，无需翻阅节点日志即可在 Dashboard 中看到原因。

  此前，当仅临时文件写入失败（如只读目录）时，API 会静默返回 HTTP 200，尽管更改未持久化到磁盘。API 现在在此情况下也会正确报告失败。

- [#17246](https://github.com/emqx/emqx/pull/17246) 将 `jose` 库从 1.11.10 升级至 1.11.12，获取针对新版 OTP 的 EC 和 EdDSA 密钥修复。

- [#17247](https://github.com/emqx/emqx/pull/17247) 当插件的 REST API 回调崩溃或超出超时预算时，Broker 现在会将失败的 API 方法和路径连同配置的超时时间一并记录，使混合流量日志中的问题调用可被识别。超时记录为警告（非错误），并包含指向 `plugins.api_endpoint.timeout`（在插件回调合理需要更多时间时可调整的配置键）的提示。

- [#17254](https://github.com/emqx/emqx/pull/17254) 改进了容器内的内存使用报告。Broker 现在从 cgroup v2、cgroup v1 和宿主机 `/proc/meminfo` 中选取约束最严格的内存读数（最小非零总量优先，使用率更大的在并列时胜出）。此前报告可能在两种情况下产生误导：在具有严格 cgroup 限制的容器中，宿主机视图可能显示 >70% 而 cgroup 限制实际 <10%（或反之）；在未设置内存限制的 cgroup 挂载下，cgroup 读数可能将报告的使用率压缩至约 0%。过载保护阈值和"已用内存"指标现在反映实际约束进程的限制。

- [#17319](https://github.com/emqx/emqx/pull/17319) `GET /api/v5/schemas/{hotconf,actions,connectors}` 现在以 `Content-Type: application/json` 返回响应。此前响应体是有效 JSON，但头部为 `text/plain; charset=utf-8`，导致按响应内容类型分发的客户端出错。

- [#17406](https://github.com/emqx/emqx/pull/17406) 现在，由命名空间管理员发起的追踪所捕获的事件，对于主题、IP 地址和客户端 ID 类型的追踪，将限制在该管理员的命名空间内。规则 ID 类型的追踪此前已有此行为。

- [#17473](https://github.com/emqx/emqx/pull/17473) 当插件的 Erlang 应用因其他运行中的应用仍依赖它而无法停止时，将 `unabled_to_stop_plugin_apps` 的日志级别从 warning 降至 info。这是插件卸载时的预期、无需操作的情况，不应再触发警告。

#### 部署

- [#17311](https://github.com/emqx/emqx/pull/17311) 修复了容器主机名无法解析时 Docker 启动失败的问题。入口点现在在自动生成节点名称前回退到网络接口 IP 地址，若无法确定节点主机则以清晰的错误信息退出。

- [#17369](https://github.com/emqx/emqx/pull/17369) 将 Dashboard 监听器默认值（`http.bind` 和占位符 HTTPS `ssl_options`）从用户可编辑的 `etc/emqx.conf` 迁移至随附的 `etc/base.hocon`。此前，硬编码的 `emqx.conf` 块会在重启时静默将运行时更新回滚为默认自签名证书。现在，通过 Dashboard、REST API 或 `emqx_acme` 插件自动 HTTPS 配置所做的运行时更新可在重启后正确保留。

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

  因订阅过滤不匹配而被丢弃的消息将触发 `delivery.dropped` 钩子/事件，触发原因为 `subscription_filter`；并由新增的 `delivery.dropped.filter` 指标进行统计。

- [#16929](https://github.com/emqx/emqx/pull/16929) 引入两种新的限速器类型：`delivery_messages` 和 `delivery_bytes`。与现有的 `messages` 和 `bytes` 限速器不同，新限速器用于限制服务器客户端进程从任意来源接收消息的速率。当达到限制时，QoS 0 消息将被丢弃，QoS 1/2 消息将在内部排队并按配置的重试间隔调度重试。

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

- [#16707](https://github.com/emqx/emqx/pull/16707) EMQX 新增与 Azure Event Grid 数据集成，支持从 Azure Event Grid 消费消息及向其发布消息。

- [#16750](https://github.com/emqx/emqx/pull/16750) GCP 连接器（GCP PubSub 生产者/消费者、BigQuery）现已通过 Service Account 模拟方式支持工作负载身份联合（Workload Identity Federation，WIF）认证。当前仅支持使用 Client Credentials 授权类型的 OIDC 工作负载身份池提供程序。

- [#16773](https://github.com/emqx/emqx/pull/16773) 使用 MQTT 连接器并启用 SSL 时，如果未设置服务器名称指示（SNI），现在将自动使用服务器主机名填充该字段。

- [#16893](https://github.com/emqx/emqx/pull/16893) EMQX 新增与 QuasarDB 数据集成。

- [#16962](https://github.com/emqx/emqx/pull/16962) 改进了 Kafka Source 的轮询行为。当没有可用记录时，Fetch 请求现在会短暂等待数据到来，而不是立即返回空批次。这减少了不必要的轮询延迟，并有助于 Kafka 消费者更及时地接收新记录。

#### 访问控制

- [#16597](https://github.com/emqx/emqx/pull/16597) 改进了 MySQL 和 PostgreSQL 认证与授权中 SQL 模板对非法变量和带引号变量的处理方式。

- [#16616](https://github.com/emqx/emqx/pull/16616) 为 SSO OIDC 后端新增配置项，支持通过 `jq` 表达式在创建新 Dashboard 用户时提取所需的角色和命名空间。

- [#16759](https://github.com/emqx/emqx/pull/16759) 在 Variform 表达式中新增 `timestamp_s` 和 `timestamp_ms` 函数，分别用于获取当前系统时间（秒和毫秒），例如可在客户端连接阶段用于填充自定义客户端属性。

- [#16817](https://github.com/emqx/emqx/pull/16817) 新增重置认证和授权指标计数器的 REST API 端点：
  - `POST /authentication/:id/metrics/reset`：重置指定认证器的计数器。
  - `POST /authorization/sources/:type/metrics/reset`：重置指定授权源的计数器。

#### 管理

- [#16958](https://github.com/emqx/emqx/pull/16958) 新增 `emqx ctl api_keys` 命令，支持通过命令行对 API Key 进行列出、查看、添加、删除、启用和禁用操作。

#### 插件

- [#16849](https://github.com/emqx/emqx/pull/16849) 为插件 API 端点新增基于 Cookie 的认证回退机制。Dashboard 提供的插件 UI iframe 现在可以在未携带 `Authorization` 请求头时，通过 `emqx_auth` Cookie 进行认证。此机制仅适用于 `/api/v5/plugin_api/...` 路径。

#### 网关

- [#16734](https://github.com/emqx/emqx/pull/16734) 为 NATS 网关新增 `token`、`nkey` 和 `jwt` 内置认证方式，按顺序依次尝试，以缩小与 NATS Server 之间的认证功能差异。

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

- [#16842](https://github.com/emqx/emqx/pull/16842) 减少了在没有 peer 节点持有插件配置时拉取插件配置产生的多余 warning 日志。此前，节点启动时从集群中的节点拉取插件配置，即使在无害的情况下（例如该插件首次被加载，没有任何节点持有配置），也会记录 warning 日志。此类情况现在改为 debug 级别日志，而真正的错误（如 RPC 失败、超时）仍保留为 warning。
- [#16843](https://github.com/emqx/emqx/pull/16843) 修复了 HTTP 头和查询字符串参数未被透传至插件 API 处理器的问题，导致插件收到空的请求头和缺失的查询参数。
- [#16904](https://github.com/emqx/emqx/pull/16904) 防止同一插件的多个版本同时被启用或运行。当启用更新版本时，已配置的旧版本现在将自动禁用。管理 API 操作也不再在另一版本仍处于活动状态时报告成功，而是返回明确的错误。

#### 网关

- [#16536](https://github.com/emqx/emqx/pull/16536) 修复了 CoAP 网关在 DTLS 连接模式下运行时存在的问题。

#### 可观测性

- [#16879](https://github.com/emqx/emqx/pull/16879) 新增 `log.audit.cache_size` 作为审计日志数据库缓存大小的主配置项，同时保留 `log.audit.max_filter_size` 以保持向后兼容。

#### 部署

- [#16901](https://github.com/emqx/emqx/pull/16901) 修复了 RHEL 9.6 LTS 的 RPM 安装包 OpenSSL 依赖问题：RHEL >= 9.7 固定依赖 `openssl >= 3.5.1`，旧版 RHEL 9 固定依赖 `openssl >= 3.0.7`。

#### ExHook

- [#16890](https://github.com/emqx/emqx/pull/16890) 修复了 ExHook 在重连成功，可能存在的回调被重复触发的问题。

#### 许可证

- [#16764](https://github.com/emqx/emqx/pull/16764) 优化了许可证客户层级的执行逻辑：引入 `STANDARD` 和 `VIP` 两个层级，并将官方许可证 `STANDARD` 层级的到期宽限期从 90 天缩短至 15 天，超过宽限期后新会话将受到限制。

## 6.1.4

*发布日期: 2026-08-03*

在升级到 EMQX 6.1.4 之前，请务必查阅不兼容变更和已知问题。

### 增强

#### 多租户

- [#17732](https://github.com/emqx/emqx/pull/17732) 为 API 密钥创建和更新端点新增 `namespace` 字段。运维人员不再需要在 `role` 字符串中编码命名空间。仍支持 `ns:<namespace>::<role>` 格式；同时提供两种形式时，其中的命名空间值必须一致。
- [#17855](https://github.com/emqx/emqx/pull/17855) 支持命名空间范围的 EMQX Dashboard 管理员创建、列出、查看、更新和删除其所属命名空间中的 API 密钥。这些管理员不能创建全局 API 密钥或其他命名空间中的密钥，也无法查看其命名空间之外的 API 密钥。

#### 访问控制

- [#17813](https://github.com/emqx/emqx/pull/17813) 为 Dashboard 用户和 API 密钥端点新增校验，拒绝将等同管理员权限的 scope（EMQX 校验错误消息中称为 `privilege scopes`），包括 `system`、`user_management`、`api_key_management` 和 `sso_management`，与其他 scope 组合。上述每个 scope 都会授予与管理员等效的权限，因此添加其他 scope 并不会限制账户权限。请根据所需权限仅使用等同管理员权限的 scope，或仅使用其他 scope。

  受此校验约束的现有记录即使混合使用 scope 也可继续工作。更新时如果显式提交 scope 列表，该列表必须仅包含等同管理员权限的 scope，或仅包含其他 scope。命名空间范围的 Dashboard 管理员不受此互斥规则限制，其权限仍由命名空间 RBAC 管理。

#### 数据集成

- [#17933](https://github.com/emqx/emqx/pull/17933) RabbitMQ 连接器新增对多节点 `servers` 列表的支持，例如 `rmq1:5672,rmq2:5672`。连接器支持连接时故障转移，并为连接池轮换起始偏移量。未设置 `servers` 时，仍支持 `server` 和 `port` 设置。

- [#17944](https://github.com/emqx/emqx/pull/17944) 为 HTTP 连接器以及基于 HTTP 的认证和授权新增 OAuth2 客户端凭证认证。启用后，EMQX 会从配置的令牌端点获取并刷新访问令牌，并将其作为 Bearer Authorization 标头添加到出站请求中。

  无法获取令牌时，连接器健康检查会报告 `disconnected`。同时启用 OAuth2 并提供 `Authorization` 标头的配置将被拒绝。

  EMQX 在令牌请求正文中以表单字段形式发送客户端 ID 和客户端密钥。不支持在 HTTP Basic `Authorization` 标头中发送这些凭证。

- [#18014](https://github.com/emqx/emqx/pull/18014) 为 Datalayers Arrow Flight 连接器新增预处理语句自动重建功能。如果服务器丢失预处理语句，例如服务器重启后，客户端会自动重新创建该语句并重试写入操作，避免因语句缺失导致写入失败。

- [#18042](https://github.com/emqx/emqx/pull/18042) DynamoDB 连接器新增 AWS IAM 角色凭证支持。

  同时省略访问密钥 ID 和秘密访问密钥时，EMQX 会从 ECS 任务角色或 EC2 实例元数据获取临时凭证，并在凭证过期前进行刷新。

- [#18081](https://github.com/emqx/emqx/pull/18081) 提升了 Snowflake Streaming 动作的韧性。在追加行的过程中，如果通道内部状态不同步，动作会将该错误视为可恢复错误，重试失败的行，并尝试重新打开通道，无需人工干预。

- [#18085](https://github.com/emqx/emqx/pull/18085) 为 Kafka、Confluent 和 Azure Event Hubs 生产者新增以下配置项：

  - `max_batch_age`（动作）：消息可在生产者缓冲区中保留的最长时间。EMQX 会丢弃超过该时长的消息，并同时递增 `dropped` 和 `dropped.expired` 指标。过期消息不计入 `failed` 或 `success`。默认值：`infinity`，表示消息在缓冲期间不会过期。
  - `max_retries`（动作）：消息批次允许的 Kafka 失败重试次数。达到配置的重试次数后，EMQX 会丢弃该批次。受影响的消息计入 `failed`，而不是 `dropped`。仅当 Kafka 返回错误码时才会增加重试计数；连接中断后的重新发送不会增加该计数。默认值：`infinity`，表示无限重试。
  - `reconnect_delay`（动作）：连接中断后，生产者重新连接前的等待时间。默认值：`2s`，此前为硬编码值。
  - `request_timeout`（连接器）：等待 Kafka 响应的最长时间，超过该时间后连接将被视为失效并重新建立。默认值：`30s`。

  Kafka 客户端库已升级至 `wolff` 4.2.1，恢复了内存模式缓冲区对 `max_linger_time` 的支持。未达到大小上限的批次最多等待 `max_linger_time` 以接收更多消息，从而降低生产请求频率。已满的批次会立即发送。

- [#18110](https://github.com/emqx/emqx/pull/18110) Schema Registry 新增对 JSON Schema 2019-09 和 2020-12 草案的支持。

- [#18137](https://github.com/emqx/emqx/pull/18137) GCP Pub/Sub 生产者和消费者的主题配置现在接受完全限定的主题路径（`projects/<project-id>/topics/<topic-name>`），从而可以向服务账户所属 GCP 项目以外的主题发布消息，或从该主题消费消息。与此前一样，仅提供主题名称时，仍会基于服务账户所属项目进行解析。对于消费者，订阅仍创建在服务账户所属项目中，只有主题引用可以指向其他项目。

#### 插件

- [#18012](https://github.com/emqx/emqx/pull/18012) 新增 `emqx_sync_request` 插件，用于通过 EMQX REST API 实现同步 MQTT 请求/响应流程。该插件还提供节点本地 CLI 诊断，可查看请求计数器和当前待处理状态。

#### 安装包

- [#18037](https://github.com/emqx/emqx/pull/18037) 新增适用于 Red Hat Enterprise Linux 10、Rocky Linux 10 及兼容发行版的 Enterprise Linux 10（EL10）安装包。
- [#18127](https://github.com/emqx/emqx/pull/18127) 开始发布适用于 macOS 26（Tahoe）的安装包。

### BUG 修复

#### 核心 MQTT 功能

- [#17895](https://github.com/emqx/emqx/pull/17895) 修复了将 TLS 监听器从托管证书包切换回基于文件的证书时，如果引用的证书包已被删除会导致切换失败的问题。

- [#17911](https://github.com/emqx/emqx/pull/17911) 当 Erlang/OTP `ssl` 应用支持 `ECDHE-PSK-CHACHA20-POLY1305` 密码套件时，DTLS 监听器现在可以校验该密码套件。

- [#18062](https://github.com/emqx/emqx/pull/18062) 修复了将 TLS/WSS 监听器从托管证书包切换回基于文件的证书时，如果请求通过发送 `null` 清除 `managed_certs`（与 Dashboard 的行为一致）且证书包已被删除，会导致切换失败的问题。

- [#18102](https://github.com/emqx/emqx/pull/18102) 修复了 EMQX 6.1.2 和 6.2.0 中引入的一个问题，该问题可能导致 MQTT 客户端乱序接收 QoS 1 和 QoS 2 消息。此问题仅影响配置了投递速率限制的部署。EMQX 现在会将后续消息保留在队列中，直到受速率限制的消息可以发送。

- [#18108](https://github.com/emqx/emqx/pull/18108) 删除仍被某项配置引用的托管证书包或其中的单个文件时，现在始终会失败，并返回列出引用配置的明确错误。`force_delete` 查询参数不再绕过此检查，且已从 API schema 中移除。

  此外，当监听器引用磁盘上缺失的证书包时，Prometheus 统计端点不再整体失败。证书过期指标会跳过受影响的监听器，并记录警告。

- [#18111](https://github.com/emqx/emqx/pull/18111) 将 `mqtt.strict_mode` 校验扩展到 MQTT v3.1 CONNECT 报文。EMQX 现在会拒绝设置密码标志但未设置用户名标志的报文，与 MQTT v3.1.1 的行为保持一致。MQTT v3.1 规范不允许在没有用户名的情况下提供密码。

  改进了连接日志的可读性。CONNECT 报文跟踪使用 `Password=undefined` 区分未提供密码与提供空密码。日志还会将 `peername` 字段呈现为普通字符串，例如 `10.0.0.1:54123`。

#### 规则引擎

- [#17957](https://github.com/emqx/emqx/pull/17957) 修复了当 `rule_engine.limit_selects_in_namespace = true` 时，多个规则事件（例如 `$events/client/connack`）无法在全局命名空间中触发规则的问题。
- [#18049](https://github.com/emqx/emqx/pull/18049) 修复了设置 `rule_engine.limit_selects_in_namespace = true` 后，由告警激活或停用事件触发的全局规则无法触发的问题。

#### 数据集成

- [#17859](https://github.com/emqx/emqx/pull/17859) 修复了 MQTT 连接器无法连接 IPv6 Broker 的问题。

  此前，将 MQTT 连接器配置为连接 IPv6 Broker 时会以两种方式失败：保存时会以 `bad_host_port` 校验错误拒绝 `[::1]:1883` 等 IPv6 字面量；仅解析为 IPv6（`AAAA`）地址的主机名会因连接默认使用 IPv4 而以“Could not resolve host”错误连接失败。

  服务器地址解析器现在接受带方括号的 IPv6 字面量，例如 `[::1]`、`[::1]:1883` 和 `mqtt://[::1]:1883`。MQTT 连接器现在还会在连接时启用 IPv6 探测，因此可以连接仅支持 IPv6 的 Broker。

  MQTT 连接器和集群连接的 `server` 地址现在接受正式的 MQTT URI scheme：`mqtt`（普通 TCP）和 `mqtts`（TLS），例如 `mqtt://broker:1883` 和 `mqtts://broker:8883`。仍支持不含 scheme 的 `host:port`。其他 scheme 现在会被拒绝，并返回 `unsupported_scheme` 校验错误。

- [#17947](https://github.com/emqx/emqx/pull/17947) 修复了更新 HTTP 连接器时，连接器重建后其动作缓冲区 worker 可能保持阻塞，导致消息持续排队直至下一重试间隔的问题。

- [#17955](https://github.com/emqx/emqx/pull/17955) 修复了低写入速率下执行健康检查后，GreptimeDB 异步批次可能未刷新的问题。

- [#17961](https://github.com/emqx/emqx/pull/17961) 修复了 Kafka 或 Pulsar 连接器在健康检查超时后可能转换为 `disconnected` 状态，并可能导致其内部队列被重新创建的问题。发生此类超时后，Kafka 和 Pulsar 连接器现在会转换为 `connecting` 状态。

- [#17970](https://github.com/emqx/emqx/pull/17970) 启用 SSRF 防护后，管理连接器的操作不再受地址现已被策略阻止的现有连接器影响。

  此前，在创建连接器后启用 SSRF 防护或扩展其拒绝列表，可能导致不相关的连接器操作出现内部错误；删除受影响的连接器时，也可能在其动作和规则已被删除后仍残留该连接器。

  SSRF 防护现在适用于 HTTP 和 MQTT 连接器，并在创建或更新连接器时执行：使用被阻止地址创建或更新此类连接器会被拒绝。启用、禁用和删除连接器的操作不会被阻止，其他类型的连接器不受此策略约束。

- [#17973](https://github.com/emqx/emqx/pull/17973) 修复了 Kafka 生产者动作的重试指标。动作指标中的 `retried`、`retried.success` 和 `retried.failed` 计数器现在会反映 Broker 重新连接后内部缓冲区重新发送的消息，使运维人员可以判断重试消息最终成功还是失败。此前，无论发生多少次内部重试，这些计数器都保持为 `0`。`success` 和 `failed` 计数器不受影响，也不会重复计数。

- [#17982](https://github.com/emqx/emqx/pull/17982) 更新了 GCP Pub/Sub 消费者，使其使用 HTTP/2，并在活跃的拉取请求超时时取消该请求。取消 HTTP/2 流可以更明确地通知 GCP 服务器该请求已结束，并可能使消息可被后续拉取请求租用，从而降低尾延迟。

- [#18055](https://github.com/emqx/emqx/pull/18055) 修复了不同集群节点上的 Snowflake Streaming 动作因以下错误而失败的问题：

  ```text
  {unrecoverable_error,#{body => <<"{\"code\":\"STALE_CONTINUATION_TOKEN_SEQUENCER\",\"message\":\"Channel sequencer in the continuation token is stale. Please reopen the channel\"}">>,...
  ```

- [#18110](https://github.com/emqx/emqx/pull/18110) 修复了在 Schema Registry 的 JSON Schema draft-06 中使用 `examples` 注解会导致有效数据被判定为无效的问题。

#### 集群

- [#17995](https://github.com/emqx/emqx/pull/17995) 修复了节点加入持久化 `mqtt.max_packet_size` 与其本地配置不同的集群时，节点可能终止的问题。EMQX 现在会在监听器启动前跳过监听器刷新产生的副作用，并在 EMQX 应用启动时根据已同步的配置创建监听器。

- [#17999](https://github.com/emqx/emqx/pull/17999) 修复了使用社区版（单节点）License 的节点加入其他节点持有集群 License 的集群时，可能出现启动崩溃循环的问题。

  此前，如果集群成员关系在对端 License 复制到加入节点之前建立，该节点会以 `SINGLE_NODE_LICENSE` 错误拒绝启动，并在自动重启管理程序下持续崩溃循环。节点现在会在启动前等待有限的宽限期，以同步集群 License。如果宽限期结束后集群中仍无任何节点获得集群 License，该集群仍会被拒绝。

- [#18077](https://github.com/emqx/emqx/pull/18077) 修复了节点在完全启动前收到 `cluster join` 请求（CLI 或 API）时发生崩溃的问题：加入操作会在应用仍在启动时重启内部数据库，可能导致整个节点停止。此类请求现在会被拒绝并返回明确的错误消息；请在节点完全启动后重试。

#### 访问控制

- [#17806](https://github.com/emqx/emqx/pull/17806) 按最小权限原则调整了数据备份导入和导出端点：scope 集合未同时包含 `user_management` 和 `api_key_management` 的 Dashboard 用户不能再导入或导出包含 `dashboard_users` 或 `api_keys` 表集的归档文件。全局管理员和具有所需 scope 的 API 密钥调用方不受影响。

- [#17853](https://github.com/emqx/emqx/pull/17853) 改进了连接器调试日志中敏感 HTTP 请求标头的脱敏处理。与 `Authorization` 和 `Proxy-Authorization` 相同，EMQX 会将连接器状态中的 `x-api-key`、`x-auth-token`、`api-key` 和 `cookie` 标头存储为 Secret。在 trace 或 debug 级别记录连接器状态时，会省略这些标头的值。

  共享标头脱敏辅助函数现在还可以识别以 iolist 存储的标头名称，包括连接器模板解析器生成的名称。

- [#17871](https://github.com/emqx/emqx/pull/17871) 通过内置数据库批量导入或 bootstrap 文件在非全局命名空间中创建超级用户时，现在会拒绝该操作，使其行为与单用户管理 API 保持一致。此类记录会被报告为失败，且不会存储。

- [#17974](https://github.com/emqx/emqx/pull/17974) 连接日志现在默认对原始 MQTT 报文数据进行脱敏；可按监听器将可信客户端 IP 地址加入允许列表，以用于诊断。

- [#18005](https://github.com/emqx/emqx/pull/18005) 修复了 CLI 审计日志可能存储敏感命令参数的问题。

- [#18009](https://github.com/emqx/emqx/pull/18009) 统一了 Dashboard 管理员账户和 API 密钥的 `scopes` 字段处理。读取账户或密钥信息时，API 会返回其有效权限范围。创建或更新时，如果提交的权限范围列表与角色的隐含默认权限范围一致，或提交 `unset`，EMQX 会将其视为“未显式设置权限范围”。此时，EMQX 不会保存固定的权限范围列表，而是继续使用角色的隐含默认权限范围，因此后续版本新增的默认权限范围会自动生效。

  - 修复了通过 `PUT /api/v5/users/{username}` 仅更新默认管理员备注（描述）时请求被拒绝的问题。EMQX 不再将此类请求中携带的默认权限范围列表视为显式设置。如果提交的值为 `unset`，或提交的列表与管理员角色的完整默认权限范围一致，EMQX 会将其视为“未显式设置权限范围”并允许更新备注。
  - [#18196](https://github.com/emqx/emqx/pull/18196) 创建或更新 API 密钥时，EMQX 会将 `unset` 或与角色默认权限范围一致的列表视为“未显式设置权限范围”。因此，查询 API 密钥时返回的 `scopes` 值可以原样提交，不再导致请求失败。
  - [#18221](https://github.com/emqx/emqx/pull/18221) EMQX 启动并创建默认管理员账户时，不再保存显式权限范围列表。对于已保存显式列表的现有默认管理员账户，EMQX 会在启动时清除该列表，使其改用管理员角色的隐含默认权限范围。

#### 多租户

- [#17807](https://github.com/emqx/emqx/pull/17807) 为命名空间管理员新增隔离的备份空间。通过数据备份端点（`/data/export`、`/data/import`、`/data/files` 和 `/data/files/:filename`）执行导出、上传、列出、下载、导入和删除操作时，仅会作用于其所属命名空间中的备份。命名空间管理员无法查看、下载或删除全局备份或其他命名空间中的备份。

  全局管理员默认继续管理全局备份，包括此变更前创建的备份。他们可以向 `GET /data/files`、`DELETE /data/files` 或 `GET /data/files/:filename` 传递 `namespace` 查询参数，以检查或删除特定命名空间中的备份。

- [#17975](https://github.com/emqx/emqx/pull/17975) 禁止命名空间范围的调用方通过 `PUT /api/v5/tracing` 更新全局追踪配置。命名空间范围的 Dashboard 用户和 API 密钥会收到 HTTP 状态码 `403`。全局 Dashboard 管理员仍可访问该端点。

- [#18008](https://github.com/emqx/emqx/pull/18008) 支持全局管理员在通过 `POST /api/v5/data/import` 和 `POST /api/v5/data/files` 为特定命名空间导入或上传备份时，传递可选的 `namespace` 查询参数。如果省略该参数，操作将使用全局备份范围。

  对于命名空间管理员，EMQX 会忽略 `namespace` 查询参数，并将操作限制在调用方所属命名空间中。此行为与备份列出和下载操作保持一致。

- [#18117](https://github.com/emqx/emqx/pull/18117) 删除命名空间时，现在还会删除该命名空间的内置数据库认证用户（包括基于密码和 SCRAM 的用户）及授权规则。此前，这些记录会在命名空间删除后保留，并在之后创建同名命名空间时重新出现。

  此外，新增 `emqx ctl mt purge_ns <namespace>` CLI 命令，用于删除命名空间并清除其所有数据。该命令具有幂等性，且不要求命名空间存在，因此可在之前的命名空间删除操作中断后，作为最后手段清理残留数据。

#### 网关

- [#17796](https://github.com/emqx/emqx/pull/17796) 修复了新设备通过最近由已断开设备使用的 UDP 源端口连接时，MQTT-SN 网关崩溃的问题。这种情况常见于环回网络和 NAT 后方，因为操作系统或 NAT 设备会重新分配相同端口。现在会正确清理旧通道，并将新连接作为全新会话处理。

- [#17805](https://github.com/emqx/emqx/pull/17805) 修复了此前的网关加载尝试中途终止后，例如因配置无效或监听端口被占用，再次加载网关可能以 `already_started` 错误失败的问题。现在会自动回收失败尝试遗留的 locker 进程，使下次 `load` 或运维人员重试可以从干净状态开始。

- [#17815](https://github.com/emqx/emqx/pull/17815) 修复了 UDP 源元组发生变化或被复用时的 MQTT-SN UDP 会话路由。

  MQTT-SN UDP 监听器现在通过 `esockd_udp_proxy`，根据从报文中解析出的 ClientId 路由报文，使休眠会话可以从不同的 UDP 源元组恢复，同时防止复用的 UDP 源元组将另一个 ClientId 的报文传递到旧会话。

- [#17888](https://github.com/emqx/emqx/pull/17888) 修复了 LwM2M 网关可能在注册和更新 MQTT 报告中包含敏感 REGISTER 查询字段的问题，例如 `password`、`secret`、`private_key` 和 `access_token`。

- [#18051](https://github.com/emqx/emqx/pull/18051) 修复了 CoAP 调试日志可能泄露敏感 URI 查询值的问题。

#### 插件

- [#17861](https://github.com/emqx/emqx/pull/17861) 恢复了此前的插件启动行为：节点启动或重新加入集群时，不再删除集群插件配置中缺失的本地插件包。

- [#17884](https://github.com/emqx/emqx/pull/17884) 修复了插件管理 HTTP API，使其忽略集群插件配置中不存在且未在本地运行的过期已解压插件目录。

  此类过期插件包不会出现在插件列表、详情、配置或 schema 响应中。插件操作 API 无法操作这些插件包，且这些插件包不会阻止通过 HTTP 安装 API 重新安装。已配置的预安装插件仍然可见，并继续遵循文档所述的预安装工作流。

  如果插件包已解压，但在 `plugins.states` 中既未启用也未禁用，EMQX 会在启动或访问 HTTP API 时记录错误。

- [#17932](https://github.com/emqx/emqx/pull/17932) 修复了通过 CLI 安装插件时产生多余 `failed_to_get_plugin_config_from_cluster` 警告的问题。

  `emqx ctl plugins install` 命令现在使用 `fresh_install` 模式，与 HTTP API 的行为保持一致。此模式会跳过新安装插件的集群配置查找，避免在每个集群节点上重复产生 `config_not_found_on_node` 警告。

  为 `emqx ctl plugins install` 新增 `--cluster` 标志，用于在整个集群中安装插件。指定该标志时，此命令会将插件包分发并安装到所有正在运行的节点上。

- [#18018](https://github.com/emqx/emqx/pull/18018) 修复了插件安装在校验插件包的应用声明、配置 schema 和默认配置之前加载代码的问题。

#### 可观测性

- [#17886](https://github.com/emqx/emqx/pull/17886) 在 Prometheus 中将发布配额超限报文指标公开为 `emqx_packets_publish_quota_exceeded`。

- [#18114](https://github.com/emqx/emqx/pull/18114) 修复了节点正在加入集群时，Dashboard 指标 API（`GET /api/v5/monitor_current` 和 `GET /api/v5/monitor`）返回 `500 INTERNAL_ERROR` 的问题。

  当加入节点重启其应用而无法采样指标时，API 会返回其余可达节点的聚合指标并记录警告，而不是使整个请求失败。

  此外，修复了每次成功执行 `DELETE /api/v5/monitor` 请求后错误记录 `clear_monitor_metrics_rpc_errors` 警告的问题。

#### 文件传输

- [#18069](https://github.com/emqx/emqx/pull/18069) 修复了文件传输文件 API（`GET /api/v5/file_transfer/files`）列出名称包含非 ASCII 字符（例如中文）的文件时返回 500 错误的问题。

#### 部署

- [#17877](https://github.com/emqx/emqx/pull/17877) 修复了 `emqx-enterprise` Helm Chart 在节点主机名中硬编码 `svc.cluster.local` 的问题。在 DNS 域不是 `cluster.local` 的 Kubernetes 集群中，节点会使用无法解析的 FQDN 作为自身名称，导致 Erlang 分布式通信无法启动，节点也无法组成集群。主机名现在遵循 Chart 的 `clusterDomain` 值，该值此前已用于控制 DNS 和 Kubernetes 服务发现设置。

## 6.1.3

*发布日期: 2026-07-01*

在升级到 EMQX 6.1.3 之前，请务必查阅不兼容变更和已知问题。

### 增强

#### 集群

- [#17530](https://github.com/emqx/emqx/pull/17530) 集群连接现在需要非社区版 License。在默认社区版 License 下，已配置的连接会保持非活动状态（不转发消息，也不复制路由），REST API 在尝试启用连接时会拒绝请求，并明确提示需要加载非社区版 License。禁用和删除连接仍可使用，以便清理遗留配置。升级 License 后，可通过 Dashboard 或 REST API 启用连接，无需重启节点。
- [#17549](https://github.com/emqx/emqx/pull/17549) 新增 EMQX Backup Sync 插件，通过数据备份 API 定期将选定配置从主集群同步到备集群。该插件支持为对主集群的 HTTPS 调用配置 TLS 选项。

- [#17620](https://github.com/emqx/emqx/pull/17620) 新增面向运维人员的诊断模块 `emqx_router_tool`，用于检查和修复路由表。该模块可通过 `emqx ctl eval` 运行，提供三个辅助函数：

  - `cluster_schema_view/0` 用于报告每个集群节点正在使用的路由存储 schema。
  - `scan_missing_routes/0,1` 会流式扫描本地订阅表，并报告本节点缺少对应路由条目的主题。该扫描会执行两遍、限流运行，并可容忍并发订阅和取消订阅。
  - `reconcile_missing_routes/0,1` 会通过现有 `emqx_router:add_route/2` API 重新添加缺失的路由。

  该模块不依赖特定 schema，可安全用于运行中的集群。

#### 多租户

- [#17711](https://github.com/emqx/emqx/pull/17711) 统一了内置数据库认证用户 HTTP API 中的命名空间选择方式，并允许清理已删除命名空间遗留的记录。

  此前，只有创建用户时支持在请求体中传入 `namespace` 字段；更新和删除用户时只能通过 `ns` 查询参数指定目标命名空间。现在，更新和删除端点也支持在请求体中传入 `namespace` 字段。当二者同时提供时，`ns` 查询参数优先。用户列表仍继续使用 `ns` 查询参数。

- [#17665](https://github.com/emqx/emqx/pull/17665) 为多租户应用新增按命名空间统计的消息丢弃计数器和投递丢弃计数器。这些计数器通过 `/api/v5/prometheus/namespaced_stats` 暴露，并带有 `namespace` 标签，与现有按命名空间划分的指标族一起提供。运维人员现在可以直接通过 Prometheus 按租户诊断丢弃率，而无需依赖日志排查。

  已知限制：QoS 2 PUBREL 等待超时导致的丢弃目前还无法按命名空间归因，因为该丢弃路径只递增全局计数器，且不会触发 `message.dropped` 钩子。

#### 数据集成

- [#17481](https://github.com/emqx/emqx/pull/17481) 为 MQTT Bridge 入口（Source）订阅新增 `retain_as_published` 选项。当 Bridge 使用 MQTT 5.0 连接到远端 Broker 且 `retain_as_published = true` 时，转发消息会保留原始 `retain` 标志，而不是清除该标志，从而可以如实重新发布来自上游的保留消息。默认值为 `false`，以保持现有行为。当 `proto_ver` 为 `v3` 或 `v4` 时，该选项不生效。

  此外，当同时配置 `bridge_mode = true` 和 `proto_ver = v5` 时，连接器现在会输出一条警告日志，因为旧版 bridge-mode 标志在 MQTT 5.0 下不生效；请改为在单个订阅上设置 `retain_as_published`。

- [#17508](https://github.com/emqx/emqx/pull/17508) 为 PostgreSQL 和 TimescaleDB 连接器连接设置 PostgreSQL `application_name` 启动参数为 `emqx`。

  这使得 EMQX 数据库会话更容易在 PostgreSQL 日志和 `pg_stat_activity` 等视图中识别。

- [#17576](https://github.com/emqx/emqx/pull/17576) 通过现有 `ssl.ciphers` 字段为 GreptimeDB 连接器新增 TLS 密码套件配置支持。指定密码套件列表后，TLS 协商将被限制为这些套件。不支持的密码套件会在连接器启动时被拒绝。

- [#17594](https://github.com/emqx/emqx/pull/17594) 支持为 Google Cloud Pub/Sub 和 BigQuery 连接器的 `service_account_json` 配置 `file://` 密钥文件，从而可以从外部文件注入服务账号凭证。

- [#17717](https://github.com/emqx/emqx/pull/17717) 为 Confluent Producer 连接器新增启用 TLS 对端验证的选项。

- [#17718](https://github.com/emqx/emqx/pull/17718) 为 GCP PubSub Producer/Consumer 和 BigQuery 连接器新增启用 TLS 对端验证的选项。

#### 可观测性

- [#17712](https://github.com/emqx/emqx/pull/17712) 新增 `emqx_session_tool` 诊断模块，运维人员可通过远程控制台调用。使用 `emqx_session_tool:top_by(mqueue_len)`，可在连接数较多的集群中按 gauge 或 counter 值查找 top-K 会话。还支持其他会话指标，例如 `mqueue_dropped` 和 `inflight_cnt`。这有助于运维人员定位最繁忙的会话，而无需手动翻阅客户端列表。

  该扫描会流式遍历 channel registry，仅保留有界的 top-K 结果，并读取缓存的单会话指标，而不会向连接进程发送消息。`emqx_session_tool:cluster_top_by/1` 会汇总所有集群节点上的结果。

- [#17558](https://github.com/emqx/emqx/pull/17558) 在 `GET /monitor_current` HTTP API 中新增两个指标及其对应速率：`rules_matched` 和 `actions_executed`，分别用于跟踪规则匹配数量和动作执行速率（成功 + 失败）。

  同时修复了非批处理模式（`batch_size = 1`）下 `actions.executed` 少计动作调用次数的问题：计数器现在会在每次动作回调调用时递增，不再依赖缓冲区 Worker 的遥测刷新窗口。

### 修复

#### 核心 MQTT 功能

- [#17529](https://github.com/emqx/emqx/pull/17529) 修复了通过消息队列订阅投递的 QoS 0 消息可能在内部保持未确认状态的问题。该问题会导致队列订阅者在达到本地 inflight 限制后停止接收更多消息。
- [#17540](https://github.com/emqx/emqx/pull/17540) 修复在 SSL 监听器上设置 `password = "file://..."` 时，如果 keyfile 已加密，配置校验会因 `bad_password_or_invalid_keyfile` 失败的问题。现在，`file://` 引用会在校验期间解析，而不只是在运行时解析。
- [#17569](https://github.com/emqx/emqx/pull/17569) 将 MQTT v5 User Property 解析成本从平方复杂度降低为线性复杂度。

  此前，当 CONNECT、PUBLISH 或 SUBSCRIBE 报文携带大量 User Property 时，每个解析出的属性都会追加到累积列表末尾，导致拥有该连接的进程出现超线性的调度耗时。现在，解析会在保留属性顺序的同时按条目数量线性扩展。

- [#17731](https://github.com/emqx/emqx/pull/17731) 修复更新 WS 或 WSS 监听器选项时可能出现的临时性 "address already in use" 错误（例如轮换 TLS 证书时）。更新此类监听器会重新绑定端口，而操作系统可能尚未释放旧 socket；现在 EMQX 会短暂重试重新绑定，而不是直接让更新失败。

- [#17798](https://github.com/emqx/emqx/pull/17798) 修复保留消息可能使用原始发布 QoS 投递，而不是使用通配符订阅 QoS 上限投递的问题。

- [#17801](https://github.com/emqx/emqx/pull/17801) `ssl_opts.ciphers` 校验器现在接受 OpenSSL 或 IANA/RFC 命名格式的密码套件名称。此前，仅支持 OpenSSL 格式的名称，因此以 IANA 名称提供的有效 TLS 1.2 密码套件（例如 `TLS_ECDHE_ECDSA_WITH_AES_256_GCM_SHA384`）会被错误地拒绝为 `bad_ciphers`，即使 Erlang 的 `ssl` 模块本可以接受该名称。TLS 1.3 密码套件不受影响，因为它们的 IANA 名称和 OpenSSL 名称相同。

#### 队列与流

- [#17515](https://github.com/emqx/emqx/pull/17515) 修复了使用 QoS 0 的消息队列订阅在队列订阅者本地 inflight 窗口满后可能停止接收消息的问题。
- [#17733](https://github.com/emqx/emqx/pull/17733) 修复消息队列消费者在持久存储订阅恢复后，可能无法恢复空流缓冲区的问题。

#### 规则引擎

- [#17725](https://github.com/emqx/emqx/pull/17725) 修复 6.0.3、6.1.2 和 6.2.1 中引入的问题：当发布客户端携带租户命名空间（`client_attrs.tns`）时，全局规则可能无法再匹配其 `FROM` 主题上的消息。

  当启用 `rule_engine.limit_selects_in_namespace`（默认启用）时，全局规则现在会保留系统范围可见性，并匹配来自任意命名空间的消息。在命名空间内创建的规则仍隔离在各自命名空间内。若运维人员希望完全禁用命名空间限制，仍可设置 `rule_engine.limit_selects_in_namespace = false`。

#### 数据集成

- [#17568](https://github.com/emqx/emqx/pull/17568) 将 Kafka 客户端库 `brod` 升级到 4.5.5。

  消费者组：当 join 响应携带 `member_id_required` 错误码（由不支持静态成员实例 ID 的旧版 Kafka Broker 返回，例如 2.2.0）时，尊重 Broker 分配的 member ID。此前，错误返回中的 member ID 会被丢弃，导致重试无法成功。

- [#17579](https://github.com/emqx/emqx/pull/17579) 修复 Redis Sentinel 连接器，使其为每个资源使用独立的 Sentinel 管理器，并在资源停止时清理这些管理器，避免连接器之间共享 Sentinel 状态。

- [#17584](https://github.com/emqx/emqx/pull/17584) 限制了 Snowflake 聚合连接器健康检查期间返回的数据量。仅当已有 schema 列表非常大时才有可观察影响，此时健康检查的执行时间将大幅缩短。

- [#17588](https://github.com/emqx/emqx/pull/17588) 限制了 Kinesis 集成的连接器和动作健康检查期间返回的数据量。仅当已有 schema 列表非常大时才有可观察影响，此时健康检查的执行时间将大幅缩短。

- [#17595](https://github.com/emqx/emqx/pull/17595) 限制了 S3 和 S3 Tables 集成的连接器健康检查期间返回的数据量。仅当已有桶（bucket）列表非常大时才有可观察影响，此时健康检查的执行时间将大幅缩短。

- [#17598](https://github.com/emqx/emqx/pull/17598) 修复 MongoDB 8.0+ 在需要认证时的连接失败问题。此前，驱动会在认证前查询 `buildInfo` 以选择认证机制；MongoDB 8.0 将该命令限制为只有已认证调用方可用。现在驱动会跳过该探测，并直接使用所有受支持 MongoDB 版本都接受的 SCRAM-SHA-1。

- [#17605](https://github.com/emqx/emqx/pull/17605) 修复 Oracle 动作的 prepare/status 检查，使其在不执行 SQL 的情况下解析动作 SQL，并拒绝不受支持的顶层 DDL/DCL/TCL 语句。同时改进了对超过 4000 字节文本 Payload 的支持，适用于 Payload 占位符是最后一个绑定参数的场景。

- [#17625](https://github.com/emqx/emqx/pull/17625) 修复 GCP PubSub Consumer Source 的问题：如果 Source 初始创建时使用的服务账号缺少为配置主题创建订阅所需的权限，即使后来向该服务账号授予了权限，该 Source 也无法变为 `connected`。

- [#17633](https://github.com/emqx/emqx/pull/17633) 修复 6.1.2 和 6.2.1 中引入的问题：MQTT Bridge 和集群连接的 TLS 连接在短时间流量后可能停滞。受影响节点会反复记录来自 `emqtt` 客户端的类似 `unexpected_event ... ssl_passive ...` 的错误消息。EMQX 现在内置 `emqtt` 1.15.3，在 [#17617](https://github.com/emqx/emqx/issues/17617) 首次报告该问题后恢复了正常流量传输。

- [#17649](https://github.com/emqx/emqx/pull/17649) 提升启动和停止 GCP PubSub Consumer 连接器的响应性。此前，如果连接较慢或繁忙，可能出现超时，导致连接器仍在运行且状态与配置不一致。

- [#17681](https://github.com/emqx/emqx/pull/17681) 修复禁用预处理语句时 PostgreSQL 连接器的批量写入问题。

  此前，同一连接上的并发批次可能会交错执行原始 SQL 解析，并导致 PostgreSQL 协议错误。表存在性检查现在也会通过连接器 worker 串行执行，以避免与批量执行交错。

- [#17701](https://github.com/emqx/emqx/pull/17701) 修复 PostgreSQL 动作在批处理中使用会返回结果行的 SQL 模板（例如 `SELECT ...`）时，出现含义不清的 `badarith` 错误的问题。

  PostgreSQL 动作批处理不支持返回结果行的 SQL。现在 EMQX 会返回明确的不支持 SQL 错误，而不是让批处理结果处理器崩溃。

#### 集群

- [#17586](https://github.com/emqx/emqx/pull/17586) 定期清理全局会话注册表中的陈旧条目。

  此前，如果会话的属主进程在未正常注销的情况下退出，并且相同的客户端 ID 再也没有重新连接，注册表行可能会永久残留。例如，短暂网络分区导致注销操作未能复制，或在 down 事件清理期间某个 core 节点的一致性检查超时，都可能触发该问题。

  现在，每个 core 节点上都有一个受限流控制的后台清理任务来移除此类行。该任务限制为每个节点每秒最多 500 行，且运行间隔不短于 10 分钟，因此即使在持有数百万会话的注册表上也不会对 Broker 吞吐量产生可观测影响。

- [#17773](https://github.com/emqx/emqx/pull/17773) 修复配置更新命令（REST API 和 CLI）在底层集群 RPC 层意外中止时，可能触发 `function_clause` 崩溃报告的问题。例如，当节点启动或恢复期间集群 RPC 表尚不可用时，可能会出现 `{no_exists, cluster_rpc_mfa}`。现在，此类失败会作为结构化错误返回给调用方。

- [#17764](https://github.com/emqx/emqx/pull/17764) 修复某节点离线期间插件已从集群卸载时，该节点重新加入集群后本地仍可能残留过期插件条目的问题。现在，EMQX 会在插件启动过程中移除本地不再存在于集群插件配置中的插件包。

#### 访问控制

- [#17575](https://github.com/emqx/emqx/pull/17575) 修复 `emqx_username_quota` 插件中的竞争条件。该问题可能导致按用户名统计的会话计数器与实际跟踪的客户端记录数量不一致。计数器可能被递减到零以下，随后被删除；与此同时，并发会话注册又递增该计数器，导致该增量永久丢失。

- [#17644](https://github.com/emqx/emqx/pull/17644) 修复 `plain` 密码哈希算法在认证时接受仅大小写不同的密码的问题。

- [#17646](https://github.com/emqx/emqx/pull/17646) 修复 JWT 认证中 JWKS 获取客户端的 HTTP/1.1 协议合规性问题。早期版本由于 Erlang/OTP `inets` HTTP 客户端中的长期默认行为（已在 inets 9.4.2 / OTP 28.1 中修复），会发送空值 `TE:` 头。一些身份提供商（尤其是 PingFederate）会拒绝此类请求。EMQX 现在在获取 JWKS 时发送显式且有效的 `TE: trailers` 头。

- [#17653](https://github.com/emqx/emqx/pull/17653) 修复 Prometheus 配置 API 在响应中返回 Pushgateway 头部里已存储的 `Authorization` 头值的安全问题。现在 API 会在响应中对这些值进行脱敏。

- [#17654](https://github.com/emqx/emqx/pull/17654) 修复通过 `POST /authentication` 创建认证器时，返回的新认证器配置未对提供方密钥进行脱敏的问题，这些密钥包括 JWT HMAC 密钥、HTTP `Authorization` 头以及请求体密码等。创建响应现在会应用与 list 和 get 端点相同的脱敏处理。

- [#17657](https://github.com/emqx/emqx/pull/17657) 修复原始 `authorization` 和 `cookie` 头被转发到插件 API 回调的安全问题。现在这些包含凭据的头在到达插件代码前会被脱敏。

- [#17711](https://github.com/emqx/emqx/pull/17711) 当创建或更新内置数据库用户时，如果目标命名空间不是已知托管命名空间，现在会失败并返回 "Managed namespace not found"。此前，当命名空间在请求体中提供时，即使该命名空间不存在，也可能创建用户。

  此外，全局管理员现在可以删除属于已删除命名空间的内置数据库用户，而不再收到 "Managed namespace not found" 错误。

- [#17736](https://github.com/emqx/emqx/pull/17736) 限制 JWT 认证器只能使用与配置密钥类型一致的 JWS 算法验证令牌。基于 HMAC 的认证器现在只接受 `HS256`、`HS384` 和 `HS512`。公钥和 JWKS 认证器接受 `RS*`、`PS*`、`ES*` 和 `EdDSA` 算法。`alg` 头与配置密钥类型不匹配的令牌（包括 `alg=none`）都会被拒绝。

- [#17739](https://github.com/emqx/emqx/pull/17739) 改进了日志、追踪和审计记录中敏感数据的脱敏处理。

- [#17787](https://github.com/emqx/emqx/pull/17787) 防止当 `ehttpc` worker 在请求过程中被终止时，HTTP 连接器错误日志中包含请求头。

  此前，如果 HTTP 连接器的 `ehttpc` worker 在请求尚未返回时被终止（例如在请求返回前删除对应 Source），生成的 EXIT reason 会携带原始 `gen_server:call` 参数。由于这些参数包含请求头，请求头会被原样写入错误日志。现在，EMQX 会在记录日志前从 reason 中移除这些调用参数。

- [#17790](https://github.com/emqx/emqx/pull/17790) 停止将 TOTP 共享密钥写入 `dashboard_login_failed` 服务器日志。此前，在首次设置 MFA 期间，该密钥会包含在此日志条目中。

- [#17791](https://github.com/emqx/emqx/pull/17791) 改进日志脱敏，避免 JWT HMAC 密钥字节出现在配置更新期间输出的 `cluster_rpc_apply_result` 和 `cluster_rpc_apply_ok` 调试日志中。

  脱敏器现在可以识别内部 JWK record 结构，并在记录日志前将其替换为占位符，同时也会将 `jwk` 字段视为敏感字段。

#### 多租户

- [#17715](https://github.com/emqx/emqx/pull/17715) 修复一个多租户准入检查缺口。此前，当配置了 `multi_tenancy.post_auth_tns_expression` 且表达式求值为空字符串或错误时，命名空间准入检查（`allow_only_managed_namespaces` 强制检查、会话配额等）会被跳过，从而允许客户端通过。

  空字符串和错误结果现在会被视为 "no namespace assigned"，并与认证前未提供命名空间的客户端一样经过同一准入检查。当 `allow_only_managed_namespaces = true` 时客户端会被拒绝；当其为 `false` 时，客户端会在不带命名空间的情况下被接受。在这种情况下，认证前 `client_attrs.tns` 中携带的任何命名空间值也会被清除，因此当表达式拒绝分配命名空间时，该值不会被保留。

- [#17757](https://github.com/emqx/emqx/pull/17757) 修复 `/prometheus/namespaced_stats`，使命名空间管理员和 API 密钥只能查看其所属命名空间的数据。全局管理员和 API 密钥仍可查看所有命名空间的数据。

#### 网关

- [#17556](https://github.com/emqx/emqx/pull/17556) 修复 OCPP 网关未将监听器 `enable_authn` 选项传递给共享认证流程的问题。该问题是由于该选项存储在拼写错误的 client-info key 下导致的。
- [#17581](https://github.com/emqx/emqx/pull/17581) 修复 JT/T 808 网关，使其使用认证期间接受的手机号作为连接身份，拒绝不匹配的注册码认证尝试以及手机号不同的后续上行帧。
- [#17604](https://github.com/emqx/emqx/pull/17604) 修复 GBT32960 网关路由：车辆对下行命令（参数查询、参数设置、终端控制）的响应现在会正确发布到 `upstream/response`，而不是 `upstream/transparent`。
- [#17765](https://github.com/emqx/emqx/pull/17765) 修复多个网关发布和订阅流程中缺少授权检查的问题。现在，以下操作会在发布或订阅前执行授权检查：MQTT-SN Will 消息发布；JT/T 808 上行发布和自动下行订阅；GBT32960 上行发布和自动下行订阅；以及 OCPP 上行发布和自动下行订阅。

#### 可观测性

- [#17497](https://github.com/emqx/emqx/pull/17497) 修复非批处理模式（`batch_size = 1`）下配置的动作中，`actions.executed` 指标低于 `actions.messages` 的问题。

  此前实现会在每次 buffer-worker 遥测刷新时递增一次 `actions.executed`，一次刷新可能聚合多个单独完成事件，因此即使未配置批处理，`actions.executed` 也会落后于 `actions.messages`。

  现在，这两个指标会在独立调用点递增：`actions.executed` 按动作回调调用次数递增（批处理模式下每批一次，单条模式下每条消息一次），`actions.messages` 按处理的消息数递增。

- [#17513](https://github.com/emqx/emqx/pull/17513) 修复 Prometheus 匹配授权允许/拒绝指标，使其反映实际匹配到的授权决策。

- [#17536](https://github.com/emqx/emqx/pull/17536) 在 Dashboard 中为 SSL 监听器 `password` 以及其他 secret 类型配置字段（MQTT Bridge 密码、集群连接密码、Dashboard OIDC 客户端密钥、S3 Secret Access Key、AI Completion API Key、Pulsar/RocketMQ 凭据等）的工具提示补充 `file://` 选项说明。通用 secret 类型描述已提到这一约定，但字段级描述会覆盖该说明，导致用户误以为这些字段只接受字面值。

- [#17708](https://github.com/emqx/emqx/pull/17708) 修复 logger JSON formatter 崩溃可能导致部分 debug 级别追踪事件被替换为 `FORMATTER CRASH` 行的问题。

## 6.1.2

*发布日期: 2026-06-09*

在升级到 EMQX 6.1.2 之前，请务必查阅不兼容变更和已知问题。

### 增强

#### 安全加固

- [#17040](https://github.com/emqx/emqx/pull/17040) 限制 API 密钥访问 Dashboard 用户账户管理端点。

  此前，具有 `administrator` 角色的 API 密钥可通过 HTTP Basic 认证调用 Dashboard 用户管理端点 `POST/DELETE /users/:username/mfa` 和 `POST /users/:username/change_pwd`，这意味着 API 密钥可以重置或禁用其他 Dashboard 用户的 MFA，或修改其密码，绕过了人工 Dashboard 会话与机器 API 密钥之间的预期隔离。

  这些接口现在在通过 API 密钥访问时返回 `401 API_KEY_NOT_ALLOW`，与已阻止 API 密钥访问 `/users`、`/users/:username`、`/logout` 和 `/api_key` 的现有策略保持一致。Dashboard 用户仍可通过 Bearer Token（JWT）会话在 Dashboard UI 中管理自己的 MFA 和密码。

- [#17065](https://github.com/emqx/emqx/pull/17065) 为规则引擎可访问的连接器和 Bridge 配置添加 SSRF 防护。

  当 `rule_engine.ssrf.enable` 设置为 `true` 时，EMQX 对连接器、Bridge 和动作配置应用出站 SSRF 策略。策略对每个目标的评估流程如下：`rule_engine.ssrf.deny_hosts` 中的精确匹配项立即被拒绝；解析后的目标 IP 先经 `rule_engine.ssrf.allow_cidrs` 检查，再经 `rule_engine.ssrf.deny_cidrs` 检查。默认拒绝范围涵盖回环地址、链路本地地址（包括云实例元数据端点）、RFC 1918、ULA、未指定地址和多播地址。检查在配置更新时执行，覆盖所有连接器类型的 HTTP `url` 字段及 `server`、`servers`、`bootstrap_hosts` 字段。

  该功能默认禁用，以保持与连接器合法指向内部服务的部署的兼容性。建议在多租户或对外暴露的环境中启用此功能，并配合网络层出站防火墙一同使用。

- [#17173](https://github.com/emqx/emqx/pull/17173) 限制 API 密钥通过数据备份端点导出或导入 Dashboard 账户及 API 密钥。

  使用 API 密钥调用 `POST /data/export` 时，生成的归档文件中将静默省略 `dashboard_users` 和 `api_keys` Mnesia 表集。使用 API 密钥调用 `POST /data/import` 时，若上传的备份包含上述任一表集，将返回 `403 FORBIDDEN`。

  使用 Dashboard bearer-token（登录）调用不受影响，仍可备份和还原完整数据库，包括 Dashboard 用户和 API 密钥。

  此修复关闭了一个权限提升漏洞。现有的 `/users` 和 `/api_key` 端点已拒绝 API 密钥访问 Dashboard 登录凭据和 API 密钥记录，但 API 密钥持有者此前可通过数据备份端点绕过这些限制。

- [#17187](https://github.com/emqx/emqx/pull/17187) 从未经认证的 `GET /status?format=json` 响应中移除 EMQX 发行版本号（`rel_vsn`），避免向未认证调用方泄露 Broker 版本信息。版本信息仍可通过需要认证的节点信息 API 获取。

- [#17201](https://github.com/emqx/emqx/pull/17201) 加强插件安装端点对上传 tarball 中路径穿越的防护，并收紧安装白名单。

  - 安装路径现在拒绝解压任何条目会解析到插件安装目录以外的 tarball。
  - `emqx ctl plugins allow <name-vsn>` 条目在签发后 5 分钟过期，并可通过 `emqx ctl plugins allow <name-vsn> sha256:<HEX>` 固定到软件包的 SHA-256 哈希值。内容与固定哈希值不匹配的上传将被拒绝并返回 `403 Forbidden`。省略可选的 `sha256:` 参数时，保留原有的接受任何名为 `<name-vsn>.tar.gz` 的载荷的行为。
  - 通过 HTTP 插件安装端点（及其封装的 Dashboard 上传）成功安装后，白名单条目会立即在集群范围内撤销，防止同一授权被重复用于不同的 tarball。

- [#17252](https://github.com/emqx/emqx/pull/17252) 在官方下载站点的插件包旁发布 `.sha256` 校验和附件，允许用户验证下载的插件归档完整性。

- [#17271](https://github.com/emqx/emqx/pull/17271) 加固官方 EMQX Docker 镜像，清除镜像扫描器报告的问题：

  - 在运行时镜像构建期间应用 Debian 安全升级，使镜像获取最新修复版 `libssl3t64`。
  - 移除未使用的 `libgnutls30t64` 包。EMQX 通过 Erlang/OTP 使用 OpenSSL 进行 TLS 通信，从不链接 GnuTLS，该包仅作为 `curl` 的传递依赖存在并出现在扫描报告中。
  - 将 Debian `curl` 包替换为来自 [stunnel/static-curl](https://github.com/stunnel/static-curl) 的静态链接 `curl` 二进制文件（OpenSSL、HTTP/2、HTTP/3；无 RTMP，无 GnuTLS）。Debian 包会通过 `librtmp1` 重新引入 `libgnutls30t64`；静态二进制文件避免了这一问题，同时保持调用 `curl` 的容器健康检查正常工作。

- [#17309](https://github.com/emqx/emqx/pull/17309) 对 PROXY Protocol v2 SSL Common Name 和 Subject 字段进行净化处理，防止控制字符被带入客户端身份信息。

  当监听器配置了 `proxy_protocol = true` 时，Broker 现在会拒绝 PROXY Protocol SSL TLV 字节中包含 ASCII 控制字符的连接（与已应用于 MQTT 摄取的 `clientid`、`username` 和 `password` 的字节类检查相同）。这阻止了攻击者控制的字节通过 `${cert_common_name}` 和 `${cert_subject}` 模板到达出站 HTTP 认证、授权或规则引擎头部值。

  HTTP 认证和授权客户端现在也会在渲染后的请求头名称或值包含 CR、LF 或 NUL 字节时拒绝发送请求。

- [#17315](https://github.com/emqx/emqx/pull/17315) 将 MQTT clientid/username/password 的字节类检查扩展至其他填充 `ClientInfo` 和 HTTP 请求模板的字段：

  - `peersni`（TLS 服务器名称指示；也可从 PROXY Protocol v2 的 `authority` TLV 接受）现在在连接摄取边界进行验证。包含控制字符的连接会被拒绝并记录警告日志。
  - 由 `mqtt.client_attrs_init` Variform 表达式生成的客户端属性值，若包含控制字符则被丢弃（并记录警告），从而防止 `${client_attrs.tns}` 等模板将注入字节传播至下游。
  - HTTP 动作/Bridge 连接器渲染头部时，任何渲染后名称或值包含 NUL、CR 或 LF 的头部都会被丢弃。

- [#17440](https://github.com/emqx/emqx/pull/17440) 将 `GET /api/v5/data/files/<filename>`（备份文件下载）限制为全局 Dashboard 管理员。备份归档可能包含 Dashboard 账户（含密码哈希及 MFA/TOTP 状态）和 API 密钥记录，因此 API 密钥调用方、Dashboard 查看者和命名空间管理员不再被允许下载。列出备份目录（`GET /api/v5/data/files`）的权限对之前有访问权限的所有角色保持不变。

- [#17491](https://github.com/emqx/emqx/pull/17491) 修复了网关认证 API、错误路径和调试日志中密码和密钥被暴露的问题。网关认证 API 响应现在在保留原始配置结构的同时对密钥进行脱敏处理。以下日志路径不再打印原始密码或密钥：网关认证失败日志、监听器启动错误日志、ExProto 认证日志、CoAP 令牌必需日志和 LwM2M 无效注册日志。

- [#17501](https://github.com/emqx/emqx/pull/17501) 阻止命名空间 Dashboard 用户跨命名空间读取 MQTT 消息内容。

  - 以下接口对任何非全局调用方返回 `403 FORBIDDEN`，因为它们可能暴露调用方命名空间之外的 MQTT Payload。此前，命名空间用户可以读取或删除其他命名空间产生的消息。

    - `GET /clients/:clientid/mqueue_messages`
    - `GET /clients/:clientid/inflight_messages`
    - `GET|DELETE /mqtt/retainer/messages`
    - `GET|DELETE /mqtt/retainer/message/:topic`
    - `GET /mqtt/delayed/messages`
    - `GET|DELETE /mqtt/delayed/messages/:node/:msgid`
    - `DELETE /mqtt/delayed/messages/:topic`

  - Trace API 现已按命名空间隔离：`GET /trace` 仅列出由调用方命名空间创建的追踪。单个追踪的端点（`/trace/:name`、`/trace/:name/download`、`/trace/:name/log`、`/trace/:name/log_detail`、`/trace/:name/stop`）在追踪属于其他命名空间时返回 `404`，防止调用方发现其他命名空间的追踪记录。批量 `DELETE /trace` 仅限全局管理员使用，命名空间调用方将收到 `403`。命名空间管理员对自己的追踪仍拥有完整权限，包括创建、列出、下载、流式传输、停止和删除。

#### 集群

- [#17076](https://github.com/emqx/emqx/pull/17076) 引入新的路由表同步机制。路由表 schema 版本升级至 `v3`，并向下兼容 `v2`。

  在 schema v3 中，每个节点（核心节点或副本节点）对指向自身的路由表条目拥有完全所有权，其他节点只有只读访问权限。这提升了 EMQX 集群的分区容忍度，分区集群中的对等节点无法代表其他节点修改路由表，同时也降低了副本节点的 `SUBACK` 延迟。

  **向下兼容性：** 当支持 v3 的节点加入仅支持 v2 的集群时，它将继续使用 v2 以保持兼容。要将集群切换至 v3，请在升级后执行完整集群重启。若需阻止自动切换，请将 `broker.routing.storage_schema` 设置为 `v2`。

  **降级说明：** 集群切换至 v3 后，不支持滚动降级。

  查看节点当前路由 schema 版本：

  ```
  emqx eval 'emqx_router:get_schema_vsn()'
  ```

- [#17152](https://github.com/emqx/emqx/pull/17152) 支持为分发端口配置 Erlang inet 端口选项，默认 `buffer` 大小为 1 MB。

  此前，Erlang 分发端口使用极小的默认端口缓冲区（1460 字节，某些平台约为 9 KB），即使分发端口缓冲区（`+zdbbl`）配置了更大的值（如 32 MB），仍会导致性能瓶颈。该问题影响集群通信可靠性，可能表现为 `erpc timeout` 错误、Mnesia 事务拥塞以及多核节点支持下降。

#### 可观测性

- [#16911](https://github.com/emqx/emqx/pull/16911) 通过避免对 Mria 统计数据进行意外的重复查询，降低 Prometheus 指标采集开销。

- [#16916](https://github.com/emqx/emqx/pull/16916) `emqx_cert_expiry_at` Prometheus 指标现在会考虑 MQTT 监听器中使用的托管证书包内证书的过期日期。

- [#16958](https://github.com/emqx/emqx/pull/16958) 新增专用的 `/api-spec` 端点和 Dashboard API 规范浏览页面，便于查阅 EMQX HTTP API 文档。

  Dashboard 现在提供按标签分组和下钻的 OpenAPI 切片，当 `dashboard.swagger_support` 设置为 `false` 时，这些端点与 Swagger 一同被禁用。新增 `emqx ctl api_keys` CLI 命令，支持从命令行列出、查看、添加、删除、启用和禁用 API 密钥。

- [#17018](https://github.com/emqx/emqx/pull/17018) 减少调用 Prometheus 采集 API 端点时对其他节点的请求次数，使 API 调用返回更快，并降低集群高负载时超时的概率。

  具体而言，`emqx_mria_lag` 指标（副本节点关注）现在每 10 秒定期刷新一次（默认值），而非每次 API 调用时按需刷新。

- [#17162](https://github.com/emqx/emqx/pull/17162) 通过 Prometheus 指标（`emqx_license_max_sessions`、`emqx_license_expiry_at`、`emqx_license_issued_at`）暴露每节点的 License 信息，无需对每个节点执行 CLI 检查即可对集群级 License 一致性进行告警。

  运维人员现在可以通过比较这些指标对集群节点间的 License 不一致进行告警。该实现通过单次 `emqx_license_checker:dump/0` gen_server 调用获取全部三个值，消除了每次 Prometheus 采集时的冗余往返。

- [#17176](https://github.com/emqx/emqx/pull/17176) 新增 `emqx_routes_count` 和 `emqx_routes_max` Prometheus 指标，用于导出每个节点的路由表条目数量。

- [#17329](https://github.com/emqx/emqx/pull/17329) 在 `/api/v5/prometheus/stats` 端点新增两个节点级 gauge 指标：

  - `emqx_vm_uptime_ms`：报告 EMQX 节点运行时间（毫秒）。
  - `emqx_vm_max_fds`：报告节点可用的最大文件描述符数量。

- [#17031](https://github.com/emqx/emqx/pull/17031) 新增 License 用量审计的会话高水位线历史记录功能。

  EMQX 现在记录每日峰值会话数，并保留至少 24 个月的历史数据。运维人员可通过 `emqx ctl license history` 命令查询这些数据，支持可选的 `--period daily|monthly` 和 `--json` 参数。新增 `license.high_watermark_timezone` 配置项，用于控制分桶时的日期边界。

#### 访问控制

- [#16849](https://github.com/emqx/emqx/pull/16849) 为插件 API 端点新增基于 Cookie 的认证回退机制。

  由 Dashboard 嵌入的插件 UI iframe 在没有 `Authorization` 头部时，现在可以通过 `emqx_auth` cookie 进行认证。此功能仅适用于 `/api/v5/plugin_api/...` 路径。

- [#16942](https://github.com/emqx/emqx/pull/16942) [#17235](https://github.com/emqx/emqx/pull/17235) 为 API 密钥和 Dashboard 登录用户引入细粒度的基于 Scope 的访问控制。

  API 密钥现在可以使用源自 OpenAPI 标签的 scope 限制到特定 API 路径类别。没有 scope 的密钥保留完整访问权限（向后兼容）。scope 列表为空时拒绝所有受 scope 保护的 API 路径。`publisher` API 密钥角色现在仅限于 `[publish]` scope。

  Dashboard 登录用户现在也支持可选的 `scopes` 字段；设置后，请求将在现有基于角色的检查之上，与 API 密钥所用的路径到 scope 目录进行授权。四个新 scope（`user_management`、`mfa_management`、`sso_management`、`api_key_management`）覆盖仅限 Dashboard 的端点，除 `mfa_management`（任何角色均可持有以豁免强制 MFA）外，其余仅限管理员持有。API 密钥不能持有四个登录专用 scope 中的任何一个。两项检查均适用于 HTTP API 和引导文件加载（不兼容的 scope 会被丢弃并记录警告）。

  新增公开目录端点供 UI 使用：`GET /api_key_scopes` 和 `GET /user_scopes`，任何 bearer 认证调用方均可访问。`scopes` 字段也在 `GET /users`、`POST /users` 和 `PUT /users/:username` 响应中展示；未显式设置时，响应将投影角色默认的 scope 列表。

  新 scope 模型带来的其他行为变更：

  - `dashboard.default_username` 用户被保护为紧急访问账户，不可删除、不可降级（取消管理员角色）、不可设置 `scopes` 字段，仅允许修改其 `description`。这确保运维人员在其他管理员丢失或错误配置 scope 时始终保留管理员访问权限。
  - 用户对自身记录的自助服务操作现在受 scope 约束。只有专用的修改密码和 MFA 自助端点仍绕过 scope 检查；其他操作（如 `PUT /users/:self`）受用户的 scope 约束。
  - `PUT /users/:username` 和 `PUT /api_key/:name` 在请求体省略 `scopes` 字段时，会根据持久化的有效 scope 验证角色变更。若持久化 scope 与新角色不兼容，则拒绝降级用户或变更 API 密钥角色。
  - API 密钥引导文件支持可选的第四列 scope（`key:secret:role:scopes`）。未知或与角色不兼容的 scope 名称会被丢弃并记录警告，而非拒绝整个文件，因此现有的三列引导文件仍可正常加载。
  - SAML SP 元数据端点（`GET /sso/saml/metadata`）现在无需认证即可访问，与 `/sso/saml/acs` 保持一致。

- [#16943](https://github.com/emqx/emqx/pull/16943) 为 SSO（OIDC/SAML/LDAP）新增每后端 `force_mfa` 选项。

  启用后，无论身份提供商侧的 MFA 设置如何，SSO 用户在获得 Dashboard token 之前必须完成 TOTP MFA 设置或验证。支持三种 MFA 状态：`not_configured`（强制设置）、`enabled`（要求验证）和 `admin_disabled`（跳过 MFA）。新增 API 端点 `POST /sso/mfa/setup` 和 `POST /sso/mfa/verify` 处理 MFA 流程。

  管理员可以通过对 `/users/:username/mfa` 执行 DELETE/POST 操作对现有用户进行豁免或强制要求，该操作优先于实时后端策略，直到管理员再次修改。在 `force_mfa = true` 后端上禁用了自身 MFA 的 SSO 用户，下次登录时须重新设置 MFA；只有管理员发起的禁用操作才能豁免用户不受实时策略约束。

- [#17178](https://github.com/emqx/emqx/pull/17178) `emqx ctl api_keys add` CLI 命令现在支持 `--scopes <scope1,scope2,...>` 选项，与 REST API 已支持的基于 scope 的权限控制保持一致。

- [#17218](https://github.com/emqx/emqx/pull/17218) 新增 ACME 客户端插件（`emqx_acme`），可从任何符合 RFC 8555 的 ACME CA（如 Let's Encrypt）为 EMQX 托管证书包签发和续签 TLS 证书，并将配置的 SSL/WSS 和/或 Dashboard HTTPS 监听器切换为使用该证书包。

#### 多租户

- [#17053](https://github.com/emqx/emqx/pull/17053) 新增多租户配置选项 `multi_tenancy.post_auth_tns_expression`。

  配置后，它是一个在认证链完成后求值的 [Variform](https://docs.emqx.com/en/emqx/latest/configuration/configuration.html#variform-expressions) 表达式，渲染结果写入 `client_attrs.tns`（多租户配额和路由决策使用的租户命名空间键）。

  这使运维人员能够从认证响应属性（例如 HTTP 认证后端返回的 `tag` 字段）派生租户命名空间，而不必仅依赖认证前的 `mqtt.client_attrs_init`。示例表达式：`client_attrs.tag`，或带回退的写法 `coalesce(client_attrs.tag, username)`。

  表达式为空时（默认值），行为不变。

- [#17078](https://github.com/emqx/emqx/pull/17078) 在 `GET /api/v5/mt/managed_ns_list_details` 的响应中内联每个托管命名空间的配置（会话和限速器），使管理 UI 可以通过单次请求渲染命名空间列表及其配置，而无需为每个命名空间额外发起一次请求。

#### 网关

- [#17013](https://github.com/emqx/emqx/pull/17013) 为 GBT32960 网关新增 GBT32960-2025 协议支持。

  网关现在通过帧头（`##` 对应 2016 版，`$$` 对应 2025 版）自动检测协议版本，并处理版本相关的解析和序列化，包括：

  - 2025 版新信息类型：整车、驱动电机、燃料电池、发动机、位置、报警、动力电池电压/温度、燃料电池堆、超级电容、超级电容极值和数字签名。
  - 新命令：激活（0x09/0x0A）。
  - 参数查询/设置（0x02/0x03）中版本相关的参数大小（2025 版为 BYTE，2016 版为 WORD）。
  - 2025 版整车登录含 BMS 电池包编码字段。

#### 数据集成

- [#16929](https://github.com/emqx/emqx/pull/16929) 引入两种新限速器类型：`delivery_messages` 和 `delivery_bytes`。与现有的 `messages` 和 `bytes` 限速器（限制单个客户端发布的消息）不同，新限速器对单个客户端从任意来源接收的消息进行限速。达到限制时，QoS 0 消息被丢弃，QoS > 0 消息在内部排队并安排重试，重试时间根据限速器配置推算。

  新限速器仅支持内存会话（`durable_sessions.enable = false`）。

  未指定时默认值为不限速，保持向后兼容性。

- [#16962](https://github.com/emqx/emqx/pull/16962) 改进 Kafka 消费者的轮询行为：在没有可用记录时，Fetch 请求现在会短暂等待数据，而非立即返回空批次，从而减少不必要的轮询延迟，帮助 Kafka 消费者更稳定地接收新记录。

- [#17011](https://github.com/emqx/emqx/pull/17011) 为 EMQX Tables（Rust NIF 驱动）连接器新增 `ts_column` 和 `ttl` 配置字段。

  - `ts_column`：为自动创建的表指定自定义时间戳列名（未设置时默认为 `ts`）。
  - `ttl`：为自动创建的表设置存活时间提示（如 `3 days`）。

  这两个字段在底层 `greptimedb-ingester-erlnif` 驱动（0.1.8 起）中已受支持，现在在 EMQX Tables 连接器配置中正式暴露。

- [#17025](https://github.com/emqx/emqx/pull/17025) 更改了 InfluxDB 数据库执行健康检查和凭据验证的方式。

  不再通过执行 `SHOW DATABASES` 进行检查，该操作可能被某些审计系统误报为系统渗透。

  另请参阅 [emqx/influxdb-client-erl#54](https://github.com/emqx/influxdb-client-erl/pull/54)。

- [#17046](https://github.com/emqx/emqx/pull/17046) 新增 `actions.messages` 指标（及 Dashboard 监控 API 中对应的 `actions_messages_rate`），统计规则引擎动作执行处理的消息总数。

  由于单次动作执行可能处理一批消息，`actions.messages` 大于或等于 `actions.executed`，`actions_messages_rate` 反映动作的真实每消息吞吐量。

- [#17089](https://github.com/emqx/emqx/pull/17089) MQTT 入口 Bridge 现在支持在远端 Broker 支持 MQTT 5 订阅标识符时，从以 `$queue/{name}/{bind-filter}` 形式暴露的远端消息队列中消费消息。当订阅标识符不可用时，队列订阅将被拒绝；若远端 Broker 不接受订阅标识符，普通主题订阅会自动重试（不带订阅标识符）。

- [#17104](https://github.com/emqx/emqx/pull/17104) 为聚合上传动作（Azure Blob Storage、Amazon S3、GCS、Snowflake、S3 Tables）的 Blob 名称模板新增日期部分占位符。占位符以聚合开始时间为基准渲染，默认使用 UTC。这支持 Hive 分区对象布局（如 `year=2025/month=04/day=22/hour=07/...`），可直接供 Spark、Databricks 和 Synapse 使用。

  支持的占位符：

  - `${datetime.YYYY}`
  - `${datetime.MM}`
  - `${datetime.DD}`
  - `${datetime.hh}`
  - `${datetime.mm}`
  - `${datetime.ss}`
  - `${datetime.DOY}`（年中第几天）

  每个占位符可添加显式时区前缀：

  - `utc`（默认）：如 `${datetime.utc.YYYY}`
  - `local`（EMQX 节点的系统时区）：如 `${datetime.local.YYYY}`

- [#17120](https://github.com/emqx/emqx/pull/17120) 为 `GET /clients_v2` 新增查询字符串过滤选项 `node`。指定后，将返回连接到该节点的在线客户端，以及上次连接到该节点的离线客户端。

- [#17136](https://github.com/emqx/emqx/pull/17136) 为 InfluxDB 连接器新增 `ping_with_auth` 选项。启用后，健康检查将包含配置的凭据，适用于要求认证健康检查请求的 InfluxDB 兼容服务。同时修复了 InfluxDB 连接器/动作在从 `write_syntax` 字面量或 MQTT 载荷写入值时的 Unicode 文本保留问题。

- [#17165](https://github.com/emqx/emqx/pull/17165) 为动作新增 `resource_opts.dispatch_strategy` 选项。

  新选项默认为 `per_clientid`，保持此前的缓冲工作器分发行为。设置为 `random` 时，没有显式 `pick_key` 的查询将使用随机分发键，有助于在少量客户端发布大量消息时将流量分散到多个缓冲工作器。

- [#17170](https://github.com/emqx/emqx/pull/17170) [#17282](https://github.com/emqx/emqx/pull/17282) [#17297](https://github.com/emqx/emqx/pull/17297) 为 MQTT Bridge 连接器和集群连接配置新增 `tcp_opts`（`nodelay`、`sndbuf`、`recbuf`、`buffer`、`keepalive`、`delay_send`、`active_n`），支持为每个连接调整出站 MQTT 客户端 TCP socket 参数。未设置的字段保持操作系统/`gen_tcp` 默认值。`delay_send`（默认关闭）合并小写入以提升吞吐量，代价是轻微的延迟增加。

- [#17245](https://github.com/emqx/emqx/pull/17245) 在 Dashboard 中为 MQTT Disk-Queue Bridge 插件的配置 UI 新增中文和英文翻译。

#### 集群连接

- [#17221](https://github.com/emqx/emqx/pull/17221) 改进集群连接中 MQTT 消息转发的诊断信息。

  当消息转发连接出现连通性问题时，链路资源状态和相应告警现在会包含断开原因，使配置问题更易于识别。

#### 部署

- [#17079](https://github.com/emqx/emqx/pull/17079) 在 Helm chart 中新增 `service.wsEnabled` 选项，当 MQTT WebSocket 监听器被禁用时可省略 Service 中的 ws/wss 端口条目。默认值为 `true` 以保持现有行为。

### 修复

#### 核心 MQTT 功能

- [#16779](https://github.com/emqx/emqx/pull/16779) 改进了格式错误的首个 CONNECT 报文的处理方式，将其归类为无效 CONNECT 报文，并在日志中增加更好的协议提示。

- [#16781](https://github.com/emqx/emqx/pull/16781) 修复了保留消息不可用时的 CONNECT 验证。

  当 `mqtt.retain_available` 设置为 `false` 时，带有 Will Retain 标志的 CONNECT 包现在会被正确拒绝，并返回 CONNACK 原因码 `Retain not supported (0x9A)`。

- [#16783](https://github.com/emqx/emqx/pull/16783) 修复了 MQTT v5 SUBSCRIBE 验证中 `Subscription-Identifier` 上限的问题。

  EMQX 现在接受 `268435455`（0x0FFFFFFF），即 MQTT 规范定义的最大有效订阅标识符值。

- [#16847](https://github.com/emqx/emqx/pull/16847) 修复了在消息转换表达式中使用非 ASCII Unicode 字符串时发生崩溃的问题。

- [#16874](https://github.com/emqx/emqx/pull/16874) 修复了使用 DS Raft 支撑的持久化存储在集群领导权快速连续变更后偶发停止接受新消息的问题，该问题需要重启节点才能恢复。

- [#16876](https://github.com/emqx/emqx/pull/16876) 将日志消息 `msg_publish_not_allowed` 更名为 `msg_not_routed_to_subscribers`。

- [#16974](https://github.com/emqx/emqx/pull/16974) 修复了 EMQX 6.1.1 中的一个问题：当会话订阅了包含保留消息的主题过滤器后，若在未重新订阅该主题过滤器的情况下进行接管或恢复，会话会再次收到已接收的消息。现在恢复了之前的行为，即在不显式重新订阅的情况下恢复或接管会话时，保留消息迭代将停止。

- [#17139](https://github.com/emqx/emqx/pull/17139) 恢复 `retainer.enable` 作为保留消息子系统的真实运行时开关。

  这使部署可以在保持 MQTT 保留消息协议支持的同时禁用保留消息存储，而无需依赖 `mqtt.retain_available`（后者会在协议层拒绝保留发布）。

- [#17172](https://github.com/emqx/emqx/pull/17172) 修复了客户端在断开连接前发送的 MQTT 包（如 PUBACK）可能丢失的问题（当连接进程邮箱中有待处理的出站消息时）。现在连接进程会在关闭前正确清空邮箱，确保入站包在 socket 关闭后也能被处理。

- [#17175](https://github.com/emqx/emqx/pull/17175) 修复了从 Stream 分发的消息未应用订阅选项（如来自 stream 订阅的订阅标识符）的问题。

- [#17353](https://github.com/emqx/emqx/pull/17353) 修复了 `socket` TCP 后端中，当客户端连接反复遭遇发送拥塞时，出站 MQTT 包可能以错误顺序发送的问题。该场景在实际中极少发生。

- [#17383](https://github.com/emqx/emqx/pull/17383) 会话接管后，Dashboard 和 REST API 反映的 channel 信息（`mqueue_len`、`inflight_cnt`）现在在接管重放完成后立即更新，而不再等待下一次 15 秒的统计刷新周期。

#### 规则引擎

- [#16699](https://github.com/emqx/emqx/pull/16699) 修复了在某些竞争条件下可能打印如下冗长错误日志的问题：

  ```
  2026-02-03T13:53:54.576326+00:00 [error] Generic server <0.11323236.0> terminating. Reason: {{badkey,'actions.success'},[{erlang,map_get,['actions.success',#{}],[{error_info,#{module => erl_erts_errors}}]},{emqx_metrics_worker,idx_metric,4,[{file,"emqx_metrics_worker.erl"},{line,683}]},{emqx_metrics_worker,inc,4,[{file,"emqx_metrics_worker.erl"},{line,322}]},{emqx_rule_runtime,do_eval_action_reply_t...
  ```

  现在 EMQX 会打印更有意义的信息以辅助问题排查。

- [#16780](https://github.com/emqx/emqx/pull/16780) 修复了授权源验证中缺少 `type` 字段的请求可能触发内部错误的问题。

  现在 EMQX 对此类情况返回清晰的 `BAD_REQUEST` 验证错误。

- [#16796](https://github.com/emqx/emqx/pull/16796) 修复了连接器动作中多行 SQL 语句的处理问题。

- [#17211](https://github.com/emqx/emqx/pull/17211) 在 `$events/client/connack` 规则事件中补充了 `connected_at` 字段，该字段在文档中有说明但此前在实际数据中缺失。

#### 数据集成

- [#16936](https://github.com/emqx/emqx/pull/16936) 修复了 Azure Blob Storage 动作在聚合模式下，当容器包含过多 blob 时健康检查可能超时的问题。

- [#16955](https://github.com/emqx/emqx/pull/16955) 消除了 Kafka 生产者动作的误报健康检查警告日志。

  此前，若 Kafka 生产者长时间空闲，Kafka 可能关闭连接（默认通常为 10 分钟），若此时恰好执行 Kafka 生产者动作的健康检查，可能出现 "not_all_kafka_partitions_connected" 误报警告信息。

- [#16972](https://github.com/emqx/emqx/pull/16972) 修复了 HTTP 和 GCP PubSub 动作，将原因为 `closing` 的瞬时连接错误视为可恢复错误，减少日志噪声。

- [#17001](https://github.com/emqx/emqx/pull/17001) 修复了当远端 Broker 启用消息队列（mq）功能时，MQTT Source 无法从 `$queue/` 订阅接收消息的问题。

  根本原因是 MQ 消息分发在 PUBLISH 包中未包含 MQTT v5 订阅标识符属性，而 MQTT Bridge 入口依赖该属性从队列订阅路由消息。

- [#17068](https://github.com/emqx/emqx/pull/17068) 修复了当 `ssl.verify` 为 `verify_none` 且证书文件路径留空时，EMQX Tables TLS 连接器无法启动的问题，并对齐了 Rust NIF TLS verify 与连接器配置的传播行为。

- [#17084](https://github.com/emqx/emqx/pull/17084) 修复了 MQTT Source 的一个问题：若其连接器使用 `clean_start = false` 并重新连接到含有消息会话的 Broker，这些消息不会触发规则动作。

- [#17111](https://github.com/emqx/emqx/pull/17111) 修复了 PostgreSQL 连接器在禁用 prepared statements 模式下的查询执行问题。此前，并发查询可能相互交错并产生错误。

- [#17113](https://github.com/emqx/emqx/pull/17113) 修复了 RocketMQ 连接器隔离问题：配置错误或不可达的 RocketMQ 连接器不再影响同节点的其他 RocketMQ 连接器。此前，一个连接到不可达 Broker 的连接器可能导致共享客户端 supervisor 最多阻塞 60 秒，使同级连接器因 `resource_health_check_timed_out` 而反复抖动，Dashboard 对这些连接器的操作也会挂起。

  默认 TCP/TLS 连接超时也从 60 秒降至 10 秒，使配置错误的服务器快速显示为失败状态，而不是看起来卡住。

- [#17180](https://github.com/emqx/emqx/pull/17180) 修复了在高负载下对 MongoDB 进程调用超时会被当作不可恢复错误而不进行重试的问题。现在此类事件发生时会进行重试。

- [#17216](https://github.com/emqx/emqx/pull/17216) 修复了 Timescale/PostgreSQL 动作在将带引号的 JSON 数字字符串映射到 `FLOAT` 列时，会崩溃数据库连接进程而非报告结构化错误参数的问题。

- [#17250](https://github.com/emqx/emqx/pull/17250) 修复了 Redis Sentinel 连接器，支持对 Redis 数据节点和 Sentinel 节点分别配置认证。

- [#17293](https://github.com/emqx/emqx/pull/17293) 修复了写入 Parquet 文件时，对象包含必填键但值为 `undefined`/`null` 时写入损坏文件而非抛出错误的问题。

- [#17303](https://github.com/emqx/emqx/pull/17303) 升级 Kafka 客户端库：`brod` 从 4.5.2 升级至 4.5.4，`wolff` 从 4.1.9 升级至 4.1.10。

  上游修复的主要问题：

  - `brod`：修复 Kafka 连接重新认证时的竞争条件（via `kafka_protocol` 4.3.4）。
  - `wolff`：在高内存负载控制（`drop_if_highmem`）下保留最小缓冲区，防止生产者耗尽飞行中数据；仅丢弃超出保留量的字节。

- [#17343](https://github.com/emqx/emqx/pull/17343) 修复了集群配置复制的一个问题：导入包含 `file` 类型授权源的数据备份（或通过 `emqx ctl conf load`/`PUT /api/v5/configs` 加载 HOCON 配置）时，可能导致对等节点出现 `cluster_rpc_apply_failed`/`failed_to_read_acl_file` 错误并滞后。

  导入程序此前会在本地写入 ACL 文件并将内联 `rules` 替换为 `path`，然后将 path 形式的配置下发到集群。对等节点磁盘上没有该文件，因此无法应用变更。现在发送到集群的配置保持 `rules` 内联，每个对等节点从复制内容中写入自己的 ACL 文件副本。

- [#17347](https://github.com/emqx/emqx/pull/17347) 升级 RocketMQ 客户端依赖至 `v0.7.2`，修复异步生产者请求中的内存增长问题。

- [#17439](https://github.com/emqx/emqx/pull/17439) 修复了 Azure Blob Storage 连接器的健康检查在存储账户包含过多容器时可能超时或产生大量带宽消耗的问题。为 #16935 的配套修复。

- [#17450](https://github.com/emqx/emqx/pull/17450) 修复了使用 `mode=node` 时 `/prometheus/data_integration` Prometheus 端点可能返回 500 状态码的问题。该问题仅在动作和连接器配置被手动编辑且不一致（动作的连接器不存在）时才会出现。

#### 集群

- [#17132](https://github.com/emqx/emqx/pull/17132) 修复了在副本节点上，当其原始配置或运行时状态发生偏移时，添加或删除主题指标可能失败的问题，该问题会触发 `cluster_rpc_apply_failed` 告警并阻塞集群 RPC 复制。重复添加和删除不存在的操作现在仅在发起节点上被拒绝，副本节点以幂等方式应用变更。

- [#17182](https://github.com/emqx/emqx/pull/17182) 升级至 emqx-OTP 27.3.4.2-8（针对 mria）。

  未修复此问题前，EMQX 启动期间若未连接到集群，Mria 应用启动可能卡住。

- [#17214](https://github.com/emqx/emqx/pull/17214) 移除了集群连接消息转发 MQTT 客户端断开事件的晦涩错误级日志，改为包含足够排查上下文的用户友好消息。类似以下的事件不应再出现在错误日志中：

  ```
  2026-05-06T03:00:48.738654+00:00 [error] [PoolWorker] unexpected info: {disconnected,141,#{}}
  ```

- [#17218](https://github.com/emqx/emqx/pull/17218) 避免 `bin/emqx` 和 `bin/emqx_ctl` 调用在运行中的 Broker 上触发 `nodeup`/`nodedown` 事件，这些事件此前在 Broker 日志中表现为误导性的 `cm_registry_node_down` 警告。这些脚本启动的临时辅助节点现在按预期注册为隐藏 Erlang 节点。

- [#17269](https://github.com/emqx/emqx/pull/17269) 改进了网络分区后的集群恢复能力。

  - 此前，连接到副本节点的部分客户端可能从全局注册表中丢失，导致接管时行为不一致以及 Dashboard 显示信息不正确。

    此修复新增了一个后台进程，在网络分区恢复后重新注册现有客户端。同时新增告警："Broker is recovering after a network partition"，在全局注册表重建期间触发。

  - 引入新的集群自愈算法，可自动恢复重叠网络分区。

- [#17342](https://github.com/emqx/emqx/pull/17342) 修复了当导出的 `cluster.hocon` 包含部分 `node` 节时，集群配置导入因 "required_field: node.cookie" schema 检查错误而失败的问题。只读根配置（`node`、`rpc`）本就不属于数据导入范围，现在在预检 schema 校验前从导入配置中删除，使验证使用运行节点自身的值。

- [#17348](https://github.com/emqx/emqx/pull/17348) 修复了当集群节点的有效配置相同但原始配置表示不同时，`emqx ctl conf cluster_sync status` 诊断输出嘈杂且具有误导性的问题。

  该命令现在会抑制不对应已检查配置变更的原始表示差异，同时在已检查配置不一致时仍会发出警告。当某个原始配置键在一个节点存在而在另一个节点缺失时，不再崩溃。

  同时忽略动作、Source、Bridge 和规则元数据中 `created_at` 和 `last_modified_at` 的纯时间戳元数据差异。数据导入或启动时配置加载可能仅在部分节点上刷新这些生成的时间戳，即使有效运行时配置完全相同。

- [#17349](https://github.com/emqx/emqx/pull/17349) 改进了集群连接在路由复制卡在连接到无响应目标集群时的响应速度。现在删除此类集群连接的速度会略有提升。

- [#17382](https://github.com/emqx/emqx/pull/17382) 修复了集群遭遇网络分区时可能发生的全局 channel 注册表损坏问题。

- [#17424](https://github.com/emqx/emqx/pull/17424) 修复了网络分区后 Mnesia 自愈可能导致同一客户端 ID 在全局会话注册表中留下重复或过期条目的泄漏问题。

  Discard 和 takeover-kick RPC 处理程序现在也会在目标进程不再存活时删除注册表行；连接路径上的注册限流现在能识别墓碑行（无本地 channel 状态）并清理它们，而不是无限期阻塞同一客户端 ID 的新连接。

- [#17432](https://github.com/emqx/emqx/pull/17432) 修复了并发集群连接 API 请求可能返回通用错误响应而非成功或未找到的问题。

- [#17469](https://github.com/emqx/emqx/pull/17469) 修复了启用或禁用活动集群连接时出现如下警告的问题：

  ```
  [warning] tag: RESOURCE, msg: handle_resource_metrics_failed, reason: {badkey, matched}, event: matched, ...
  ```

#### 访问控制

- [#16805](https://github.com/emqx/emqx/pull/16805) 新增对 authz hook 结果选择退出授权缓存存储的支持，用于动态 ACL 决策。

- [#17045](https://github.com/emqx/emqx/pull/17045) 修复了基于密码的认证后端在 CONNECT 包不含密码时会立即拒绝连接而非继续认证链的问题。

  此前，若客户端连接时不带密码，认证链中第一个基于密码的认证器（内置数据库、MySQL、PostgreSQL、MongoDB、Redis 或 LDAP）会返回错误，阻止后续认证器被尝试。

- [#17064](https://github.com/emqx/emqx/pull/17064) 修复了 `/authentication/:id/users` REST 端点的授权漏洞，命名空间管理员不再能通过省略 `ns` 查询参数或 `namespace` 请求体字段来列出或创建全局（或其他租户）命名空间中的用户。非全局命名空间的认证用户不再能被标记为 `is_superuser`；创建或更新此类用户的请求将被拒绝，确保始终对租户 MQTT 客户端强制执行显式 ACL 规则。

- [#17100](https://github.com/emqx/emqx/pull/17100) 修复了当身份提供商返回的 JWKS 响应 `Content-Type` 使用 `+json` 结构化语法后缀（如 `application/jwk-set+json; charset=utf-8`）时，OIDC SSO 登录失败并报 `provider_not_ready` 的问题。此类响应现在被接受为有效的 JWKS 内容。

- [#17122](https://github.com/emqx/emqx/pull/17122) 修复了 Dashboard RBAC 对含 URL 编码用户名（如电子邮件地址）的 SSO 用户的权限检查，确保 `force_mfa` 禁用时查看者的 MFA 自助禁用请求能正常工作。

- [#17140](https://github.com/emqx/emqx/pull/17140) 修复了 EMQX 通过 HTTP 获取证书吊销列表（CRL）时，服务器返回 DER 编码内容（`Content-Type: application/pkix-crl`，RFC 5280 §5 规定的格式）时的静默失败问题。

  此前，EMQX 仅解码 PEM 编码的 CRL 内容；DER 内容被静默视为零条 CRL 并缓存为空列表，导致 `enable_crl_check = true` 监听器上的每次 TLS 握手都以 `bad_crls, no_relevant_crls` 失败，且无日志说明原因。

  EMQX 现在同时解码 PEM 和 DER CRL 内容。当获取的内容两者都不是时，记录带有 URL 的警告日志，使配置错误可见。

- [#17171](https://github.com/emqx/emqx/pull/17171) 修复了阻止命名空间 Dashboard 管理员启用或禁用自身账户 MFA 的 RBAC 问题。

  命名空间管理员仍受限于无法管理其他 Dashboard 用户的 MFA 设置。

- [#17177](https://github.com/emqx/emqx/pull/17177) Dashboard 创建的 REST API 密钥现在随机生成，而非从 API 密钥名称派生。

- [#17223](https://github.com/emqx/emqx/pull/17223) 修复了在 SSL 监听器前放置 TCP 透传代理（如 GCP TCP Proxy NLB、AWS NLB）且配置 `proxy_protocol = true` 时，客户端证书丢失的问题。监听器处的 TLS 握手成功完成且客户端证书存在，但未暴露给认证或规则事件。依赖客户端证书（CN、subject、完整 PEM）的函数、ACL 规则和认证后端现在在此部署形态下可正常工作。

- [#17330](https://github.com/emqx/emqx/pull/17330) 加固了启用 `proxy_protocol` 的 TCP 和 SSL 监听器上的 PROXY Protocol v2 TLV 解析器。此前，TLV 声明长度超出缓冲区时会导致解析器静默截断 TLV 流，丢弃后续字段。解析器现在是严格模式：畸形 TLV 流会导致连接被拒绝并记录警告日志，而不是以部分解析的 PROXY 头部接受连接。

- [#17428](https://github.com/emqx/emqx/pull/17428) 修复了当提供商的 `.well-known/openid-configuration` 响应包含 `Cache-Control` 头（如 Kanidm 观察到的 `max-age=0`）时，Dashboard OIDC SSO 崩溃导致 EMQX 无法完成 OpenID 提供商发现的问题。该崩溃会导致 OIDC supervisor 在单次失败后耗尽重启预算，使 SSO 在不重新保存配置的情况下无法恢复。cache-control 解析器现在对这些值更具容错性，worker 不再因过期值错误而硬崩溃，OIDC supervisor 允许在一分钟内多次重启，使瞬时失败能自动重试。

#### 网关

- [#17141](https://github.com/emqx/emqx/pull/17141) 修复了 CoAP 连接模式的 token 接管问题，使重新连接的 UDP/DTLS 客户端可以使用有效 token 恢复，同时拒绝无效的 token/clientid 组合。同时确保在运行 CoAP 接管 connected hook 之前所需的连接信息字段已就绪。

- [#17258](https://github.com/emqx/emqx/pull/17258) 修复了 MQTT-SN 网关中，已连接客户端在同一会话上发送第二个 CONNECT 包会导致连接进程崩溃的问题。网关现在以 DISCONNECT 响应并优雅关闭会话。

- [#17287](https://github.com/emqx/emqx/pull/17287) 修复了 MQTT-SN 客户端因在意外连接或 Will 状态下收到包而崩溃的问题，包括连接设置期间的 `DISCONNECT`、Will 握手完成前的 `REGISTER`，以及 Will topic 不存在时的 `WILLMSGUPD`。

- [#17419](https://github.com/emqx/emqx/pull/17419) 修复了 CoAP 网关 observe 通知未遵守 `gateway.coap.notify_type` 设置的问题。

  Observe 通知现在使用每会话确认性飞行窗口为 1、所有 observe token 共享固定待处理队列（100 条）。当一条确认性通知正在飞行时，后续 observe 通知进入队列而非被静默丢弃。队列满时，最旧的待处理通知被丢弃，`delivery.dropped.queue_full` 递增，并记录经过限流的警告日志。

  取消 observe 关系现在也会移除该 observe token 对应的待处理通知，确保客户端取消 observe 后不会收到已排队的通知（包括通配符 observe 过滤器）。

#### 可观测性

- [#16842](https://github.com/emqx/emqx/pull/16842) 减少了在无对等节点持有插件配置时产生的嘈杂插件配置警告日志。

  此前，节点启动期间从对等节点获取插件配置时，即使所有对等节点仅是尚未拥有该配置（如第一个加载插件的节点），也会记录警告。现在此类无害情况以 debug 级别记录，仅真正的错误（RPC 失败、超时）保留为警告。

- [#16843](https://github.com/emqx/emqx/pull/16843) 修复了 HTTP 头部和查询字符串参数未传递到插件 API 处理程序的问题，该问题导致插件接收到空头部和缺失的查询参数。

- [#16863](https://github.com/emqx/emqx/pull/16863) 当收到已过期请求的异步回复时，新增警告日志。

- [#16868](https://github.com/emqx/emqx/pull/16868) 改进了 REST API 认证错误消息，引导程序化客户端使用 API 密钥（Basic auth）而非反复登录获取 bearer token。错误响应现在会提及 `api_key.bootstrap_file` 配置选项和用于创建持久 API 密钥的 `POST /api_key` 端点。

- [#16879](https://github.com/emqx/emqx/pull/16879) 新增 `log.audit.cache_size` 作为审计日志数据库缓存大小的主要配置键，同时保留 `log.audit.max_filter_size` 以保持向后兼容性。

- [#16890](https://github.com/emqx/emqx/pull/16890) 修复了 ExHook 的一个问题：成功的重连重加载可能在运行列表中重复同一服务器名称并触发重复的回调分发。

- [#16939](https://github.com/emqx/emqx/pull/16939) 修复了内置数据库认证器在配置了默认引导文件路径但文件不存在时记录警告的问题。

- [#16956](https://github.com/emqx/emqx/pull/16956) 当连接终止原因为 `emsgsize`（接收的包超过 `mqtt.max_packet_size`）时，将客户端连接终止日志级别从 info 提升至 warning。

- [#17002](https://github.com/emqx/emqx/pull/17002) 将 `minirest` 库升级至 1.4.12 版本。该版本修复了一个导致 EMQX API 在返回 `204 No Content` 状态行时生成格式错误响应的问题：错误地附带了无效的 `content-length` 响应头。

- [#17024](https://github.com/emqx/emqx/pull/17024) Dashboard HTTP 监听器现在在绑定地址为 IPv6 地址时自动使用 IPv6，无需显式设置 `inet6 = true`。

- [#17054](https://github.com/emqx/emqx/pull/17054) 修复了设置 `Accept: application/json` 时 `GET /api/v5/configs?key=...` 返回不完整数据的问题。

  此前，JSON 响应忽略 `key` 查询参数，始终返回固定的根配置子集，不包含 `multi_tenancy` 等键。该端点现在与 hocon（text/plain）响应一致地处理 JSON 响应中的 `key` 参数。

- [#17118](https://github.com/emqx/emqx/pull/17118) 改进了多租户列表端点的分页功能（`/mt/ns_list`、`/mt/ns_list_details`、`/mt/managed_ns_list`、`/mt/managed_ns_list_details`、`/mt/ns/{ns}/client_list`）：

  - 新增符合 RFC 8288 的 `Link: <?...>; rel="next"` 响应头。当有更多页面时，该头部携带下一页的仅查询 URI 引用；缺失时表示当前响应是最后一页。这消除了此前需要额外请求才能区分整页（`len(results) == limit`）与精确边界"无更多数据"情况的歧义。
  - 在现有排他游标（`last_ns`、`last_clientid`）旁边新增包含性 keyset 游标查询参数（`first_ns`、`first_clientid`）。包含形式支持精确匹配查询（如 `?first_ns=foo&limit=1`），并在调用方选择使用时通过分页 Link 头传递。两种形式在单个请求中互斥；同时提供两者返回 HTTP 400。

- [#17134](https://github.com/emqx/emqx/pull/17134) 修复了禁止客户端列表 API 对 6.2.0 之前创建的客户端 ID 和用户名正则禁令返回 `invalid json term` 错误的问题。旧版本数据库中保留的已编译正则现在在序列化响应时被转换回原始模式字符串。

- [#17227](https://github.com/emqx/emqx/pull/17227) 集群配置文件保存错误现在会指明文件名和底层原因。

  当 `cluster.hocon`（或其目录）为只读、不可变或以其他方式不可写（如挂载为只读的容器）时，通过 Dashboard 或 REST API 修改配置此前会返回不透明的 HTTP 400，内容为 `{config_update_crashed,{badmatch,{error,ebusy}}}`，且只记录未指明文件名的 badmatch 崩溃日志。

  现在，错误将：

  - 记录 `failed_to_save_conf_file`，包含实际文件路径和原因（`eacces`、`eperm`、`ebusy` 等），以及列出常见运维侧原因的提示。
  - 返回结构化的 HTTP 400 响应体，同时指明文件和原因，无需翻阅节点日志即可在 Dashboard 中看到原因。

  此前，当仅临时文件写入失败（如只读目录）时，API 会静默返回 HTTP 200，尽管更改未持久化到磁盘。API 现在在此情况下也会正确报告失败。

- [#17246](https://github.com/emqx/emqx/pull/17246) 将 `jose` 库从 1.11.10 升级至 1.11.12，获取针对新版 OTP 的 EC 和 EdDSA 密钥修复。

- [#17247](https://github.com/emqx/emqx/pull/17247) 当插件的 REST API 回调崩溃或超出超时预算时，Broker 现在会将失败的 API 方法和路径连同配置的超时时间一并记录，使混合流量日志中的问题调用可被识别。超时记录为警告（非错误），并包含指向 `plugins.api_endpoint.timeout`（在插件回调合理需要更多时间时可调整的配置键）的提示。

- [#17254](https://github.com/emqx/emqx/pull/17254) 改进了容器内的内存使用报告。Broker 现在从 cgroup v2、cgroup v1 和宿主机 `/proc/meminfo` 中选取约束最严格的内存读数（最小非零总量优先，使用率更大的在并列时胜出）。此前报告可能在两种情况下产生误导：在具有严格 cgroup 限制的容器中，宿主机视图可能显示 >70% 而 cgroup 限制实际 <10%（或反之）；在未设置内存限制的 cgroup 挂载下，cgroup 读数可能将报告的使用率压缩至约 0%。过载保护阈值和"已用内存"指标现在反映实际约束进程的限制。

- [#17319](https://github.com/emqx/emqx/pull/17319) `GET /api/v5/schemas/{hotconf,actions,connectors}` 现在以 `Content-Type: application/json` 返回响应。此前响应体是有效 JSON，但头部为 `text/plain; charset=utf-8`，导致按响应内容类型分发的客户端出错。

- [#17406](https://github.com/emqx/emqx/pull/17406) 现在，由命名空间管理员发起的追踪所捕获的事件，对于主题、IP 地址和客户端 ID 类型的追踪，将限制在该管理员的命名空间内。规则 ID 类型的追踪此前已有此行为。

- [#17473](https://github.com/emqx/emqx/pull/17473) 当插件的 Erlang 应用因其他运行中的应用仍依赖它而无法停止时，将 `unabled_to_stop_plugin_apps` 的日志级别从 warning 降至 info。这是插件卸载时的预期、无需操作的情况，不应再触发警告。

#### 插件

- [#16904](https://github.com/emqx/emqx/pull/16904) 阻止同一插件的多个版本同时启用或启动。启用新版本时，旧版本的已配置插件会自动禁用，管理 API 操作现在返回清晰的错误，而不是在另一个版本仍活跃时报告成功。

#### 部署

- [#16901](https://github.com/emqx/emqx/pull/16901) 修复了 RHEL 9.6 LTS 的 RPM 包 OpenSSL 依赖问题：RHEL >= 9.7 固定为 `openssl >= 3.5.1`，旧版 RHEL 9 固定为 `openssl >= 3.0.7`。
- [#17311](https://github.com/emqx/emqx/pull/17311) 修复了容器主机名无法解析时 Docker 启动失败的问题。入口点现在在自动生成节点名称前回退到网络接口 IP 地址，若无法确定节点主机则以清晰的错误信息退出。

- [#17369](https://github.com/emqx/emqx/pull/17369) 将 Dashboard 监听器默认值（`http.bind` 和占位符 HTTPS `ssl_options`）从用户可编辑的 `etc/emqx.conf` 迁移至随附的 `etc/base.hocon`。此前，硬编码的 `emqx.conf` 块会在重启时静默将运行时更新回滚为默认自签名证书。现在，通过 Dashboard、REST API 或 `emqx_acme` 插件自动 HTTPS 配置所做的运行时更新可在重启后正确保留。

- [#17504](https://github.com/emqx/emqx/pull/17504) 修复了 `bin/emqx` 在命令行宽度超过终端宽度时无法检测到运行中节点的问题。进程发现调用由 `ps -ef` 改为 `ps -efww`，防止长 `-root <path>` 参数被截断，确保运行中的 EMQX 进程能被可靠匹配。

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

## 6.0.3

*发布日期: 2026-06-17*

在升级到 EMQX 6.0.3 之前，请务必查阅不兼容变更和已知问题。

### 增强

#### 安全加固

- [#17040](https://github.com/emqx/emqx/pull/17040) 限制 API 密钥访问 Dashboard 用户账户管理端点。

  此前，具有 `administrator` 角色的 API 密钥可通过 HTTP Basic 认证调用 Dashboard 用户管理端点 `POST/DELETE /users/:username/mfa` 和 `POST /users/:username/change_pwd`，这意味着 API 密钥可以重置或禁用其他 Dashboard 用户的 MFA，或修改其密码，绕过了人工 Dashboard 会话与机器 API 密钥之间的预期隔离。

  这些接口现在在通过 API 密钥访问时返回 `401 API_KEY_NOT_ALLOW`，与已阻止 API 密钥访问 `/users`、`/users/:username`、`/logout` 和 `/api_key` 的现有策略保持一致。Dashboard 用户仍可通过 Bearer Token（JWT）会话在 Dashboard UI 中管理自己的 MFA 和密码。

- [#17065](https://github.com/emqx/emqx/pull/17065) 为规则引擎可访问的连接器和 Bridge 配置添加 SSRF 防护。

  当 `rule_engine.ssrf.enable` 设置为 `true` 时，EMQX 对连接器、Bridge 和动作配置应用出站 SSRF 策略。策略对每个目标的评估流程如下：`rule_engine.ssrf.deny_hosts` 中的精确匹配项立即被拒绝；解析后的目标 IP 先经 `rule_engine.ssrf.allow_cidrs` 检查，再经 `rule_engine.ssrf.deny_cidrs` 检查。默认拒绝范围涵盖回环地址、链路本地地址（包括云实例元数据端点）、RFC 1918、ULA、未指定地址和多播地址。检查在配置更新时执行，覆盖所有连接器类型的 HTTP `url` 字段及 `server`、`servers`、`bootstrap_hosts` 字段。

  该功能默认禁用，以保持与连接器合法指向内部服务的部署的兼容性。建议在多租户或对外暴露的环境中启用此功能，并配合网络层出站防火墙一同使用。

- [#17173](https://github.com/emqx/emqx/pull/17173) 限制 API 密钥通过数据备份端点导出或导入 Dashboard 账户及 API 密钥。

  使用 API 密钥调用 `POST /data/export` 时，生成的归档文件中将静默省略 `dashboard_users` 和 `api_keys` Mnesia 表集。使用 API 密钥调用 `POST /data/import` 时，若上传的备份包含上述任一表集，将返回 `403 FORBIDDEN`。

  使用 Dashboard bearer-token（登录）调用不受影响，仍可备份和还原完整数据库，包括 Dashboard 用户和 API 密钥。

  此修复关闭了一个权限提升漏洞。现有的 `/users` 和 `/api_key` 端点已拒绝 API 密钥访问 Dashboard 登录凭据和 API 密钥记录，但 API 密钥持有者此前可通过数据备份端点绕过这些限制。

- [#17187](https://github.com/emqx/emqx/pull/17187) 从未经认证的 `GET /status?format=json` 响应中移除 EMQX 发行版本号（`rel_vsn`），避免向未认证调用方泄露 Broker 版本信息。版本信息仍可通过需要认证的节点信息 API 获取。

- [#17201](https://github.com/emqx/emqx/pull/17201) 加强插件安装端点对上传 tarball 中路径穿越的防护，并收紧安装白名单。

  - 安装路径现在拒绝解压任何条目会解析到插件安装目录以外的 tarball。
  - `emqx ctl plugins allow <name-vsn>` 条目在签发后 5 分钟过期，并可通过 `emqx ctl plugins allow <name-vsn> sha256:<HEX>` 固定到软件包的 SHA-256 哈希值。内容与固定哈希值不匹配的上传将被拒绝并返回 `403 Forbidden`。省略可选的 `sha256:` 参数时，保留原有的接受任何名为 `<name-vsn>.tar.gz` 的载荷的行为。
  - 通过 HTTP 插件安装端点（及其封装的 Dashboard 上传）成功安装后，白名单条目会立即在集群范围内撤销，防止同一授权被重复用于不同的 tarball。

- [#17309](https://github.com/emqx/emqx/pull/17309) 对 PROXY Protocol v2 SSL Common Name 和 Subject 字段进行净化处理，防止控制字符被带入客户端身份信息。

  当监听器配置了 `proxy_protocol = true` 时，Broker 现在会拒绝 PROXY Protocol SSL TLV 字节中包含 ASCII 控制字符的连接（与已应用于 MQTT 摄取的 `clientid`、`username` 和 `password` 的字节类检查相同）。这阻止了攻击者控制的字节通过 `${cert_common_name}` 和 `${cert_subject}` 模板到达出站 HTTP 认证、授权或规则引擎头部值。

  HTTP 认证和授权客户端现在也会在渲染后的请求头名称或值包含 CR、LF 或 NUL 字节时拒绝发送请求。

- [#17315](https://github.com/emqx/emqx/pull/17315) 将 MQTT clientid/username/password 的字节类检查扩展至其他填充 `ClientInfo` 和 HTTP 请求模板的字段：

  - `peersni`（TLS 服务器名称指示；也可从 PROXY Protocol v2 的 `authority` TLV 接受）现在在连接摄取边界进行验证。包含控制字符的连接会被拒绝并记录警告日志。
  - 由 `mqtt.client_attrs_init` Variform 表达式生成的客户端属性值，若包含控制字符则被丢弃（并记录警告），从而防止 `${client_attrs.tns}` 等模板将注入字节传播至下游。
  - HTTP 动作/Bridge 连接器渲染头部时，任何渲染后名称或值包含 NUL、CR 或 LF 的头部都会被丢弃。

- [#17330](https://github.com/emqx/emqx/pull/17330) 加固了启用 `proxy_protocol` 的 TCP 和 SSL 监听器上的 PROXY Protocol v2 TLV 解析器。此前，TLV 声明长度超出缓冲区时会导致解析器静默截断 TLV 流，丢弃后续字段。解析器现在是严格模式：畸形 TLV 流会导致连接被拒绝并记录警告日志，而不是以部分解析的 PROXY 头部接受连接。

- [#17440](https://github.com/emqx/emqx/pull/17440) 将 `GET /api/v5/data/files/<filename>`（备份文件下载）限制为全局 Dashboard 管理员。备份归档可能包含 Dashboard 账户（含密码哈希及 MFA/TOTP 状态）和 API 密钥记录，因此 API 密钥调用方、Dashboard 查看者和命名空间管理员不再被允许下载。列出备份目录（`GET /api/v5/data/files`）的权限对之前有访问权限的所有角色保持不变。

- [#17491](https://github.com/emqx/emqx/pull/17491) 修复了网关认证 API、错误路径和调试日志中密码和密钥被暴露的问题。网关认证 API 响应现在在保留原始配置结构的同时对密钥进行脱敏处理。以下日志路径不再打印原始密码或密钥：网关认证失败日志、监听器启动错误日志、ExProto 认证日志、CoAP 令牌必需日志和 LwM2M 无效注册日志。

- [#17501](https://github.com/emqx/emqx/pull/17501) 阻止命名空间 Dashboard 用户跨命名空间读取 MQTT 消息内容。

  - 以下接口对任何非全局调用方返回 `403 FORBIDDEN`，因为它们可能暴露调用方命名空间之外的 MQTT Payload。此前，命名空间用户可以读取或删除其他命名空间产生的消息。

    - `GET /clients/:clientid/mqueue_messages`
    - `GET /clients/:clientid/inflight_messages`
    - `GET|DELETE /mqtt/retainer/messages`
    - `GET|DELETE /mqtt/retainer/message/:topic`
    - `GET /mqtt/delayed/messages`
    - `GET|DELETE /mqtt/delayed/messages/:node/:msgid`
    - `DELETE /mqtt/delayed/messages/:topic`

  - Trace API 现已按命名空间隔离：`GET /trace` 仅列出由调用方命名空间创建的追踪。单个追踪的端点（`/trace/:name`、`/trace/:name/download`、`/trace/:name/log`、`/trace/:name/log_detail`、`/trace/:name/stop`）在追踪属于其他命名空间时返回 `404`，防止调用方发现其他命名空间的追踪记录。批量 `DELETE /trace` 仅限全局管理员使用，命名空间调用方将收到 `403`。命名空间管理员对自己的追踪仍拥有完整权限，包括创建、列出、下载、流式传输、停止和删除。

#### 集群

- [#17076](https://github.com/emqx/emqx/pull/17076) 引入新的路由表同步机制。路由表 schema 版本升级至 `v3`，并向下兼容 `v2`。

  在 schema v3 中，每个节点（核心节点或副本节点）对指向自身的路由表条目拥有完全所有权，其他节点只有只读访问权限。这提升了 EMQX 集群的分区容忍度，分区集群中的对等节点无法代表其他节点修改路由表，同时也降低了副本节点的 `SUBACK` 延迟。

  **向下兼容性：** 当支持 v3 的节点加入仅支持 v2 的集群时，它将继续使用 v2 以保持兼容。要将集群切换至 v3，请在升级后执行完整集群重启。若需阻止自动切换，请将 `broker.routing.storage_schema` 设置为 `v2`。

  **降级说明：** 集群切换至 v3 后，不支持滚动降级。

  查看节点当前路由 schema 版本：

  ```
  emqx eval 'emqx_router:get_schema_vsn()'
  ```

- [#17152](https://github.com/emqx/emqx/pull/17152), [#17181](https://github.com/emqx/emqx/pull/17181) 支持为 Erlang 分布端口配置 inet 端口选项（connect 和 listen），默认 `buffer` 大小为 1 MB。

  此前，Erlang 分布端口使用的默认端口缓冲区极小（1460 字节，部分平台约为 9 KB），即使分布端口缓冲区（`+zdbbl`）已配置为更大的值（例如 32 MB），仍会造成性能瓶颈。这会影响集群通信可靠性，并可能表现为 `erpc timeout` 错误、Mnesia 事务拥塞以及多核节点支持退化。

- [#17221](https://github.com/emqx/emqx/pull/17221) 改进集群连接中 MQTT 消息转发的诊断信息。

  当消息转发连接出现连通性问题时，链路资源状态和相应告警现在会包含断开原因，使配置问题更易于识别。

- [#17530](https://github.com/emqx/emqx/pull/17530) 集群连接现在需要非社区版 License。在默认社区版 License 下，已配置的连接会保持非活动状态（不转发消息，也不复制路由），REST API 在尝试启用连接时会拒绝请求，并明确提示需要加载非社区版 License。禁用和删除连接仍可使用，以便清理遗留配置。升级 License 后，可通过 Dashboard 或 REST API 启用连接，无需重启节点。

#### 可观测性

- [#16656](https://github.com/emqx/emqx/pull/16656) 系统监控报告（如 `busy_port` 和 `long_schedule`）现在会包含进程标签，便于故障排查。

- [#16744](https://github.com/emqx/emqx/pull/16744) 支持对通过 HTTP API 发布的消息进行端到端追踪。

- [#16757](https://github.com/emqx/emqx/pull/16757) 默认将 `os_mon` 设置为仅收集系统级内存统计信息，从而降低逐进程内存扫描开销。

- [#16911](https://github.com/emqx/emqx/pull/16911) 通过避免对 Mria 统计数据进行意外的重复查询，降低 Prometheus 指标采集开销。

- [#17018](https://github.com/emqx/emqx/pull/17018) 减少调用 Prometheus 采集 API 端点时对其他节点的请求次数，使 API 调用返回更快，并降低集群高负载时超时的概率。

  具体而言，`emqx_mria_lag` 指标（副本节点关注）现在每 10 秒定期刷新一次（默认值），而非每次 API 调用时按需刷新。

- [#17031](https://github.com/emqx/emqx/pull/17031) 新增 License 用量审计的会话高水位线历史记录功能。

  EMQX 现在记录每日峰值会话数，并保留至少 24 个月的历史数据。运维人员可通过 `emqx ctl license history` 命令查询这些数据，支持可选的 `--period daily|monthly` 和 `--json` 参数。新增 `license.high_watermark_timezone` 配置项，用于控制分桶时的日期边界。

- [#17162](https://github.com/emqx/emqx/pull/17162) 通过 Prometheus 指标（`emqx_license_max_sessions`、`emqx_license_expiry_at`、`emqx_license_issued_at`）暴露每节点的 License 信息，无需对每个节点执行 CLI 检查即可对集群级 License 一致性进行告警。

  运维人员现在可以通过比较这些指标对集群节点间的 License 不一致进行告警。该实现通过单次 `emqx_license_checker:dump/0` gen_server 调用获取全部三个值，消除了每次 Prometheus 采集时的冗余往返。

- [#17176](https://github.com/emqx/emqx/pull/17176) 新增 `emqx_routes_count` 和 `emqx_routes_max` Prometheus 指标，用于导出每个节点的路由表条目数量。

- [#17329](https://github.com/emqx/emqx/pull/17329) 在 `/api/v5/prometheus/stats` 端点新增两个节点级 gauge 指标：

  - `emqx_vm_uptime_ms`：报告 EMQX 节点运行时间（毫秒）。
  - `emqx_vm_max_fds`：报告节点可用的最大文件描述符数量。

- [#17558](https://github.com/emqx/emqx/pull/17558) 在 `GET /monitor_current` HTTP API 中新增两个指标及其对应速率：`rules_matched` 和 `actions_executed`，分别用于跟踪规则匹配数量和动作执行速率（成功 + 失败）。

  同时修复了非批处理模式（`batch_size = 1`）下 `actions.executed` 少计动作调用次数的问题：计数器现在会在每次动作回调调用时递增，不再依赖缓冲区 Worker 的遥测刷新窗口。

#### 访问控制

- [#16741](https://github.com/emqx/emqx/pull/16741) 为 SAML SSO 后端新增配置项 `idp_signs_envelopes` 和 `idp_signs_assertions`，用于控制签名验证行为。

  此前，SAML 签名验证无法正常工作，因为未从元数据中提取 IdP 证书指纹并传递给 esaml 进行验证。

  这两个选项默认值均为 `false`，以保持与现有配置的向后兼容性。若用户希望启用签名验证，应在 IdP 配置为签署 SAML 响应时显式将其设置为 `true`。

- [#16942](https://github.com/emqx/emqx/pull/16942) [#17235](https://github.com/emqx/emqx/pull/17235) 为 API 密钥和 Dashboard 登录用户引入细粒度的基于 Scope 的访问控制。

  API 密钥现在可以使用源自 OpenAPI 标签的 scope 限制到特定 API 路径类别。没有 scope 的密钥保留完整访问权限（向后兼容）。scope 列表为空时拒绝所有受 scope 保护的 API 路径。`publisher` API 密钥角色现在仅限于 `[publish]` scope。

  Dashboard 登录用户现在也支持可选的 `scopes` 字段；设置后，请求将在现有基于角色的检查之上，与 API 密钥所用的路径到 scope 目录进行授权。四个新 scope（`user_management`、`mfa_management`、`sso_management`、`api_key_management`）覆盖仅限 Dashboard 的端点，除 `mfa_management`（任何角色均可持有以豁免强制 MFA）外，其余仅限管理员持有。API 密钥不能持有四个登录专用 scope 中的任何一个。两项检查均适用于 HTTP API 和引导文件加载（不兼容的 scope 会被丢弃并记录警告）。

  新增公开目录端点供 UI 使用：`GET /api_key_scopes` 和 `GET /user_scopes`，任何 bearer 认证调用方均可访问。`scopes` 字段也在 `GET /users`、`POST /users` 和 `PUT /users/:username` 响应中展示；未显式设置时，响应将投影角色默认的 scope 列表。

  新 scope 模型带来的其他行为变更：

  - `dashboard.default_username` 用户被保护为紧急访问账户，不可删除、不可降级（取消管理员角色）、不可设置 `scopes` 字段，仅允许修改其 `description`。这确保运维人员在其他管理员丢失或错误配置 scope 时始终保留管理员访问权限。
  - 用户对自身记录的自助服务操作现在受 scope 约束。只有专用的修改密码和 MFA 自助端点仍绕过 scope 检查；其他操作（如 `PUT /users/:self`）受用户的 scope 约束。
  - `PUT /users/:username` 和 `PUT /api_key/:name` 在请求体省略 `scopes` 字段时，会根据持久化的有效 scope 验证角色变更。若持久化 scope 与新角色不兼容，则拒绝降级用户或变更 API 密钥角色。
  - API 密钥引导文件支持可选的第四列 scope（`key:secret:role:scopes`）。未知或与角色不兼容的 scope 名称会被丢弃并记录警告，而非拒绝整个文件，因此现有的三列引导文件仍可正常加载。
  - SAML SP 元数据端点（`GET /sso/saml/metadata`）现在无需认证即可访问，与 `/sso/saml/acs` 保持一致。

- [#16943](https://github.com/emqx/emqx/pull/16943), [#17361](https://github.com/emqx/emqx/pull/17361) 为 SSO（LDAP、OIDC 和 SAML）新增按后端配置的 `force_mfa` 强制 MFA 功能。

  启用后，SSO 用户必须先完成 TOTP MFA 设置或验证，才能获得 Dashboard Token，无论 IdP 侧是否已启用 MFA。新增 API 端点 `POST /sso/mfa/setup` 和 `POST /sso/mfa/verify` 用于处理 MFA 流程。

  管理员可通过 `/users/:username/mfa` 上的 `POST` / `DELETE` 单独要求或豁免已有用户，该决定会覆盖实时后端策略，直到管理员再次修改。若 `force_mfa = true` 后端上的 SSO 用户自行禁用 MFA，则下次登录时必须重新设置；只有管理员发起的禁用操作才会使用户豁免实时策略。

- [#17178](https://github.com/emqx/emqx/pull/17178) `emqx ctl api_keys add` CLI 命令现在支持 `--scopes <scope1,scope2,...>` 选项，与 REST API 已支持的基于 scope 的权限控制保持一致。

#### 网关

- [#16736](https://github.com/emqx/emqx/pull/16736) 改进 JT/T 808 网关，包含协议更新、编码支持和消息处理修复。

  - 支持 JT/T 808 2019 协议。
  - 新增 `jt808.frame.parse_unknown_message` 选项，用于透明转发未知消息。
  - 通过新的 `frame.string_encoding` 选项支持 GBK 字符串编码。默认的 `utf8` 模式保持现有透传行为；`gbk` 模式会将设备侧 GBK 编码字符串转换为 UTF-8 后发送到 MQTT，并将 MQTT 侧 UTF-8 字符串转换为 GBK 后发送给设备。该设置适用于车牌号、驾驶员姓名、文本消息、区域名称和客户端参数等字段。无论该设置如何，MQTT Payload 始终使用 UTF-8。
  - 支持在下行消息中使用自定义 `msg_sn`。当下行 MQTT Payload 的 header 中包含 `msg_sn` 时，网关会使用该值，而不是自动生成的通道序列号。
  - 修复 JT/T 808 网关参数设置（0x8103）和查询响应（0x0104）消息中 CAN 总线 ID 参数（0x0110~0x01FF）的处理问题。这些参数应使用 BYTE[8] 数据类型，并在 JSON 中以 base64 编码表示，而不是使用字符串类型。
  - 修复 JT/T 808 0x0702 驾驶员身份信息上报消息的解析。

- [#17013](https://github.com/emqx/emqx/pull/17013) 为 GBT32960 网关新增 GBT32960-2025 协议支持。

  网关现在通过帧头（`##` 对应 2016 版，`$$` 对应 2025 版）自动检测协议版本，并处理版本相关的解析和序列化，包括：

  - 2025 版新信息类型：整车、驱动电机、燃料电池、发动机、位置、报警、动力电池电压/温度、燃料电池堆、超级电容、超级电容极值和数字签名。
  - 新命令：激活（0x09/0x0A）。
  - 参数查询/设置（0x02/0x03）中版本相关的参数大小（2025 版为 BYTE，2016 版为 WORD）。
  - 2025 版整车登录含 BMS 电池包编码字段。

#### 数据集成

- [#16511](https://github.com/emqx/emqx/pull/16511) 数据集成新增对 IoTDB Table Model 的支持。

- [#16962](https://github.com/emqx/emqx/pull/16962) 改进 Kafka 消费者的轮询行为：在没有可用记录时，Fetch 请求现在会短暂等待数据，而非立即返回空批次，从而减少不必要的轮询延迟，帮助 Kafka 消费者更稳定地接收新记录。

- [#17025](https://github.com/emqx/emqx/pull/17025) 更改了 InfluxDB 数据库执行健康检查和凭据验证的方式。

  不再通过执行 `SHOW DATABASES` 进行检查，该操作可能被某些审计系统误报为系统渗透。

  另请参阅 [emqx/influxdb-client-erl#54](https://github.com/emqx/influxdb-client-erl/pull/54)。

- [#17089](https://github.com/emqx/emqx/pull/17089) MQTT 入口 Bridge 现在支持在远端 Broker 支持 MQTT 5 订阅标识符时，从以 `$queue/{name}/{bind-filter}` 形式暴露的远端消息队列中消费消息。当订阅标识符不可用时，队列订阅将被拒绝；若远端 Broker 不接受订阅标识符，普通主题订阅会自动重试（不带订阅标识符）。

- [#17104](https://github.com/emqx/emqx/pull/17104) 为聚合上传动作（Azure Blob Storage、Amazon S3、GCS、Snowflake、S3 Tables）的 Blob 名称模板新增日期部分占位符。占位符以聚合开始时间为基准渲染，默认使用 UTC。这支持 Hive 分区对象布局（如 `year=2025/month=04/day=22/hour=07/...`），可直接供 Spark、Databricks 和 Synapse 使用。

  支持的占位符：

  - `${datetime.YYYY}`
  - `${datetime.MM}`
  - `${datetime.DD}`
  - `${datetime.hh}`
  - `${datetime.mm}`
  - `${datetime.ss}`
  - `${datetime.DOY}`（年中第几天）

  每个占位符可添加显式时区前缀：

  - `utc`（默认）：如 `${datetime.utc.YYYY}`
  - `local`（EMQX 节点的系统时区）：如 `${datetime.local.YYYY}`

- [#17136](https://github.com/emqx/emqx/pull/17136) 为 InfluxDB 连接器新增 `ping_with_auth` 选项。启用后，健康检查将包含配置的凭据，适用于要求认证健康检查请求的 InfluxDB 兼容服务。同时修复了 InfluxDB 连接器/动作在从 `write_syntax` 字面量或 MQTT 载荷写入值时的 Unicode 文本保留问题。

- [#17165](https://github.com/emqx/emqx/pull/17165) 为动作新增 `resource_opts.dispatch_strategy` 选项。

  新选项默认为 `per_clientid`，保持此前的缓冲工作器分发行为。设置为 `random` 时，没有显式 `pick_key` 的查询将使用随机分发键，有助于在少量客户端发布大量消息时将流量分散到多个缓冲工作器。

- [#17170](https://github.com/emqx/emqx/pull/17170) [#17282](https://github.com/emqx/emqx/pull/17282) [#17297](https://github.com/emqx/emqx/pull/17297) 为 MQTT Bridge 连接器和集群连接配置新增 `tcp_opts`（`nodelay`、`sndbuf`、`recbuf`、`buffer`、`keepalive`、`delay_send`、`active_n`），支持为每个连接调整出站 MQTT 客户端 TCP socket 参数。未设置的字段保持操作系统/`gen_tcp` 默认值。`delay_send`（默认关闭）合并小写入以提升吞吐量，代价是轻微的延迟增加。

- [#17474](https://github.com/emqx/emqx/pull/17474) IoTDB REST API 连接器健康检查现在使用有界版本查询，而不是每次检查都列出所有数据库，从而降低健康检查开销。

- [#17481](https://github.com/emqx/emqx/pull/17481) 为 MQTT Bridge 入口（Source）订阅新增 `retain_as_published` 选项。当 Bridge 使用 MQTT 5.0 连接到远端 Broker 且 `retain_as_published = true` 时，转发消息会保留原始 `retain` 标志，而不是清除该标志，从而可以如实转发上游保留消息。默认值为 `false`，以保持现有行为。当 `proto_ver` 为 `v3` 或 `v4` 时，该选项不生效。

  此外，当同时配置 `bridge_mode = true` 和 `proto_ver = v5` 时，连接器现在会输出警告日志，因为旧的 Bridge Mode 标志在 MQTT 5.0 下不起作用；应改为在各个订阅上设置 `retain_as_published`。

- [#17508](https://github.com/emqx/emqx/pull/17508) 为 PostgreSQL 和 TimescaleDB 连接器连接设置 PostgreSQL `application_name` 启动参数为 `emqx`。

  这使 EMQX 数据库会话更容易在 PostgreSQL 日志和 `pg_stat_activity` 等视图中识别。

- [#17594](https://github.com/emqx/emqx/pull/17594) 支持为 Google Cloud Pub/Sub 和 BigQuery 连接器的 `service_account_json` 配置 `file://` 密钥文件，从而可以从外部文件注入服务账号凭证。

#### 插件

- [#16735](https://github.com/emqx/emqx/pull/16735) EMQX 现在支持插件在 `/api/v5/plugin_api/{plugin}/...` 下定义 HTTP API 回调。

  这允许插件作者通过 Dashboard API 服务暴露插件专用 API 端点，并复用一致的认证和 HTTP 错误处理。

- [#16849](https://github.com/emqx/emqx/pull/16849) 为插件 API 端点新增基于 Cookie 的认证回退机制。

  由 Dashboard 嵌入的插件 UI iframe 在没有 `Authorization` 头部时，现在可以通过 `emqx_auth` cookie 进行认证。此功能仅适用于 `/api/v5/plugin_api/...` 路径。

- [#17549](https://github.com/emqx/emqx/pull/17549) 新增 EMQX Backup Sync 插件，通过数据备份 API 定期将选定配置从主集群同步到备集群。该插件支持为对主集群的 HTTPS 调用配置 TLS 选项。

#### REST API

- [#16718](https://github.com/emqx/emqx/pull/16718) 改进 REST API Swagger 规范。

  此前，规范字段的 summary 和 description 混在一起。现在，summary 简短、清晰且不带标点，description 则提供详细说明。

- [#16958](https://github.com/emqx/emqx/pull/16958) 新增专用的 `/api-spec` 端点和 Dashboard API 规范浏览页面，便于查阅 EMQX HTTP API 文档。

  Dashboard 现在提供按标签分组和下钻的 OpenAPI 切片，当 `dashboard.swagger_support` 设置为 `false` 时，这些端点与 Swagger 一同被禁用。新增 `emqx ctl api_keys` CLI 命令，支持从命令行列出、查看、添加、删除、启用和禁用 API 密钥。

#### 部署

- [#17079](https://github.com/emqx/emqx/pull/17079) 在 Helm chart 中新增 `service.wsEnabled` 选项，当 MQTT WebSocket 监听器被禁用时可省略 Service 中的 ws/wss 端口条目。默认值为 `true` 以保持现有行为。

### 修复

#### 核心 MQTT 功能

- [#16651](https://github.com/emqx/emqx/pull/16651) 修复了关闭过程中一个少见的连接进程崩溃问题。该问题通常发生在系统高压场景下，原因是对已关闭的 socket 进行操作。修复前，此竞争条件通常会产生包含 `{badmatch,{ok,{sock_error,closed}...` 的 `error` 级别日志。

- [#16675](https://github.com/emqx/emqx/pull/16675) 修复了会话接管或丢弃场景中 `disconnected_at` 可能晚于 `connected_at` 的时间戳顺序问题。

  此前，`disconnected_at` 记录过晚（在 `ensure_disconnected` 中记录），此时新会话的 `connected_at` 已经设置。这会导致 `disconnected_at > connected_at` 的竞争条件，使外部系统难以跟踪客户端在线状态。

  修复后，在接管开始或收到丢弃请求时立即记录 `disconnected_at`，确保它始终早于新会话的 `connected_at`，从而保证外部在线状态跟踪系统中的时间戳顺序正确。

- [#16684](https://github.com/emqx/emqx/pull/16684) `mqtt.client_attrs_init` 表达式现在可在初始化客户端属性时使用密码，例如将密码传递给 `jwt_value`。

  此前，`client_attrs_init` 在密码加入渲染上下文之前执行，因此依赖密码的表达式无法解析。

- [#16715](https://github.com/emqx/emqx/pull/16715) 修复了保留 `$SYS` 消息（例如 Broker/节点身份主题）未设置过期时间的问题。该问题可能导致 StatefulSet 轮转后，Dashboard 视图中仍显示过期的节点标识。

  现在，新发布的保留 `$SYS` 消息会包含 `Message-Expiry-Interval = 3600`（1 小时）。

  对于此变更前已存在的过期保留 `$SYS` 条目，可通过向对应过期主题发布空保留消息手动清除：

  ```
  emqx eval 'emqx:publish(emqx_message:set_flag(retain, true, emqx_message:make(emqx_sys, <<"$SYS/brokers/emqx@127.0.0.1/sysdescr">>, <<>>))).'
  ```

  请将命令中的主题替换为要删除的过期 `$SYS/...` 主题。

- [#16731](https://github.com/emqx/emqx/pull/16731) 修复了存在共享订阅时 `emqx ctl subscriptions list` 可能崩溃的问题。

  修复前，列出订阅时可能因某些客户端失败而不返回任何输出。

  修复后，`emqx ctl subscriptions list` 可稳定处理普通订阅和共享订阅。

- [#16779](https://github.com/emqx/emqx/pull/16779) 改进了格式错误的首个 CONNECT 报文的处理方式，将其归类为无效 CONNECT 报文，并在日志中增加更好的协议提示。

- [#16781](https://github.com/emqx/emqx/pull/16781) 修复了保留消息不可用时的 CONNECT 验证。

  当 `mqtt.retain_available` 设置为 `false` 时，带有 Will Retain 标志的 CONNECT 包现在会被正确拒绝，并返回 CONNACK 原因码 `Retain not supported (0x9A)`。

- [#16782](https://github.com/emqx/emqx/pull/16782) 修复 MQTT v5 对非法 PUBLISH 属性的协议处理。

  如果客户端发送的 PUBLISH 报文包含 `Subscription-Identifier`，EMQX 现在会将其视为协议错误并断开该客户端连接。

- [#16783](https://github.com/emqx/emqx/pull/16783) 修复了 MQTT v5 SUBSCRIBE 验证中 `Subscription-Identifier` 上限的问题。

  EMQX 现在接受 `268435455`（0x0FFFFFFF），即 MQTT 规范定义的最大有效订阅标识符值。

- [#16956](https://github.com/emqx/emqx/pull/16956) 当连接终止原因为 `emsgsize`（接收的包超过 `mqtt.max_packet_size`）时，将客户端连接终止日志级别从 info 提升至 warning。

- [#17139](https://github.com/emqx/emqx/pull/17139) 恢复 `retainer.enable` 作为保留消息子系统的真实运行时开关。

  这使部署可以在保持 MQTT 保留消息协议支持的同时禁用保留消息存储，而无需依赖 `mqtt.retain_available`（后者会在协议层拒绝保留发布）。

- [#17172](https://github.com/emqx/emqx/pull/17172) 修复了客户端在断开连接前发送的 MQTT 包（如 PUBACK）可能丢失的问题（当连接进程邮箱中有待处理的出站消息时）。现在连接进程会在关闭前正确清空邮箱，确保入站包在 socket 关闭后也能被处理。

- [#17353](https://github.com/emqx/emqx/pull/17353) 修复了 `socket` TCP 后端中，当客户端连接反复遭遇发送拥塞时，出站 MQTT 包可能以错误顺序发送的问题。该场景在实际中极少发生。

- [#17383](https://github.com/emqx/emqx/pull/17383) 会话接管后，Dashboard 和 REST API 反映的 channel 信息（`mqueue_len`、`inflight_cnt`）现在在接管重放完成后立即更新，而不再等待下一次 15 秒的统计刷新周期。

- [#17515](https://github.com/emqx/emqx/pull/17515) 修复了使用 QoS 0 的消息队列订阅在队列订阅者本地 inflight 窗口满后可能停止接收消息的问题。

- [#17569](https://github.com/emqx/emqx/pull/17569) 将 MQTT v5 User Property 解析成本从平方复杂度降低为线性复杂度。

  此前，携带大量 User Property 的 CONNECT、PUBLISH 或 SUBSCRIBE 报文会在所属连接进程上造成超线性调度耗时，因为每个解析出的属性都会追加到累积列表末尾。现在解析会随条目数量线性扩展，同时保持其在报文中的顺序。

#### 规则引擎

- [#16699](https://github.com/emqx/emqx/pull/16699) 修复了在某些竞争条件下可能打印如下冗长错误日志的问题：

  ```
  2026-02-03T13:53:54.576326+00:00 [error] Generic server <0.11323236.0> terminating. Reason: {{badkey,'actions.success'},[{erlang,map_get,['actions.success',#{}],[{error_info,#{module => erl_erts_errors}}]},{emqx_metrics_worker,idx_metric,4,[{file,"emqx_metrics_worker.erl"},{line,683}]},{emqx_metrics_worker,inc,4,[{file,"emqx_metrics_worker.erl"},{line,322}]},{emqx_rule_runtime,do_eval_action_reply_t...
  ```

  现在 EMQX 会打印更有意义的信息以辅助问题排查。

- [#16847](https://github.com/emqx/emqx/pull/16847) 修复了在消息转换表达式中使用非 ASCII Unicode 字符串时发生崩溃的问题。

- [#17211](https://github.com/emqx/emqx/pull/17211) 在 `$events/client/connack` 规则事件中补充了 `connected_at` 字段，该字段在文档中有说明但此前在实际数据中缺失。

#### 数据集成

- [#16622](https://github.com/emqx/emqx/pull/16622) 修复了一个问题：当动作使用异步查询模式，且其连接器在多次健康检查后变为断开状态时，其备用动作可能被触发两次。

- [#16659](https://github.com/emqx/emqx/pull/16659) 在较新 EMQX 版本中使用旧版 MQTT 连接器静态 clientid 配置（来自 5.10.0 及更早版本）时，配置根部的用户名和密码会被忽略。升级并保留相同配置时，这可能导致 MQTT 客户端停止使用这些凭据。

  现在，如果根连接器中存在用户名和/或密码字段，这些凭据会与按 clientid 单独指定的凭据合并，后者优先级更高。

- [#16685](https://github.com/emqx/emqx/pull/16685) 修复了 EMQX 节点从 5.10.x 升级到 6.0.y 后，Sparkplug B 指标别名映射功能可能失效的问题。

  由于 6.0.0 之前生成 Protobuf 代码的方式不同，如果 EMQX 节点曾在较旧版本（< 6.0.0）上以与新版本相同的 OTP 版本启动，缓存的 Protobuf 代码会被保留，但已不再符合新代码的预期。这会导致从 5.10.x 升级到 6.0.y 后 Sparkplug B 别名映射失败。

- [#16723](https://github.com/emqx/emqx/pull/16723) 修复了 RabbitMQ 连接器/动作/Source 的问题：当某些连接或通道进程意外退出时，连接器/动作/Source 会被报告为断开，且在不重启的情况下无法恢复。

- [#16742](https://github.com/emqx/emqx/pull/16742) 修复 GreptimeDB TLS 连接失败问题。

- [#16796](https://github.com/emqx/emqx/pull/16796) 修复了连接器动作中多行 SQL 语句的处理问题。

- [#16863](https://github.com/emqx/emqx/pull/16863) 当收到已过期请求的异步回复时，新增警告日志。

- [#16890](https://github.com/emqx/emqx/pull/16890) 修复了 ExHook 的一个问题：成功的重连重加载可能在运行列表中重复同一服务器名称并触发重复的回调分发。

- [#16936](https://github.com/emqx/emqx/pull/16936) 修复了 Azure Blob Storage 动作在聚合模式下，当容器包含过多 blob 时健康检查可能超时的问题。

- [#16955](https://github.com/emqx/emqx/pull/16955) 消除了 Kafka 生产者动作的误报健康检查警告日志。

  此前，若 Kafka 生产者长时间空闲，Kafka 可能关闭连接（默认通常为 10 分钟），若此时恰好执行 Kafka 生产者动作的健康检查，可能出现 "not_all_kafka_partitions_connected" 误报警告信息。

- [#16972](https://github.com/emqx/emqx/pull/16972) 修复了 HTTP 和 GCP PubSub 动作，将原因为 `closing` 的瞬时连接错误视为可恢复错误，减少日志噪声。

- [#17084](https://github.com/emqx/emqx/pull/17084) 修复了 MQTT Source 的一个问题：若其连接器使用 `clean_start = false` 并重新连接到含有消息会话的 Broker，这些消息不会触发规则动作。

- [#17111](https://github.com/emqx/emqx/pull/17111) 修复了 PostgreSQL 连接器在禁用 prepared statements 模式下的查询执行问题。此前，并发查询可能相互交错并产生错误。

- [#17113](https://github.com/emqx/emqx/pull/17113) 修复了 RocketMQ 连接器隔离问题：配置错误或不可达的 RocketMQ 连接器不再影响同节点的其他 RocketMQ 连接器。此前，一个连接到不可达 Broker 的连接器可能导致共享客户端 supervisor 最多阻塞 60 秒，使同级连接器因 `resource_health_check_timed_out` 而反复抖动，Dashboard 对这些连接器的操作也会挂起。

  默认 TCP/TLS 连接超时也从 60 秒降至 10 秒，使配置错误的服务器快速显示为失败状态，而不是看起来卡住。

- [#17180](https://github.com/emqx/emqx/pull/17180) 修复了在高负载下对 MongoDB 进程调用超时会被当作不可恢复错误而不进行重试的问题。现在此类事件发生时会进行重试。

- [#17216](https://github.com/emqx/emqx/pull/17216) 修复了 Timescale/PostgreSQL 动作在将带引号的 JSON 数字字符串映射到 `FLOAT` 列时，会崩溃数据库连接进程而非报告结构化错误参数的问题。

- [#17250](https://github.com/emqx/emqx/pull/17250) 修复了 Redis Sentinel 连接器，支持对 Redis 数据节点和 Sentinel 节点分别配置认证。

- [#17293](https://github.com/emqx/emqx/pull/17293) 修复了写入 Parquet 文件时，对象包含必填键但值为 `undefined`/`null` 时写入损坏文件而非抛出错误的问题。

- [#17303](https://github.com/emqx/emqx/pull/17303) 升级 Kafka 客户端库：`brod` 从 4.5.2 升级至 4.5.4，`wolff` 从 4.1.9 升级至 4.1.10。

  上游修复的主要问题：

  - `brod`：修复 Kafka 连接重新认证时的竞争条件（via `kafka_protocol` 4.3.4）。
  - `wolff`：在高内存负载控制（`drop_if_highmem`）下保留最小缓冲区，防止生产者耗尽飞行中数据；仅丢弃超出保留量的字节。

- [#17347](https://github.com/emqx/emqx/pull/17347) 升级 RocketMQ 客户端依赖至 `v0.7.2`，修复异步生产者请求中的内存增长问题。

- [#17439](https://github.com/emqx/emqx/pull/17439) 修复了 Azure Blob Storage 连接器的健康检查在存储账户包含过多容器时可能超时或产生大量带宽消耗的问题。为 #16935 的配套修复。

- [#17450](https://github.com/emqx/emqx/pull/17450) 修复了使用 `mode=node` 时 `/prometheus/data_integration` Prometheus 端点可能返回 500 状态码的问题。该问题仅在动作和连接器配置被手动编辑且不一致（动作的连接器不存在）时才会出现。

- [#17568](https://github.com/emqx/emqx/pull/17568) 将 Kafka 客户端库 `brod` 升级到 4.5.5。

  消费者组：当 join 响应携带 `member_id_required` 错误码（由不支持静态成员实例 ID 的旧版 Kafka Broker 返回，例如 2.2.0）时，现在会使用 Broker 分配的成员 ID。此前，出错时成员 ID 会被丢弃，导致重试无法成功。

- [#17579](https://github.com/emqx/emqx/pull/17579) 修复 Redis Sentinel 连接器，使其为每个资源使用独立的 Sentinel 管理器，并在资源停止时清理这些管理器，避免连接器之间共享 Sentinel 状态。

- [#17584](https://github.com/emqx/emqx/pull/17584) 限制了 Snowflake 聚合连接器健康检查期间返回的数据量。仅当已有 schema 列表非常大时才有可观察影响，此时健康检查的执行时间将大幅缩短。

- [#17588](https://github.com/emqx/emqx/pull/17588) 限制了 Kinesis 集成的连接器和动作健康检查期间返回的数据量。仅当已有 schema 列表非常大时才有可观察影响，此时健康检查的执行时间将大幅缩短。

- [#17595](https://github.com/emqx/emqx/pull/17595) 限制了 S3 和 S3 Tables 集成的连接器健康检查期间返回的数据量。仅当已有桶（bucket）列表非常大时才有可观察影响，此时健康检查的执行时间将大幅缩短。

#### 集群

- [#16393](https://github.com/emqx/emqx/pull/16393) 提升了不稳定网络条件下集群连接路由复制的稳定性。

- [#16739](https://github.com/emqx/emqx/pull/16739) 缩短所有节点同时重启后的集群恢复时间。

  内置 Mria 数据库管理系统不再等待用于生成事务同步事件的内部表完成全量同步。

- [#17132](https://github.com/emqx/emqx/pull/17132) 修复了在副本节点上，当其原始配置或运行时状态发生偏移时，添加或删除主题指标可能失败的问题，该问题会触发 `cluster_rpc_apply_failed` 告警并阻塞集群 RPC 复制。重复添加和删除不存在的操作现在仅在发起节点上被拒绝，副本节点以幂等方式应用变更。

- [#17182](https://github.com/emqx/emqx/pull/17182) 升级至 emqx-OTP 27.3.4.2-8（针对 mria）。

  未修复此问题前，EMQX 启动期间若未连接到集群，Mria 应用启动可能卡住。

- [#17214](https://github.com/emqx/emqx/pull/17214) 移除了集群连接消息转发 MQTT 客户端断开事件的晦涩错误级日志，改为包含足够排查上下文的用户友好消息。类似以下的事件不应再出现在错误日志中：

  ```
  2026-05-06T03:00:48.738654+00:00 [error] [PoolWorker] unexpected info: {disconnected,141,#{}}
  ```

- [#17218](https://github.com/emqx/emqx/pull/17218) 避免 `bin/emqx` 和 `bin/emqx_ctl` 调用在运行中的 Broker 上触发 `nodeup`/`nodedown` 事件，这些事件此前在 Broker 日志中表现为误导性的 `cm_registry_node_down` 警告。这些脚本启动的临时辅助节点现在按预期注册为隐藏 Erlang 节点。

- [#17269](https://github.com/emqx/emqx/pull/17269) 改进了网络分区后的集群恢复能力。

  - 此前，连接到副本节点的部分客户端可能从全局注册表中丢失，导致接管时行为不一致以及 Dashboard 显示信息不正确。

    此修复新增了一个后台进程，在网络分区恢复后重新注册现有客户端。同时新增告警："Broker is recovering after a network partition"，在全局注册表重建期间触发。

  - 引入新的集群自愈算法，可自动恢复重叠网络分区。

- [#17343](https://github.com/emqx/emqx/pull/17343) 修复了集群配置复制的一个问题：导入包含 `file` 类型授权源的数据备份（或通过 `emqx ctl conf load`/`PUT /api/v5/configs` 加载 HOCON 配置）时，可能导致对等节点出现 `cluster_rpc_apply_failed`/`failed_to_read_acl_file` 错误并滞后。

  导入程序此前会在本地写入 ACL 文件并将内联 `rules` 替换为 `path`，然后将 path 形式的配置下发到集群。对等节点磁盘上没有该文件，因此无法应用变更。现在发送到集群的配置保持 `rules` 内联，每个对等节点从复制内容中写入自己的 ACL 文件副本。

- [#17348](https://github.com/emqx/emqx/pull/17348) 修复了当集群节点的有效配置相同但原始配置表示不同时，`emqx ctl conf cluster_sync status` 诊断输出嘈杂且具有误导性的问题。

  该命令现在会抑制不对应已检查配置变更的原始表示差异，同时在已检查配置不一致时仍会发出警告。当某个原始配置键在一个节点存在而在另一个节点缺失时，不再崩溃。

  同时忽略动作、Source、Bridge 和规则元数据中 `created_at` 和 `last_modified_at` 的纯时间戳元数据差异。数据导入或启动时配置加载可能仅在部分节点上刷新这些生成的时间戳，即使有效运行时配置完全相同。

- [#17349](https://github.com/emqx/emqx/pull/17349) 改进了集群连接在路由复制卡在连接到无响应目标集群时的响应速度。现在删除此类集群连接的速度会略有提升。

- [#17382](https://github.com/emqx/emqx/pull/17382) 修复了集群遭遇网络分区时可能发生的全局 channel 注册表损坏问题。

- [#17424](https://github.com/emqx/emqx/pull/17424) 修复了网络分区后 Mnesia 自愈可能导致同一客户端 ID 在全局会话注册表中留下重复或过期条目的泄漏问题。

  Discard 和 takeover-kick RPC 处理程序现在也会在目标进程不再存活时删除注册表行；连接路径上的注册限流现在能识别墓碑行（无本地 channel 状态）并清理它们，而不是无限期阻塞同一客户端 ID 的新连接。

- [#17432](https://github.com/emqx/emqx/pull/17432) 修复了并发集群连接 API 请求可能返回通用错误响应而非成功或未找到的问题。

- [#17469](https://github.com/emqx/emqx/pull/17469) 修复了启用或禁用活动集群连接时出现如下警告的问题：

  ```
  [warning] tag: RESOURCE, msg: handle_resource_metrics_failed, reason: {badkey, matched}, event: matched, ...
  ```

- [#17586](https://github.com/emqx/emqx/pull/17586) 定期清理全局会话注册表中的陈旧条目。此前，当会话的属主进程在未正常注销的情况下退出（例如，短暂网络分区导致注销操作未能复制，或在 down 事件清理期间某个 core 节点的一致性检查超时），如果相同的客户端 ID 再也没有重新连接，注册表行可能会永久残留。现在每个 core 节点上有一个受限流控制的后台清理任务来移除此类行。该任务限制为每个节点每秒最多 500 行，且运行间隔不短于 10 分钟，因此即使在持有数百万会话的注册表上也不会对 Broker 吞吐量产生可观测影响。

#### 访问控制

- [#16692](https://github.com/emqx/emqx/pull/16692) 修复 CRL 缓存回归问题：`emqx_crl_cache:evict/1` 未完全清除内部 URL 状态。驱逐后，相同 CRL URL 现在可在下次使用时正确重新注册、恢复刷新定时器，并避免每次连接都重复发起 HTTP 获取。

- [#16780](https://github.com/emqx/emqx/pull/16780) 修复了授权源验证中缺少 `type` 字段的请求可能触发内部错误的问题。

  现在 EMQX 对此类情况返回清晰的 `BAD_REQUEST` 验证错误。

- [#16805](https://github.com/emqx/emqx/pull/16805) 新增对 authz hook 结果选择退出授权缓存存储的支持，用于动态 ACL 决策。

- [#16865](https://github.com/emqx/emqx/pull/16865) 为 `mqtt.client_attrs_init` 表达式新增 `cert_common_name` 和 `cert_subject` 别名，与现有的 `cn` 和 `dn` 变量并列使用。

- [#16868](https://github.com/emqx/emqx/pull/16868) 改进了 REST API 认证错误消息，引导程序化客户端使用 API 密钥（Basic auth）而非反复登录获取 bearer token。错误响应现在会提及 `api_key.bootstrap_file` 配置选项和用于创建持久 API 密钥的 `POST /api_key` 端点。

- [#16939](https://github.com/emqx/emqx/pull/16939) 修复了内置数据库认证器在配置了默认引导文件路径但文件不存在时记录警告的问题。

- [#17045](https://github.com/emqx/emqx/pull/17045) 修复了基于密码的认证后端在 CONNECT 包不含密码时会立即拒绝连接而非继续认证链的问题。

  此前，若客户端连接时不带密码，认证链中第一个基于密码的认证器（内置数据库、MySQL、PostgreSQL、MongoDB、Redis 或 LDAP）会返回错误，阻止后续认证器被尝试。

- [#17100](https://github.com/emqx/emqx/pull/17100) 修复了当身份提供商返回的 JWKS 响应 `Content-Type` 使用 `+json` 结构化语法后缀（如 `application/jwk-set+json; charset=utf-8`）时，OIDC SSO 登录失败并报 `provider_not_ready` 的问题。此类响应现在被接受为有效的 JWKS 内容。

- [#17122](https://github.com/emqx/emqx/pull/17122) 修复了 Dashboard RBAC 对含 URL 编码用户名（如电子邮件地址）的 SSO 用户的权限检查，确保 `force_mfa` 禁用时查看者的 MFA 自助禁用请求能正常工作。

- [#17140](https://github.com/emqx/emqx/pull/17140) 修复了 EMQX 通过 HTTP 获取证书吊销列表（CRL）时，服务器返回 DER 编码内容（`Content-Type: application/pkix-crl`，RFC 5280 §5 规定的格式）时的静默失败问题。

  此前，EMQX 仅解码 PEM 编码的 CRL 内容；DER 内容被静默视为零条 CRL 并缓存为空列表，导致 `enable_crl_check = true` 监听器上的每次 TLS 握手都以 `bad_crls, no_relevant_crls` 失败，且无日志说明原因。

  EMQX 现在同时解码 PEM 和 DER CRL 内容。当获取的内容两者都不是时，记录带有 URL 的警告日志，使配置错误可见。

- [#17171](https://github.com/emqx/emqx/pull/17171) 修复了阻止命名空间 Dashboard 管理员启用或禁用自身账户 MFA 的 RBAC 问题。

  命名空间管理员仍受限于无法管理其他 Dashboard 用户的 MFA 设置。

- [#17177](https://github.com/emqx/emqx/pull/17177) Dashboard 创建的 REST API 密钥现在随机生成，而非从 API 密钥名称派生。

- [#17223](https://github.com/emqx/emqx/pull/17223) 修复了在 SSL 监听器前放置 TCP 透传代理（如 GCP TCP Proxy NLB、AWS NLB）且配置 `proxy_protocol = true` 时，客户端证书丢失的问题。监听器处的 TLS 握手成功完成且客户端证书存在，但未暴露给认证或规则事件。依赖客户端证书（CN、subject、完整 PEM）的函数、ACL 规则和认证后端现在在此部署形态下可正常工作。

- [#17428](https://github.com/emqx/emqx/pull/17428) 修复了当提供商的 `.well-known/openid-configuration` 响应包含 `Cache-Control` 头（如 Kanidm 观察到的 `max-age=0`）时，Dashboard OIDC SSO 崩溃导致 EMQX 无法完成 OpenID 提供商发现的问题。该崩溃会导致 OIDC supervisor 在单次失败后耗尽重启预算，使 SSO 在不重新保存配置的情况下无法恢复。cache-control 解析器现在对这些值更具容错性，worker 不再因过期值错误而硬崩溃，OIDC supervisor 允许在一分钟内多次重启，使瞬时失败能自动重试。

#### 网关

- [#16603](https://github.com/emqx/emqx/pull/16603) 修复 CoAP 网关在 DTLS 连接模式下的问题。

- [#16670](https://github.com/emqx/emqx/pull/16670) NATS 网关现在会强制执行最大发布 Payload 限制，支持 `echo` 选项（不进行本地投递），并改进发布/订阅主题处理及相关错误信息。

- [#17141](https://github.com/emqx/emqx/pull/17141) 修复了 CoAP 连接模式的 token 接管问题，使重新连接的 UDP/DTLS 客户端可以使用有效 token 恢复，同时拒绝无效的 token/clientid 组合。同时确保在运行 CoAP 接管 connected hook 之前所需的连接信息字段已就绪。

- [#17258](https://github.com/emqx/emqx/pull/17258) 修复了 MQTT-SN 网关中，已连接客户端在同一会话上发送第二个 CONNECT 包会导致连接进程崩溃的问题。网关现在以 DISCONNECT 响应并优雅关闭会话。

- [#17287](https://github.com/emqx/emqx/pull/17287) 修复了 MQTT-SN 客户端因在意外连接或 Will 状态下收到包而崩溃的问题，包括连接设置期间的 `DISCONNECT`、Will 握手完成前的 `REGISTER`，以及 Will topic 不存在时的 `WILLMSGUPD`。

- [#17581](https://github.com/emqx/emqx/pull/17581) 修复 JT/T 808 网关，使其使用认证期间接受的手机号作为连接身份，拒绝不匹配的注册码认证尝试以及手机号不同的后续上行帧。

#### 多租户

- [#17118](https://github.com/emqx/emqx/pull/17118) 改进了多租户列表端点的分页功能（`/mt/ns_list`、`/mt/ns_list_details`、`/mt/managed_ns_list`、`/mt/managed_ns_list_details`、`/mt/ns/{ns}/client_list`）：

  - 新增符合 RFC 8288 的 `Link: <?...>; rel="next"` 响应头。当有更多页面时，该头部携带下一页的仅查询 URI 引用；缺失时表示当前响应是最后一页。这消除了此前需要额外请求才能区分整页（`len(results) == limit`）与精确边界"无更多数据"情况的歧义。
  - 在现有排他游标（`last_ns`、`last_clientid`）旁边新增包含性 keyset 游标查询参数（`first_ns`、`first_clientid`）。包含形式支持精确匹配查询（如 `?first_ns=foo&limit=1`），并在调用方选择使用时通过分页 Link 头传递。两种形式在单个请求中互斥；同时提供两者返回 HTTP 400。

- [#17406](https://github.com/emqx/emqx/pull/17406) 现在，由命名空间管理员发起的追踪所捕获的事件，对于主题、IP 地址和客户端 ID 类型的追踪，将限制在该管理员的命名空间内。规则 ID 类型的追踪此前已有此行为。

#### 插件

- [#16784](https://github.com/emqx/emqx/pull/16784) 减少单节点部署中的插件启动噪声警告。

  EMQX 在集群配置同步期间不再尝试从本地节点获取插件配置，从而避免启动时反复出现 `config_not_found_on_node` 警告。

- [#16823](https://github.com/emqx/emqx/pull/16823) 修复预安装插件的 Dashboard 插件管理问题。

  当插件包在节点启动前已解压到 `plugins/` 目录中时，从 Dashboard 启动该插件后，插件配置页面不再出现 `Plugin Config Not Found`。

- [#16842](https://github.com/emqx/emqx/pull/16842) 减少了在无对等节点持有插件配置时产生的嘈杂插件配置警告日志。

  此前，节点启动期间从对等节点获取插件配置时，即使所有对等节点仅是尚未拥有该配置（如第一个加载插件的节点），也会记录警告。现在此类无害情况以 debug 级别记录，仅真正的错误（RPC 失败、超时）保留为警告。

- [#16843](https://github.com/emqx/emqx/pull/16843) 修复了 HTTP 头部和查询字符串参数未传递到插件 API 处理程序的问题，该问题导致插件接收到空头部和缺失的查询参数。

- [#16904](https://github.com/emqx/emqx/pull/16904) 阻止同一插件的多个版本同时启用或启动。启用新版本时，旧版本的已配置插件会自动禁用，管理 API 操作现在返回清晰的错误，而不是在另一个版本仍活跃时报告成功。

- [#17247](https://github.com/emqx/emqx/pull/17247) 当插件的 REST API 回调崩溃或超出超时预算时，Broker 现在会将失败的 API 方法和路径连同配置的超时时间一并记录，使混合流量日志中的问题调用可被识别。超时记录为警告（非错误），并包含指向 `plugins.api_endpoint.timeout`（在插件回调合理需要更多时间时可调整的配置键）的提示。

- [#17473](https://github.com/emqx/emqx/pull/17473) 当插件的 Erlang 应用因其他运行中的应用仍依赖它而无法停止时，将 `unabled_to_stop_plugin_apps` 的日志级别从 warning 降至 info。这是插件卸载时的预期、无需操作的情况，不应再触发警告。

- [#17575](https://github.com/emqx/emqx/pull/17575) 修复 emqx_username_quota 插件中的竞争条件。该问题可能导致按用户名统计的会话计数器与实际跟踪的客户端记录数量不一致。计数器可能被递减到零以下，随后被删除；与此同时，并发会话注册又递增该计数器，导致该增量永久丢失。

#### REST API

- [#17002](https://github.com/emqx/emqx/pull/17002) 将 `minirest` 库升级至 1.4.12 版本。该版本修复了一个导致 EMQX API 在返回 `204 No Content` 状态行时生成格式错误响应的问题：错误地附带了无效的 `content-length` 响应头。

- [#17054](https://github.com/emqx/emqx/pull/17054) 修复了设置 `Accept: application/json` 时 `GET /api/v5/configs?key=...` 返回不完整数据的问题。

  此前，JSON 响应忽略 `key` 查询参数，始终返回固定的根配置子集，不包含 `multi_tenancy` 等键。该端点现在与 hocon（text/plain）响应一致地处理 JSON 响应中的 `key` 参数。

- [#17319](https://github.com/emqx/emqx/pull/17319) `GET /api/v5/schemas/{hotconf,actions,connectors}` 现在以 `Content-Type: application/json` 返回响应。此前响应体是有效 JSON，但头部为 `text/plain; charset=utf-8`，导致按响应内容类型分发的客户端出错。

#### 可观测性

- [#16661](https://github.com/emqx/emqx/pull/16661) 改进请求非法主题时 `topic_metrics` 和 `cluster_rpc` 的日志记录。

- [#16674](https://github.com/emqx/emqx/pull/16674) 确保 Erlang PID 作为日志数据字段打印。

- [#16876](https://github.com/emqx/emqx/pull/16876) 将日志消息 `msg_publish_not_allowed` 更名为 `msg_not_routed_to_subscribers`。

- [#16879](https://github.com/emqx/emqx/pull/16879) 新增 `log.audit.cache_size` 作为审计日志数据库缓存大小的主要配置键，同时保留 `log.audit.max_filter_size` 以保持向后兼容性。

- [#17513](https://github.com/emqx/emqx/pull/17513) 修复 Prometheus 匹配授权允许/拒绝指标，使其反映实际匹配到的授权决策。

#### 部署

- [#16545](https://github.com/emqx/emqx/pull/16545) 修复 `node.cookie` 对 `#` 字符的处理问题。此前，如果 cookie 包含 `#`，只有 `#` 前的前缀会生效。例如，配置为 `abc#d` 时，实际使用的只有 `abc`。

  新增校验以拒绝有问题的字符：反斜杠、单引号、双引号和空格。

- [#16620](https://github.com/emqx/emqx/pull/16620) 修复 aarch64 上 CRC32C 动态库加载问题。

- [#16657](https://github.com/emqx/emqx/pull/16657) 修复从旧节点版本向新版本导入配置时，配置值未按新代码升级而导致异常行为的问题。

  一个例子是将 5.10.0 中带静态 clientid 的 MQTT 连接器导入 6.0.0。在 5.10.0 中，用户名和密码无法与特定静态 clientid 关联，其内部表示方式也不同。后续版本增加了创建这些关联的能力，并采用了不同的内部表示。此前导入这类配置时，缺少这种细微的内部表示转换。

- [#17024](https://github.com/emqx/emqx/pull/17024) Dashboard HTTP 监听器现在在绑定地址为 IPv6 地址时自动使用 IPv6，无需显式设置 `inet6 = true`。

- [#17227](https://github.com/emqx/emqx/pull/17227) 集群配置文件保存错误现在会指明文件名和底层原因。

  当 `cluster.hocon`（或其目录）为只读、不可变或以其他方式不可写（如挂载为只读的容器）时，通过 Dashboard 或 REST API 修改配置此前会返回不透明的 HTTP 400，内容为 `{config_update_crashed,{badmatch,{error,ebusy}}}`，且只记录未指明文件名的 badmatch 崩溃日志。

  现在，错误将：

  - 记录 `failed_to_save_conf_file`，包含实际文件路径和原因（`eacces`、`eperm`、`ebusy` 等），以及列出常见运维侧原因的提示。
  - 返回结构化的 HTTP 400 响应体，同时指明文件和原因，无需翻阅节点日志即可在 Dashboard 中看到原因。

  此前，当仅临时文件写入失败（如只读目录）时，API 会静默返回 HTTP 200，尽管更改未持久化到磁盘。API 现在在此情况下也会正确报告失败。

- [#17246](https://github.com/emqx/emqx/pull/17246) 将 `jose` 库从 1.11.10 升级至 1.11.12，获取针对新版 OTP 的 EC 和 EdDSA 密钥修复。

- [#17252](https://github.com/emqx/emqx/pull/17252) 在官方下载站点的插件包旁发布 `.sha256` 校验和附件，允许用户验证下载的插件归档完整性。

- [#17254](https://github.com/emqx/emqx/pull/17254) 改进了容器内的内存使用报告。Broker 现在从 cgroup v2、cgroup v1 和宿主机 `/proc/meminfo` 中选取约束最严格的内存读数（最小非零总量优先，使用率更大的在并列时胜出）。此前报告可能在两种情况下产生误导：在具有严格 cgroup 限制的容器中，宿主机视图可能显示 >70% 而 cgroup 限制实际 <10%（或反之）；在未设置内存限制的 cgroup 挂载下，cgroup 读数可能将报告的使用率压缩至约 0%。过载保护阈值和"已用内存"指标现在反映实际约束进程的限制。

- [#17271](https://github.com/emqx/emqx/pull/17271) 加固官方 EMQX Docker 镜像，清除镜像扫描器报告的问题：

  - 在运行时镜像构建期间应用 Debian 安全升级，使镜像获取最新修复版 `libssl3t64`。
  - 移除未使用的 `libgnutls30t64` 包。EMQX 通过 Erlang/OTP 使用 OpenSSL 进行 TLS 通信，从不链接 GnuTLS，该包仅作为 `curl` 的传递依赖存在并出现在扫描报告中。
  - 将 Debian `curl` 包替换为来自 [stunnel/static-curl](https://github.com/stunnel/static-curl) 的静态链接 `curl` 二进制文件（OpenSSL、HTTP/2、HTTP/3；无 RTMP，无 GnuTLS）。Debian 包会通过 `librtmp1` 重新引入 `libgnutls30t64`；静态二进制文件避免了这一问题，同时保持调用 `curl` 的容器健康检查正常工作。

- [#17311](https://github.com/emqx/emqx/pull/17311) 修复了容器主机名无法解析时 Docker 启动失败的问题。入口点现在在自动生成节点名称前回退到网络接口 IP 地址，若无法确定节点主机则以清晰的错误信息退出。

- [#17342](https://github.com/emqx/emqx/pull/17342) 修复了当导出的 `cluster.hocon` 包含部分 `node` 节时，集群配置导入因 "required_field: node.cookie" schema 检查错误而失败的问题。只读根配置（`node`、`rpc`）本就不属于数据导入范围，现在在预检 schema 校验前从导入配置中删除，使验证使用运行节点自身的值。

- [#17369](https://github.com/emqx/emqx/pull/17369) 将 Dashboard 监听器默认值（`http.bind` 和占位符 HTTPS `ssl_options`）从用户可编辑的 `etc/emqx.conf` 迁移至随附的 `etc/base.hocon`。此前，硬编码的 `emqx.conf` 块会在重启时静默将运行时更新回滚为默认自签名证书。现在，通过 Dashboard、REST API 或 `emqx_acme` 插件自动 HTTPS 配置所做的运行时更新可在重启后正确保留。

- [#17536](https://github.com/emqx/emqx/pull/17536) 在 Dashboard 中为 SSL 监听器 `password` 以及其他 secret 类型配置字段（MQTT Bridge 密码、集群连接密码、Dashboard OIDC 客户端密钥、S3 Secret Access Key、AI Completion API Key、Pulsar/RocketMQ 凭据等）的工具提示补充 `file://` 选项说明。通用 secret 类型描述已提到这一约定，但字段级描述会覆盖该说明，导致用户误以为这些字段只接受字面值。

- [#17540](https://github.com/emqx/emqx/pull/17540) 修复在 SSL 监听器上设置 `password = "file://..."` 时，如果 keyfile 已加密，配置校验会因 `bad_password_or_invalid_keyfile` 失败的问题。现在，`file://` 引用会在校验期间解析，而不只是在运行时解析。


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
