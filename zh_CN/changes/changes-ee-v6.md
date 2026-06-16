# EMQX 企业版 v6 版本

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

  - `durable_storage.db_groups.<group>.storage_quota`： SST 文件大小的软配额
  - `durable_storage.db_groups.<group>.write_buffer_size`：最大 memtable 大小
  - `durable_storage.db_groups.<group>.rocksdb_nthreads_high` 与 `durable_storage.db_groups.<group>.rocksdb_nthreads_low`：RocksDB 线程池大小

  新增告警 `db_storage_quota_exceeded:<DB>`，当存储配额被超出时触发。更多信息请参考文档中的"存储配额"章节。

  默认的会话检查点（checkpoint）间隔已更改为 15 秒。

- [#16286](https://github.com/emqx/emqx/pull/16286) 优化了默认的持久存储配置以降低 CPU 负载。该 PR 禁用了未使用订阅功能的数据库的订阅支持。

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
