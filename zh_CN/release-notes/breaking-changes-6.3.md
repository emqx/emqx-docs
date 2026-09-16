# EMQX 6.3 中的不兼容变更

## 6.3.0

- [#17185](https://github.com/emqx/emqx/pull/17185) MQTT 解析器现在默认以严格模式运行。如需恢复此前的宽松行为，请在全局或 Zone 级别设置 `mqtt.strict_mode = false`。

  在严格模式下，Broker 会根据协议规范校验传入的 MQTT 报文，并断开发送畸形报文的客户端。仅在严格模式下执行以下校验：

  - **固定报头标志。** 对于非 PUBLISH 报文，保留的 DUP/QoS/RETAIN 位必须为零；PUBREL/SUBSCRIBE/UNSUBSCRIBE 必须使用 QoS 1（`bad_frame_header`）。
  - **CONNECT 保留位**必须为零（`reserved_connect_flag`）。
  - **CONNECT 遗嘱标志一致性**：Will Flag=0 时，Will QoS 和 Will Retain 必须均为 0；Will Flag=1 时，Will QoS 必须为 0、1 或 2（`invalid_will_qos`、`invalid_will_retain`）。
  - **CONNECT Password/Username 标志（仅限 MQTT 3.1.1）。** 根据 `[MQTT-3.1.2-22]`，Username Flag=0 时，Password Flag 也必须为 0（`invalid_password_flag`）。MQTT 5.0 已取消此限制，因此不受影响。
  - **UTF-8 字符串**（协议名称、客户端 ID、主题、用户名、密码、遗嘱主题和 MQTT 5 字符串属性）必须是有效的 UTF-8，且不得包含 U+0000–U+001F 或 U+007F–U+009F 控制字符（`utf8_string_invalid`）。
  - **报文标识符**在必需时不得为零，包括 QoS>0 的 PUBLISH、PUBACK/REC/REL/COMP、SUBSCRIBE/SUBACK 和 UNSUBSCRIBE/UNSUBACK（`bad_packet_id`）。

  客户端违反上述任一校验时，Broker 会记录一条 `info` 级别日志，其中包含 `msg=frame_parse_error` 和结构化的 `reason`，例如 `cause=invalid_password_flag`、`proto_ver` 或 `received_prefix`，以便排查问题。对于 MQTT 5.0 连接，Broker 还会在关闭连接前返回携带原因码 `0x81 Malformed Packet` 的 CONNACK/DISCONNECT 报文；对于 MQTT 3.1/3.1.1，连接会被静默关闭，因为这些版本没有为畸形报文定义 CONNACK 原因码。

- [#17215](https://github.com/emqx/emqx/pull/17215) 从 EMQX 发布包中移除了内置 Swagger UI 资源，使 tarball 大小减少约 11 MB。

  `/api-docs/swagger.json` 仍会提供完整的 OpenAPI 3 JSON 规范，因此通过 URL 加载该规范的外部 Swagger UI 部署可继续工作。旧版 `/api-docs` URL 会返回 HTTP 308 重定向，指向 6.3.0 中新增的内置规范浏览器 `/api-spec.html`。其他 `/api-docs/*` 子路径（内嵌 Swagger UI 资源）不再提供服务，并返回 404。

- [#17267](https://github.com/emqx/emqx/pull/17267) `node.max_ports` 配置的默认值现为 `auto`，会根据逻辑 CPU 核数调整 Erlang VM 端口上限（`+Q`）：不超过 8 核时每核 65536 个端口，超过 8 核时为 1048576（此前的固定默认值）。仍可显式设置整数值。

  对于从早期版本升级且 `max_ports` 使用固定默认值 1048576 的节点，这是一项行为变更：CPU 核数不超过 8 的主机现在会以更小的端口表启动。如果部署需要接受超过 `cores * 65536` 个连接，必须在升级前显式设置 `node.max_ports` 并重启节点。

  隐藏配置 `node.process_limit` 重新作为覆盖项生效：当其值大于推导出的上限（`2 * max_ports`）时采用该值；更小的值会被忽略，以确保进程表不会小于端口表所需的容量。

  新增 `node.schedulers` 配置（默认值为 `auto`），用于控制 Erlang 调度器数量（`+S`）。设置为 `auto` 时，数量上限为 VM 实际可用的逻辑处理器数（Linux 上通过 `sched_getaffinity` 获取），因此受 `--cpuset-cpus` 或 Kubernetes CPU request 限制的容器不会再创建无法并行运行的调度器 OS 线程。可将其设置为正整数以覆盖自动检测值。

- [#17437](https://github.com/emqx/emqx/pull/17437) Prometheus 抓取端点（`/api/v5/prometheus/*`）现在默认要求认证。如需恢复此前无需认证的行为，请显式设置 `prometheus.enable_basic_auth = false`。对于未携带凭据抓取这些端点的部署，需要为抓取程序配置凭据或设置该配置项。推荐为抓取程序使用具有 `monitoring` scope 的专用 API 密钥，并采用 Bearer 认证。

- [#17582](https://github.com/emqx/emqx/pull/17582) Prometheus VM 和 Mnesia 采集器的指标名称现在采用符合 promtool 规范的 `prometheus.erl` 6.x 名称。

  受影响的指标重命名如下：

  - `erlang_mnesia_failed_transactions` -> `erlang_mnesia_failed_transactions_total`
  - `erlang_mnesia_committed_transactions` -> `erlang_mnesia_committed_transactions_total`
  - `erlang_mnesia_logged_transactions` -> `erlang_mnesia_logged_transactions_total`
  - `erlang_mnesia_restarted_transactions` -> `erlang_mnesia_restarted_transactions_total`
  - `erlang_vm_memory_atom_bytes_total` -> `erlang_vm_memory_atom_bytes`
  - `erlang_vm_memory_bytes_total` -> `erlang_vm_memory_bytes`
  - `erlang_vm_memory_processes_bytes_total` -> `erlang_vm_memory_processes_bytes`
  - `erlang_vm_memory_system_bytes_total` -> `erlang_vm_memory_system_bytes`
  - `erlang_vm_statistics_context_switches` -> `erlang_vm_statistics_context_switches_total`
  - `erlang_vm_statistics_garbage_collection_number_of_gcs` -> `erlang_vm_statistics_garbage_collection_number_of_gcs_total`
  - `erlang_vm_statistics_garbage_collection_words_reclaimed` -> `erlang_vm_statistics_garbage_collection_words_reclaimed_total`
  - `erlang_vm_statistics_garbage_collection_bytes_reclaimed` -> `erlang_vm_statistics_garbage_collection_bytes_reclaimed_total`
  - `erlang_vm_statistics_runtime_milliseconds` -> `erlang_vm_statistics_runtime_seconds_total`
  - `erlang_vm_statistics_wallclock_time_milliseconds` -> `erlang_vm_statistics_wallclock_time_seconds_total`
  - `erlang_vm_port_count` -> `erlang_vm_ports`
  - `erlang_vm_process_count` -> `erlang_vm_processes`
  - `erlang_vm_atom_count` -> `erlang_vm_atoms`

- [#17596](https://github.com/emqx/emqx/pull/17596) 新增授权选项，用于禁止在授权规则的主题过滤器模板中插入 `/`、`+` 和 `#` 符号。新增选项如下：

  ```hocon
  authorization.topic_template_allow {
    plus = false,
    hash = false,
    slash = false
  }
  ```

  设置为 `false` 时，插入主题模板的值中不能包含对应符号。例如，当 `plus = false` 时，规则 `{allow, all, publish, ["userspace/${username}"]}` 中不允许使用用户名 `bad+user`。处理结果取决于启用的安全配置：在传统安全配置下，该规则不匹配；在加固安全配置下，该操作会被拒绝。

- [#17677](https://github.com/emqx/emqx/pull/17677) Prometheus REST API 不再支持 JSON 输出格式。

  `/api/v5/prometheus` 下的端点（`stats`、`auth`、`data_integration`、`schema_validation`、`message_transformation`）现在仅生成 Prometheus 文本格式。发送 `Accept: application/json` 的请求会被拒绝，并返回 `400 Bad Request`（"only prometheus format is supported"）；此前这些请求会返回指标的 JSON 表示。

- [#17626](https://github.com/emqx/emqx/pull/17626) [#18123](https://github.com/emqx/emqx/pull/18123) 新增 `multi_tenancy.deny_namespaces` 配置，用于保存不能用作命名空间标识符的名称。这些名称既不能用作管理员命名空间（Dashboard 角色、API 密钥和多租户管理 API），也不能用作客户端的 `client_attrs.tns`；如果客户端的 `client_attrs.tns` 解析为被禁用的名称，该客户端将被拒绝。

  这是一项不兼容变更：默认值 `["global", "undefined", "null", "none"]` 会禁用此前允许使用的名称。这些名称与内部哨兵值冲突，会导致日志行和 Dashboard 输出含义不明确。现有的同名命名空间不会自动迁移；请在升级前重命名，或将 `multi_tenancy.deny_namespaces` 设置为空列表以取消限制。

  此外，如果已配置 `multi_tenancy.post_auth_tns_expression`，但表达式求值为空或求值失败，则认证前 `client_attrs.tns` 为禁用名称的客户端现在也会被拒绝，与表达式求值为非空值时的处理方式一致。

- [#18228](https://github.com/emqx/emqx/pull/18228) 默认授权规则文件（`acl.conf`）不再向从 `127.0.0.1` 连接的客户端授予对所有主题（包括 `$SYS/#` 和 `#`）的无限制发布和订阅权限。

  从本地主机连接的客户端现在与其他客户端采用相同的规则进行授权，最终由 `authorization.no_match` 配置决定。无论采用哪种安全配置，默认规则现在都会拒绝本地主机客户端订阅 `$SYS/#` 以及通配符主题过滤器 `#` 和 `+/#`。

  依赖内置本地主机放行规则的部署必须在 `acl.conf` 中添加显式规则。此前的规则仍以注释形式保留在文件中，便于重新启用：

  ```erlang
  %% {allow, {ipaddr, "127.0.0.1"}, all, ["$SYS/#", "#"]}.
  ```

  注意：此变更适用于全新安装以及尚未自定义 `acl.conf` 的部署；升级不会修改现有的自定义 `acl.conf` 文件。

- [#18244](https://github.com/emqx/emqx/pull/18244) 已移除 ExProto 网关。

- [#18271](https://github.com/emqx/emqx/pull/18271) [#18329](https://github.com/emqx/emqx/pull/18329) MQTT 和网关 WebSocket 监听器默认不再从转发请求头读取客户端地址和端口：`proxy_address_header` 和 `proxy_port_header` 的默认值已从 `x-forwarded-for` / `x-forwarded-port` 改为空值，即始终使用套接字源地址和端口。对于位于负载均衡器或反向代理之后且依赖转发请求头的部署，现在必须显式配置请求头名称，例如将 `proxy_address_header` 设置为 `x-forwarded-for`。请求头名称为空时会禁用转发请求头查找。

  此变更还修复了网关 WebSocket 监听器的转发请求头查找。此前，已配置的请求头名称无法与请求头匹配，因此即使请求中包含转发请求头，也仍使用套接字源地址和端口。

- [#18377](https://github.com/emqx/emqx/pull/18377) 创建托管命名空间时现在会校验其名称。名称长度必须为 1 至 255 字节，并且只能包含 ASCII 字母、数字以及 `.`、`-` 和 `_` 字符；不接受 `.` 和 `..`。现有命名空间不受影响。

- [#18390](https://github.com/emqx/emqx/pull/18390) `mqtt.clientid_override` 表达式执行失败时，不再回退到客户端提供的客户端 ID。

  配置 `mqtt.clientid_override` 后，如果表达式引发错误（例如引用了客户端未提供的属性）或渲染为空字符串，EMQX 现在会拒绝连接，并返回 CONNACK 原因码 0x85（Client Identifier not valid；MQTT 3.1 和 3.1.1 客户端的返回码为 2）。此前，此类客户端仍会使用原始客户端 ID 保持连接，导致覆盖配置在无提示的情况下未生效。

  升级前，请确认每个连接客户端都能将配置的表达式渲染为非空字符串。升级前无法渲染表达式的客户端会使用原始客户端 ID 连接；升级后，这些客户端会被拒绝，直至修复表达式或客户端数据。

- [#18419](https://github.com/emqx/emqx/pull/18419) 已移除 Google Cloud IoT Core 迁移兼容功能，包括 GCP Device 认证器和设备管理 API。

- [#18515](https://github.com/emqx/emqx/pull/18515) Azure Blob Storage 动作的 `blob` 模板字段现在使用与 Aggregated S3 动作的 `key` 字段相同的 Schema 校验。该校验会拒绝不支持的模板绑定。

- [#18528](https://github.com/emqx/emqx/pull/18528) 新增校验，要求 OpenTelemetry 集成的 Exporter 端点必须是显式包含协议和端口的 URL。支持的协议为 `http` 和 `https`。

- [#18627](https://github.com/emqx/emqx/pull/18627) Dashboard SAML SSO 现在默认在所有安全配置下验证 IdP 签名。

  此前，默认行为取决于安全配置：加固安全配置会验证签名，而传统安全配置（7.0 之前的默认配置）不会验证，因此会接受未签名、伪造的 SAMLResponse 并签发 Dashboard 会话。

  如果有意使用不签名的 IdP，请显式设置 `sso.saml.idp_signs_envelopes = false` 和 `sso.saml.idp_signs_assertions = false`。如果 IdP 会签名但其元数据不包含证书，SAML 后端现在会以 `missing_idp_certificate` 错误启动失败。
