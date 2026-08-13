# 安全检查清单

本清单可帮助您在正式投入生产环境之前，对整体安全配置进行系统检查。内容按安全层次组织，便于从操作系统层逐层检查至 Dashboard。建议在首次上线前、拓扑发生重大变更后，以及定期安全巡检时使用。

## 阶段 1：基础设施与操作系统

- 根据预期连接规模，提升操作系统文件描述符上限以及服务级别的 `LimitNOFILE` 配置，避免节点在正常流量或恶意连接压力下提前耗尽资源。
- 针对长连接 MQTT 流量加固 TCP 栈和防火墙策略，包括 SYN Flood 防护、连接跟踪容量以及仅在受信任接口上暴露监听器。
- 仅暴露客户端实际需要的监听器。在不可信网络上，优先使用 `8883`、`8084` 等加密监听器，并将 `1883` 等明文监听器限制在内网或过渡性场景中使用。详见[监听器配置](../configuration/listener.md)和[开启 SSL/TLS 连接](../network/emqx-mqtt-tls.md)。
- 使用安全组或防火墙规则限制节点间通信端口。关于集群内部使用的端口映射规则，详见[集群安全](../deploy/cluster/security.md)。
- 如果节点存在多个网卡接口，应将 Erlang 分布式通信仅绑定到私网接口。
- 如果 EMQX 部署在负载均衡器或 TCP 代理之后，仅应在确实需要获取真实客户端 IP 或客户端证书信息的监听器上启用 [Proxy Protocol](../deploy/cluster/lb.md)。
- 如果某个监听器启用了 Proxy Protocol，则该监听地址和端口只应暴露给指定的代理或负载均衡器，不能再直接对公网客户端开放。可在 EMQX 中通过 `listeners.{type}.{name}.access_rules = ["allow <trusted-LB-CIDR>", "deny all"]` 进行限制，并结合网络层控制（防火墙、私有网络或 Unix socket）。否则，能直接访问该端口的客户端可以伪造任意对端证书字段的 PROXY v2 帧，从而冒充任意身份。
- 如果 WebSocket 监听器（`ws` 或 `wss`）前面没有会覆盖 `x-forwarded-for` 请求头的受信任代理，应设置 `listeners.{type}.{name}.websocket.proxy_address_header = ""`（以及 `websocket.proxy_port_header = ""`），使基于 IP 的授权规则、客户端封禁、连接抖动检测和审计日志使用真实的 TCP 对端地址。一旦启用该请求头，除非受信任代理覆盖它，派生出的源 IP 就是由客户端提供的——仅向入站请求头追加内容的代理并不能提供保护。详见 [WebSocket 监听器的转发客户端地址](../configuration/listener.md#websocket-监听器的转发客户端地址)。

## 阶段 2：Erlang 与集群

- 在所有集群节点上替换默认 Erlang Cookie，并确保所有节点使用相同的高熵机密值。详见[集群安全](../deploy/cluster/security.md)。
- 对 `emqx.conf`、ACL 文件、证书、私钥以及其他敏感配置文件设置严格的文件权限，并使用安全的密钥管理流程进行分发和轮换。
- 尽可能将 secret 类型字段存储为 `file://` 引用，而不是直接写入明文值。对于所有标记为 secret 的字段（例如 SSL 密钥口令、桥接和连接器的密码、API Key），将其值设置为 `file:///path/to/secret`，EMQX 会在启动以及每次配置重载时从该文件读取实际值。这样可以避免明文机密出现在配置文件、API 请求体、配置备份和版本控制中，降低共享或导出配置时泄露机密的风险。详见[从文件加载 Secret](../configuration/secret-from-file.md)。
- 保持集群端口仅在内网开放；当节点间流量经过低信任网络或公有云边界时，应为节点间通信启用 TLS。详见[集群安全](../deploy/cluster/security.md)。
- 在新增节点、迁移网络或调整部署拓扑后，应重新检查防火墙规则、证书配置以及集群成员控制策略。

## 阶段 3：传输层安全

- 当流量经过不可信网络时，应为生产环境中的 MQTT 监听器启用 TLS。详见[网络与 TLS](../network/overview.md)。
- 根据组织的安全基线禁用过时的 TLS 协议版本和弱密码套件，并在发布前于测试环境验证监听器的最终配置。
- 使用受信任 CA 或内部 PKI 签发的证书，并在证书到期前完成轮换。
- 当设备身份需要通过客户端证书建立信任时，应启用双向 TLS。在该模式下，需要同时校验证书链以及客户端在 TLS 握手期间是否实际提供证书。详见[X.509 证书认证](./authn/x509.md)。
- 若将对端证书字段映射为 MQTT 用户名或客户端 ID（`peer_cert_as_username` / `peer_cert_as_clientid`），监听器**必须**强制启用 mTLS（`verify = verify_peer`、`fail_if_no_peer_cert = true`），并使用您自己掌控的 CA 证书集合。否则，客户端可以出示一张 CN/DN 任意填写的自签名证书，从而冒充任意身份。针对空用户名场景，可在监听器上同时设置 `listeners.{type}.{name}.enable_authn = quick_deny_anonymous` 作为补充防护。详见[证书信息映射](./authn/x509.md#证书信息映射)。
- 如果您的环境要求校验证书吊销状态，可评估启用 [CRL 检查](../network/crl.md)或 [OCSP Stapling](../network/ocsp.md)。
- 当 EMQX 连接外部资源（例如 HTTP 认证服务、数据库或其他集成组件）时，也应启用 TLS。

## 阶段 4：MQTT 访问控制与资源保护

- 在将公共监听器暴露到生产环境之前，至少配置一种认证方式。默认情况下，如果未启用认证，EMQX 将允许所有客户端连接。详见[认证](./authn/authn.md)。
- 优先使用每设备或每应用独立凭据，避免多个客户端共享用户名、密码或证书。
- 在认证机制支持时，将 MQTT 客户端 ID 与认证身份绑定。例如，校验 JWT 的 `clientid` 声明、使用 [`peer_cert_as_clientid`](./authn/x509.md#证书信息映射) 映射证书字段、让 HTTP 认证器拒绝不匹配的请求，或将认证器与 [Client-Info](./authn/cinfo.md) 规则配合使用。缺少此类绑定时：
  - 泄露的凭据可能使攻击者使用随机客户端 ID 和较长的[会话过期间隔](../messaging/mqtt-concepts.md)不受限制地创建会话，导致空闲持久会话持续累积，直至耗尽 Broker 内存。
  - 持有任意有效凭据的攻击者如果知道受害者的客户端 ID，就可以接管受害者的会话。MQTT 仅使用客户端 ID 标识和恢复会话。当攻击者使用相同的客户端 ID 连接时，EMQX 会断开受害者客户端的连接。对于 MQTT 5.0 客户端，EMQX 会发送原因码为 `0x8E`（`Session taken over`）的 `DISCONNECT` 报文。
  - 当 `Clean Start = 0` 时，攻击者会恢复受害者的会话并继承其已有订阅。EMQX 在创建订阅时执行授权检查，不会根据恢复会话的新身份重新检查继承的订阅。因此，攻击者可能收到其自身授权规则本应拒绝的消息。

  将客户端 ID 与认证身份绑定后，认证机制会在连接阶段拒绝身份不匹配的连接，从而阻止此类会话接管。该订阅继承风险不影响发布操作，因为 EMQX 会根据当前身份对每次发布执行授权检查。
- 根据实际信任模型选择认证机制，例如 X.509、JWT、SCRAM，或基于安全后端数据库的密码认证。
- 使用密码认证时，应存储加盐哈希后的密码，而不是明文密码，并优先采用 `bcrypt`、`pbkdf2` 等强哈希算法。
- 尽可能最小化主题权限范围，并谨慎审查通配符规则。详见[授权](./authz/authz.md)。
- 当 ACL 主题模板中包含 `${clientid}`、`${username}` 或 `${client_attrs.X}` 占位符时（参见[授权占位符](./authz/authz.md#authorization-placeholders)），必须对这些身份字段进行校验，禁止其包含 MQTT 主题通配符（`+`、`#`）或主题分隔符（`/`）。例如模板 `clients/${clientid}/data` 在未做校验的情况下，若客户端 ID 为 `+`，整段模板会退化为通配模式，可访问其他客户端的所有子主题；若取值为 `tenantA/+` 或含有 `/`，则会突破原本分配的主题子树范围。应在认证侧强制约束身份格式，例如通过 [Client-Info](./authn/cinfo.md) 规则、JWT 声明的正则校验或 HTTP 认证拒绝不合规的请求。应在握手阶段直接拒绝连接，而不是寄希望于 ACL 兜底处理非法替换值。
- 在设计发往外部服务的请求时，包括 HTTP [认证](./authn/http.md)、HTTP [授权](./authz/http.md)，以及数据集成的连接器、桥接与动作，应通过 EMQX 能识别的敏感字段或请求头传递密码、令牌、密钥等敏感信息。这样，当这些数据经过 EMQX 的脱敏处理时，其中的敏感值会在相关日志、追踪和配置 API 响应中显示为 `******`，而不是明文。脱敏是依据字段名或请求头名进行的：对于放在 HTTP 请求头中的凭据，应使用标准的 `Authorization`（或 `Proxy-Authorization`）请求头，EMQX 会始终对其脱敏；对于放在其他配置字段中的密钥，应将字段命名为已识别的敏感键名，例如 `password`、`token`、`secret`、`secret_key` 或 `jwt`。非标准的自定义请求头（例如 `x-custom-secret`）或不符合约定的字段名不会被识别，其值可能在 `debug` 级别日志和错误信息中以明文出现。
- 在生产环境依赖授权能力之前，应移除或调整过于宽松的默认规则。
- 对于基于文件的 ACL，可在适用场景下采用默认拒绝策略，例如以 `{deny, all}` 作为结尾规则，并设置 `authorization.no_match = deny`。详见[使用 ACL 文件](./authz/file.md)。
- 对于暴露在不受信任网络或公共网络中的 Broker，可考虑设置 `authorization.deny_action = disconnect`（默认值为 `ignore`）。如果客户端尝试发布或订阅未授权主题，EMQX 会断开该客户端连接，而不是保持连接打开。配合[连接抖动检测](./flapping-detect.md)使用时，反复重连并触发授权拒绝的客户端会被自动封禁。`deny_action` 是全局配置，因此同样会断开尝试了被拒绝操作的合法客户端。建议在客户端正常只发布和订阅已授权主题的场景下使用。应调整连接抖动检测阈值，避免正常重连风暴导致客户端被封禁。详见[授权](./authz/authz.md)。
- 检查授权缓存配置以及 Authorizer 的执行顺序，确保策略变更能够按预期生效。
- 限制 MQTT 资源使用范围，降低异常客户端或恶意客户端的影响面，例如检查报文大小、主题层级、订阅数量、Inflight 窗口和排队消息等限制。详见[MQTT 配置](../configuration/mqtt.md)。
- 在需要时，对监听器启用速率限制，控制连接突发和消息突发。详见[速率限制器配置](../configuration/limiter.md)。
- 在需要时，使用[黑名单](./blacklist.md)和[连接抖动检测](./flapping-detect.md)抑制异常或不稳定客户端。
- 如果启用了集群连接（Cluster Linking），请在接收对端连入的监听器上强制认证，将 `$LINK/` 控制命名空间限定给专用的集群连接 ClientID，并拒绝其它任何客户端访问。详见[集群连接安全加固](../cluster-linking/security.md)。

## 阶段 5：管理面与运维维护

- 在生产环境使用前修改 Dashboard 默认密码，并定期审查谁拥有管理权限。详见[系统](../dashboard/system.md)。
- 仅在受信任网络上暴露 Dashboard。管理员访问应优先使用 HTTPS，并尽可能将 Dashboard 监听器绑定到 localhost、私网地址或受保护的管理网络。详见[Dashboard 配置](../configuration/dashboard.md)。
- 在**管理** -> **集群配置** -> **规则引擎安全**中启用 SSRF 防护，以在测试、创建或更新 HTTP、MQTT 连接器配置时校验目标地址。从 EMQX 6.0.4 开始，该策略不覆盖其他连接器类型或运行时连接。如果允许委派管理员创建或修改规则引擎资源，或需要完整的出站网络边界，请增加主机级出站访问控制。详见[规则引擎安全](../dashboard/cluster_settings.md#规则引擎安全)和[结合规则引擎策略与防火墙规则防御 SSRF](../deploy/cluster/security.md#结合规则引擎策略与防火墙规则防御-ssrf)。
- 如果开放管理 API，应使用 API Key 而不是 Dashboard 用户凭据进行调用，只授予所需的最小权限，并尽可能设置过期时间。详见[REST API](../admin/api.md)和[系统](../dashboard/system.md#api-key)。
- 如果您使用的是 EMQX 企业版，可为管理用户配置[单点登录（SSO）](../dashboard/sso.md)，并在身份提供方侧启用 MFA（如果可用）。
- 定期执行备份并演练恢复流程。请注意，若证书或 ACL 文件存放在 EMQX 数据目录之外，则需要单独备份。详见[备份与恢复](../operations/backup-restore.md)。
- 在可用场景下启用审计能力，并将日志与指标统一接入可观测性平台，用于异常检测和事件响应。详见[审计日志](../dashboard/audit-log.md)、[日志配置](../configuration/logs.md)和[日志与可观测性](../observability/overview.md)。

## 变更后重新验证

- 在证书轮换、监听器变更、负载均衡器调整、集群扩容、备份策略变化，或认证与授权链发生变更后，应重新执行本清单。
- 在生产切换前验证关键失败场景是否符合预期，例如匿名客户端被拒绝、无效证书握手失败，以及越权的发布或订阅请求被正确拒绝。
