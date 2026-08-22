# 安全配置方案

从 6.3 版本开始，EMQX 支持节点级的安全配置方案（Security Profile）。安全配置方案决定一组与安全相关的默认行为。EMQX 提供两种方案：

- `legacy`（默认）：保持早期 EMQX 版本的默认行为。
- `hardened`：采用更严格的安全默认行为。

EMQX 计划从 7.0 版本开始默认使用 `hardened`。首次部署时，建议使用 `hardened`。将部署从 `legacy` 迁移到 `hardened` 前，请评估下文列出的行为变更。

## 选择方案

安全配置方案由环境变量 `EMQX_SECURITY_PROFILE` 选择。请在 `emqx.env` 文件中设置该变量，`emqx` 命令的每次调用都会加载此文件：

- rpm 和 deb 安装：`/etc/emqx/emqx.env`
- Docker 镜像：`/opt/emqx/etc/emqx.env`
- tar.gz 安装：`etc/emqx.env`

取消文件中 `EMQX_SECURITY_PROFILE` 行的注释并设置值：

```bash
EMQX_SECURITY_PROFILE=hardened
```

然后重启节点。

`emqx.env` 中的值会覆盖从环境继承的变量，包升级会保留对该文件的修改。也可以直接在环境中设置该变量，例如使用 `docker run -e EMQX_SECURITY_PROFILE=hardened`，或在前台启动前执行 `export EMQX_SECURITY_PROFILE=hardened`。

EMQX 在启动时读取该变量一次，读取发生在解析配置文件之前，因此无法在配置文件中设置安全配置方案。有效值为 `legacy`、`hardened` 或空（使用默认值）。其他值会导致节点启动失败。集群中的每个节点应设置相同的值。

::: tip
安全配置方案只改变默认行为。下文列出的大多数行为也可以单独配置，与所选方案无关。
:::

## `hardened` 方案的行为变更

与 `legacy` 相比，`hardened` 方案改变以下行为。

### 节点和集群安全

- **拒绝已知的不安全 Erlang cookie。** 如果节点使用内置的默认 Erlang cookie 或常用示例值 `emqxsecretcookie`，节点将无法启动。启动 EMQX 前，请配置非默认的 `node.cookie` 或设置 `EMQX_NODE__COOKIE`。集群中的所有节点必须使用相同的 cookie。

### 监听器暴露

- **MQTT 监听器默认绑定回环地址。** MQTT TCP、SSL、WebSocket、安全 WebSocket 和 QUIC 监听器在 `bind` 省略或仅指定端口时，只监听回环接口。配置显式的绑定地址（例如 `bind = "0.0.0.0:1883"`）以接受外部连接。
- **Dashboard HTTP 监听器默认绑定回环地址。** Dashboard HTTP 监听器在 `bind` 省略或仅指定端口时，只监听回环接口。配置显式的绑定地址以接受外部连接。

### 认证

- **必须显式配置认证。** 当没有配置认证器，或所有认证器都被禁用时，客户端将被拒绝。要在某个监听器上显式允许匿名访问，设置该监听器的 `enable_authn = false`。
- **认证后端故障将拒绝访问。** 发生认证器后端错误、后端响应格式错误或认证器前置条件求值错误，或者 JWT 验证密钥不可用时，EMQX 将拒绝客户端，而不是继续尝试下一个认证器。设置 `authentication_settings.ignore_backend_failures = true` 可允许回退到后续认证器。
- **JWT 认证器不忽略缺失的 JWT。** 客户端未提供配置的 JWT 字段时，JWT 认证器将拒绝该客户端。设置认证器的 `on_missing_jwt = ignore` 可允许这些客户端继续尝试下一个认证器。
- **在混合认证链中，非 JWT 凭据必须跳过 JWT 认证器。** JWT 认证器收到格式错误的 JWT 时，认证结果为失败。当 JWT 认证器和后续的密码认证器从同一字段（例如 `password`）读取 JWT 或密码时，设置 JWT 认证器的 `precondition = "is_jwt(password)"`，使普通密码继续交给下一个认证器处理。
- **验证 JWKS 出站 TLS。** JWT 认证器从 JWKS HTTPS 端点获取密钥时会验证对端证书和主机名。使用不受信任证书的端点将不可用。在特定 JWKS 端点上设置 `ssl.verify = verify_none` 可禁用验证。

### 授权

- **授权后端故障将拒绝操作。** 授权后端错误、规则格式错误和模板求值错误会直接拒绝发布或订阅操作，而不是继续尝试后续数据源或回退到未匹配规则时的处理逻辑。设置 `authorization.ignore_backend_failures = true` 可忽略后端故障并继续下一个授权数据源。
- **授权主题模板替换值中的禁用字符将拒绝操作。** 默认情况下，替换值包含 `/`、`+` 或 `#` 时，该规则在 `legacy` 方案下视为未匹配，在 `hardened` 方案下直接拒绝操作。例如，客户端 ID 为 `i/am/+/good/#` 的客户端匹配规则 `{allow, all, all, ["t/${clientid}/#"]}.` 时即属于这种情况。可通过 `authorization.topic_template_allow.slash`、`authorization.topic_template_allow.plus` 或 `authorization.topic_template_allow.hash` 单独放行对应字符。
- **默认文件授权数据源变为默认拒绝。** 默认 `acl.conf` 的最后一条规则是 `{allow, {security_profile, legacy}}.`，该规则在 `legacy` 方案下允许操作，在 `hardened` 方案下不生效。在 `hardened` 方案下，未匹配任何规则的操作将落入 `authorization.no_match`，其默认值为 `deny`。将最后一条规则改为 `{allow, all}.` 可恢复宽松行为。`{security_profile, legacy}` 和 `{security_profile, hardened}` 条件可用于 `acl.conf` 中的任意规则（包括在 `and` 和 `or` 表达式中），使自定义规则仅在所选方案下生效。
- **内部订阅需要授权。** 自动订阅（Auto Subscribe）等功能发起的订阅会经过主题验证、授权检查、MQTT 能力检查和订阅钩子。特权管理类的强制订阅操作仍然绕过 MQTT 授权。

### 延迟发布

- **重放延迟消息时重新授权。** EMQX 使用调度消息时保存的授权上下文，根据重放时的发布授权规则和封禁记录重新检查消息。调度时已通过授权的消息可能在重放时被丢弃。

::: warning 重要提示
在 `hardened` 方案下，EMQX 会丢弃升级前创建的待处理延迟消息，因为这些消息不包含授权上下文。`legacy` 方案仍会重放这些消息。
:::

### 扩展

- **访问控制钩子故障将拒绝请求。** 认证或授权钩子抛出的异常会中断处理并拒绝请求。这对插件或 ExHook 扩展提供的自定义认证和授权尤其重要。
- **ExHook `message.publish` 失败将拒绝发布。** 如果没有可用的 ExHook 服务端，或者 `failed_action` 为 `deny` 的 ExHook 服务端在处理 `message.publish` 时失败，EMQX 将阻止消息发布。在 `legacy` 方案下，相同的失败不会阻止消息发布。

### Dashboard

- **不接受 Dashboard 默认凭据。** 使用默认密码 `public` 的本地 Dashboard 账户无法登录，包括升级前创建的管理员账户。切换到 `hardened` 方案前请先修改密码。
- **验证 SAML 签名。** SAML 单点登录要求响应信封和断言都带有签名。根据身份提供商的实际行为配置 `idp_signs_envelopes` 和 `idp_signs_assertions`。

## 滚动升级

集群中的所有节点必须使用相同的安全配置方案。节点之间方案不一致时，访问控制结果将取决于客户端连接到哪个节点。运行 6.3 之前版本的节点始终按 `legacy` 方案运行。

从 6.3 之前的版本执行滚动升级时：

1. 不要在已升级的节点上设置 `EMQX_SECURITY_PROFILE=hardened`。保持该变量为空，或将其设置为 `legacy`，使已升级节点与仍在运行旧版本的节点行为一致。
2. 在所有节点上完成滚动升级。
3. 之后再按照下文的迁移步骤将集群切换到 `hardened`。

## 迁移

将已有部署从 `legacy` 迁移到 `hardened`：

1. 逐项评估上述行为变更，在严格默认值不适用的地方应用显式配置。
2. 配置非默认的 Erlang cookie，并确认集群中的每个节点使用相同的值。
3. 确认需要接受外部连接的监听器和 Dashboard 已配置显式绑定地址。
4. 确认每个节点都已配置认证，或在需要的地方显式启用匿名访问。
5. 修改仍在使用默认密码的 Dashboard 账户。
6. 升级后启用 `hardened` 前，等待升级前创建的待处理延迟消息完成重放；否则，应接受 EMQX 将丢弃这些消息。
7. 在所有节点上设置 `EMQX_SECURITY_PROFILE=hardened`，然后逐个重启节点。

要保持原有行为，设置 `EMQX_SECURITY_PROFILE=legacy` 或不设置该变量。
