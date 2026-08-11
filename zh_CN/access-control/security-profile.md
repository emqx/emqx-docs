# 安全配置方案

从 6.3 版本开始，EMQX 支持节点级的安全配置方案（Security Profile）。安全配置方案决定一组与安全相关的默认行为。EMQX 提供两种方案：

- `legacy`（默认）：保持早期 EMQX 版本的默认行为。
- `hardened`：采用严格的、默认安全的行为。

`hardened` 方案计划在未来的大版本中成为默认方案。新部署应直接使用 `hardened` 方案。已有部署可以在评估下文列出的行为变更后切换到 `hardened`。

## 选择方案

在启动节点前设置环境变量 `EMQX_SECURITY_PROFILE`：

```bash
export EMQX_SECURITY_PROFILE=hardened
```

EMQX 在启动时读取该变量一次。有效值为 `legacy`、`hardened` 或空（使用默认值）。其他值会导致节点启动失败。集群中的每个节点应设置相同的值。

::: tip
安全配置方案只改变默认行为。下文列出的大多数行为也可以单独配置，与所选方案无关。
:::

## Hardened 方案的行为变更

与 `legacy` 相比，`hardened` 方案改变以下行为。

### 监听器暴露

- **MQTT 监听器默认绑定回环地址。** MQTT TCP、SSL、WebSocket、安全 WebSocket 和 QUIC 监听器在 `bind` 省略或仅指定端口时，只监听回环接口。配置显式的绑定地址（例如 `bind = "0.0.0.0:1883"`）以接受外部连接。
- **Dashboard HTTP 监听器默认绑定回环地址。** Dashboard HTTP 监听器在 `bind` 省略或仅指定端口时，只监听回环接口。配置显式的绑定地址以接受外部连接。

### 认证

- **必须显式配置认证。** 当没有配置认证器，或所有认证器都被禁用时，客户端将被拒绝。要在某个监听器上显式允许匿名访问，设置该监听器的 `enable_authn = false`。
- **认证后端故障将拒绝访问。** 认证器后端错误、后端响应格式错误、认证器前置条件求值错误，以及 JWT 验证密钥不可用时，将拒绝客户端，而不是继续尝试下一个认证器。设置 `authentication_settings.ignore_backend_failures = true` 可允许回退到后续认证器。
- **JWT 认证器不忽略缺失的 JWT。** 客户端未提供配置的 JWT 字段时，JWT 认证器将拒绝该客户端。设置认证器的 `on_missing_jwt = ignore` 可允许这些客户端继续尝试下一个认证器。
- **验证 JWKS 出站 TLS。** JWT 认证器从 JWKS HTTPS 端点获取密钥时会验证对端证书和主机名。使用不受信任证书的端点将不可用。在特定 JWKS 端点上设置 `ssl.verify = verify_none` 可禁用验证。

### 授权

- **授权后端故障将拒绝操作。** 授权后端错误、规则格式错误和模板求值错误会直接拒绝发布或订阅操作，而不是继续尝试后续数据源或回退到 no-match 行为。设置 `authorization.ignore_backend_failures = true` 可忽略后端故障并继续下一个授权数据源。
- **默认文件授权数据源变为默认拒绝。** 内置文件数据源中最后一条 allow 规则仅在 `legacy` 方案下生效。在 `hardened` 方案下，未匹配任何规则的操作将落入 `authorization.no_match`，其默认值为 `deny`。在 ACL 文件末尾添加 `{allow, all}.` 规则可恢复宽松行为。
- **内部订阅需要授权。** 自动订阅（Auto Subscribe）等功能发起的订阅会经过主题验证、授权检查、能力检查和订阅钩子。特权管理类的强制订阅操作仍然绕过 MQTT 授权。

### 扩展

- **访问控制钩子故障将拒绝请求。** 认证或授权钩子抛出的异常会中断处理并拒绝请求。这对插件或 ExHook 扩展提供的自定义认证和授权尤其重要。

### Dashboard

- **不接受 Dashboard 默认凭据。** 使用默认密码 `public` 的本地 Dashboard 账户无法登录，包括升级前创建的管理员账户。切换到 `hardened` 方案前请先修改密码。
- **验证 SAML 签名。** SAML 单点登录要求响应信封和断言都带有签名。根据身份提供商的实际行为配置 `idp_signs_envelopes` 和 `idp_signs_assertions`。

## 迁移

将已有部署从 `legacy` 迁移到 `hardened`：

1. 逐项评估上述行为变更，在严格默认值不适用的地方应用显式配置。
2. 确认需要接受外部连接的监听器和 Dashboard 已配置显式绑定地址。
3. 确认每个节点都已配置认证，或在需要的地方显式启用匿名访问。
4. 修改仍在使用默认密码的 Dashboard 账户。
5. 在所有节点上设置 `EMQX_SECURITY_PROFILE=hardened`，然后逐个重启节点。

要保持原有行为，设置 `EMQX_SECURITY_PROFILE=legacy` 或不设置该变量。
