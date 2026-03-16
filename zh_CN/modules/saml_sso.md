# SAML 2.0 单点登录

SAML 2.0 单点登录（SSO）是 EMQX 企业版专属功能，支持用户通过组织内部的身份提供商（IDP，如 Keycloak、Okta、Azure AD 等）登录 EMQX Dashboard。用户在 IDP 完成认证后，EMQX 会自动完成用户初始化并跳转至 Dashboard，无需单独设置密码。

## 前提条件

在配置 SAML SSO 之前，请确认以下条件已满足：

- 已有可用的 SAML 2.0 兼容的身份提供商，且该 IDP 可正常访问。
- 已获取 IDP 的元数据 URL（通常为 IDP 提供的 XML 接口地址）。
- EMQX 节点与 IDP 之间网络可互通。

## 添加 SAML SSO 模块

1. 在 Dashboard 左侧导航栏中点击**模块**。
2. 点击**添加模块**。
3. 在模块列表中选择 **SAML 2.0 单点登录**，点击**选择**。
4. 根据下方说明填写配置参数。
5. 点击**添加**以启用模块。

   ![SAML SSO 模块配置](./assets/saml_sso_config.png)

### 配置参数说明

| 参数 | 类型 | 默认值 | 说明 |
|------|------|--------|------|
| **Dashboard 地址** | string | `https://127.0.0.1:18083` | Dashboard 的外部访问基础地址，末尾不带路径。EMQX 会使用该地址构造 SP ACS URL 和 SP 元数据 URL，并在 IDP 侧进行注册。 |
| **IDP 元数据 URL** | string | 必填 | EMQX 从该地址获取 IDP 的 SAML 元数据 XML。例如，Keycloak 的元数据地址格式为 `http://<keycloak>/realms/<realm>/protocol/saml/descriptor`。 |
| **SP 对认证请求签名** | boolean | `false` | 启用后，EMQX（作为 SP）会对发出的 SAML `AuthnRequest` 消息进行签名。启用此选项需要同时上传 SP 证书和私钥。 |
| **强制 SSO 用户启用 MFA** | boolean | `false` | 启用后，所有通过 SAML SSO 登录的用户在首次登录时必须配置基于 TOTP 的多因素认证。 |
| **要求 IDP 对响应信封签名** | boolean | `true` | 启用后，EMQX 要求 IDP 对 SAML `Response` 信封进行签名。关闭此选项将降低安全性，仅建议在测试环境中使用。 |
| **要求 IDP 对断言签名** | boolean | `true` | 启用后，EMQX 要求 IDP 对响应中的 SAML `Assertion` 元素进行签名。关闭此选项将降低安全性，仅建议在测试环境中使用。 |
| **SP 公钥/证书** | file | — | PEM 格式的 SP 证书。启用 **SP 对认证请求签名** 时必须填写。 |
| **SP 私钥** | file | — | PEM 格式的 SP 私钥。启用 **SP 对认证请求签名** 时必须填写。 |

## 配置 IDP（以 Keycloak 为例）

以下步骤以 Keycloak 为例，其他 IDP 的配置方式有所不同，但关键参数（ACS URL、Entity ID、元数据地址）的含义相同。

1. 登录 Keycloak 管理控制台。
2. 选择对应的 Realm，进入 **Clients** 页面。
3. 点击 **Create client**，Client 类型选择 **SAML**。
4. 将 **Client ID** 设置为 SP Entity ID，该地址为 EMQX 发布的元数据地址：

   ```
   http://<dashboard-addr>/api/v4/sso/saml/metadata
   ```

5. 将 **Valid Redirect URIs** 和 **ACS URL** 设置为：

   ```
   http://<dashboard-addr>/api/v4/sso/saml/acs
   ```

6. 保存 Client 配置，然后获取 IDP 元数据 URL，格式如下：

   ```
   http://<keycloak>/realms/<realm>/protocol/saml/descriptor
   ```

7. 将该 URL 填入 EMQX Dashboard 中添加 SAML SSO 模块时的 **IDP 元数据 URL** 字段。

::: tip

如果启用了签名功能，请从 Keycloak 下载 IDP 签名证书，同时确保上传至 EMQX 的 SP 证书已在 IDP 侧完成信任配置。

:::

## SP 元数据

模块启用后，EMQX 会在以下地址发布 SP 元数据：

```
GET /api/v4/sso/saml/metadata
```

响应内容为标准 SAML 元数据 XML 文档。支持自动导入 SP 元数据的 IDP 可直接使用该 URL，也可以下载 XML 文件后手动上传至 IDP。

SP 元数据包含以下信息：

- SP 的 Entity ID
- ACS（断言消费服务）地址：`http://<dashboard-addr>/api/v4/sso/saml/acs`
- SP 签名证书（启用 SP 签名时包含）

## SSO 登录流程

完整的 SAML SSO 登录流程如下：

1. 用户打开 Dashboard 登录页面。前端调用 `GET /api/v4/sso/status` 检查 SSO 是否已启用，若已启用则显示**使用 SSO 登录**按钮。
2. 用户点击**使用 SSO 登录**，前端向 `POST /api/v4/sso/saml/login` 发起请求。
3. EMQX 返回 `302` 重定向，将浏览器跳转至 IDP 认证页面。
4. 用户在 IDP 完成身份认证（输入凭据、在 IDP 侧完成 MFA 等操作）。
5. IDP 将 `SAMLResponse` 以 POST 方式提交至 EMQX 的 ACS 端点：`POST /api/v4/sso/saml/acs`。
6. EMQX 验证断言，对不存在的用户进行自动初始化，并将浏览器重定向回 Dashboard，携带 `login_meta` 令牌完成登录。

### 用户自动初始化

首次通过 SSO 成功登录的用户会被自动初始化（即时供给，Just-in-Time Provisioning）：

- 默认分配 `viewer` 角色。
- 已存在于 Dashboard 中且用户名匹配的用户，将保留当前角色和设置不变。

如需为 SSO 用户授予更高权限，可在用户首次登录后，前往 **Dashboard → 用户** 编辑该用户记录。

## 签名配置

EMQX 提供三种独立的签名机制，每项均通过单独的配置开关控制，可独立启用或禁用。

| 机制 | 配置项 | 签名方 | 方向 |
|------|--------|--------|------|
| SP 对 `AuthnRequest` 签名 | **SP 对认证请求签名** | EMQX（SP） | SP → IDP |
| IDP 对 `Response` 信封签名 | **要求 IDP 对响应信封签名** | IDP | IDP → SP |
| IDP 对 `Assertion` 签名 | **要求 IDP 对断言签名** | IDP | IDP → SP |

::: warning

在生产环境中，**要求 IDP 对响应信封签名**和**要求 IDP 对断言签名**中至少应启用一项。同时禁用两者意味着放弃对身份断言的所有密码学验证，存在严重安全风险，仅可在隔离的测试环境中使用。

:::

启用 **SP 对认证请求签名** 后，必须上传 SP 证书和 SP 私钥，并在 IDP 侧注册该证书，以便 IDP 可以验证签名请求的合法性。

## MFA 集成

启用 **强制 SSO 用户启用 MFA** 后，所有通过 SAML SSO 登录的用户在首次登录成功后，都需要完成基于 TOTP 的多因素认证配置。此后每次登录，在 SAML 断言验证通过后，还需额外输入有效的 TOTP 验证码。

即使全局启用了**强制 SSO 用户启用 MFA**，管理员仍可为单个用户单独关闭 MFA。操作方法：进入 **Dashboard → 用户**，找到对应用户，关闭其 MFA 选项即可。

有关 MFA 的完整配置说明，请参考 MFA 相关文档。

## API 参考

以下端点支持 SAML SSO 功能流程。标注为「公开」的端点无需 Dashboard 认证凭据即可访问。

### GET /api/v4/sso/status

检查 SSO 是否当前已启用。此端点为公开端点，无需认证。

**响应示例：**

```json
{
  "code": 0,
  "data": {
    "enabled": true,
    "providers": [
      {
        "type": "saml",
        "enabled": true
      }
    ]
  }
}
```

### POST /api/v4/sso/saml/login

发起 SAML 登录，返回 `302` 重定向至 IDP 认证页面。

### POST /api/v4/sso/saml/acs

SAML 断言消费服务端点。用户在 IDP 完成认证后，IDP 将 `SAMLResponse` POST 至此地址。EMQX 验证响应内容，按需初始化用户，并将浏览器重定向回 Dashboard。

::: tip

此端点由 IDP 调用，而非浏览器直接访问。在 IDP 侧配置 EMQX SP 客户端时，请将此 URL 填入 ACS URL 字段。

:::

### GET /api/v4/sso/saml/metadata

返回 SP 元数据 XML 文档。可将此 URL 用于 IDP 的自动 SP 元数据导入，或下载 XML 文件后手动上传至 IDP。
