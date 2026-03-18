# Dashboard 用户与角色管理

EMQX Dashboard 支持多用户访问，并提供基于角色的访问控制（RBAC）。每个 Dashboard 账户都会被分配一个角色，该角色决定了账户可以查看和修改的内容。本页介绍如何通过 Dashboard 界面和 API 两种方式管理 Dashboard 用户与角色。

![用户列表](./assets/dashboard_users.png)

## 角色说明

EMQX Dashboard 内置两种角色：

| 角色 | 说明 |
|------|------|
| `administrator` | 拥有所有 Dashboard 功能和 REST API 的完整访问权限。可管理用户、模块、规则、客户端及集群的所有配置。 |
| `viewer` | 只读权限。可查看监控数据、客户端列表、订阅信息和统计数据，但无法修改任何配置。 |

:::tip
默认账户 `admin` 拥有 `administrator` 角色，且无法被删除。在生产环境部署前，请务必修改其密码。
:::

## 调用 Dashboard API

Dashboard 用户凭证（用户名和密码）可直接用于调用 Dashboard API（端口 18083），适用于需要程序化操作 Dashboard 的场景，例如自动化用户管理或 API Key 管理。

:::tip
如需通过程序方式访问客户端管理、规则引擎、插件等集成接口，应使用**管理 API（端口 8081）**及 API Key，详见 [API Key](../advanced/api-key-permission.md)。
:::

D支持以下两种认证方式：

### Basic Auth（基础认证）

将用户名和密码经 Base64 编码后放入 `Authorization` 请求头：

```
Authorization: Basic base64(username:password)
```

示例：

```bash
curl -u admin:public "http://127.0.0.1:18083/api/v4/users/"
```

### Bearer Token（令牌认证）

Bearer Token 仅适用于已启用 MFA 的用户。完成 MFA 登录流程后，将获得一个会话令牌，将其放入 `Authorization` 请求头即可用于后续 API 调用：

```
Authorization: Bearer <token>
```

示例：

```bash
curl -H "Authorization: Bearer <token>" \
  "http://127.0.0.1:18083/api/v4/users/"
```

获取 Bearer Token 的完整流程详见 [Dashboard 多因素认证](./dashboard-mfa.md)。

:::warning
注销登录或删除用户账户后，对应的 Bearer Token 将立即失效。请妥善保管令牌，避免将其嵌入客户端代码中。
:::

以下各节的 API 示例均使用 Basic Auth（`-u admin:public`），可按需替换为 Bearer Token。

## 用户管理

本节介绍如何创建、查看、更新和删除 Dashboard 用户，以及如何修改用户密码。所有操作均支持通过 Dashboard 界面或 REST API 执行，且需要 `administrator` 角色。

### 创建用户

**通过 Dashboard：**

1. 在左侧导航菜单中点击**用户**。
2. 点击**添加用户**。
3. 填写用户名、密码、角色及可选描述。
4. 点击**确认**。

**通过 API：**

```bash
curl -i -X POST "http://127.0.0.1:18083/api/v4/users/" \
  -u admin:public \
  -H "Content-Type: application/json" \
  -d '{"username":"newuser","password":"Password123!","role":"viewer"}'
```

**请求参数：**

| 字段 | 类型 | 是否必填 | 说明 |
|------|------|----------|------|
| `username` | String | 是 | 用户名，允许使用字母、数字、下划线和连字符。 |
| `password` | String | 是 | 密码，长度为 8-64 个字符，至少包含以下两种字符类型：字母、数字、特殊字符。仅支持 ASCII 字符。 |
| `role` | String | 否 | `administrator` 或 `viewer`，默认为 `viewer`。 |
| `tags` | String | 否 | 用户的可选描述信息。 |
| `enable_mfa` | Boolean | 否 | 设置为 `true` 时，该用户在首次登录时将被要求配置 MFA。详见 [Dashboard 多因素认证](./dashboard-mfa.md)。 |

**响应示例：**

```json
{
  "code": 0
}
```

### 查看用户列表

**通过 Dashboard：**

在左侧导航菜单中点击**用户**，即可查看所有 Dashboard 用户及其角色信息。

**通过 API：**

```bash
curl -u admin:public "http://127.0.0.1:18083/api/v4/users/"
```

**响应示例：**

```json
{
  "code": 0,
  "data": [
    {
      "username": "admin",
      "tags": "administrator",
      "role": "administrator",
      "mfa_enabled": false,
      "mfa_setup_required": false
    }
  ]
}
```

**响应字段说明：**

| 字段 | 类型 | 说明 |
|------|------|------|
| `username` | String | 用户名 |
| `tags` | String | 用户描述信息 |
| `role` | String | 分配的角色：`administrator` 或 `viewer` |
| `mfa_enabled` | Boolean | 该用户当前是否已启用 MFA |
| `mfa_setup_required` | Boolean | 该用户下次登录时是否需要完成 MFA 配置 |

### 更新用户

此接口用于更新用户的角色或描述信息。不支持通过此接口修改用户名或密码。

**通过 Dashboard：**

1. 在左侧导航菜单中点击**用户**。
2. 在用户列表中点击目标用户的**编辑**按钮。
3. 修改角色或描述信息后点击**确认**。

**通过 API：**

```bash
curl -i -X PUT "http://127.0.0.1:18083/api/v4/users/newuser" \
  -u admin:public \
  -H "Content-Type: application/json" \
  -d '{"role":"administrator"}'
```

**请求参数：**

| 字段 | 类型 | 是否必填 | 说明 |
|------|------|----------|------|
| `role` | String | 否 | 新角色：`administrator` 或 `viewer` |
| `tags` | String | 否 | 更新后的描述信息 |

**响应示例：**

```json
{
  "code": 0
}
```

### 删除用户

**通过 Dashboard：**

1. 在左侧导航菜单中点击**用户**。
2. 在用户列表中点击目标用户的**删除**按钮。
3. 在确认弹窗中点击**确认**。

**通过 API：**

```bash
curl -i -X DELETE "http://127.0.0.1:18083/api/v4/users/newuser" \
  -u admin:public
```

:::danger
内置的 `admin` 用户不可删除，尝试删除将返回错误。
:::

:::warning
删除用户后，该用户的 MFA 配置将立即被清除，所有有效 Token 也会失效。该用户的所有活跃会话将被终止。
:::

### 修改密码

用户可以修改自己的密码。管理员可以修改任意用户的密码。

**通过 Dashboard：**

1. 在左侧导航菜单中点击**用户**。
2. 在用户列表中点击目标用户的**编辑**按钮。
3. 填写新密码后点击**确认**。

**通过 API：**

```bash
curl -i -X PUT "http://127.0.0.1:18083/api/v4/change_pwd/newuser" \
  -u admin:public \
  -H "Content-Type: application/json" \
  -d '{"old_pwd":"OldPassword1!","new_pwd":"NewPassword2@"}'
```

**请求参数：**

| 字段 | 类型 | 是否必填 | 说明 |
|------|------|----------|------|
| `old_pwd` | String | 是 | 当前密码 |
| `new_pwd` | String | 是 | 新密码 |

**密码规则：**
- 长度为 8 到 64 个字符
- 至少包含以下两种字符类型：字母、数字、特殊字符
- 仅支持 ASCII 字符

## SSO 用户

当用户首次通过 SAML 单点登录（SSO）进行认证时，EMQX 会自动为其创建一个 Dashboard 账户，并分配 `viewer` 角色。

- SSO 用户的内部密码为随机生成，无法直接使用用户名和密码方式登录。
- 管理员可通过更新用户 API 或 Dashboard 用户页面修改 SSO 用户的角色。
- SAML SSO 的配置详情请参考 [SAML 2.0 单点登录](../modules/saml_sso.md)文档。

:::tip
如需为 SSO 用户授予管理员权限，可通过 `PUT /api/v4/users/:username` 接口将其角色更新为 `"role": "administrator"`。
:::

## API 参考

| 接口路径 | 请求方式 | 说明 |
|----------|----------|------|
| `/api/v4/auth` | POST | 登录 |
| `/api/v4/auth` | DELETE | 注销 |
| `/api/v4/users/` | GET | 获取用户列表 |
| `/api/v4/users/` | POST | 创建用户 |
| `/api/v4/users/:username` | PUT | 更新用户 |
| `/api/v4/users/:username` | DELETE | 删除用户 |
| `/api/v4/change_pwd/:username` | PUT | 修改用户密码 |

MFA 相关接口详见 [Dashboard 多因素认证 — API 参考](./dashboard-mfa.md#api-参考)。
