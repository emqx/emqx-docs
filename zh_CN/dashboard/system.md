# 系统设置

EMQX Dashboard 中的**系统设置**菜单提供一系列管理功能入口，包括用户与角色管理、审计日志、API 密钥、许可证、单点登录（SSO）、数据备份与恢复以及通用设置。

## 用户

**用户**页面提供了所有活跃的 Dashboard 用户的概览，包括通过[命令行](../admin/cli.md)生成的用户。

要添加新用户，只需点击页面右上角的**创建**按钮。一个弹出的对话框将提示您输入必要的用户详细信息。输入完毕后，点击**创建**按钮即可生成用户帐户。对于进一步的用户管理，如编辑用户信息、更新密码或删除用户，您可以通过**操作**列轻松访问这些选项。

> EMQX 开源版本不提供基于角色的权限管理能力，所有的用户都有管理员权限可删除其他用户，但无法在 Dashboard 上删除当前登录用户。
> 出于安全考虑，从 EMQX 5.0.0 开始 Dashboard 用户无法用于 REST API 认证。

<img src="./assets/ee-users.png" alt="image" style="zoom:67%;" />

### 基于角色的访问控制

从 EMQX 5.3 开始，Dashboard 用户引入了 基于角色的访问控制 （RBAC）功能。RBAC 允许根据用户在组织中的角色为其分配权限。此功能简化了授权管理，通过限制访问权限提高安全性，并改善组织合规性，因此是 Dashboard 必不可少的访问控制机制。

目前，可以为用户设置以下两种预定义角色之一。您可以在创建用户时从**角色**下拉菜单中选择角色。
+ **管理员**

    管理员拥有对 EMQX 所有功能和资源的完全管理访问权限，包括客户端管理、系统配置、API 密钥以及用户管理。

+ **查看者**

    查看者可以访问 EMQX 的所有数据和配置信息，对应 REST API 中的所有 `GET` 请求，但无权进行创建、修改和删除操作。

### 登录用户权限范围（Scopes）

从 EMQX 5.10 开始，您可以为 Dashboard 登录用户分配权限范围（Scope），在角色基础上进一步限制用户可访问的 API 区域。除 [10 个 API 密钥 Scope](../admin/api.md#内置范围) 外，Dashboard 用户还拥有 4 个仅适用于浏览器会话的专属 Scope：

| Scope | 所需角色 | 用途 |
| --- | --- | --- |
| `user_management` | 管理员 | 管理 Dashboard 用户（创建 / 修改 / 删除）。 |
| `sso_management` | 管理员 | 管理 SSO 后端与 SSO 用户记录。 |
| `api_key_management` | 管理员 | 管理 API 密钥。 |
| `mfa_management` | 任意 | 管理自己的 MFA；管理员可管理其他用户的 MFA。 |

其中 `user_management`、`sso_management` 和 `api_key_management` 需要管理员角色，不能分配给查看者。`mfa_management` 是例外：可以授予查看者，但仅允许其管理自己账号的 MFA，不授予对其他用户 MFA 设置的访问权限。当您希望查看者账号能够自助重新绑定或恢复认证设备而不获得其他额外权限时，此 Scope 非常有用。

创建用户时，**Scopes** 字段是可选的。省略该字段时，用户会得到一个由其角色推导出的默认 Scope 集：

- **管理员**：拥有全部 Scope，包括上述 4 个登录专属 Scope。
- **查看者**：拥有全部通用 API 密钥 Scope；`mfa_management` 仅在显式分配时才会被授予。

更新用户时，省略 `scopes` 将保留用户已存储的 Scope 设置。

![user_scopes](./assets/user_scopes.png)

#### `scopes` 写入行为

从 EMQX 6.0.4 开始，`POST /api/v5/users` 和 `PUT /api/v5/users/:username` 支持以下 `scopes` 请求值：

| 请求值 | 创建用户 | 更新用户 |
| --- | --- | --- |
| 省略字段 | 应用角色的默认 Scope。 | 保留用户已存储的 Scope 设置。如果请求同时更改角色，已存储的 Scope 必须适用于新角色。 |
| `"unset"` | 使用角色的隐式默认 Scope，不存储显式列表。 | 清除显式列表，恢复角色的隐式默认 Scope。 |
| 与角色默认值相同的列表 | 按 `"unset"` 处理，列表顺序不影响比较结果。 | 按 `"unset"` 处理，列表顺序不影响比较结果。 |
| 空列表 `[]` | 拒绝访问所有受 Scope 控制的 API 区域。 | 将已存储的设置替换为拒绝访问所有受 Scope 控制的 API 区域。 |
| 其他显式列表 | 校验并存储该列表。 | 校验并存储该列表。 |

使用 `"unset"` 可以使用户 Scope 在升级到增加了新 Scope 的 EMQX 版本后继续与角色默认值保持一致。对于没有显式 Scope 列表的用户，API 可以返回 `"unset"`。API 客户端执行读取、修改、写回操作时，可以原样回传该值，例如只修改用户备注时。

::: warning 特权 Scope 必须单独使用

以下 Scope 等同管理员权限：

- `system` 覆盖配置管理（`/configs*`、`/data/*` 等）。持有 `system` 的用户可以更新任意配置子树，或恢复包含已存储用户和 API 密钥记录的备份文件。
- `user_management` 允许持有者创建或修改其他 Dashboard 用户，包括具有任意 Scope 集的用户。
- `api_key_management` 允许持有者创建或修改 API 密钥，包括具有任意 Scope 集的密钥。
- `sso_management` 允许持有者轮换或重新配置 SSO 后端，从而改变管理员的身份认证方式。

从 EMQX 6.0.4 开始，全局 Dashboard 用户的显式 Scope 列表不能将以上任一特权 Scope 与非特权 Scope 组合。创建或更新请求将返回 HTTP 400。需要等同管理员权限时，仅使用特权 Scope；需要受限访问时，仅使用非特权 Scope。`mfa_management` 属于非特权 Scope。

在 EMQX 6.0.4 之前创建且使用混合 Scope 列表的用户可以继续工作。后续请求显式提交 Scope 列表时，必须拆分特权 Scope 和非特权 Scope。省略 `scopes` 字段、使用 `"unset"`、使用与角色默认值相同的列表或使用空列表 `[]` 时，不会按显式混合列表处理。

此互斥规则不适用于命名空间 Dashboard 管理员。命名空间管理员的 Scope 组合仍受命名空间角色兼容性和端点级授权控制。

:::

#### 角色变更与 Scope 兼容性

变更用户角色时，EMQX 会检查该用户当前的 Scope 是否与新角色兼容。如果不兼容，请求将返回 HTTP 400。要解决此问题，请在同一请求中提供一个对新角色有效的 `scopes` 列表。

例如，如果您将一个管理员降级为查看者，而该用户持有 `user_management`、`sso_management` 或 `api_key_management`，请求将被拒绝，因为这三个 Scope 需要管理员角色。请在同一请求中提供一个仅包含查看者兼容 Scope 的列表以完成变更。（`mfa_management` 不是仅限管理员的 Scope，不会导致此拒绝。）

### 默认管理员保护

`dashboard.default_username` 账号（其密码由 `dashboard.default_password` 配置）是一个应急（break-glass）账号。为了保证在其他管理员配置错误或失联时系统仍可恢复，默认用户受到下列保护，以防止误操作导致整个系统失去管理入口：

- **不能被删除**：无论是从 Dashboard 还是 REST API，**删除**按钮始终不可用。
- 角色**不能被更改**，始终保持 `administrator`。
- Scope 集**不能被自定义**，始终使用隐式的完整管理员 Scope。通过用户 API 更新其他字段时，可以省略 `scopes` 字段、使用 `"unset"`，或使用与管理员角色默认值相同的列表。
- 描述和密码**可以**正常修改。

其他管理员不受此限制，只要系统中至少还存在一个管理员，就可以被删除。

### 自助操作边界

每个 Dashboard 用户无论持有哪些 Scope，都被允许执行以下两类自助操作：

- 修改自己的密码。
- 绑定或重新绑定自己的 TOTP / MFA。禁用 MFA 同样允许，但若管理员已为该用户账号显式要求启用 MFA，则需持有 `mfa_management` Scope 方可禁用。

其他个人信息变更（描述、角色、由管理员授予的 Scope）都需要操作者持有对应 Scope，即使目标用户就是操作者自己也不能绕过此检查。

### 命名空间角色

从 EMQX 6.0 开始，Dashboard 支持命名空间角色功能。该特性扩展了基于角色的访问控制（RBAC），以支持多租户场景：每个用户仅被授权访问特定的命名空间，实现资源隔离与权限精细化管理。

::: warning 仅适用于受信任部署

命名空间管理员访问仅适用于受信任的内部部署场景，例如在同一组织内隔离不同团队或业务单元，以降低误修改其他配置的风险。命名空间功能不提供强隔离保障，不适合作为面向公共环境或非受信任用户的多租户安全边界。

如果您允许委派管理员管理命名空间范围内的资源，建议在**管理** > **集群配置** > **[规则引擎安全](./cluster_settings.md#规则引擎安全)**中启用 SSRF 防护，以校验规则引擎管理的出站目标。如果还需要运行时网络边界，再增加主机级出站访问控制，例如 `iptables` 或 `nftables`。参见[结合规则引擎策略与防火墙规则防御 SSRF](../deploy/cluster/security.md#结合规则引擎策略与防火墙规则防御-ssrf)。

:::

::: tip

如需了解命名空间功能的详细信息，请参阅：[命名空间](../multi-tenancy/namespace-overview.md)。

:::

#### 创建具有命名空间角色的用户

在 Dashboard 中创建新用户时，界面将新增一个**命名空间**选项。

::: tip 前提条件

1. 在 Dashboard 中预先创建一个托管命名空间（如：`namespace_01`）。参考：[创建命名空间](../multi-tenancy/create-namespace.md)。
2. 确保当前使用的 EMQX License 版本为 6.0 或更高版本，并已正确部署集群。

:::

创建步骤如下：

1. 进入**系统设置 > 用户**页面，点击 **+ 创建**。
2. 填写以下字段：
   - **用户名**：用户的唯一标识。
   - **备注**：可选说明信息。
   - **密码**：用户登录密码。
   - **角色**：选择**管理员**或**查看者**。
3. 启用**命名空间**选项，并选择一个已存在的命名空间（例如 `namespace_01`）。
4. 点击**创建**完成用户配置。

若通过 CLI 或 API 创建用户，需显式指定角色格式为：

```
ns:<NAMESPACE>::<ROLE>
```

例如：

- `ns:namespace_01::administrator`
- `ns:namespace_01::viewer`

#### 命名空间用户的行为说明

- **资源作用域限制**：命名空间用户只能查看和管理其所属命名空间下的资源，包括连接器、动作、数据源、规则等支持命名空间的模块。
- **集群级设置访问限制**：尚未支持命名空间隔离的全局配置项对命名空间用户为只读，只有系统管理员可进行修改。
- **消息内容端点限制**：部分访问或操作原始 MQTT 消息内容的 REST API 端点对命名空间用户不可用，调用时将返回 `403 Forbidden`。这些端点仅供全局管理员使用：
  - 消息队列消息：`GET /clients/:clientid/mqueue_messages`
  - 飞行窗口消息：`GET /clients/:clientid/inflight_messages`
  - 保留消息：`GET /mqtt/retainer/messages`、`GET /mqtt/retainer/message/:topic`、`DELETE /mqtt/retainer/message/:topic`、`DELETE /mqtt/retainer/messages`
  - 延迟消息：`GET /mqtt/delayed/messages`、`GET /mqtt/delayed/messages/:node/:msgid`、`DELETE /mqtt/delayed/messages/:node/:msgid`、`DELETE /mqtt/delayed/messages/:topic`
- **日志追踪隔离**：命名空间用户访问追踪端点时，仅能看到属于其命名空间的追踪记录。对不同命名空间的追踪执行停止、下载、流式读取日志或删除操作（`PUT /trace/:name/stop`、`GET /trace/:name/download`、`GET /trace/:name/log`、`GET /trace/:name/log_detail`、`DELETE /trace/:name`）将返回 `404 Not Found`，不会泄露其他命名空间的追踪是否存在。批量删除端点（`DELETE /trace`）对命名空间用户返回 `403 Forbidden`，仅全局管理员可清空所有追踪记录。
- **API 密钥管理**：命名空间管理员可以创建、查询、查看、更新和删除自己命名空间中的 API 密钥。命名空间管理员不能创建全局 API 密钥或其他命名空间中的密钥，所属命名空间之外的密钥不会显示。REST API 的详细行为参见[命名空间管理员管理 API 密钥](../admin/api.md#命名空间管理员管理-api-密钥)。
- **默认登录首页**：命名空间用户登录 Dashboard 后默认进入**概览**页面，菜单项与普通用户一致，但资源数据将自动过滤，仅显示其命名空间内的数据。
- **License 管理限制**：命名空间用户不显示 License 相关提示，License 相关操作仅由系统管理员负责。

#### 命名空间内角色含义

- **管理员**：对指定命名空间下的资源拥有完整权限（创建、读取、更新、删除）。
- **查看者**：仅具备只读权限（等同于 GET 请求）权限，仅可查看资源数据。

## 审计日志

**审计日志**页面允许管理员配置审计日志功能，以实时监控 EMQX 集群中的关键操作变更。

有关审计日志功能的详细说明，请参见[审计日志](../dashboard/audit-log.md)。

## API 密钥

**API 密钥**页面用于创建和管理访问 [HTTP API](../admin/api.md) 所需的 API 密钥。有关创建和管理 API 密钥（包括角色与范围分配）的操作说明，请参见[创建 API 密钥](../admin/api.md#创建-api-密钥)。

## License

点击左侧**系统设置**菜单下的 **License** 可以来到 License 页面。在该页面上可以查看当前 License 的基础信息，包括**签发对象**、 **License 使用情况**、**EMQX 版本信息**、**签发邮箱**、**签发时间**和**到期时间**。

点击**更新 License** 可以上传 License Key。在 **License 设置**区域可以设置 License 连接配额使用量的高水位线和低水位线。更多关于 License 的内容，参考[EMQX 企业版 License](../deploy/license.md)。

## 单点登录

单点登录页面为管理员提供了用户登录管理中单点登录功能的配置。有关单点登录功能的详细介绍，参阅[单点登录](./sso.md)。

## 备份与恢复

**备份与恢复**页面提供用于备份运行数据和配置文件的相关设置。您可以在此页面执行数据导入和导出操作。

有关备份与恢复功能的详细信息，请参见[备份与恢复](../operations/backup-restore.md)。

## 设置

要访问设置，请点击 Dashboard 右上角的齿轮图标。

在**设置**菜单中，您可以自定义 Dashboard 的语言和主题样式：

- **语言**：选择您偏好的显示语言。
- **主题**：可在浅色和深色主题之间切换，或启用与操作系统主题的自动同步。当启用同步后，Dashboard 的主题将跟随操作系统设置，无法手动选择。

此外，设置菜单中还包含一个开关，用于启用或禁用**规则**页面中的 [SQL 生成器](../data-integration/rule-get-started.md#sql-generator)功能。

<img src="./assets/settings_ee.png" alt="settings_ee" style="zoom:67%;" />
