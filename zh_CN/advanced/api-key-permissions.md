# API Key 权限管理

EMQX Enterprise 提供管理 API（默认端口 8081），用于通过程序方式访问集群管理操作。API Key 由 AppID 和 AppSecret 组成，用于对管理 API 的请求进行认证。每个 Key 可以限定为只访问特定的 API 类别，从而精细控制每个集成应用或自动化工具的操作权限。

![API Key 权限管理](./assets/api_key_permissions.png)

## 管理 API 与 Dashboard API 的区别

EMQX 提供两套独立的 HTTP API 服务，各自有独立的认证体系：

| | 管理 API | Dashboard API |
|---|---|---|
| **端口** | 8081 | 18083 |
| **认证方式** | AppID + AppSecret（Basic Auth） | Dashboard 用户凭证（Basic Auth） |
| **用途** | 插件/模块 API、自动化、系统集成、CI/CD | Dashboard Web 界面、API Key 管理 |
| **路径前缀** | `/api/v4/` | `/api/v4/` |

两套服务共享同一个 `/api/v4/` 路径命名空间，但认证体系完全独立。一套服务的凭证无法用于另一套服务。

**重要提示：** API Key 管理接口（`/api/v4/apps/`）属于 **Dashboard API（18083 端口）**，而不是管理 API（8081 端口）。使用 Dashboard 用户凭证创建和管理 API Key。

## 创建 API Key

### 通过 Dashboard

1. 在左侧导航栏中，点击 **管理**，再点击 **应用**（HTTP API）。
2. 点击 **添加应用**。
3. 填写各字段并配置所需权限。
4. 点击 **确认** 保存。

### 通过 API

**接口：** `POST /api/v4/apps/`

**请求参数（JSON）：**

| 字段 | 类型 | 必填 | 说明 |
|------|------|------|------|
| `app_id` | string | 是 | API Key 的唯一标识符 |
| `name` | string | 否 | 显示名称 |
| `secret` | string | 否 | 自定义密钥。不填则自动生成 |
| `desc` | string | 否 | 描述信息 |
| `status` | boolean | 否 | 是否启用。默认为 `true` |
| `expired` | integer | 否 | 过期时间戳（Unix 秒）。不填则永不过期 |
| `permissions` | object | 否 | 以类别名为键的权限映射。参见[权限类别](#权限类别) |
| `fallback` | boolean | 否 | 未覆盖路径的默认行为。默认为 `false`（拒绝） |

**示例：**

```bash
curl -i -X POST "http://127.0.0.1:18083/api/v4/apps/" \
  -u admin:public \
  -H "Content-Type: application/json" \
  -d '{
    "app_id": "my_automation",
    "name": "CI/CD Pipeline",
    "desc": "Used by CI/CD for rule engine management",
    "status": true,
    "permissions": {
      "rule_engine": true,
      "resources": true,
      "plugins": false,
      "modules": false,
      "banned": false
    },
    "fallback": false
  }'
```

**响应示例：**

```json
{
  "code": 0,
  "data": {
    "secret": "<generated_secret_token>"
  }
}
```

::: warning

AppSecret 在 API 响应中明文可见（包括创建和查询接口）。请妥善保管。`/api/v4/apps/` 接口仅可通过 Dashboard API（18083 端口）使用 Dashboard 用户凭证访问，不能通过 API Key 访问。

:::

## 权限类别

每个 API Key 对以下五个类别各有独立的布尔权限。将某类别设为 `true` 则允许访问对应接口，设为 `false` 则拒绝访问。

| 类别 | 权限键 | 覆盖的接口 |
|------|--------|-----------|
| 黑名单 | `banned` | `/api/v4/banned/` — 客户端黑名单管理 |
| 规则引擎 | `rule_engine` | `/api/v4/rules/`、`/api/v4/actions/`、`/api/v4/rule_events/` |
| 资源 | `resources` | `/api/v4/resources/`、`/api/v4/resource_types/` |
| 插件 | `plugins` | `/api/v4/plugins/` |
| 模块 | `modules` | `/api/v4/modules/`、`/api/v4/trace/`、`/api/v4/topic-metrics/`、`/api/v4/quota/`、`/api/v4/client_tags/` |

新创建的 API Key 默认将所有五个类别设为 `false`，遵循最小权限原则。按需开启所需权限。

### `fallback` 设置

许多常用接口不属于上述五个命名类别，例如 `/api/v4/clients/`、`/api/v4/subscriptions/`、`/api/v4/stats/`、`/api/v4/metrics/` 和 `/api/v4/nodes/`。`fallback` 参数控制 Key 访问这些接口时的行为：

- `false`（默认）：拒绝访问。
- `true`：允许访问。

::: tip

大多数只读监控类接口（客户端、订阅、统计、指标、节点）均属于 `fallback` 管控的"未覆盖"类别。若需要 API Key 读取监控数据，请将 `fallback` 设为 `true`。

:::

## 兼容模式

在权限系统引入之前创建的 API Key 会以兼容模式运行。兼容模式下的 Key 可访问所有 API，等同于所有类别设为 `true` 且 `fallback` 设为 `true`。

通过 API 响应中的 `compatibility_mode: true` 字段可以识别兼容模式 Key。

若要对兼容模式 Key 应用权限限制，只需通过更新接口传入显式的 `permissions` 对象。此操作会退出兼容模式，并按照指定权限运行。

::: warning

将兼容模式 Key 更新为指定 `permissions` 后，模式转换不可逆。退出兼容模式后，该 Key 将在正常权限体系下运行。

:::

## 管理 API Key

### 列出所有 Key

**接口：** `GET /api/v4/apps/`

**示例：**

```bash
curl -u admin:public "http://127.0.0.1:18083/api/v4/apps/"
```

**响应示例：**

```json
{
  "code": 0,
  "data": [
    {
      "status": true,
      "permissions": {
        "rule_engine": true,
        "resources": true,
        "plugins": false,
        "modules": false,
        "banned": false
      },
      "name": "Documentation Test",
      "expired": "undefined",
      "desc": "Created for documentation examples",
      "compatibility_mode": false,
      "app_id": "doc_test_key"
    }
  ]
}
```

### 查看 Key 详情

**接口：** `GET /api/v4/apps/:appid`

**示例：**

```bash
curl -u admin:public "http://127.0.0.1:18083/api/v4/apps/my_automation"
```

**响应示例：**

```json
{
  "code": 0,
  "data": {
    "status": true,
    "secret": "<secret>",
    "permissions": {
      "rule_engine": true,
      "resources": true,
      "plugins": false,
      "modules": false,
      "banned": false
    },
    "name": "Documentation Test",
    "expired": "undefined",
    "desc": "Created for documentation examples",
    "compatibility_mode": false,
    "app_id": "doc_test_key"
  }
}
```

::: tip

`secret` 字段仅在查询单个 Key 详情（lookup）时返回。列出所有 Key 时，出于安全考虑不会返回 `secret` 字段。

:::

### 更新 Key

**接口：** `PUT /api/v4/apps/:appid`

可以独立更新 `name`、`desc`、`status`、`expired`、`permissions` 和 `fallback`。请求体中只包含需要修改的字段。

**示例 — 禁用 Key：**

```bash
curl -i -X PUT "http://127.0.0.1:18083/api/v4/apps/my_automation" \
  -u admin:public \
  -H "Content-Type: application/json" \
  -d '{"status": false}'
```

**示例 — 仅更新权限：**

```bash
curl -i -X PUT "http://127.0.0.1:18083/api/v4/apps/my_automation" \
  -u admin:public \
  -H "Content-Type: application/json" \
  -d '{
    "permissions": {
      "rule_engine": true,
      "resources": true,
      "plugins": true,
      "modules": false,
      "banned": false
    }
  }'
```

**响应：**

```json
{"code": 0}
```

### 删除 Key

**接口：** `DELETE /api/v4/apps/:appid`

```bash
curl -i -X DELETE "http://127.0.0.1:18083/api/v4/apps/my_automation" \
  -u admin:public
```

**响应：**

```json
{"code": 0}
```

## 引导文件（Bootstrap File）

可以在 EMQX 启动之前通过引导文件预配置 API Key。这对于初始部署或容器化环境尤为实用，在任何 API 调用可用之前就能确保凭证已就绪。

**配置方式：**

设置环境变量，指向文件路径：

```bash
EMQX_API_KEY__BOOTSTRAP_FILE=/path/to/bootstrap_keys.txt
```

**文件格式：**

每行一个 Key，AppID 和 AppSecret 以冒号分隔：

```
my_app_id:my_app_secret
another_app:another_secret
```

引导文件中创建的 Key 具有完全访问权限，不设任何权限限制，`fallback` 为 `true`，描述标签为 `Bootstrapped From File`。EMQX 启动后，可通过 API 对这些 Key 进行权限限制。

::: tip

建议使用引导文件创建初始管理 Key，用于管理其他 API Key。启动后再通过该管理 Key 创建权限受限的 Key 供各集成服务使用。

:::

## 认证方式

所有管理 API 请求均需 HTTP Basic 认证：

```
Authorization: Basic base64(AppID:AppSecret)
```

大多数 HTTP 客户端通过 `-u` 参数自动处理：

```bash
curl -u my_app_id:my_app_secret "http://127.0.0.1:8081/api/v4/clients"
```

AppID 或 AppSecret 无效的请求返回 HTTP `401`。禁用（`status: false`）或已过期的 Key 同样返回 `401`。

## API 接口汇总

| 方法 | 接口 | 说明 |
|------|------|------|
| `POST` | `/api/v4/apps/` | 创建 API Key |
| `GET` | `/api/v4/apps/` | 列出所有 API Key |
| `GET` | `/api/v4/apps/:appid` | 查看 API Key 详情 |
| `PUT` | `/api/v4/apps/:appid` | 更新 API Key |
| `DELETE` | `/api/v4/apps/:appid` | 删除 API Key |

## 安全建议

- **最小权限原则：** 只授予 Key 实际所需的权限。仅管理规则引擎的 CI/CD 流水线只需开启 `rule_engine: true`，其余保持 `false`。
- **谨慎管理 `fallback`：** 除非该 Key 明确需要访问客户端或统计等监控接口，否则将 `fallback` 保持为 `false`。
- **设置过期时间：** 对临时 Key 或短期流水线 Key，通过 `expired` 字段设置到期时间。
- **定期轮换密钥：** 定期删除并重建 Key，或通过更新接口更换 `secret`。
- **引导文件用于初始化，API 用于日常管理：** 用引导文件创建初始管理 Key，后续所有 Key 通过 API 管理。
