# UNS 治理

该插件在 ACL 检查阶段强制执行统一命名空间（Unified Namespace）的主题结构。

## 插件 API

基础路径：`/api/v5/plugin_api/emqx_unsgov`

## 引导模型（Bootstrap Models）

- 启动时，UNS 治理会扫描 `priv/bootstrap_models/*.json`。
- 对每个引导模型：
  - 如果其 `id` 在数据库中不存在，插件会存储该模型并将其标记为活跃。
  - 如果其 `id` 已存在于数据库中，插件会跳过加载并以 info 级别记录日志。
- 内置的默认引导模型：`priv/bootstrap_models/model-v1.json`。

> NOTE: 引导模型会在集群中第一个插件启动时加载到数据库，之后的插件或节点重启不会触发重新加载。请使用 API 更新存储在数据库中的模型。

### JSON 数据端点

- `GET /status` — 插件状态（on_mismatch、exempt_topics）。
- `GET /stats` — 集群聚合的计数器及最近的丢弃记录。
- `GET /models` — 列出所有已存储的模型（每一项包含一个 `active` 标志）。
- `GET /models/:id` — 按 ID 获取指定模型。未找到则返回 404。
- `POST /models` — 创建或更新模型；可选 `activate` 标志。
- `POST /models/:id/activate` — 激活已存储的模型。
- `POST /models/:id/deactivate` — 停用模型。
- `DELETE /models/:id` — 删除已存储的模型。
- `POST /validate/topic` — 依据活跃模型校验主题。

### 其他端点

- `GET /ui` — 交互式模型编辑器 UI。
- `GET /metrics` — Prometheus 文本导出格式。

## UNS 模型 Schema

本节定义 UNS 治理所接受的完整模型 JSON 格式。

### 顶层键

- `id`（必填，字符串）：模型 ID。必须匹配 `^[A-Za-z0-9_-]+$`。控制评估顺序（按 ID 字母序）。
- `name`（可选，字符串）：模型显示名称。默认为 `id`。
- `variable_types`（可选，对象）：可复用的变量约束。
- `tree`（必填，对象）：主题树定义。
- `payload_types`（可选，对象）：可复用的载荷 schema。

### `variable_types`

从变量类型名称到约束对象的映射。

支持的形式：
- 字符串正则匹配器：
  - `{"type":"string","pattern":"^...$"}`
- 枚举匹配器：
  - `{"type":"enum","values":["A","B","C"]}`

如果某个变量类型缺失或无效，匹配器会回退为宽松的 `any`。

### `payload_types`

从载荷 schema 名称到 schema 对象的映射。

校验使用 JSON Schema，并带有一个兼容性补丁：
- 如果省略了顶层 `type`，UNS 治理会将其补全为 `"object"`。
- 顶层载荷 schema 必须以对象为根。以基本类型为根的 schema 会被拒绝。

这样便同时支持：
- 完整、自包含的对象 JSON Schema。
- 已有的简写对象 schema（例如仅包含 `required`/`properties`）。

端点载荷绑定：
- 端点的 `_payload` 可以引用 `payload_types` 中的某个键，或使用 `"any"` 以跳过载荷校验。

### `tree`

`tree` 是一个对象，其中每个键是一个根主题段，每个值是一个节点对象。

节点对象的键：
- `children`（可选，对象）：子段映射。
- `_payload`（可选，字符串）：端点节点的载荷类型名称，默认 `"any"`。
- `_type`（可选，兼容性）：显式指定 `namespace | variable | endpoint`。
- `_var_type`（可选，兼容性）：变量类型名称。

节点类型推断：
- 如果存在 `children`：该节点为非端点节点。
- 如果不存在 `children`：该节点为端点节点。
- 对于非端点键：
  - 键 `{name}` => 变量节点
  - 键 `+` => 变量通配符节点
  - 其他任意键 => 命名空间节点

变量类型解析：
- 对于键 `{name}`：
  - 如果提供了 `_var_type` 则使用它
  - 否则使用推断出的类型名称 `name`
- 对于键 `+`：
  - 匹配器为 `any`（匹配一个段）

主题树中的通配符键：
- `+`：精确匹配一个主题段。
- `#`：匹配剩余的主题段（包括剩余零个段）。

### 完整示例

```json
{
  "id": "model-v1",
  "name": "UNS Model V1",
  "variable_types": {
    "site_id": { "type": "string", "pattern": "^[A-Za-z][A-Za-z0-9_]{0,31}$" },
    "line_id": { "type": "string", "pattern": "^Line[0-9]{1,4}$" },
    "mode": { "type": "enum", "values": ["auto", "manual"] }
  },
  "payload_types": {
    "line_control": {
      "type": "object",
      "required": ["Status", "Mode"],
      "properties": {
        "Status": { "type": "string", "enum": ["running", "stopped"] },
        "Mode": { "type": "string", "enum": ["auto", "manual"] }
      },
      "additionalProperties": false
    }
  },
  "tree": {
    "default": {
      "children": {
        "{site_id}": {
          "children": {
            "Lines": {
              "children": {
                "{line_id}": {
                  "children": {
                    "LineControl": { "_payload": "line_control" }
                  }
                }
              }
            },
            "stream": {
              "children": {
                "#": { "_payload": "any" }
              }
            }
          }
        }
      }
    }
  }
}
```

## 强制执行行为

UNS 治理会同时校验主题结构以及（可选的）载荷 schema。

- 主题违规（`topic_nomatch`、`topic_invalid`、`not_endpoint`）：
  - `topic_nomatch`：没有任何活跃模型的主题过滤器匹配该主题。
    （不会运行任何模型特定的校验。）
    如果不存在活跃模型且 UNS 治理处于启用状态，主题会以
    `topic_nomatch` 采取失败即拒绝（fail-closed）策略（`exempt_topics` 除外）。
  - `topic_invalid`：选定的模型过滤器匹配，但主题未通过所选
    模型的结构/段约束。
  - `not_endpoint`：选定的模型匹配了主题路径，但目标节点不是
    端点。
  - QoS 0：消息被忽略。
  - QoS 1/2：发布被拒绝，并向客户端返回一个协议原因码
    （`Not Authorized`）。
  - 如果 EMQX 的 `authorization.deny_action` 设置为 `disconnect`，客户端会在
    主题授权失败时被断开连接（该设置为 `disconnect`，
    而非 `drop`）。
  - 如果 `authorization.deny_action` 为 `ignore`（默认），则不会断开连接；
    QoS 1/2 仍会收到拒绝原因码。
  - 可观测计数器：`messages_dropped`、`topic_nomatch`、
    `topic_invalid`、`not_endpoint`，以及 `per_model` 中的按模型计数器。

- 载荷违规（`payload_invalid`）：
  - 消息由 UNS 治理在发布处理阶段丢弃。
  - 此路径不需要执行授权拒绝/断开连接。
  - 可观测计数器：`messages_dropped`、`payload_invalid`，
    以及 `per_model` 中的按模型计数器。

## 主题过滤器预检（Topic-Filter Pre-Check）

当有多个模型活跃时，UNS 治理会在进行完整校验之前对模型进行预筛选：

- 每个模型会被编译成由其树路径派生出的主题过滤器模式。
- 变量段会被转换为单层通配符（`+`）。
  - 示例：`foo/{bar}/x` 变为 `foo/+/x`。
- 活跃模型按模型 ID 排序。
- UNS 治理会选取（按 ID 顺序）第一个其编译后的过滤器匹配
  发布主题的模型。
- 预检仅使用直接的主题/过滤器匹配；它不会隐式扩展
  发布主题的前缀（例如追加 `/#`）。
- 只有该选定的模型会被完整校验；UNS 治理不会继续检查
  下一个模型。
- 未通过此预检的模型会被跳过，且不会贡献按模型的
  丢弃计数器。

这可以避免不相关的活跃模型抬高计数器，并保持模型行为的
确定性。这也意味着应避免不同模型之间出现相互重叠的主题树。

## 计数器

`GET /stats` 返回集群聚合的计数器。

顶层计数器：
- `messages_total`：处理的消息总数（`messages_allowed + messages_dropped`）；
  含豁免流量。
- `messages_allowed`：允许的消息加上豁免的消息。
- `messages_dropped`：因 UNS 校验失败而被丢弃/拒绝的消息。
- `topic_nomatch`：因没有活跃模型过滤器匹配而被丢弃/拒绝。
- `topic_invalid`：因所选模型的主题不匹配而被丢弃/拒绝。
- `not_endpoint`：因主题匹配到非端点节点而被丢弃/拒绝。
- `payload_invalid`：因载荷 schema 不匹配而被丢弃。
- `exempt`：被 `exempt_topics` 跳过的消息。
- `per_model`：以模型 ID 为键的按模型细分映射。
- `recent_drops`：最近的丢弃事件（`topic`、`error_type`、`error_detail`、
  `timestamp_ms`）。

按模型计数器（`per_model.<model_id>`）：
- `messages_total`
- `messages_allowed`
- `messages_dropped`
- `topic_invalid`
- `not_endpoint`
- `payload_invalid`

计数器语义：
- `record_allowed` 会为匹配的模型累加 `messages_total` 和 `messages_allowed`。
- 主题/载荷丢弃会累加 `messages_total`、`messages_dropped`，以及所选模型对应的
  具体原因计数器。
- 如果没有任何模型通过主题过滤器预检，则全局累加 `topic_nomatch`，
  且不累加任何按模型的丢弃计数器。
  这也包括活跃模型集合为空的情况。

<!-- PLUGIN-DOWNLOADS:BEGIN (auto-generated, do not edit) -->

## 下载

各 EMQX 版本对应的插件安装包：

| EMQX 版本 | 插件版本 | 安装包 |
|---|---|---|
| 6.1.2 | 0.1.3 | [emqx_unsgov-0.1.3.tar.gz](https://packages.emqx.io/emqx-plugins/6.1.2/emqx_unsgov-0.1.3.tar.gz) |
| 6.1.3 | 0.1.3 | [emqx_unsgov-0.1.3.tar.gz](https://packages.emqx.io/emqx-plugins/6.1.3/emqx_unsgov-0.1.3.tar.gz) |

<!-- PLUGIN-DOWNLOADS:END -->
