# UNS Governance

该插件在 ACL 检查阶段强制执行 Unified Namespace 主题结构。

## 插件 API

基础路径：`/api/v5/plugin_api/emqx_unsgov`

## Bootstrap 模型

- 启动时，UNS Governance 会扫描 `priv/bootstrap_models/*.json`。
- 对于每个 bootstrap model：
  - 如果其 `id` 在数据库中不存在，插件会保存该模型并将其标记为激活状态。
  - 如果其 `id` 已存在，插件会跳过加载并记录 info 级别日志。
- 随插件打包的默认 bootstrap model：`priv/bootstrap_models/model-v1.json`。

> 注意：Bootstrap model 会在集群中第一个插件实例启动时加载到数据库中，之后插件重启或节点重启都不会重复加载。请使用 API 更新数据库中的模型存储。

### JSON 数据端点

- `GET /status` — 插件状态（`on_mismatch`、`exempt_topics`）。
- `GET /stats` — 集群聚合计数器与最近丢弃记录。
- `GET /models` — 列出所有已存储模型（每项均包含 `active` 标志）。
- `GET /models/:id` — 获取指定 ID 的模型；不存在时返回 404。
- `POST /models` — 创建或更新模型；可选 `activate` 标志。
- `POST /models/:id/activate` — 激活指定已存储模型。
- `POST /models/:id/deactivate` — 停用指定已存储模型。
- `DELETE /models/:id` — 删除指定模型。
- `POST /validate/topic` — 使用激活模型校验某个主题。

### 其他端点

- `GET /ui` — 交互式模型编辑 UI。
- `GET /metrics` — Prometheus 文本暴露格式。

## UNS 模型结构

本节定义 UNS Governance 接受的完整模型 JSON 格式。

### 顶层键

- `id`（必填，字符串）：模型 ID。必须匹配 `^[A-Za-z0-9_-]+$`。用于控制评估顺序（按 ID 字母顺序）。
- `name`（可选，字符串）：模型显示名称。默认为 `id`。
- `variable_types`（可选，对象）：可复用的变量约束定义。
- `tree`（必填，对象）：主题树定义。
- `payload_types`（可选，对象）：可复用的负载模式定义。

### `variable_types`

该对象是从变量类型名到约束对象的映射。

支持的形式：
- 字符串正则匹配器：
  - `{"type":"string","pattern":"^...$"}`
- 枚举匹配器：
  - `{"type":"enum","values":["A","B","C"]}`

如果某个变量类型缺失或无效，则匹配器会回退为宽松的 `any`。

### `payload_types`

该对象是从负载模式名到模式对象的映射。

校验基于 JSON Schema，并带有一个兼容性补丁：
- 如果顶层未声明 `type`，UNS Governance 会将其补成 `"object"`。
- 顶层负载模式必须是 object 根；原始类型根会被拒绝。

这意味着以下两种形式都可使用：
- 完整、自包含的 object JSON Schema。
- 现有的简写 object schema（例如仅包含 `required`/`properties`）。

端点负载绑定：
- 端点节点的 `_payload` 可以引用 `payload_types` 中的某个键，或设置为 `"any"` 以跳过负载校验。

### `tree`

`tree` 是一个对象，其中每个键都是根主题段，每个值都是一个节点对象。

节点对象的键：
- `children`（可选，对象）：子段映射。
- `_payload`（可选，字符串）：端点节点的负载类型名，默认 `"any"`。
- `_type`（可选，兼容字段）：显式声明 `namespace | variable | endpoint`。
- `_var_type`（可选，兼容字段）：变量类型名。

节点类型推断规则：
- 如果存在 `children`：节点为非端点节点。
- 如果不存在 `children`：节点为端点节点。
- 对于非端点键：
  - 键为 `{name}` => 变量节点
  - 键为 `+` => 变量通配节点
  - 其他键 => 命名空间节点

变量类型解析：
- 对于键 `{name}`：
  - 如提供 `_var_type`，则使用其值
  - 否则使用推断出的类型名 `name`
- 对于键 `+`：
  - 匹配器为 `any`（匹配一个主题段）

主题树中的通配符键：
- `+`：匹配恰好一个主题段。
- `#`：匹配剩余所有主题段（包括剩余为 0 的情况）。

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

## 校验行为

UNS Governance 会同时校验主题结构以及（可选的）负载模式。

- 主题违规（`topic_nomatch`、`topic_invalid`、`not_endpoint`）：
  - `topic_nomatch`：没有任何激活模型的 topic filter 匹配该主题。
    （不会继续执行模型级校验。）
    如果当前没有任何激活模型且 UNS Governance 已启用，则除 `exempt_topics` 外，主题将以 `topic_nomatch` 的方式 fail-close。
  - `topic_invalid`：选中的模型 filter 匹配了该主题，但主题未通过该模型的结构/段约束校验。
  - `not_endpoint`：选中的模型匹配了主题路径，但目标节点不是端点。
  - QoS 0：消息会被忽略。
  - QoS 1/2：发布会被拒绝，并向客户端返回协议原因码（`Not Authorized`）。
  - 如果 EMQX `authorization.deny_action` 设置为 `disconnect`，客户端会在主题鉴权失败时被断开（该设置值是 `disconnect`，不是 `drop`）。
  - 如果 `authorization.deny_action` 为 `ignore`（默认值），则不会断开连接；QoS 1/2 仍会收到拒绝原因码。
  - 可观测计数器包括：`messages_dropped`、`topic_nomatch`、`topic_invalid`、`not_endpoint` 以及 `per_model` 中的逐模型计数器。

- 负载违规（`payload_invalid`）：
  - 消息会在发布处理阶段被 UNS Governance 丢弃。
  - 该路径不要求执行 auth reject/disconnect。
  - 可观测计数器包括：`messages_dropped`、`payload_invalid` 以及 `per_model` 中的逐模型计数器。

## Topic-Filter 预检查

当存在多个激活模型时，UNS Governance 会先对模型进行预筛选，再执行完整校验：

- 每个模型都会被编译为从其树路径派生出的 topic-filter 模式。
- 变量段会被转换为单层通配符（`+`）。
  - 例如：`foo/{bar}/x` 会变成 `foo/+/x`。
- 激活模型按模型 ID 排序。
- UNS Governance 会选择第一个（按 ID 顺序）其编译后 filter 能匹配发布主题的模型。
- 预检查只做直接 topic/filter 匹配；不会隐式扩展发布主题前缀（例如追加 `/#`）。
- 只有被选中的那个模型会进入完整校验；UNS Governance 不会继续尝试后续模型。
- 未通过此预检查的模型会被跳过，也不会贡献逐模型丢弃计数。

这样可以避免无关的激活模型放大计数器，并保持模型行为确定可预测。这也意味着应尽量避免不同模型之间出现重叠的主题树。

## 计数器

`GET /stats` 返回集群聚合计数器。

顶层计数器：
- `messages_total`：处理的消息总数（`messages_allowed + messages_dropped`）；豁免流量也计算在内。
- `messages_allowed`：允许的消息数，加上豁免消息数。
- `messages_dropped`：因 UNS 校验失败而被丢弃/拒绝的消息数。
- `topic_nomatch`：因没有激活模型的 filter 匹配该主题而被丢弃/拒绝。
- `topic_invalid`：因选中模型的主题结构不匹配而被丢弃/拒绝。
- `not_endpoint`：因主题命中了非端点节点而被丢弃/拒绝。
- `payload_invalid`：因负载模式不匹配而被丢弃。
- `exempt`：被 `exempt_topics` 跳过的消息数。
- `per_model`：按模型 ID 组织的逐模型统计映射。
- `recent_drops`：最近丢弃事件（`topic`、`error_type`、`error_detail`、`timestamp_ms`）。

逐模型计数器（`per_model.<model_id>`）：
- `messages_total`
- `messages_allowed`
- `messages_dropped`
- `topic_invalid`
- `not_endpoint`
- `payload_invalid`

计数语义：
- `record_allowed` 会对匹配模型的 `messages_total` 和 `messages_allowed` 加一。
- 主题/负载丢弃会对选中模型的 `messages_total`、`messages_dropped` 以及对应原因计数器加一。
- 如果没有任何模型通过 topic-filter 预检查，则只会全局增加 `topic_nomatch`，不会增加任何逐模型丢弃计数。
  这也包括激活模型集合为空的情况。

<!-- PLUGIN-DOWNLOADS:BEGIN (auto-generated, do not edit) -->

## 下载

各 EMQX 版本对应的插件安装包：

| EMQX 版本 | 插件版本 | 安装包 |
|---|---|---|
| 6.2.0 | 0.1.2 | [emqx_unsgov-0.1.2.tar.gz](https://packages.emqx.io/emqx-plugins/6.2.0/emqx_unsgov-0.1.2.tar.gz) |

<!-- PLUGIN-DOWNLOADS:END -->
