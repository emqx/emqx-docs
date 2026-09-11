# MQTT Agent

MQTT Agent 将 EMQX 从 MQTT 基础设施升级为 MQTT 原生的 AI 编排平台。

MQTT Agent 使 EMQX 能够运行事件驱动的 AI 自动化流程，利用 EMQX 的连接能力对客户端事件做出响应。

与常见的面向人类的 Agent 不同，MQTT Agent 并非以聊天界面为核心。它是为无人值守的 AI 自动化而构建的：面向大量设备、大量并发工作流、受限的外部系统访问，以及可审计的工具调用。

MQTT Agent 无需将消息代理、API 网关、无服务器运行时、AI 服务、工作流引擎和集成平台分别搭建、再逐一对接，而是将这些能力整合进同一个 MQTT 原生运行时中。

该插件围绕三个可组合的基础能力构建，均通过 MQTT 主题提供：

- **工具（Tools）**：可复用、经过 Schema 校验的能力，用于 MQTT 发布、MQTT 请求/响应、HTTP 调用、数据库查询等。
- **会话（Sessions）**：通过 MQTT 主题路由的可寻址 LLM 对话。会话是上下文的保存者：它保存对话历史、待处理事件、排队中的请求、工具调用状态以及用量计数。
- **流水线（Pipelines）**：事件触发的工作流实例，编排工具与会话的调用以处理 MQTT 事件。

这使得 EMQX 成为一个场所：连接设备产生的事件可以直接触发安全的 AI 工作流——LLM 只能看到被授权的工具，工具可以限定主题与资源边界，会话可以跟踪用量，流水线可以借助 OTP 的故障隔离能力以 EMQX 的规模运行。

## 能力概览

- **将 AI 引入 MQTT 运维场景**：连接设备产生的事件可以直接触发模型辅助的决策、信息补全、检测、分类以及后续动作。
- **让自动化贴近 Broker**：工作流运行在 MQTT 连接、路由、授权边界与运维遥测已经具备的位置。
- **限定 AI 的可执行范围**：LLM 只能获得被授权的工具，且每个工具都可以被限定在特定的主题、端点、数据库或流上。
- **应对机器规模的事件流**：该能力面向大量设备与大量并发工作流设计，而非面向单一的人类聊天会话。

## MQTT Agent 接口

MQTT Agent 使用 MQTT 主题来提供功能。Agent 相关主题使用 `$` 前缀，因此它们属于 MQTT 系统主题，不会被普通的 `#` 订阅匹配到。

## 工具

工具是受限的动作，可以直接被流水线的某一步骤使用，也可以提供给流水线中 LLM 步骤使用。

工具通过类型和 ID 进行寻址：`type@id`。类型标识工具的具体实现，例如发起一次 HTTP 请求或执行一次数据库查询；`id` 标识该工具的一组已配置的选项与限制。

### 工具主题

工具调用是一次 MQTT 请求/响应交换。调用方将 JSON 请求发布到工具实例的请求主题（`$cap/<type>/<tool_id>/request/<req_id>`），并在对应的响应主题（`$cap/<type>/<tool_id>/response/<req_id>`）上等待 JSON 响应：

工具会解码请求负载，依据工具的输入 Schema 校验 `args` 字段，执行相应动作，并将结果发布到带有相同 `req_id` 的响应主题上。

例如，以请求 ID `req-42` 调用 `message__publish@alerts` 时使用：

- 请求主题为 `$cap/message__publish/alerts/request/req-42`
- 响应主题为 `$cap/message__publish/alerts/response/req-42`

调用方将以下请求负载发布到请求主题：

```json
// PUBLISH $cap/message__publish/alerts/request/req-42
{
  "args": {
    "topic": "factory/line-1/alerts",
    "payload": {"severity": "warning", "reason": "temperature_high"}
  },
  "iid": "pipeline-instance-id",
  "trace_id": "trace-id"
}
```

发布该 MQTT 消息后，工具会将以下响应负载发布到响应主题：

```json
// PUBLISH $cap/message__publish/alerts/response/req-42
{
  "status": "ok",
  "result": {"published": true}
}
```

### 类型、实例与上下文

工具类型是通用的实现。工具 ID 是该实现的一个已配置实例。该实例携带一个上下文：调用方在调用时无法更改的固定配置。

例如，`postgresql__query` 是一个通用的 PostgreSQL 查询执行器。就其本身而言，它只知道如何渲染 SQL 参数、通过一个 EMQX PostgreSQL 连接运行预置查询并返回行数据。而一个已配置的实例会将其收窄为一个更具体的能力：

```json
{
  "type": "postgresql__query",
  "id": "orders_by_device",
  "desc": "Read recent orders for one device",
  "resource": "pg-main",
  "query": "select id, status, created_at from orders where device_id = ${device_id} order by created_at desc limit 10"
}
```

这会创建工具引用 `postgresql__query@orders_by_device`。流水线或 LLM 步骤可以携带 `{"device_id": "dev-001"}` 调用该引用，但不能选择其他数据库连接、执行任意 SQL、移除 `where` 子句或修改 `limit`。这些固定的部分保存在与 `orders_by_device` 关联的实例上下文中。

其他工具类型也遵循同样的模式：`message__publish` 实例固定了发布边界，`http` 实例固定了端点形态，stream 或 KV 实例固定了存储目标。

### 内置工具类型

| 工具类型 | 用途 |
|---|---|
| `message__publish` | 在配置的主题前缀下发布 MQTT 消息。 |
| `message__request` | 发送 MQTT 5 请求/响应消息并等待响应。 |
| `http` | 使用 Schema 定义的输入调用外部 HTTP 端点。 |
| `postgresql__query` | 通过已配置的连接执行带参数的 PostgreSQL 查询。 |
| `stream__write` | 向 EMQX 流写入带键数据。 |
| `stream__read` | 从 EMQX 流读取带键数据。 |
| `stream__del` | 删除带键数据或清空一个 EMQX 流。 |
| `kv__write` | 向 EMQX 的 last-value 流写入一条键值数据。 |
| `kv__read` | 从 EMQX 的 last-value 流读取一条键值数据。 |
| `kv__read_all` | 从 EMQX 的 last-value 流读取全部键值数据。 |
| `kv__del` | 从 EMQX 的 last-value 流删除一条键值数据。 |
| `kv__clear` | 清空 EMQX 的 last-value 流中的全部键值数据。 |

### 图像处理机制

`http` 和 `message__request` 工具可以从工具响应中提取图像，以便安全地将多模态数据传递给 LLM。兼容 OpenAI 的 API 不接受直接嵌入在工具响应消息中的图像，因此 Agent 会将负载中提取出的图像替换为 `Image <id>` 占位符，并将图像数据作为独立的附件返回。

图像提取支持两种模式：

- `autodiscover_images`：扫描响应负载，查找 `data:image/...;base64,...` 形式的值。
- `images`：使用如 `.image_url` 或 `.`（表示根值）这样的路径显式指定图像位置。

当响应的内容类型为图像媒体类型（例如 `image/png`）时，也可以提取二进制图像响应。

#### 自动发现示例

假设一个 HTTP 工具返回如下包含内联 data URI 的 JSON：

```json
{
  "inspection_status": "accepted",
  "image_url": "data:image/png;base64,iVBORw0KGgoAAA...",
  "comment": "front camera frame"
}
```

启用 `autodiscover_images` 后，工具响应中会包含脱敏后的结果以及提取出的附件：

```json
{
  "status": "ok",
  "result": {
    "inspection_status": "accepted",
    "image_url": "Image .image_url",
    "comment": "front camera frame"
  },
  "attachments": [
    {
      "id": ".image_url",
      "type": "image",
      "mime_type": "image/png",
      "data": "iVBORw0KGgoAAA..."
    }
  ]
}
```

`result` 字段会进一步作为工具响应传递给 LLM，`attachments` 则作为额外的多模态数据传递。

#### 显式路径示例

如果响应中有多个类似图像的字段，可以只配置需要模型检查的那一个：

```json
{
  "autodiscover_images": false,
  "images": [".inspection.photo"]
}
```

对于以下响应：

```json
{
  "inspection": {
    "photo": "data:image/jpeg;base64,/9j/4AAQSk...",
    "thumbnail": "data:image/jpeg;base64,/9j/2wBD..."
  }
}
```

只有 `.inspection.photo` 会被提取；`thumbnail` 保持为普通的负载数据。完整的工具响应如下：

```json
{
  "status": "ok",
  "result": {
    "inspection": {
      "photo": "Image .inspection.photo",
      "thumbnail": "data:image/jpeg;base64,/9j/2wBD..."
    }
  },
  "attachments": [
    {
      "id": ".inspection.photo",
      "type": "image",
      "mime_type": "image/jpeg",
      "data": "/9j/4AAQSk..."
    }
  ]
}
```

#### 二进制响应示例

如果一个 HTTP 端点以 `Content-Type: image/png` 返回原始 PNG 字节，该二进制数据会被视为根 “value”：

```text
Content-Type: image/png

<raw PNG bytes>
```

根负载会被表示为 `Image .`，PNG 字节会作为独立附件返回：

```json
{
  "status": "ok",
  "result": "Image .",
  "attachments": [
    {
      "id": ".",
      "type": "image",
      "mime_type": "image/png",
      "data": "iVBORw0KGgoAAA..."
    }
  ]
}
```

### 元工具（Meta-Tools）

元工具允许构建用于修改 Agent 配置本身的流水线。它们本质上也是普通工具，但通常只暴露给受信任的构建类工作流。

- `agent__create_tool`
- `agent__update_tool`
- `agent__delete_tool`
- `agent__query_tools`
- `agent__create_pipeline`
- `agent__update_pipeline`
- `agent__delete_pipeline`
- `agent__query_pipelines`
- `agent__insert_pipeline_step`
- `agent__update_pipeline_step`
- `agent__delete_pipeline_step`
- `agent__query_providers`
- `agent__query_connections`

## 会话

会话是通过 MQTT 主题路由的可寻址 LLM 状态机。一个会话保存对话历史、待处理事件、排队中的请求、工具调用状态以及用量计数。

会话流量使用两种主题格式：

- `$sess/in/<sid>` —— 发往会话的入站帧。
- `$sess/out/<sid>` —— 来自会话的出站帧。

每个会话由一个集群内唯一的 `sid`（会话 ID）标识。

`$sess/in/<sid>` 上的入站帧：

| 帧类型 | 用途 |
|---|---|
| `request` | 携带 provider、model、instructions、input、tools 及持久化设置，启动一次 LLM 处理。 |
| `tool_result` | 返回会话所请求的工具调用结果。 |
| `event` | 为下一轮 LLM 对话添加新的事件上下文。 |
| `stop` | 显式终止该会话。 |

`$sess/out/<sid>` 上的出站帧：

| 帧类型 | 用途 |
|---|---|
| `intermediate` | 在本轮结束前流式返回一个中间模型输出块。携带 `chunk_type`（例如 `content`）以及 `chunk` 中的数据块。 |
| `tool_request` | 请求等待中的流水线通过 `$cap/...` 调用一个工具。 |
| `final` | 结束当前这一轮 LLM 对话，返回结果及用量计数。 |
| `error` | 报告会话侧的失败，例如 provider 不可用或历史压缩出错。 |

每个出站帧都包含 `sid`、`iid`、`trace_id` 以及累计的 `usage`。目前模型的推理/思考数据块保留在会话内部；只有已发布的流式数据块会以 `intermediate` 帧的形式出现。

启用持久化后，会话不会在 `final` 发布后终止，而是继续存在，可以接收后续请求，形成多轮对话。

## 流水线

一个流水线定义包含一个 ID、一个 MQTT 触发条件以及若干有序步骤。当一条到来的 MQTT 消息匹配触发主题过滤器时，MQTT Agent 会启动一个流水线实例，该消息会作为流水线上下文中的 `$.event` 提供给该实例。

流水线的触发主题是普通的 MQTT 主题过滤器，匹配 `$evt/...` 事件主题，例如：

```text
$evt/device/+/done
```

流水线生命周期事件会以 JSON 形式发布到：

```text
$pipe/<pipeline_id>/inst/<iid>/events
```

支持的步骤类型：

- `call_tool`：调用一个工具（例如 MQTT 发布、HTTP、PostgreSQL、KV 或流存储），并将其结果写入上下文。
- `llm_loop`：将工作发送给一个会话，将选定的工具作为 LLM 工具暴露出去，并在会话返回结果时保存最终结果或结构化结果。
- `break`：根据上下文中的某个值提前终止流水线。

### 流水线上下文

流水线上下文是一个在同一流水线实例的所有步骤间共享的、以二进制为键的映射。它以 `{event: event_payload}` 起始。步骤输入可以使用类似 JSONPath 的字符串（例如 `$.event.device_id` 或 `$.inspection.status`）引用先前的值；步骤输出则通过该步骤设置的 `result_path` 写入，例如 `$.inspection`。

### 流水线逻辑

流水线是单轮处理器：一个触发事件创建一个流水线实例，该实例针对这一事件协调有序的工作，发布完成或失败结果，然后终止。它不是一个长时间运行、面向人类的 Agent 循环。

这是有意为之的设计。面向人类的 Agent 通常依赖人类不断向主会话中补充带反馈的提示，因此以轮次为单位的对话是其天然的外层模型。而 MQTT Agent 面向的是无人值守场景：事件来自设备、Broker 钩子、订阅、规则、外部系统及其他自动化来源，没有单一的人类对话需要维系。取而代之，流水线负责处理单个事件，会话在需要时提供 LLM 层面的连续性，而 `kv_*` 或 `stream_*` 工具则在多个事件之间提供显式的工作流记忆。

流水线可以处于激活或草稿状态；草稿流水线会被保存，但在被激活前不会运行。

### LLM 步骤的键表达式

为了模拟多轮对话，仍然可以在 `llm_loop` 步骤中使用持久化的 LLM 会话。此时，该步骤会为每个流水线实例使用由该步骤的 _键表达式（key expression）_ 所决定的会话标识符。通过使用不同的键表达式，可以按 `clientid`、主题或其他条件各自维护独立的会话。

## 管理入口

主管理界面通过插件 API 网关提供：

```text
/api/v5/plugin_api/emqx_agent/ui
```

其他页面：

```text
/api/v5/plugin_api/emqx_agent/builder/ui
/api/v5/plugin_api/emqx_agent/apple-box/ui
```

同一管理入口也可以通过 `/api/v5/plugin_api/emqx_agent` 下的插件 API 路径访问：

| 路径 | 用途 |
|---|---|
| `/tools` | 列出并创建工具。 |
| `/tools/:type/:id` | 获取、更新或删除一个工具。 |
| `/tools/statuses` | 查看工具运行时协调状态。 |
| `/connections` | 列出并创建工具连接。 |
| `/connections/:id` | 获取、更新或删除一个连接。 |
| `/connections/:id/start` | 启用并协调一个连接。 |
| `/connections/:id/stop` | 停用并协调一个连接。 |
| `/connections/statuses` | 查看连接运行时状态。 |
| `/providers` | 列出已配置的 AI provider。 |
| `/pipelines` | 列出并创建流水线定义。 |
| `/pipelines/:id` | 获取、更新或删除一个流水线。 |

## 演示页面

该插件包含两个浏览器端演示：

- **Pipeline Builder**（位于 `/builder/ui`）：一个聊天式界面，用于构建事件驱动的 AI 工作流。
- **Apple Box Conveyor**（位于 `/apple-box/ui`）：一个模拟苹果箱质检流程的 MQTT/AI 工作流演示。

在 EMQX 启动并启用该插件后，从代码仓库根目录预置演示资源。两个演示都需要一个兼容 OpenAI 的 API Key：

```bash
export OPENAI_API_KEY='sk-...'
```

可选的环境变量：

| 变量 | 默认值 | 用途 |
|---|---|---|
| `EMQX_BASE_URL` | `http://localhost:18083/api/v5/plugin_api/emqx_agent` | MQTT Agent 插件 API 的基础 URL。 |
| `EMQX_CORE_BASE_URL` | `http://localhost:18083/api/v5` | 用于 AI provider 管理的 EMQX 核心 API 基础 URL。 |
| `EMQX_API_CREDS` | `key:secret` | Basic Auth API 凭证。 |
| `OPENAI_BASE_URL` | `https://api.openai.com/v1` | 兼容 OpenAI 的 API 基础 URL。 |
| `OPENAI_MODEL` | 脚本各自的默认值 | 演示流水线使用的模型。 |
| `PGHOST`、`PGPORT`、`PGDATABASE`、`PGUSER`、`PGPASSWORD` | `pgsql`、`5432`、`mqtt`、`root`、`public` | 演示工具使用的 PostgreSQL 连接信息。 |

预置 Apple Box Conveyor 演示：

```bash
python3 plugins/emqx_agent/demo_apple_box_init.py
```

该脚本会创建 `apple-inspector` AI provider、PostgreSQL 连接、apple-box 相关工具、数据库表，以及处于激活状态的 `apple-box-inspection` 流水线。在以下地址打开界面：

```text
/api/v5/plugin_api/emqx_agent/apple-box/ui
```

预置 Pipeline Builder 演示：

```bash
python3 plugins/emqx_agent/demo_builder_init.py
```

该脚本会创建 builder AI provider、PostgreSQL 连接、builder 相关元工具、回复工具、数据库表，以及处于激活状态的 `pipeline-builder` 流水线。在以下地址打开界面：

```text
/api/v5/plugin_api/emqx_agent/builder/ui
```

两个脚本在预置资源前，都可能重新创建各自的演示资源，并删除已存在的 Agent 演示资源。如需显式移除演示资源，运行：

```bash
python3 plugins/emqx_agent/demo_teardown.py
```

## 构建与测试

从代码仓库根目录构建该插件：

```bash
make plugin-emqx_agent
```

运行该插件的 Common Test 测试套件：

```bash
make plugins/emqx_agent-ct
```

依赖 LLM 的演示测试套件需要一个具备相应能力的 LLM，因此只有在设置了 `OPENAI_API_KEY` 时才会运行，否则会被跳过。

## 开发

在节点中构建、安装、启用并启动该插件：

```bash
plugins/emqx_agent/script/start_dev.sh
```

管理界面可通过插件 API 网关访问：

```text
/api/v5/plugin_api/emqx_agent/ui
```

<!-- PLUGIN-DOWNLOADS:BEGIN (auto-generated, do not edit) -->

## 下载

各 EMQX 版本对应的插件安装包：

| EMQX 版本 | 插件版本 | 安装包 |
|---|---|---|
| 6.3.0 | 1.0.0 | [emqx_agent-1.0.0.tar.gz](https://www.emqx.com/downloads/emqx-plugins/6.3.0/emqx_agent-1.0.0.tar.gz) ([sha256](https://www.emqx.com/downloads/emqx-plugins/6.3.0/emqx_agent-1.0.0.sha256)) |

<!-- PLUGIN-DOWNLOADS:END -->
