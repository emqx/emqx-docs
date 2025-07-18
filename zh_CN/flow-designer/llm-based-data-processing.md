# 基于 LLM 的 MQTT 数据处理

从 EMQX 5.10.0 开始，Flow 设计器支持集成大型语言模型（LLM），如 OpenAI GPT，Anthropic Claude 和 Gemini。借助该功能，用户可以通过自然语言提示构建智能的数据处理流程，实现日志摘要、传感器数据分类、消息增强或实时洞察等任务。

## 功能概览

Flow 设计器中的 LLM 处理节点是一类由 AI 驱动的组件，用于将消息内容发送至外部 LLM 接口进行处理。这些节点可以将 MQTT 消息传递给模型（如 `gpt-4o` 或 `claude-3-sonnet`），获取响应结果，并将其传递给后续流程节点。

::: tip 提示

由于调用 LLM 及其处理数据本身需要一定时间，整个流程可能耗时几秒到十几秒，具体取决于所使用模型的响应速度。因此，LLM 处理节点不适用于消息吞吐量（TPS）较高的场景。

:::

### 核心概念

- **LLM Provider（大语言模型提供商）**：AI 服务的命名配置（如 OpenAI，Anthropic 或 Gemini）。
- **Completion Profile（补全参数配置）**：可复用的模型参数集合（模型 ID、系统提示、Token 限制等）。
- **AI Completion Node（AI 补全节点）**：将输入发送至 LLM 并将响应结果存储为用户定义变量的节点。
- `ai_completion`：一个 SQL 函数，用于将文本或二进制数据发送至 LLM 并返回响应结果。

### 工作原理

当 Flow Designer 接收到一条 MQTT 消息时，AI 补全节点会在内部调用内置 SQL 函数 `ai_completion/2,3`，将数据发送到配置好的 LLM。

```mermaid
graph LR
  A[MQTT 消息] --> B[Flow Designer 节点]
  B --> C["Rule SQL (ai_completion)"]
  C --> D[LLM API]
  D --> E[LLM 响应]
  E --> F[下游节点]
```

1. 消息首先通过**消息**节点（订阅指定主题）进入流程。
2. 可选的 **Processing** 节点可用于提取或转换字段，如 `device_id`、`payload` 或 `timestamp`。
3. **AI 补全**节点（OpenAI，Anthropic 或 Gemini）在内部使用 `ai_completion` 函数：
   - 查找已配置的 **Completion Profile**，其中包含提供商信息、模型名、系统提示等参数。
   - 将选定的输入字段（如 `payload`）发送给 LLM。
   - 接收来自 LLM API 的响应结果（如摘要或分类信息）。
4. 响应结果存储在配置的**输出结果别名**中，供下游节点使用，例如：
   - **消息重发布**：将结果发布到新的 MQTT 主题
   - **数据库**：将结果写入数据库（如 PostgreSQL、MongoDB）
   - **桥接**：转发至远程 MQTT Broker 或云服务

### 支持的 LLM 提供商

EMQX 5.10.0 当前支持以下 LLM 服务：

- **OpenAI**：支持 GPT-3.5、GPT-4、GPT-4o 等模型
- **Anthropic**：支持 Claude 3 系列模型
- **Gemini**：支持 gemini-2.0-flash、gemini-2.5-flash、gemini-2.5-pro 等模型

::: tip 兼容性说明

除了上述官方列出的提供商，EMQX 还支持任何与 OpenAI 协议兼容的 LLM 服务。

:::

## 配置 LLM 处理节点

要在 Flow Designer 中使用 LLM，需为所选的模型提供商（OpenAI 或 Anthropic）配置专属的处理节点。每个节点允许用户指定输入字段、模型行为（通过系统提示语定义）以及结果存储的变量名。这些节点会在后台自动调用 `ai_completion` 函数，完成消息处理。

### 配置 OpenAI 节点

使用 OpenAI 节点步骤如下：

1. 从 **Processing** 面板拖拽 **OpenAI** 节点至画布。

2. 将其连接到一个消息节点或数据处理节点。

3. 配置以下参数：

   - **输入**：输入数据来源字段，支持 `event`、`id`、`clientid`、`username`、`payload` 等。

   - **系统消息**：填写提示信息，引导大语言模型生成符合预期的输出，例如："请将输入 JSON 中的所有数值类型字段相加，只返回结果"。

   - **模型**：选择使用的模型，如 `gpt-4o`、`gpt-3.5-turbo`。

   - **API 密钥**：输入你的 OpenAI API 密钥。

   - **基础 URL**：可选项，填写自定义 API 服务地址；留空表示使用默认 OpenAI API 服务地址。

     ::: tip

     你可以通过填写自定义 API 地址和密钥，连接其他与 OpenAI 协议兼容的服务商。

     :::

   - **输出结果别名**：指定一个变量名用于保存模型的响应结果，后续节点可通过该别名引用结果，例如 `summary`。

     ::: tip 提示

     如果变量名包含字母、数字、下划线以外的字符，或以数字开头，或是 SQL 保留字，请使用双引号括起变量名。

     :::

4. 点击**保存**应用配置。

### 配置 Anthropic 节点

使用 Anthropic 节点步骤如下：

1. 从 **Processing** 面板中拖拽 **Anthropic** 节点至画布。

2. 将其连接到一个消息节点或数据处理节点。

3. 配置以下参数：

   - **输入**：输入数据来源字段，支持 `event`、`id`、`clientid`、`username`、`payload` 等。

   - **系统消息**：填写提示信息，引导大语言模型生成符合预期的输出，例如："请将输入 JSON 中的所有数值类型字段相加，只返回结果"。

   - **模型**：选择使用的模型，如 `claude-3-sonnet-20240620`。

   - **最大令牌数**：设置返回结果的最大 token 数，默认值为 `100`。

   - **Anthropic 版本**：Anthropic API 版本号，默认值为 `2023-06-01`。

   - **API 密钥**：输入你的 Anthropic API 密钥。

   - **基础 URL**：可选项，填写自定义 API 服务地址；留空表示使用默认 Anthropic API 服务地址。

   - **输出结果别名**：指定一个变量名用于保存模型的响应结果，后续节点可通过该别名引用结果，例如 `summary`。

     ::: tip 提示

     如果变量名包含字母、数字、下划线以外的字符，或以数字开头，或是 SQL 保留字，请使用双引号括起变量名。

     :::

4. 点击**保存**应用配置。

### 配置 Gemini 节点

使用 Gemini 节点步骤如下：

1. 从 **Processing** 面板中拖拽一个 **Gemini** 节点至画布。

2. 将其连接到一个消息节点或数据处理节点。

3. 配置以下参数：

   - **输入**：输入数据来源字段。可选项包括：`event`、`id`、`clientid`、`username`、`payload` 等。

   - **系统消息**：填写提示信息，引导大语言模型生成符合预期的输出，例如：“将输入 JSON 数据中所有数值字段相加并输出结果；只返回结果值”。

   - **模型**：选择要使用的 Gemini 模型，例如 `gemini-2.0-flash`、`gemini-2.5-pro` 等。

   - **API Key**：输入你的 Gemini API 密钥。

   - **基础 URL**：可选项，填写自定义 API 服务地址；留空表示使用默认 Gemini API 服务地址。

   - **输出结果别名**：指定一个变量名用于保存模型的响应结果，后续节点可通过该别名引用结果，例如 `summary`。

     ::: tip 提示

     如果变量名包含字母、数字、下划线以外的字符，或以数字开头，或是 SQL 保留字，请使用双引号括起变量名。

     :::

4. 点击**保存**应用配置。

## 快速开始

以下两个示例演示如何快速创建并测试基于 LLM 的处理流程：

- [使用 OpenAI 节点创建 Flow](./openai-node-quick-start.md)：使用 GPT 模型处理或摘要 MQTT 消息。
- [使用 Anthropic 节点创建 Flow](./anthropic-node-quick-start.md)：使用 Claude 模型处理 MQTT 消息中的数值字段。
- [使用 Gemini 节点创建 Flow](./gemini-node-quick-start.md)：使用 Gemini 模型根据 MQTT 消息中的提示词生成上下文相关的回复，并利用 MQTT 客户端 ID 将结果路由到对应的客户端主题。

