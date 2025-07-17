# 快速开始：使用 Gemini 节点创建 Flow

本页演示如何通过一个实际用例，在 Flow 设计器中快速创建并测试一个使用 Gemini 节点进行数据处理的 Flow。

此示例展示了如何构建一个 Flow，集成 Gemini LLM 以处理包含纯文本 `prompt` 字段的 MQTT 设备消息，同时保留 `clientid` 用于路由。Gemini 节点基于该 `prompt` 生成回复，消息重发布节点将 AI 的回复发送到按客户端区分的主题 `device/${clientid}/reply`，确保每个设备都能收到定制化的建议。

## 场景描述

在智慧城市部署中，每个街区配备环境监测传感器，定期将 JSON 消息发布到主题 `devices/<district_id>`。每条消息的 `prompt` 字段以纯文本形式包含关键环境数据，例如空气质量指数和噪声水平。流程将执行以下步骤：

- **数据处理**：从 `prompt` 字段中提取环境监测读数，并暴露 `clientid`（即 `district_id`）以供下游使用。
- **LLM 处理**：将提取出的读数发送给 Gemini，生成可执行的公共安全或交通管理建议（例如：限行、调整路灯亮度）。
- **消息重发布**：将 AI 生成的建议发布到按街区区分的控制主题 `device/<district_id>/reply`。

**示例消息（发布到 `devices/district_1`）：**

```json
{
  "prompt": "空气质量指数为150。噪声水平为72分贝。"
}
```

**期望输出（由 LLM 生成，发布到 `device/district_1/reply`）：**

```
空气质量指数偏高，在 district_1 实施交通管制，并增加行人巡逻。
```

## 创建流程

::: tip 前置条件

确保您拥有有效的 Gemini API 密钥。

:::

1. 点击 **Flows** 页面上的 **创建 Flow** 按钮。

2. 添加一个**消息**节点：

    - 从左侧面板的 **Source** 区域拖拽一个**消息**节点。
    - 设置订阅主题为 `devices/+`。
    - 点击**保存**。

3. 添加一个 **Processing** 节点：

   - 从 **Processing** 区域拖拽一个**数据处理**节点。
   - 在表单中填写以下配置。此设置会将 `clientid` 暴露出来，以便在后续节点中使用（例如在重发布主题中使用 `${clientid}`）。
     - **字段**：`clientid`
     - **转换**：留空
     - **别名**：`clientid`
   - 点击**保存**。

4. 添加一个 **Gemini** 节点：

   - 从 **Processing** 区域拖拽一个 **Gemini** 节点。

   - 配置节点参数如下：

     - **输入**：填写 `payload.prompt`。

     - **系统消息**：填写以下提示消息：

       ```
       你是一位智慧城市领域的 AI 专家。  
       根据用户提示中提供的空气质量指数和噪声水平，为指定区域生成简洁的公共安全或交通管理建议。  
       仅返回一句包含行动步骤的建议，不要有额外说明。
       ```

     - **模型**：保持默认的 `gemini-2.0-flash`。

     - **API 密钥**：填写你的 Gemini API 密钥。

     - **基础 URL**：留空以使用默认端点。

     - **输出结果别名**：`ai_reply`。

   - 点击**保存**。

5. 添加一个**消息重发布**节点：

   - 从 **Sink** 区域拖拽一个**消息重发布**节点。
   - 设置**主题**为：`device/${clientid}/reply`。
   - 设置 **Payload** 为：`${ai_reply}`。
   - 点击**保存**。

6. 将所有节点连接，然后点击页面右上角**保存**，完成 Flow 创建。

   ![gemini_node_flow](./assets/gemini_node_flow.png)

   Flow 与规则引擎的表单规则兼容，也可以在规则页面中查看对应的 SQL 和配置。

   ![gemini_node_rule_page](./assets/gemini_node_rule_page.png)

## 测试 Flow

1. 使用 MQTT 客户端连接至 EMQX。

   你可以使用 Dashboard 中的**诊断工具 -> WebSocket 客户端**模拟发布者，也可以使用 [MQTTX](https://mqttx.app/zh) 或真实设备：

   - 连接至 EMQX；
   - 订阅主题 `devices/district_1/reply`。

2. 启动测试：

   - 在 Flow 设计器中点击任意节点打开编辑面板。

   - 点击**编辑**，再点击**开始测试**，底部将出现测试窗口。

   - 点击**输入模拟数据**，并发布如下消息至主题 `devices/district_1`：

     ```json
     {
       "prompt": "空气质量指数为150。噪声水平为72分贝。"
     }
     ```

3. 查看测试结果：

   - 若流程执行成功，将显示响应内容：

     ![openai_node_test_result](./assets/gemini_node_test_result.png)

   - 返回 WebSocket 客户端页面，可收到类似以下内容：

     > "限制高排放车辆的通行，并加强对高噪声车辆的监管。"

   - 若测试失败，系统会提示错误原因。

   - 若需查看该 **Gemini** 节点的运行统计，退出编辑页面，在创建 Flow 页面中点击节点，在弹出的编辑面板中点击**概览**选项卡。

     ![openai_node_statistics](./assets/gemini_node_statistics.png)