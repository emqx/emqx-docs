# 与 Azure Event Grid MQTT 桥接

[Azure Event Grid](https://azure.microsoft.com/zh-cn/products/event-grid) 是 Azure 上的全托管事件路由服务。其 MQTT 代理功能支持物联网设备与云端应用之间大规模、标准化的双向 MQTT 通信。EMQX 提供了 Azure Event Grid 的内置连接器，使您能够在 EMQX 与 Azure Event Grid 之间桥接 MQTT 数据，无缝融入 Azure 云服务生态系统。

本页详细介绍了 EMQX 与 Azure Event Grid MQTT 的集成，并提供了创建和验证 Sink 与 Source 的实用操作指导。

## 工作原理

Azure Event Grid 数据集成是 EMQX 的开箱即用功能，将 EMQX 的设备接入与消息传输能力同 Azure Event Grid 云原生 MQTT 代理相结合。EMQX 以 MQTT 客户端身份连接到 Azure Event Grid MQTT 代理，实现双向消息传输：

- **消息流出（Sink）**：EMQX 将本地 MQTT 主题的消息发布到 Azure Event Grid 上的指定主题。
- **消息流入（Source）**：EMQX 订阅 Azure Event Grid 上的主题，并将收到的消息转发到 EMQX 本地主题。

下图展示了该集成的典型架构：

![EMQX Integration Azure Event Grid](./assets/emqx-integration-azure-event-grid.png)

## 特性与优势

与 Azure Event Grid 的数据集成具有以下特性和优势：

- **基于标准协议的 MQTT 桥接**：Azure Event Grid 支持 MQTT 3.1.1 和 MQTT 5.0，EMQX 可使用标准 MQTT 协议与之桥接，并与任何支持 MQTT 的客户端或服务互通。
- **双向数据流**：支持从 EMQX 向 Azure Event Grid 发布消息（Sink），以及订阅 Azure Event Grid 主题并将消息转发到 EMQX（Source），实现灵活的物联网数据路由。
- **安全连接**：Azure Event Grid 要求使用 TLS，连接器默认启用 TLS，并支持客户端证书认证，这是生产环境中推荐的认证方式。
- **灵活的主题映射**：通过 EMQX 规则引擎，可以对消息进行过滤、转换，并通过动态主题映射将其路由到特定的 Azure Event Grid 主题空间。
- **丰富的 Azure 生态集成**：数据到达 Azure Event Grid 后，可路由到 Azure Functions、Azure Event Hubs、Azure Storage 等其他 Azure 服务，进行进一步的处理和分析。

## 准备工作

### 前置准备

- 了解[规则](./rules.md)。
- 了解[数据集成](./data-bridges.md)。

### 设置 Azure Event Grid

在 EMQX 中创建数据集成之前，需要先在 Azure 中设置启用了 MQTT 代理支持的 Event Grid 命名空间。以下 Microsoft 官方文档提供了分步操作指导：

- [快速入门：使用 Azure Event Grid 命名空间发布和订阅 MQTT 消息](https://learn.microsoft.com/zh-cn/azure/event-grid/mqtt-publish-and-subscribe-portal)
- [Azure Event Grid MQTT 代理概述](https://learn.microsoft.com/zh-cn/azure/event-grid/mqtt-overview)
- [如何使用证书链对 MQTT 客户端进行身份验证](https://learn.microsoft.com/zh-cn/azure/event-grid/mqtt-certificate-chain-client-authentication)

完成设置后，请记录以下连接信息，创建连接器时需要用到：

- **主机名**：Event Grid 命名空间的 MQTT 代理主机名，格式为 `<namespace>.ts.<region>.eventgrid.azure.net`，端口为 `8883`。
- **认证方式**：客户端证书（推荐）或 Microsoft Entra ID JWT。如使用客户端证书，请准备好证书和私钥文件。
- **主题空间**：您在 Azure Event Grid 中配置的主题空间和权限绑定。

::: tip

Azure Event Grid MQTT 需要使用 TLS，不支持用户名/密码认证，请使用客户端证书或 Microsoft Entra ID JWT 进行认证。

:::

## 创建连接器

本节介绍如何创建连接器，将 EMQX 连接到 Azure Event Grid。

1. 在 EMQX Dashboard 中，点击**集成** -> **连接器**。

2. 点击页面右上角的**创建**。

3. 在**创建连接器**页面，选择 **Azure Event Grid**，然后点击**下一步**。

4. 输入连接器名称，要求为大小写字母和数字的组合，例如 `my_azure_event_grid`。

5. 填写连接相关配置：

   - **服务器地址**：输入 Event Grid 命名空间的 MQTT 代理地址，例如 `myns.northeurope-1.ts.eventgrid.azure.net:8883`。默认端口为 `8883`。
   - **客户端 ID 前缀**：（可选）为 EMQX 自动生成的客户端 ID 指定前缀。EMQX 会按照 `[前缀]:{连接器名称}{随机字符串}:{连接池序号}` 的格式生成唯一客户端 ID。详情请参阅[连接池与客户端 ID 生成规则](./data-bridge-mqtt.md#连接池与客户端-id-生成规则)。
   - **用户名**和**密码**：如使用客户端证书认证，可留空。如 Event Grid 命名空间配置有其他要求，请填写相应凭证。
   - **Keepalive**：指定心跳间隔，单位为秒，默认值为 `160`。
   - **MQTT 协议版本**：选择 MQTT 协议版本。Azure Event Grid 支持 MQTT 3.1.1（`v4`）和 MQTT 5.0（`v5`）。
   - **静态客户端 ID 映射表**：（可选）为特定 EMQX 节点配置静态客户端 ID，适用于 Azure Event Grid 要求预先注册客户端 ID 的场景。详情请参阅[配置静态客户端 ID](./data-bridge-mqtt.md#配置静态客户端-id)。

     ::: tip

     配置静态客户端 ID 映射后，只有在映射中明确指定的 EMQX 节点才会创建 MQTT 连接。

     :::

   - **清除会话**：默认启用。启用后，EMQX 每次连接到 Azure Event Grid 时都会创建新的会话。
   - **启用 TLS**：默认启用。Azure Event Grid 要求使用 TLS。如使用客户端证书认证，请在此处配置证书和私钥。详细的 TLS 配置选项，请参阅[外部资源访问的 TLS](../network/overview.md#启用-tls-加密访问外部资源)。

6. 高级设置（可选）：详情请参阅[数据集成特性](./data-bridges.md#features-of-sink)。

7. 点击**创建**之前，可先点击**测试连接**，验证 EMQX 是否能成功连接到 Azure Event Grid。

8. 点击**创建**按钮完成连接器创建。弹出**创建成功**对话框，询问是否立即创建规则。点击**创建规则**可直接进入规则创建页面并自动选中该连接器，或点击**返回连接器列表**稍后再创建规则。

## 创建 Azure Event Grid Sink 规则

本节介绍如何创建规则，将 EMQX 本地主题 `t/#` 的 MQTT 消息转发到 Azure Event Grid。

1. 如果在上一步点击了**创建规则**，**添加动作**面板会自动打开，且**动作类型**已设置为 `Azure Event Grid`，连接器已预先选中，可直接跳到第 5 步。

   否则，在 EMQX Dashboard 中点击**集成** -> **规则**，点击右上角的**创建**，然后点击 **+ 添加动作**。

2. 在左侧 **SQL 编辑器**中输入规则 ID 和以下 SQL，匹配来自主题 `t/#` 的消息：

   注意：如需自定义 SQL 语法，请确保 `SELECT` 部分包含 Sink 所需的所有字段。

   ```sql
   SELECT
     *
   FROM
     "t/#"
   ```

   ::: tip

   如果您是初学者，可点击 **SQL 示例**和**启用调试**来学习和测试 SQL 规则。

   :::

3. 在右侧**添加动作**面板中，从**动作类型**下拉列表中选择 `Azure Event Grid`，**动作**下拉框保持默认的`创建动作`。

4. 在**连接器**下拉框中选择刚创建的 `my_azure_event_grid` 连接器。也可以点击下拉框旁的按钮创建新连接器，配置参数请参考[创建连接器](#创建连接器)。

5. 输入 Sink 的名称和可选描述。

6. 配置向 Azure Event Grid 发布消息的 Sink 参数：

   - **主题**：发布到 Azure Event Grid 的主题，支持 `${var}` 占位符。例如，输入 `devices/${clientid}/messages` 可根据客户端 ID 动态设置主题。
   - **QoS**：发布消息的 QoS 等级。可选 `0`、`1`、`2`，或使用 `${qos}` 等占位符跟随原始消息的 QoS。
   - **Retain**：选择 `true`、`false` 或使用 `${flags.retain}` 等占位符设置保留标志。
   - **消息模版**：消息 payload 模板。留空则转发完整的规则输出，或输入 `${payload}` 仅转发 payload。

7. **备选动作（可选）**：如需在消息投递失败时提高可靠性，可定义一个或多个备选动作，当主 Sink 处理消息失败时触发。详情请参阅[备选动作](./data-bridges.md#fallback-actions)。

8. **高级设置（可选）**：详情请参阅[数据集成特性](./data-bridges.md#features-of-sink)。

9. 点击**创建**之前，可先点击**测试连接**，验证 Sink 能否连接到 Azure Event Grid。

10. 点击**创建**按钮完成 Sink 配置，新 Sink 将添加到**动作输出**中。

11. 返回**创建规则**页面，确认配置信息无误后，点击**保存**按钮生成规则。

至此，您已成功创建规则。可以在**集成** -> **规则**页面查看新创建的规则，点击**动作（Sink）**标签可查看新的 Azure Event Grid Sink。

您还可以点击**集成** -> **流程设计器**查看拓扑图，确认主题 `t/#` 下的消息经规则 `my_rule` 处理后被转发到 Azure Event Grid。

## 创建 Azure Event Grid Source 规则

本节介绍如何创建规则，订阅 Azure Event Grid 上的消息并将其转发到 EMQX 本地主题。

### 创建 Azure Event Grid Source 并添加到规则

1. 在 EMQX Dashboard 中，点击**集成** -> **规则**，然后点击右上角的**创建**。

2. 输入 `my_rule_source` 作为规则 ID。

3. 配置规则的触发来源。在页面右侧**数据输入**标签下，删除默认的**消息**类型输入，然后点击**添加输入**以创建 Azure Event Grid Source。

4. 在**添加输入**对话框中，从**输入类型**下拉框中选择 `Azure Event Grid`，**Source** 下拉框保持默认的`创建 Source`。

5. 输入 Source 的名称和描述。

6. 在下拉框中选择 `my_azure_event_grid` 连接器。

7. 配置订阅 Azure Event Grid 的 Source 参数：

   - **主题**：订阅 Azure Event Grid 的主题，支持 `+` 和 `#` 通配符。

     ::: tip

     当 EMQX 在集群模式下运行，或连接器配置了连接池时，请使用共享订阅以避免重复消息，例如 `$share/group/devices/#`。

     :::

   - **QoS**：订阅的 QoS，从下拉框中选择 `0` 或 `1`。

8. 点击**创建**按钮完成 Source 创建，规则 SQL 将自动更新为：

   ```sql
   SELECT
     *
   FROM
     "$bridges/azure_event_grid:<source_name>"
   ```

### 创建重新发布动作

从 Azure Event Grid 订阅的消息不会自动转发到 EMQX 本地主题，需要创建重新发布（Republish）动作来完成消息路由。

1. 切换到创建规则页面右侧的**动作输出**标签，点击**添加动作**。

2. 从**动作类型**下拉框中选择`重新发布`。

3. 配置重新发布参数：
   - **主题**：输入目标本地主题，例如 `azure/${topic}`，为原始主题添加 `azure/` 前缀。
   - **QoS**：选择 `${qos}` 跟随原始消息的 QoS，或设置固定值。
   - **Retain**：选择 `false` 或使用占位符。
   - **消息模版**：输入 `${payload}` 仅转发 payload，或留空以转发完整的规则输出。

4. 点击**添加**完成动作创建，然后点击**保存**生成规则。

## 测试规则

### 测试 Sink

使用 [MQTTX](https://mqttx.app/) 向 EMQX 的主题 `t/1` 发布一条消息：

```bash
mqttx pub -i emqx_c -t t/1 -m '{ "msg": "Hello Azure Event Grid" }'
```

查看 Azure Event Grid Sink 的运行统计，应有 1 条新的匹配消息和 1 条新的发出消息。在 Azure 门户或使用 Azure Event Grid MQTT 客户端验证消息是否已被正确接收。

### 测试 Source

1. 订阅 EMQX 本地主题 `azure/#`：

   ```bash
   mqttx sub -t azure/# -q 1 -v
   ```

2. 使用配置了 Azure Event Grid 凭证的 MQTT 客户端向 Azure Event Grid 发布消息：

   ```bash
   mqttx pub -t devices/device1/messages -m "hello from azure" \
     -h myns.northeurope-1.ts.eventgrid.azure.net -p 8883 \
     --tls --cert /path/to/client.crt --key /path/to/client.key
   ```

3. 您将在 EMQX 的 `azure/devices/device1/messages` 主题上收到该消息：

   ```bash
   topic: azure/devices/device1/messages
   payload: hello from azure
   ```
