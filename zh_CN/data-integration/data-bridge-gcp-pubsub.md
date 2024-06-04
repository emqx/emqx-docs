# 将 MQTT 数据传输到 GCP Pub/Sub

::: tip

GCP Pub/Sub 数据集成是 EMQX 企业版功能。

:::

[Google Cloud Pub/Sub](https://cloud.google.com/pubsub?hl=en-us) 是一种异步消息传递服务，旨在实现极高的可靠性和可扩缩性。EMQX 支持与 Google Cloud Pub/Sub 的无缝集成，能够实时提取、处理和分析 MQTT 数据，并将数据推送到各类 Google Cloud 服务，如 Cloud Functions、App Engine、Cloud Run、Kubernetes Engine 和 Compute Engine 中，或将 Google Cloud 中的数据通过 MQTT 下发，帮助用户更快的基于 GCP 构建物联网应用。

本页详细介绍了 EMQX 与 GCP Pub/Sub 的数据集成并提供了实用的规则和 Sink/Source 创建指导。

## 工作原理

GCP Pub/Sub Sink 是 EMQX 的开箱即用功能，旨在帮助用户轻松地将 MQTT 数据流与 Google Cloud 集成，并利用其丰富的服务和功能实现物联网应用开发。

![GCP_bridge_architect](./assets/gcp_pubsub/GCP_bridge_architect.png)

EMQX 通过规则引擎与 Sink 将 MQTT 数据转发至 GCP Pub/Sub，以 GCP Pub/Sub 生产者角色为例，其完整流程如下：

1. **物联网设备发布消息**：设备通过特定的主题发布遥测和状态数据，消息将触发规则引擎。
2. **规则引擎处理消息**：通过内置的规则引擎，可以根据主题匹配处理特定来源的 MQTT 消息。规则引擎会匹配对应的规则，并对消息进行处理，例如转换数据格式、过滤掉特定信息或使用上下文信息丰富消息。
3. **桥接到 GCP Pub/Sub**：规则触发将消息转发到 GCP Pub/Sub 的动作，允许轻松配置数据到 GCP Pub/Sub 属性，排序键，以及 MQTT 主题到 GCP Pub/Sub 主题的映射关系，可以为数据集成提供更丰富的上下文信息和顺序保证，实现灵活的物联网数据处理。

MQTT 消息数据写入到 GCP PusSub 后，您可以进行灵活的应用开发，例如：

- 实时数据处理和分析：利用 Google Cloud 的强大数据处理和分析工具，如 Dataflow、BigQuery 和 Pub/Sub 自身的流处理功能，对消息数据进行实时处理和分析，从而获得有价值的洞察和决策支持。

- 事件驱动的功能：触发 Google Cloud 的事件处理如 Cloud Functions 和 Cloud Run，以实现动态、灵活的功能触发和处理。

- 数据存储和共享：将消息数据传输到 Google Cloud 的存储服务中，如 Cloud Storage 和 Firestore，以便安全地存储和管理大量的数据，并与其他 Google Cloud 服务共享和分析这些数据，以满足不同的业务需求。

## 特性与优势

将 EMQX 与 GCP Pub/Sub 结合使用具有以下特性与优势：

- **强大的消息传递服务**：EMQX 与 GCP Pub/Sub 都具备高可用、可扩展的特性，能够可靠地接收、传递和处理大规模的消息流，支持物联网数据顺序传递、消息质量保证以及持久化等特性，确保消息的可靠传递和处理。

- **灵活的规则引擎**：通过内置的规则引擎，可以根据主题匹配处理特定来源的消息和事件。并对消息和事件进行处理，例如转换数据格式、过滤掉特定信息或使用上下文信息丰富消息，结合 GCP Pub/Sub 可以进行进一步处理和分析。

- **丰富的上下文信息**：通过 GCP Pub/Sub 桥接，你可以在消息中添加更丰富的上下文信息，实现客户端属性与 Pub/Sub 属性、排序键等的映射，可以帮助在后续的应用开发和数据处理中进行更精确的分析和处理。

综上所述，将 EMQX 和 GCP Pub/Sub 结合使用可以实现高可靠性、可扩展性的消息传递，并通过丰富工具和服务进行数据分析与集成，这使得你能够构建强大的物联网应用，并基于事件驱动的功能实现灵活的业务逻辑。

## 准备工作

本节介绍如何配置 GCP Pub/Sub，并创建主题与获取连接凭证。

### 前置准备

- 了解[规则](./rules.md)。
- 了解[数据集成](./data-bridges.md)。

### 创建服务账户凭证

服务账户凭证是用于身份验证和授权的 JSON 文件，EMQX 需要通过它访问 Pub/Sub 资源。

1. 进入 GCP 控制台，在搜索框中输入 **IAM** 并进入 **IAM & Admin** 页面。
2. 在 IAM & Admin 页面中点击 **Service Accounts** -> **Email** 中对应的邮箱，选择 **KEYS** 标签页，点击 **ADD KEY** 添加以生成用于身份认证 JSON 格式的 key，请妥善保管该文件。

![GCP 服务账户凭证](./assets/gcp_pubsub/gcp-service-account.png)

### 在 GCP Pub/Sub 中创建主题

1. 打开 [Pub/Sub 控制台](https://console.cloud.google.com/cloudpubsub)，点击 **CREATE TOPIC**，输入自定义的 **Topic ID**，点击 **CREATE** 即可完成创建。

![GCP PubSub 创建主题](./assets/gcp_pubsub/gcp-pubsub-topic-create.png)

2. 点击列表页对应的 **Topic ID** 即可进入 Topic 详情页面，您需要创建一个 **subscription** 来保存消息，有关 subscription 详细介绍请参考 [CCP Pub/Sub subscription](https://cloud.google.com/pubsub/docs/subscriber)，此处选择 Pull 类型，保留 7 天历史消息。

![GCP PubSub 创建订阅](./assets/gcp_pubsub/gcp-pubsub-subscription-create.png)

3. 点击 **Subscription ID** → **MESSAGES** → **PULL** 可以在线查看发送到主题中的消息。

## 创建 GCP Pub/Sub 生产者连接器

在添加 GCP Pub/Sub 生产者 Sink 操作之前，您需要创建 GCP Pub/Sub 生产者连接器，以建立 EMQX 与 GCP Pub/Sub 之间的连接。

1. 转到 EMQX Dashboard，点击 **集成** -> **连接器**。
2. 在页面的右上角点击 **创建**，在连接器选择页面选择 **Google PubSub 生产者**，然后点击 **下一步**。
3. 输入连接器名称和描述，例如 `my-pubsubproducer`。名称用于将 GCP Pub/Sub 生产者 Sink 与连接器关联，并且必须在集群内唯一。
4. 在 **GCP 服务账户凭证** 中，上传您在 [创建服务账户凭证](#创建服务账户凭证) 中导出的 JSON 格式的服务账户凭证。
5. 在点击 **创建** 之前，您可以点击 **测试连接** 以测试连接器是否能连接到 GCP Pub/Sub 服务器。
6. 点击底部的 **创建** 按钮完成连接器的创建。在弹出对话框中，您可以点击 **返回连接器列表** 或点击 **创建规则** 继续创建带有 GCP Pub/Sub 生产者 Sink 的规则，以指定要转发到 GCP Pub/Sub 的数据。详细步骤请参见 [创建 GCP Pub/Sub 生产者 Sink 规则](#创建-gcp-pub-sub-生产者-sink-规则)。

## 创建 GCP Pub/Sub 生产者 Sink 规则

本节演示了如何为 Google PubSub Sink 创建一条规则以指定需要转发至 GCP Pub/Sub 的数据。

1. 转到 Dashboard **集成** -> **规则页面**。

2. 点击页面右上角的**创建**。

3. 输入规则 ID，例如： `my_rule`。

4. 在 SQL 编辑器中输入规则，请确保规则选择出来的字段（SELECT 部分）包含 HTTP 请求消息体模版中用到的变量。例如将 `/devices/+/events` 主题的 MQTT 消息集成到 GCP Pub/Sub，此处规则 SQL 如下：

   ```sql
   SELECT
     *
   FROM
     "/devices/+/events"
   ```

   ::: tip

   如果您初次使用 SQL，可以点击 **SQL 示例** 和**启用调试**来学习和测试规则 SQL 的结果。

   :::

5. 点击右侧的**添加动作**按钮，为规则在被触发的情况下指定一个动作。在**动作类型**下拉框中选择 `Google PubSub 生产者`，以便 EMQX 将规则处理后的数据发送到 GCP Pub/Sub。

6. 保持**动作**下拉框为默认的`创建动作`选项，您也可以选择一个之前已经创建好的 Google PubSub Producer Sink。此处我们创建一个全新的 Sink 并添加到规则中。

7. 在 **名称** 字段中，为 Sink 输入一个名称。名称应为大小写字母和数字的组合。

8. 从 **连接器** 下拉框中选择刚刚创建的 `my_pubsubprodcer`。您也可以通过点击下拉框旁边的按钮创建一个新的连接器。关于配置参数，请参见 [创建连接器](#创建连接器)。

9. 在 **GCP PubSub 主题** 中，输入您在 [创建和管理 GCP 中的主题](#在-gcp-pub-sub-中创建主题) 中创建的主题 ID `my-iot-core`。

10. 在 **HTTP 请求消息体模版** 中定义模板，或留空。

   - 如果留空，它将使用 JSON 格式对 MQTT 消息中的所有可见输入进行编码，例如 clientid、topic、payload 等。
   - 如果使用定义的模板，占位符的形式为 `${variable_name}`，将用 MQTT 上下文中的相应值填充。例如，如果 MQTT 消息主题是 `my/topic`，则 `${topic}` 将被替换为 `my/topic`。


11. 在 **属性模板** 和 **排序键模板** 中定义用于格式化传出消息的属性和/或排序键的模板。
    - 对于 **属性模版**，键和值都可以使用形式为 `${variable_name}` 的占位符。这些值将从 MQTT 上下文中提取。如果键模板解析为空字符串，则该键不会包含在传出到 GCP Pub/Sub 的消息中。
    - 对于 **排序键模版**，可以使用形式为 `${variable_name}` 的占位符。如果解析的值为空字符串，则不会为 GCP Pub/Sub 传出消息设置 `orderingKey` 字段。

12. 高级设置（可选）：详细信息，请参见 [Sink 的特性](./data-bridges.md#sink-的特性)。
13. 在点击 **创建** 之前，您可以点击 **测试连接性** 来测试连接器是否能连接到 GCP Pub/Sub 服务器。
14. 点击 **创建** 按钮完成 Sink 配置，您将在 **动作输出** 标签下看到新的 Sink。
15. 回到 **创建规则** 页面，点击 **创建** 来创建规则。

您现在已成功创建了规则。您可以在 **集成** -> **规则** 页面看到新创建的规则。点击 **动作(Sink)** 标签，您可以看到新的 Google PubSub 生产者 Sink。

您也可以点击 **集成** -> **流程设计器** 查看拓扑，并且可以看到，通过规则 `my_rule` 解析后，主题 `/devices/+/events` 下的消息被发送并保存到 GCP Pub/Sub。

至此您已经完成整个创建过程，可以前往 **集成** -> **Flow 设计器** 页面查看拓扑图，此时应当看到 `/devices/+/events` 主题的消息经过名为 `my_rule` 的规则处理，处理结果写入到 GCP Pub/Sub 中。

## 测试 GCP Pub/Sub 生产者 Sink 规则

1. 使用 MQTTX 向 `/devices/+/events` 主题发布消息：

```bash
mqttx pub -i emqx_c -t /devices/+/events -m '{ "msg": "hello GCP PubSub" }'
```

2. 查看 Sink 运行统计，命中、发送成功次数均 +1。

3. 前往 GCP Pub/Sub 控制台查看数据是否已经发送成功。

## 创建 GCP Pub/Sub 消费者连接器

在添加 GCP Pub/Sub 消费者 Sink 操作之前，您需要创建 GCP Pub/Sub 消费者连接器，以建立 EMQX 与 GCP Pub/Sub 之间的连接。

1. 转到 EMQX Dashboard，点击 **集成** -> **连接器**。
2. 在页面的右上角点击 **创建**，在连接器选择页面选择 **Google PubSub 消费者**，然后点击 **下一步**。
3. 输入连接器名称和描述，例如 `my-pubsubconsumer`。名称用于将 GCP Pub/Sub 生产者 Sink 与连接器关联，并且必须在集群内唯一。
4. 在 **GCP 服务账户凭证** 中，上传您在 [创建服务账户凭证](#创建服务账户凭证) 中导出的 JSON 格式的服务账户凭证。
5. 在点击 **创建** 之前，您可以点击 **测试连接** 以测试连接器是否能连接到 GCP Pub/Sub 服务器。
6. 点击底部的 **创建** 按钮完成连接器的创建。在弹出对话框中，您可以点击 **返回连接器列表** 或点击 **创建规则** 继续创建带有 GCP Pub/Sub 消费者 Source 的规则，以消费来自 GCP Pub/Sub 的数据并转发到 EMQX 本地。详细步骤请参见 [创建 GCP Pub/Sub 消费者 Source 规则](#创建-gcp-pub-sub-消费者-source-规则)。

## 创建 GCP Pub/Sub 消费者 Source 规则 

本节演示如何在 EMQX 中创建规则，以消费来自 GCP Pub/Sub 的消息并将消息转发给 EMQX。您需要创建并配置一个 Google PubSub 消费者 Source，并将其作为数据输入添加到规则中。您还需要向规则中添加一个重发布动作，以将消息从 GCP Pub/Sub 转发至 EMQX。

1. 转到 EMQX 控制台，点击 **集成** -> **规则**。

2. 在页面右上角点击 **创建**。

3. 输入 `my_rule_source` 作为规则 ID。

4. 在右侧的 **数据输入** 页签下，删除默认输入 `消息`。点击 **添加输入**。

5. 从 **输入类型** 下拉菜单中选择 `Google PubSub 消费者`。

6. 保持 **Source** 下拉菜单中的默认值 `创建 Source`。本演示将创建一个新的 Source 并将其添加到规则中。

7. 为 Source 输入 **名称** 和 **描述**（可选）。名称应该是大小写字母和数字的组合，例如 `my-gcppubsub-source`。

8. 从 **连接器** 下拉框中选择刚刚创建的 `my_pubsubconsumer`。您也可以通过点击下拉框旁边的按钮创建一个新的连接器。关于配置参数，请参见 [创建连接器](#创建连接器)。

9. 想要从 GCP Pub/Sub 消费消息到 EMQX，可以为 Source 配置以下信息：

   - **GCP PubSub 主题**：输入要被消费的 GCP Pub/Sub 消息主题的名称，例如 `my-iot-core`。
   - **拉取的最大消息数**：指定在单次拉取请求中从 GCP PubSub 检索的最大消息数量。实际数量可能小于指定的值。

10. 高级设置（可选）：详细信息，请参见 [Sink 的特性](./data-bridges.md#sink-的特性)。

11. 在点击 **创建** 之前，您可以点击 **测试连接** 以测试是否能成功连接到 GCP Pub/Sub 服务器。

12. 点击 **创建** 完成 Source 的创建。Source 即被添加规则页面右侧的 **数据输入** 页签下，您同时可以看到 **SQL 编辑器** 中的规则显示为：

    ```sql
    SELECT
      *
    FROM
      "$bridges/gcppubsub:my-gcppubsub-source"
    ```

    注意：如果您是初级用户，点击 **SQL 示例** 和 **启用测试** 学习和测试 SQL 规则。

    从 `my-gcppubsub-source` 中，规则 SQL 可以获取在下面的 GCP PubSub 到 MQTT 主题映射表中列出的 GCP PubSub 消息字段。您可以调整 SQL 进行数据处理操作。此处使用默认 SQL 即可。

    | 字段名称          | 描述                                     |
    | ----------------- | ---------------------------------------- |
    | `attributes`      | （可选）包含字符串键值对的对象（如果有） |
    | `message_id`      | GCP Pub/Sub 分配给此消息的消息 ID        |
    | `ordering_key`    | （可选）消息排序键（如果有）             |
    | `publishing_time` | GCP Pub/Sub 定义的消息时间戳             |
    | `topic`           | 源自 GCP Pub/Sub 的主题                  |
    | `value`           | （可选）消息负载（如果存在）             |

    **注意**：每个 GCP Pub/Sub 到 MQTT 主题映射必须包含唯一的 GCP Pub/Sub 主题名称。即 GCP Pub/Sub 主题不能在多个映射中存在。

现在您已成功创建了 GCP Pub/Sub 消费者 Source，但消息不会直接发布到 EMQX。接下来，继续按照 [添加消息重发布动作](#添加消息重发布动作) 的步骤来创建消息重发布动作并将其添加到规则中。

### 添加消息重发布动作

本节演示如何在规则中添加消息重发布动作，以转发从 GCP Pub/Sub Source 消费的消息并发布到 EMQX 主题 `t/1`。

1. 选择页面右侧的 **动作输出** 页签，点击 **添加动作** 按钮，并从 **动作类型** 下拉列表中选择 `消息重发布` 动作。

2. 填写消息重发布的配置：

   - **主题**：要发布到 MQTT 的主题，在这里输入 `t/1`。

   - **QoS**：选择 `0`、`1`、`2` 或 `${qos}`，或输入占位符从其他字段设置 QoS。在这里选择 `${qos}` 意味着跟随原始消息的 QoS。

   - **Retain**：选择 `true` 或 `false`。确定是否将消息作为保留消息发布，也可以输入占位符从其他字段设置保留消息标志。此示例中选择 `false`。

   - **Payload**：设置生成转发消息 payload 的模板。默认留空意味着转发规则输出结果。这里您可以输入 `${payload}` 表示仅转发payload 。

     MQTT payload 模板的默认值是 `${.}`，包括所有可用数据编码为 JSON 对象。例如，对于包含所有可选字段的 GCP PubSub 消息，选择 `${.}` 作为模板将产生以下内容，：

     ```json
     {
       "attributes": {"attribute_key": "attribute_value"},
       "message_id": "1679665968238",
       "ordering_key": "my-ordering-key",
       "topic": "my-pubsub-topic",
       "publishing_time": "2023-08-18T14:15:18.470Z",
       "value": "my payload"
     }
     ```

     可以使用点表示法获取 GCP Pub/Sub 消息的子字段。例如，`${.value}` 将解析为 GCP Pub/Sub 消息的值，`${.attributes.h1}` 将解析为 `h1` 消息属性键的值（如果这样的子字段存在）。缺失的值将被空字符串替换。

   - **MQTT 5.0 消息属性**：默认禁用。详细设置，请参见 [添加消息重发布动作](./rule-get-started.md#添加消息重发布动作)。

3. 点击 **创建** 完成动作的创建。创建成功后，您将返回到创建规则页面，并且消息重发布动作将被添加到 **动作输出** 标签。

4. 在规则创建页面，点击 **创建** 按钮完成整个规则的创建。

现在您已成功创建了规则，您可以在 **规则** 页面看到新创建的规则。在 **Sources** 标签下，您可以看到新创建的 GCP Pub/Sub 消费者 Source。

您也可以点击 **集成** -> **Flow 设计器** 查看拓扑图。通过拓扑图，您可以直观地看到来自 GCP Pub/Sub 消费者 Source 的消息将通过消息重发布发布到 `t/1`。

## <!--测试 GCP Pub/Sub 消费者 Source 规则-->

