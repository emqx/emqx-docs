# Sparkplug B

[Sparkplug](https://www.eclipse.org/tahu/spec/sparkplug_spec.pdf) 是由 [Eclipse Foundation 的 TAHU 项目](https://www.eclipse.org/tahu/) 开发的开源规范，旨在为 MQTT 提供一套明确定义的 payload 和状态管理体系。其主要目标是在工业物联网领域实现互操作性和一致性。

Sparkplug B 定义了用于监控控制和数据采集（SCADA）系统、实时控制系统和设备的 MQTT 命名空间。它通过封装结构化数据格式，包括指标、过程变量和设备状态信息，确保了标准化的数据传输，使其呈现为简洁易处理的格式。通过使用Sparkplug B，组织可以提高运营效率，避免数据孤岛，并在 MQTT 网络中实现设备间的无缝通信。

本页面将为您介绍 Sparkplug B 在 EMQX 中的实现，包括数据格式、功能和实用示例。

## Sparkplug B 数据格式

Sparkplug B 采用明确定义的 payload 结构来标准化数据通信。它的核心是使用 [Protocol Buffers（Protobuf）](https://developers.google.com/protocol-buffers)对 Sparkplug 消息进行结构化，从而实现轻量、高效和灵活的数据交换。

EMQX 通过 [Schema Registry](./schema-registry.md) 功能提供对 Sparkplug B 的高级支持。使用 Schema Registry，您可以为多种数据格式（包括 Sparkplug B）创建自定义编码器和解码器。通过在 registry 中定义 [适当的 Sparkplug B schema](https://github.com/eclipse/tahu/blob/46f25e79f34234e6145d11108660dfd9133ae50d/sparkplug_b/sparkplug_b.proto)，您可以在 EMQX 的规则引擎中使用 `schema_decode` 和 `schema_encode` 函数访问和处理符合指定格式的数据。

此外，EMQX 还提供对于 Sparkplug B 的内置支持，无需为该特定格式使用 schema registry。在 EMQX 中，`sparkplug_encode`  和`sparkplug_decode` 函数已经可以直接使用，简化了在规则引擎内进行 Sparkplug B 消息的编码和解码。

## Sparkplug B 函数

EMQX 提供了两个规则引擎 SQL 函数用于编码和解码 Sparkplug B 数据：`sparkplug_encode` 和 `sparkplug_decode`。[实用示例](#实用示例)部分将帮助您了解如何在不同场景中使用这些函数。

由于规则引擎及其 `jq` 函数的灵活性，Sparkplug B 编码和解码函数可以执行各种任务。要了解更多有关规则引擎及其 `jq` 函数的信息，请参阅以下页面：

- [创建规则](./rule-get-started.md)
- [SQL 语法与示例](./rule-sql-syntax.md)
- [JQ 函数](./rule-sql-jq.md)
- [JQ 编程语言完整说明](https://stedolan.github.io/jq/manual/)

### sparkplug_decode

`sparkplug_decode` 函数用于解码用 Sparkplug B 格式编码的消息，例如，如果您希望根据 Sparkplug B 格式编码的消息内容将其转发到特定主题，或者以某种方式更改 Sparkplug B 编码的消息，该函数可以将原始 Sparkplug B 编码 payload 转换为更易于处理和分析的用户友好格式。

使用示例：

```sql
select
  sparkplug_decode(payload) as decoded
from t
```

上面的示例中，`payload` 指的是要解码的原始 Sparkplug B 消息。

[Sparkplug B Protobuf schema](https://github.com/emqx/emqx/blob/039e27a153422028e3d0e7d517a521a84787d4a8/lib-ee/emqx_ee_schema_registry/priv/sparkplug_b.proto) 可以进一步揭示消息结构的详细信息。

### sparkplug_encode

`sparkplug_encode` 函数用于将消息编码为 Sparkplug B 数据格式。这在您需要向 MQTT 客户端或系统的其他组件发送 Sparkplug B 格式消息时特别有用。

使用示例：

```sql
select
  sparkplug_encode(json_decode(payload)) as encoded
from t
```

上面的示例中，`payload` 指的是要编码为 Sparkplug B 格式的消息数据。

## 实用示例

本节提供使用 `sparkplug_decode` 和 `sparkplug_encode` 函数处理 Sparkplug B 消息的实用示例。请注意，所给示例仅展示了一小部分可以执行的操作，并非全部。

试想您有一个 Sparkplug B 编码的消息，具有以下结构：

```json
{
  "timestamp": 1678094561521,
  "seq": 88,
  "metrics": [
    {
      "timestamp": 1678094561525,
      "name": "counter_group1/counter1_1sec",
      "int_value": 424,
      "datatype": 2
    },
    {
      "timestamp": 1678094561525,
      "name": "counter_group1/counter1_5sec",
      "int_value": 84,
      "datatype": 2
    },
    {
      "timestamp": 1678094561525,
      "name": "counter_group1/counter1_10sec",
      "int_value": 42,
      "datatype": 2
    },
    {
      "timestamp": 1678094561525,
      "name": "counter_group1/counter1_run",
      "int_value": 1,
      "datatype": 5
    },
    {
      "timestamp": 1678094561525,
      "name": "counter_group1/counter1_reset",
      "int_value": 0,
      "datatype": 5
    }
  ]
}
```

### 提取数据

假设您从一台设备获取了发送到主题 `my/sparkplug/topic` 上的消息，并希望仅将 `counter_group1/counter1_run` 指标转发到另一个名为 `intresting_counters/counter1_run_updates ` 的主题，且以 JSON 格式发送。以下步骤演示了如何通过在 EMQX Dashboard 中创建规则并使用 [MQTTX](https://mqttx.app/) 客户端工具测试规则来实现此任务。

#### 在 Dashboard 中创建规则

1. 打开EMQX Dashboard。从左侧导航菜单中点击**集成** -> **规则**。点击 **+ 创建**进入**创建规则**页面。

2. 在 **SQL 编辑器**中输入以下 SQL 语句：

   ```sql
   FOREACH
   jq('
         .metrics[] |
         select(.name == "counter_group1/counter1_run")
      ',
      sparkplug_decode(payload)) AS item
   DO item
   FROM "my/sparkplug/topic"
   ```

   这里，`jq` 函数用于遍历指标数组并过滤出名为"`counter_group1/counter1_run`"的指标。

   ::: tip

   Sparkplug B 规范建议仅在数据发生更改时发送数据，使 payload 仅呈现指标的子集。如果数组中没有指定名称的条目，则此规则不会输出任何内容。

   :::

3. 在页面右侧点击**+ 添加动作**。从**动作**下拉列表中选择 `消息重发布`。将`intresting_counters/counter1_run_updates `作为重新发布主题输入，并在 **Payload** 字段中输入 `${item}`。点击**添加**。

4. 在**创建规则**页面上，点击**创建**。您可以在规则列表中看到已创建的规则。

#### 测试规则

您可以使用 MQTTX 客户端工具模拟一个 MQTT 客户端，将 Sparkplug B 消息发布到主题 `my/sparkplug/topic`。然后，您可以验证该消息是否以 JSON 格式转换并转发到主题 `intresting_counters/counter1_run_updates`：

1. 打开 MQTTX 客户端桌面版并连接到 EMQX 代理。有关使用 MQTTX 的详细信息，请参阅 [MQTTX](../messaging/publish-and-subscribe.md/#mqttx-client)。

2. 创建新的订阅并订阅主题 `intresting_counters/counter1_run_updates`。

3. 在右下角的消息发送区域，将 `my/sparkplug/topic` 作为主题输入。选择 `Base64` 作为 payload 类型。

4. 复制以下 Base64 编码的 Sparkplug B 消息并粘贴到负载字段。此消息对应于之前给出的 Sparkplug 消息编码示例。

   `CPHh67HrMBIqChxjb3VudGVyX2dyb3VwMS9jb3VudGVyMV8xc2VjGPXh67HrMCACUKgDEikKHGNvdW50ZXJfZ3JvdXAxL2NvdW50ZXIxXzVzZWMY9eHrseswIAJQVBIqCh1jb3VudGVyX2dyb3VwMS9jb3VudGVyMV8xMHNlYxj14eux6zAgAlAqEigKG2NvdW50ZXJfZ3JvdXAxL2NvdW50ZXIxX3J1bhj14eux6zAgBVABEioKHWNvdW50ZXJfZ3JvdXAxL2NvdW50ZXIxX3Jlc2V0GPXh67HrMCAFUAAYWA`

5. 点击发送按钮发送消息。

   如果一切按预期工作，您应该会收到以下 JSON 格式的消息：

   ```json
   {
       "timestamp":1678094561525,
       "name":"counter_group1/counter1_run",
       "int_value":1,
       "datatype":5
   }
   ```

### 更新数据

试想您发现了一个名为 `counter_group1/counter1_run`的错误指标，并希望在转发消息之前将其从 Sparkplug B 编码的 payload 中移除。

与[提取数据](#提取数据)中的演示类似，您可以在 EMQX Dashboard 中创建以下包含消息重发布动作的规则。

```sql
FOREACH
jq('
   # Save payload
   . as $payload |
   # Save name of metric to delete
   "counter_group1/counter1_run" as $to_delete |
   # Filter out metric with name $to_delete
   [ .metrics[] | select(.name != $to_delete) ] as $updated_metrics |
   # Update payload with new metrics
   $payload | .metrics = $updated_metrics
   ',
   sparkplug_decode(payload)) AS item
DO sparkplug_encode(item) AS updated_payload
FROM "my/sparkplug/topic"
```

在此规则中，`sparkplug_decode` 用于解码消息，然后使用 `jq` 过滤出名为 `counter_group1/counter1_run` 的指标。然后，`DO` 子句中的 `sparkplug_encode` 用于再次对消息进行编码。

在消息重发布动作中，将主题名设置为 `${updated_payload}`，因为它是在 `DO` 子句中分配给更新后的 Sparkplug B 编码消息的名称。

同样，您还可以使用 `sparkplug_decode` 和 `sparkplug_encode` 来更新指标的值。假设您想将名为 `counter_group1/counter1_run` 的指标值更新为 0。您可以通过以下规则实现：

```sql
FOREACH
jq('
   # Save payload
   . as $payload |
   # Save name of metric to update
   "counter_group1/counter1_run" as $to_update |
   # Update value of metric with name $to_update
   [
     .metrics[] |
     if .name == $to_update
        then .int_value = 0
        else .
     end
   ] as $updated_metrics |
   # Update payload with new metrics
   $payload | .metrics = $updated_metrics
   ',
   sparkplug_decode(payload)) AS item
DO sparkplug_encode(item) AS item
FROM "my/sparkplug/topic"
```

或者试想您希望添加一个名为 `counter_group1/counter1_new` 且值为 42 的新指标。您可以通过以下规则实现：

```sql
FOREACH
jq('
   # Save payload
   . as $payload |
   # Save old metrics
   $payload | .metrics as $old_metrics |
   # New value
   {
     "name": "counter_group1/counter1_new",
     "int_value": 42,
     "datatype": 5
   } as $new_value |
   # Create new metrics array 
   ($old_metrics + [ $new_value ]) as $updated_metrics |
   # Update payload with new metrics
   $payload | .metrics = $updated_metrics
   ',
   sparkplug_decode(payload)) AS item
DO sparkplug_encode(item) AS item 
FROM "my/sparkplug/topic"
```

### 过滤消息

试想您只希望转发那些名称为 `counter_group1/counter1_run` 且值大于 0 的指标的消息。您可以通过以下规则实现：

```sql
FOREACH
jq('
   # Save payload
   . as $payload |
   # Save name of metric to filter on
   "counter_group1/counter1_run" as $to_filter |
   .metrics[] | select(.name == $to_filter) | .int_value as $value |
   # Filter out messages where value of metric with name $to_filter is 0 or smaller
   if $value > 0 then $payload else empty end
   ',
   sparkplug_decode(payload)) AS item
DO sparkplug_encode(item) AS item 
FROM "my/sparkplug/topic"
```

在上述规则中，`jq   ` 函数在指标的名称为 `counter_group1/counter1_run` 且值小于 0 时输出一个空数组。这意味着如果值为 0 或更小，则该消息不会被转发到与规则连接的任何动作。

### 拆分消息

试想您希望将一个 Sparkplug B 编码的消息拆分为多个消息，其中指标数组中的每个指标都作为单独的 Sparkplug B 编码的消息重新发布。您可以通过以下规则实现：

```sql
FOREACH
jq('
   # Save payload
   . as $payload |
   # Output one message for each metric
   .metrics[] |
        . as $metric |
        # Let the current metric be the only one in the metrics array
        $payload | .metrics = [ $metric ]
   ',
   sparkplug_decode(payload)) AS item
DO sparkplug_encode(item) AS output_payload
FROM "my/sparkplug/topic"
```

在上述规则中，`jq` 函数输出包含多个条目的数组（假设指标数组中有多个条目）。与数组中的每个条目相关联的所有动作都会触发。 使用上述规则，您需要将重新发布动作的 payload 设置为 `${output_payload}`，因为 `output_payload` 是在 `DO` 子句中分配给 Sparkplug B 编码消息的名称。

### 拆分消息并根据内容发送到不同主题

试想您希望拆分 Sparkplug B 编码的消息，但还希望将每个消息根据指标的名称发送到不同的主题，例如，应该通过将指标的名称与字符串 `"my_metrics/"` 进行拼接来构建输出主题名。您可以通过以下稍作修改的代码实现：


```sql
FOREACH
jq('
   # Save payload
   . as $payload |
   # Output one message for each metric
   .metrics[] |
        . as $metric |
        # Let the current metric be the only one in the metrics array
        $payload | .metrics = [ $metric ]
   ',
   sparkplug_decode(payload)) AS item
DO
sparkplug_encode(item) AS output_payload,
first(jq('"my_metrics/" + .metrics[0].name', item)) AS output_topic
FROM "my/sparkplug/topic"
```

要配置重新发布动作，请将主题名称设置为 `${output_topic}`，因为它是在 `DO` 子句中分配给输出主题的名称，并将 payload 设置为 `${output_payload}`。

`jq ` 函数调用被包裹在 `DO` 子句中，并使用 `first` 函数来获取第一个且唯一的输出对象。
