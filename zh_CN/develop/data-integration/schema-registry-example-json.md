# Schema Registry 示例 - JSON Schema

本页面演示如何在 Schema Registry 中注册 draft 2020-12 JSON Schema，以及如何在规则中使用 `schema_check` 函数验证 MQTT 消息 payload。示例仅重新发布符合 Schema 的 payload。

## 支持的 JSON Schema 版本

从 EMQX 6.0.4 开始，Schema Registry 支持以下 JSON Schema 版本：

- draft-03
- draft-04
- draft-06
- draft 2019-09
- draft 2020-12

EMQX 根据 `$schema` 字段的值选择对应的 JSON Schema 版本。如果未指定 `$schema`，EMQX 使用 draft-06。

draft 2019-09 和 draft 2020-12 存在以下限制：

- draft 2019-09 不支持 `$recursiveRef`。
- draft 2020-12 不支持 `$dynamicRef`。
- 这两个版本不支持引用远程 Schema。

如果 Schema 使用了不支持的关键字，验证会返回错误，不会静默忽略该关键字。

## 创建 JSON Schema

创建一个仅接受两个整数所组成数组的 Schema：

1. 在 EMQX Dashboard 左侧导航栏中，点击 **Smart Data Hub** -> **Schema Registry**。
2. 在**内部 Schema**页签中，点击**创建**。
3. 配置以下字段：

   - **名称**：输入 `json_array`。
   - **类型**：选择 **JSON Schema**。
   - **Schema**：输入以下 draft 2020-12 Schema：

     ```json
     {
       "$schema": "https://json-schema.org/draft/2020-12/schema",
       "type": "array",
       "prefixItems": [
         { "type": "integer" },
         { "$ref": "#/prefixItems/0" }
       ],
       "minItems": 2,
       "maxItems": 2
     }
     ```

4. 点击**创建**。

`prefixItems` 数组定义每个位置的 Schema。本地 `$ref` 使第 2 个元素使用与第 1 个元素相同的整数 Schema。`minItems` 和 `maxItems` 要求 payload 必须包含两个元素。

## 创建规则

创建一条规则，仅在 payload 符合 `json_array` 时重新发布消息：

1. 在 Dashboard 左侧导航栏中，点击**数据集成** -> **规则**。
2. 在**规则**页面中，点击**创建**。
3. 在**名称**字段中输入 `validate_json_array`。
4. 在 **SQL 编辑器**中输入以下语句：

   ```sql
   SELECT *
   FROM "t/json"
   WHERE schema_check('json_array', payload)
   ```

   payload 符合 `json_array` 时，`schema_check` 函数返回 `true`。否则返回 `false`，规则不会执行动作。

5. 点击**添加动作**，选择**消息重发布**。
6. 在**主题**字段中输入 `validated/json`，在 **Payload** 字段中输入 `${payload}`。
7. 点击**创建**。

## 测试规则

使用 MQTTX CLI 验证规则：

1. 订阅重新发布消息的主题：

   ```bash
   mqttx sub -t validated/json
   ```

2. 在另一个终端中，发布符合 Schema 的 payload：

   ```bash
   mqttx pub -t t/json -m '[1, 2]'
   ```

   订阅端从 `validated/json` 主题收到 `[1, 2]`。

3. 发布第 2 个元素不是整数的 payload：

   ```bash
   mqttx pub -t t/json -m '[1, "two"]'
   ```

   规则不会执行消息重发布动作，订阅端不会收到消息。

在规则中使用 `schema_check` 只会过滤规则执行，不会拒绝原始 MQTT 消息。如需拒绝或丢弃不符合 Schema 的消息，请使用[Schema 验证](./schema-validation.md)。
