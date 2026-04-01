# 订阅过滤器快速开始

本页面将引导您完成 EMQX 订阅过滤器功能的启用，并通过实践演示进行验证。您将使用 MQTTX CLI 模拟发布者和多个订阅者，观察过滤表达式如何控制每个订阅者接收到的消息。

## 前提条件

开始前，请确保已满足以下条件：

- 运行中的 EMQX 6.2+
- 已安装 [MQTTX CLI](https://mqttx.app/cli)

## 步骤 1：启用订阅过滤器

订阅过滤器默认禁用。禁用状态下，`?` 字符被视为主题字符串的普通字符，完全兼容现有订阅。

### 通过 Dashboard

1. 进入**管理** -> **MQTT 配置** -> **通用**选项卡。
2. 找到**订阅消息过滤**字段，将其设置为 **enable**。
3. 点击**保存**。

更改立即生效，无需重启 Broker。

### 通过配置文件

在 `emqx.conf` 中添加以下内容：

```hocon
mqtt.subscription_message_filter = enable
```

修改后重启 EMQX 使配置生效，或在支持的部署环境中重新加载配置。

### 通过 REST API

```bash
curl -s -u key:secret -X PUT \
  -H "Content-Type: application/json" \
  http://localhost:18083/api/v5/configs/mqtt \
  -d '{"subscription_message_filter": "enable"}'
```

启用后，客户端即可在订阅时附加过滤表达式。有关语法详情和示例，请参阅概念文档中的[过滤语法](./subscription-filter-concept.md#过滤语法)。

## 步骤 2：启动订阅者

在本演示中，传感器将温度读数发布到 `sensor/1/temperature`，每条消息包含一个 `location` 用户属性。三个订阅者监听同一主题，但使用不同的过滤表达式：

| 订阅者 | 订阅字符串 | 接收条件 |
|---|---|---|
| `sub-roomA` | `sensor/+/temperature?location=roomA` | `location=roomA` |
| `sub-roomB` | `sensor/+/temperature?location=roomB` | `location=roomB` |
| `sub-all` | `sensor/+/temperature` | 所有消息（无过滤） |

打开三个终端窗口，分别启动各订阅者。

**终端 1：roomA 订阅者**

```bash
mqttx sub -h localhost -p 1883 \
  --mqtt-version 5 \
  --client-id sub-roomA \
  -t "sensor/+/temperature?location=roomA"
```

**终端 2：roomB 订阅者**

```bash
mqttx sub -h localhost -p 1883 \
  --mqtt-version 5 \
  --client-id sub-roomB \
  -t "sensor/+/temperature?location=roomB"
```

**终端 3：无过滤订阅者**

```bash
mqttx sub -h localhost -p 1883 \
  --mqtt-version 5 \
  --client-id sub-all \
  -t "sensor/+/temperature"
```

::: tip

必须指定 `--mqtt-version 5` 参数。订阅过滤器依赖 MQTT 5.0 协议特性。

:::

## 步骤 3：发布 Room A 的消息

在第四个终端中，发布一条用户属性为 `location=roomA` 的消息：

```bash
mqttx pub -h localhost -p 1883 \
  --mqtt-version 5 \
  --client-id publisher \
  -t "sensor/1/temperature" \
  -m '{"value": 23.5}' \
  --user-properties "location: roomA"
```

**预期结果：**

| 订阅者 | 是否收到消息？ |
|---|---|
| `sub-roomA` | 是，`location=roomA` 匹配 |
| `sub-roomB` | 否，`location` 值不匹配 |
| `sub-all` | 是，无过滤表达式 |

## 步骤 4：发布 Room B 的消息

```bash
mqttx pub -h localhost -p 1883 \
  --mqtt-version 5 \
  --client-id publisher \
  -t "sensor/1/temperature" \
  -m '{"value": 19.1}' \
  --user-properties "location: roomB"
```

**预期结果：**

| 订阅者 | 是否收到消息？ |
|---|---|
| `sub-roomA` | 否，`location` 值不匹配 |
| `sub-roomB` | 是，`location=roomB` 匹配 |
| `sub-all` | 是，无过滤表达式 |

## 步骤 5：测试多条件（AND 逻辑）

订阅过滤器支持通过 `&` 组合多个条件。启动一个要求 `location` 和 `unit` 同时匹配的新订阅者：

```bash
mqttx sub -h localhost -p 1883 \
  --mqtt-version 5 \
  --client-id sub-roomA-celsius \
  -t "sensor/+/temperature?location=roomA&unit=celsius"
```

发布一条满足两个条件的消息：

```bash
mqttx pub -h localhost -p 1883 \
  --mqtt-version 5 \
  --client-id publisher \
  -t "sensor/1/temperature" \
  -m '{"value": 22.0}' \
  --user-properties "location: roomA" \
  --user-properties "unit: celsius"
```

`sub-roomA-celsius` 订阅者收到该消息。再发布一条 `unit` 不匹配的消息：

```bash
mqttx pub -h localhost -p 1883 \
  --mqtt-version 5 \
  --client-id publisher \
  -t "sensor/1/temperature" \
  -m '{"value": 71.6}' \
  --user-properties "location: roomA" \
  --user-properties "unit: fahrenheit"
```

尽管 `location=roomA` 匹配，`sub-roomA-celsius` 订阅者**不会**收到该消息，因为 `unit` 条件不满足。

## 步骤 6：发布不含用户属性的消息

发布一条不含用户属性的消息：

```bash
mqttx pub -h localhost -p 1883 \
  --mqtt-version 5 \
  --client-id publisher \
  -t "sensor/1/temperature" \
  -m '{"value": 20.0}'
```

**预期结果：**

| 订阅者 | 是否收到消息？ |
|---|---|
| `sub-roomA` | 否，`location` 键不存在 |
| `sub-roomB` | 否，`location` 键不存在 |
| `sub-all` | 是，无过滤表达式 |

这证实了：当消息缺少过滤表达式所需的用户属性键时，使用过滤表达式的订阅者将不会收到该消息。

## 总结

| 场景 | 行为 |
|---|---|
| 消息用户属性与过滤表达式匹配 | 投递 |
| 消息用户属性部分匹配（AND 条件未满足） | 不投递 |
| 所需用户属性键不存在 | 不投递 |
| 订阅无过滤表达式 | 投递所有匹配主题的消息 |

## 下一步

- [订阅过滤器概述](./subscription-filter-concept.md)：深入了解设计原理、核心概念和应用场景。
- [通配符订阅](../messaging/mqtt-wildcard-subscription.md)：将通配符主题过滤器与订阅过滤器组合使用，实现灵活的消息路由。
