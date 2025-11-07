# 消息队列用户指南

本页面将引导你从创建消息队列到配置其行为，并通过 Dashboard、REST API 或配置文件对其进行管理，从而实际使用 EMQX 的消息队列功能。

## 通过 Dashboard 手动创建消息队列

消息队列在可以存储或分发消息之前，必须显式声明/创建。您可以通过手动或自动的方式创建消息队列，自动创建的方法请参见[通过 Dashboard 自动创建消息队列](#通过-dashboard-自动创建消息队列)。

通过 EMQX Dashboard 手动创建新消息队列的步骤如下：

1. 在左侧菜单中进入**消息队列**页面。

2. 点击页面中的**创建**按钮。

3. 在**创建消息队列**对话框中，配置以下选项：

   - **过滤主题**：输入主题或主题过滤器（例如 `t/1`）。该字段用于根据主题匹配决定哪些发布的消息将被入队。一个队列将收集所有匹配该过滤器的消息。

     若要从队列中消费消息，客户端必须订阅使用 `$q/{Topic Filter}` 格式的主题。

   - **派发策略**：选择消息应如何在订阅者之间分发。可选策略包括：

     - `最少未确认消息订阅者`：优先选择未确认（正在传输）消息最少的订阅者。
     - `随机`：（默认）随机选择一个订阅者。
     - `轮询`：均匀轮询分发给所有订阅者。

   - **数据保留期**：指定消息在队列中保留的时间。你可以设置时间单位（例如：天）。

   - **最后值语义**：此选项默认启用，如果一个新消息与已有未消费消息拥有相同的队列键，则会覆盖旧消息，仅保留最新消息。启用此选项后，每条消息应该具有“队列键表达式”所配置的属性，以保存到队列中。默认的队列键为消息发布者的客户端 ID。

     - **[队列键表达式](#队列键表达式)**：启用“最后值语义”后，该字段用于定义如何从每条消息中提取队列键，默认值为 `message.from`，即为消息发布者的客户端 ID。该字段支持使用 [Variform 表达式](../configuration/configuration.md#variform-表达式)设置。

   - **最大分片消息数量**：（可选）设置每个队列分片中允许的最大消息数量。您可以开启该选项并输入自定义数值，或保持关闭状态以表示不限制（`infinity`）。该配置将被持久化到存储中。

   - **最大分片消息字节数**：（可选）设置每个队列分片中消息允许占用的最大总字节数。您可以开启该选项并输入数值（例如 `200MB`），或保持关闭状态以表示不限制（`infinity`）。该配置将被持久化到存储中。

     ::: tip 性能提示

     启用队列大小限制可能会在高吞吐场景下导致写入性能下降。

     :::

4. 点击**创建**保存队列。

新队列将出现在消息队列列表中，并显示其主题过滤器、分发策略、是否启用了最新值语义以及数据保留时间。你可以通过**操作**栏中的按钮编辑队列设置或删除队列。

### 队列键表达式

队列键表达式用于指定在启用“最新值语义”模式下，如何从消息元数据中提取用于消息去重的键。该表达式会针对每条消息的元数据进行求值，并遵循[Variform 表达式](../configuration/configuration.md#variform-表达式)的语法。

该表达式会在包含 `from`、`topic`、`payload`、`headers.properties` 等字段的消息上下文中进行求值。例如，如果希望使用用户属性（User Property）作为键，可以将表达式设置为：

```
message.headers.properties.'User-Property'.user-prop
```

如果无法根据表达式提取出队列键（例如字段不存在），该消息将被丢弃，不会被加入队列。

#### 消息上下文示例

队列键表达式会在如下结构的消息上下文中进行求值：

<details>

<summary><strong>JSON 示例</strong></summary>

```json
{
  "message": {
    "qos": 0,
    "topic": "some/topic",
    "payload": "some-payload",
    "headers": {
      "client_attrs": {},
      "proto_ver": 5,
      "properties": {
        "User-Property": {
          "user-prop": "some-value"
        }
      },
      "peerhost": "127.0.0.1",
      "username": "undefined",
      "protocol": "mqtt",
      "peername": "127.0.0.1:49352"
    },
    "from": "clientid",
    "timestamp": 1759238376252,
    "id": "..non utf8 bytes...",
    "flags": {
      "retain": false,
      "dup": false
    },
    "extra": {}
  }
}
```

</details>

<details>

<summary><strong>Erlang Term 示例</strong></summary>

```erlang
#{
  message =>
      #{
        extra => #{},
        flags => #{dup => false, retain => false},
        id => <<0,6,64,4,154,125,229,77,244,69,0,0,28,21,0,2>>,
        timestamp => 1759238376252,
        from => <<"clientid">>,
        headers =>
            #{
              peername => <<"127.0.0.1:49352">>,
              protocol => mqtt,
              username => undefined,
              peerhost => <<"127.0.0.1">>,
              properties =>
                  #{
                    'User-Property' => #{
                      <<"user-prop">> => <<"some-value">>
                    }
                  },
              proto_ver => 5,
              client_attrs => #{}
            },
        payload => <<"some-payload">>,
        topic => <<"some/topic">>,
        qos => 0
      }
}
```

</details>

## 通过 Dashboard 自动创建消息队列

从 EMQX 6.0.1 开始，当客户端订阅以 `$q/` 开头的主题时，EMQX 可以自动创建对应的消息队列，实现动态队列创建，无需手动配置。

消息队列可以自动创建为常规消息队列和具有最后值语义的消息队列。

::: tip 提示

为了确保队列行为的正确性，**自动创建常规消息队列**与**自动创建最后值语义消息队列**只能启用其中一个，不能同时启用。

:::

### 自动创建最后值语义消息队列

此选项在**MQTT 配置**的**消息队列**选项卡中默认启用。启用后，EMQX 将自动创建具有最后值语义的队列，即仅保留每个键对应的最新消息。

1. 进入**管理** -> **MQTT 配置** -> **消息队列**选项卡。
2. 默认情况下，**启用自动创建最后值语义消息队列**选项已启用。您可以配置以下参数：
   - **[队列键表达式](#队列键表达式)**：必填项，用于定义如何从消息中提取唯一键（默认值为 `message.from`）。
   - **派发策略**：定义消息如何分发给订阅者（默认值为 `Random`）。
   - **数据保留期**：指定消息在队列中保留的时长。
3. 配置完成后，点击**保存更改**。

当客户端订阅如 `$q/test` 的主题时，EMQX 会自动创建一个具有最后值语义的队列，并在**消息队列**页面中展示该队列。

### 自动创建常规消息队列

如果您想使用常规队列（即消息独立存储，不会被覆盖），可以手动启用该选项。

1. 进入**管理** -> **MQTT 配置** -> **消息队列**选项卡。
2. 启用**启用自动创建常规消息队列**选项。
3. 配置以下参数：
   - **派发策略**：定义消息如何分发给订阅者（默认值为 `Random`）。
   - **数据保留期**：指定消息在队列中保留的时长。
4. 配置完成后。点击**保存修改**。

## 配置消息队列设置

本节介绍如何配置适用于所有消息队列的全局设置。这些设置控制消息保留时间、清理周期、内部队列行为以及自动创建行为。你可以通过 Dashboard、REST API 或配置文件进行配置。

### Dashboard

你可以在不重启 EMQX 的情况下，直接通过 Dashboard 修改消息队列的设置。这在运行时调整系统级行为时非常实用。

通过 Dashboard 配置全局设置的步骤如下：

1. 进入**管理** -> **MQTT 设置** -> **消息队列**选项卡。或者，也可以在**消息队列**页面点击右上角的**设置**按钮进入配置页面。

2. **消息队列**配置面板中包括以下选项：

   - **启用消息队列**：消息队列功能默认启用，且无法通过 Dashboard 禁用。

     > 若需禁用该功能，请前往配置文件中手动修改。
   - **最大队列数量**：设置系统允许创建的最大队列数量。
   - **垃圾回收间隔**：队列中过期消息的清理周期，默认为 `1` 小时。
   - **常规队列保留周期**：普通队列中消息的最长保留时间，默认为 `7` 天。
   - **查找队列重试间隔**：当客户端订阅 `$q/` 前缀的队列主题时，如果对应的队列尚未创建，该参数控制客户端多久重试一次查找队列。默认为 `10` 秒。
   - **自动创建选项**：EMQX 支持通过以下方式自动创建队列：

     - **自动创建最后值语义消息队列**（默认启用）：当客户端订阅 `$q/` 开头的主题且未找到对应队列时，EMQX 会自动创建一个启用了最后值语义的队列。
       详细配置请参见[自动创建最后值语义消息队列](#自动创建最后值语义消息队列)。
     - **自动创建常规消息队列**：启用后，当客户端订阅 `$q/` 开头的主题且未找到对应队列时，EMQX 会自动创建一个常规队列（不覆盖旧消息）。
       详细配置请参见[自动创建常规消息队列](#自动创建常规消息队列)。

3. 修改完成后点击**保存修改**应用设置。

### REST API

你也可以通过 REST API 配置全局消息队列设置。这些设置为系统级配置，会影响所有队列的内部管理行为。

```bash
curl -v -u key:secret -X PUT -H "Content-Type: application/json" http://localhost:18083/api/v5/message_queues/config -d '{"find_queue_retry_interval": "10s", "gc_interval": "1h", "regular_queue_retention_period": "7d"}'
```

### 配置文件

为实现持久化和版本控制，你可以在 EMQX 配置文件（`emqx.conf`）中定义消息队列设置。以下是包含主要设置的示例：

```hocon
mq {
    gc_interval = 1h
    regular_queue_retention_period = 1d
    find_queue_retry_interval = 10s
    max_queue_count = 100
    }
}
```

#### 配置项说明

- **`gc_interval`**：定义 EMQX 扫描消息队列并清理过期消息的时间间隔。
- **`regular_queue_retention_period`**：设置队列中消息的最长保留时间。超时的消息将被自动清除。
- **`find_queue_retry_interval`**：当订阅者订阅 `$q/` 主题时未找到队列，订阅者重新尝试查找队列的周期。
- **`max_queue_count`**：设置系统允许创建的最大队列数量。

## 通过 REST API 管理消息队列

EMQX 提供一组 REST API 用于管理消息队列的生命周期，包括创建、查询、更新和删除。

### 创建消息队列

通过指定主题过滤器和队列属性（如是否启用最新值语义）来创建新队列：

```bash
curl -s -u key:secret -X POST -H "Content-Type: application/json" \
http://localhost:18083/api/v5/message_queues \
-d '{"topic_filter": "t1/#", "is_lastvalue": false, "limits": {"max_shard_message_count": 10000, "max_shard_message_bytes": "200MB"}}' | jq
```

### 列出所有消息队列

获取所有已存在的消息队列列表：

```bash
curl -s -u key:secret -X GET -H "Content-Type: application/json" \
http://localhost:18083/api/v5/message_queues | jq
```

### 更新消息队列

更新现有队列的属性，例如分发策略：

```bash
curl -s -u key:secret -X PUT -H "Content-Type: application/json" \
http://localhost:18083/api/v5/message_queues/t1%2F%23 \
-d '{"dispatch_strategy": "least_inflight", "limits": {"max_shard_message_count": 5000, "max_shard_message_bytes": "100MB"}}' | jq
```

### 删除消息队列

删除指定队列及其所有已保留的消息：

```bash
curl -s -u key:secret -X DELETE \
http://localhost:18083/api/v5/message_queues/t1%2F%23
```

> **注意：**
>
> - URL 中的主题过滤器必须进行 URL 编码（例如 `t1/#` 编码为 `t1%2F%23`）。
> - 请求需要身份认证（`key:secret`）。

## 常见问题与故障排查

### 为什么消息没有被入队？

- 请确认声明的消息队列的主题过滤器是否匹配发布的消息主题。
- 检查队列是否存在且配置正确。
- 查看 EMQX 日志中是否有相关错误或警告，特别是带有 `mq_` 前缀的日志项，以帮助诊断队列问题。

### 队列容量超过后会发生什么？

EMQX 中的消息队列现在支持多种容量限制。当队列达到任一限制时，EMQX 会在垃圾回收（GC）期间删除最旧的消息，直到队列大小恢复到配置的范围内。

- **基于时间的限制**：所有队列仍受配置的*保留周期（retention period）* 限制。超过保留时间的消息将不再具备投递资格，并会在 GC 期间被自动清除。

- **基于大小的限制**：您可以为每个队列分片可选地配置以下限制：

  - **最大消息数量**（`max_shard_message_count`）
  - **最大消息总大小（字节）**（`max_shard_message_bytes`）

  这些限制为软性限制，仅在 GC 期间生效，而非实时强制执行。在两次 GC 之间，队列可能会暂时超过配置的阈值。
  
  请注意，这些限制是按持久存储的分片（shard）应用的。有关如何配置分片数量的信息，请参见：[分片数量](../durability/managing-replication.md#分片-shard-数量)。
  
  此外，大小限制不包含[副本因子（replication factor）](../durability/managing-replication.md#复制因子-replication-factor)的影响；队列实际占用的物理存储空间将乘以副本因子。

