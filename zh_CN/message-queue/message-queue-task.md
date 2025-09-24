# 消息队列用户指南

本页面将引导你从创建消息队列到配置其行为，并通过 Dashboard、REST API 或配置文件对其进行管理，从而实际使用 EMQX 的消息队列功能。

## 通过 Dashboard 创建消息队列

消息队列在可以存储或分发消息之前，必须显式声明/创建。

通过 EMQX Dashboard 创建新消息队列的步骤如下：

1. 在左侧菜单中进入**消息队列**页面。

2. 点击页面中的**创建**按钮。

3. 在**创建消息队列**对话框中：

   - **过滤主题**：输入主题或主题过滤器（例如 `t/1`）。该字段用于根据主题匹配决定哪些发布的消息将被入队。一个队列将收集所有匹配该过滤器的消息。

     若要从队列中消费消息，客户端必须订阅使用 `$q/{Topic Filter}` 格式的主题。

   - **派发策略**：选择消息应如何在订阅者之间分发。可选策略包括：

     - `最少未确认消息订阅者`：优先选择未确认（正在传输）消息最少的订阅者。
     - `随机`：（默认）随机选择一个订阅者。
     - `轮询`：均匀轮询分发给所有订阅者。

   - **数据保留期**：指定消息在队列中保留的时间。你可以设置时间单位（例如：天）。

   - **最后值语义**：若希望同一队列中使用相同队列键的新消息覆盖旧消息，请开启此开关。启用此选项后，每条消息应该具有“队列键表达式”所配置的属性，以保存到队列中。

     - **队列键表达式**：启用“最新值语义”后，该字段用于定义如何从每条消息中提取队列键，默认值为 `message.from`，即为消息的发送者客户端 ID。该字段支持使用 [Variform 表达式](../configuration/configuration.md#variform-表达式)设置。

4. 点击**创建**保存队列。

新队列将出现在消息队列列表中，并显示其主题过滤器、分发策略、是否启用了最新值语义以及数据保留时间。你可以通过**操作** 栏中的按钮编辑或删除队列。

## 配置消息队列设置

本节介绍如何配置适用于所有消息队列的全局设置。这些设置控制消息保留时间、清理周期以及内部队列行为。你可以通过 Dashboard、REST API 或配置文件进行配置。

### Dashboard

你可以在不重启 EMQX 的情况下，直接通过 Dashboard 修改消息队列的设置。这在运行时调整系统级行为时非常实用。

通过 Dashboard 配置全局设置的步骤如下：

1. 在左侧菜单进入**消息队列**页面。
2. 点击页面右上角的**设置**按钮。
3. 页面将跳转至 **MQTT 配置** -> **消息队列**选项卡。在此面板中，你可以配置以下参数：
   - **垃圾回收间隔**：队列中过期消息的清理周期，默认为 `1` 小时。
   - **常规队列保留周期**：普通队列中消息的最长保留时间，默认为 `7` 天。
   - **查找队列重试间隔**：当客户端订阅 `$q/` 前缀的队列主题时，如果对应的队列尚未创建，该参数控制客户端多久重试一次查找队列。默认为 `10` 秒。
4. 修改完成后点击**保存修改**应用设置。

### REST API

你也可以通过 REST API 配置全局消息队列设置。这些设置为系统级配置，会影响所有队列的内部管理行为。

```bash
curl -v -u key:secret -X PUT -H "Content-Type: application/json" http://localhost:18083/api/v5/message_queues/config -d '{"find_queue_retry_interval": "10s", "gc_interval": "1h", "regular_queue_retention_period": "7d"}'
```

### 配置文件

为实现持久化和版本控制，你可以在 EMQX 配置文件（`emqx.conf`）中定义消息队列设置。以下是包含主要设置的示例：

```hocon
mq {
    ## 消息队列清理过期消息的周期。
    gc_interval = 1h
    ## 普通消息队列中消息的最长保留时间。
    regular_queue_retention_period = 1d
    ## 当订阅队列主题时未找到队列，订阅者尝试重新查找的周期。
    find_queue_retry_interval = 10s
    ## 存储消息队列状态的数据库配置。
    ## 详细信息见 Durable Storage 配置。
    state_db {
        transaction {
            flush_interval = 10
            idle_flush_interval = 5
            conflict_window = 5000
        }
    }
    ## 存储消息内容的消息队列数据库配置。
    ## 详细信息见 Durable Storage 配置。
    message_db {
        transaction {
            flush_interval = 100
            idle_flush_interval = 20
            conflict_window = 5000
        }
    }
}
```

#### 配置项说明

- **`gc_interval`**：定义 EMQX 扫描消息队列并清理过期消息的时间间隔。
- **`regular_queue_retention_period`**：设置队列中消息的最长保留时间。超时的消息将被自动清除。
- **`find_queue_retry_interval`**：当订阅者订阅 `$q/` 主题但队列尚未存在时，重新尝试查找队列的周期。
- **`state_db`**：消息队列状态数据库配置，用于存储队列元数据（如属性、消费进度）。
  - `flush_interval`：活跃事务的提交间隔。
  - `idle_flush_interval`：空闲事务的提交间隔。
  - `conflict_window`：并发写入冲突检测的时间窗口（毫秒）。
- **`message_db`**：消息队列消息数据库配置，实际消息存储于此。
  - `flush_interval`：写入事务的提交间隔。
  - `idle_flush_interval`：空闲写入事务的提交间隔。
  - `conflict_window`：消息写入冲突检测的时间窗口（毫秒）。

## 通过 REST API 管理消息队列

EMQX 提供一组 REST API 用于管理消息队列的生命周期，包括创建、查询、更新和删除。

### 创建消息队列

通过指定主题过滤器和队列属性（如是否启用最新值语义）来创建新队列：

```bash
curl -s -u key:secret -X POST -H "Content-Type: application/json" \
http://localhost:18083/api/v5/message_queues \
-d '{"topic_filter": "t1/#", "is_lastvalue": false}' | jq
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
-d '{"dispatch_strategy": "least_inflight"}' | jq
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

- 当前消息队列**不限制**消息数量或总字节数，但会通过配置的*保留时长*进行时间限制。
- 一旦消息过期（即超出保留时长），将不再被投递，并会在 EMQX 定期执行垃圾回收时被自动清除。
