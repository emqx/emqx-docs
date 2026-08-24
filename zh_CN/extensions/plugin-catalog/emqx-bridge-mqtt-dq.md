# MQTT 磁盘队列桥接

使用该插件可将本地 MQTT 消息转发到另一个 MQTT Broker，并通过磁盘缓冲提升系统韧性。

## 功能特性

- 按桥接实例提供磁盘缓冲。
- 当远端 Broker 不可用时自动重试。
- 支持使用 `${topic}` 进行主题改写。
- 单个插件支持配置多个桥接。
- 配置更新按桥接粒度生效（未变更的桥接保持运行）。

## 工作原理

1. 使用每个桥接的 `filter_topic` 匹配本地发布消息。
2. 将匹配消息追加到磁盘队列分区。
3. 将队列中的消息发布到远端 Broker。
4. 如果因网络/连接问题导致发布失败，则自动重试。
5. 如果某个队列分区超过 `queue.max_total_bytes`，则会丢弃该分区中最旧的记录。

## 配置

推荐通过 EMQX Dashboard 配置，也可以直接修改插件配置文件。

在生产环境中，建议先从一个桥接开始，验证流量和行为后再扩展。

### 配置文件位置

有两个相关的配置文件位置：

- 安装后插件包内自带的默认配置文件：
  - Docker 安装示例（`0.2.0`）：
    `/opt/emqx/plugins/emqx_bridge_mqtt_dq-0.2.0/emqx_bridge_mqtt_dq-0.2.0/priv/config.hocon`
  - deb/rpm 安装示例（`0.2.0`）：
    `/usr/lib/emqx/plugins/emqx_bridge_mqtt_dq-0.2.0/emqx_bridge_mqtt_dq-0.2.0/priv/config.hocon`

- 通过 Dashboard 或 API 保存后，由 EMQX 管理的持久化插件配置文件：
  - Docker：
    `/opt/emqx/data/plugins/emqx_bridge_mqtt_dq/config.hocon`
  - deb/rpm：
    `/var/lib/emqx/plugins/emqx_bridge_mqtt_dq/config.hocon`

`priv/config.hocon` 是插件包自带的默认模板。
`data/plugins/.../config.hocon` 是 EMQX 保存配置变更后实际使用的持久化配置文件。

### 快速开始（Dashboard）

1. 启用插件。
2. 在 `remotes` 下添加一个可复用的远端配置。
3. 在 `bridges` 下添加一个桥接配置。
4. 设置 `remote`、`filter_topic` 和 `remote_topic`。
5. 保存并验证远端消息投递。
6. 只有在基线验证完成后再调整队列和连接池参数。

### 示例

```
bridges {
  to-cloud {
    enable = true
    remote = cloud
    proto_ver = "v4"
    keepalive_s = 60
    pool_size = 4
    filter_topic = "devices/#"
    remote_topic = "fwd/${topic}"
    remote_qos = "${qos}"
    remote_retain = "${retain}"
    queue {
      seg_bytes = "100MB"
      max_total_bytes = "1GB"
    }
  }
}

remotes {
  cloud {
    server = "cloud-broker.example.com:8883"
    username = "bridge_user"
    password = "secret"
    ssl {
      enable = true
      verify = verify_none
      # cacertfile = "/path/to/ca.pem"
      # certfile = "/path/to/client-cert.pem"
      # keyfile = "/path/to/client-key.pem"
    }
  }
}
```

### 环境变量替换

配置文件中的任意字符串值都可以使用 `${EMQXDQ_*}` 语法引用操作系统环境变量。只有带有 `EMQXDQ_` 前缀的变量会被解析；其他 `${...}` 模式（例如 `remote_topic` 中的 `${topic}`）不会被替换。整个值必须就是占位符本身；不支持部分插值（例如 `"prefix-${EMQXDQ_VAR}-suffix"`）。

**限制：** `${EMQXDQ_*}` 替换仅适用于接收字符串值的配置字段（例如 `server`、`username`、`password`）。它不能用于布尔字段（`enable`）或整数字段（`pool_size`、`keepalive_s`）。

示例：

```
remotes {
  cloud {
    server = "${EMQXDQ_REMOTE_SERVER}"
    username = "${EMQXDQ_REMOTE_USER}"
    password = "${EMQXDQ_REMOTE_PASSWORD}"
  }
}
```

如果环境变量未设置，插件会记录错误日志，并将原始 `${EMQXDQ_...}` 字符串保留为字面值。这通常会导致连接失败（例如尝试连接到 `"${EMQXDQ_REMOTE_SERVER}"`），从而使配置错误在日志和状态 API 中都能被明显发现。

> **警告：动态配置更新与节点本地环境变量**
>
> 环境变量会在节点解析配置时进行替换。当您通过 EMQX Dashboard、REST API 或 CLI 更新插件配置时，原始配置文本会被持久化，然后在集群中每个节点上重新解析。如果各节点上的相关环境变量值不同（或有的节点缺失），每个节点解析出的生效配置也会不同。
>
> 因此，除非您能确保集群中每个节点都设置了完全一致的环境变量，否则**不要在通过 Dashboard、API 或 CLI 下发的配置中使用 `${EMQXDQ_...}` 替换**。如果需要使用节点本地 secret，建议直接编辑配置文件并重载插件，或者使用统一的 secret 注入方式（例如在 Kubernetes 中以相同方式挂载 ConfigMap/Secret）。

### 配置参考

#### 顶层

| 字段 | 类型 | 默认值 | 说明 |
|------|------|--------|------|
| `bridges` | map | `{}` | 桥接名称到桥接配置的映射。 |
| `remotes` | map | `{}` | 可复用远端 Broker 定义的映射。 |

#### Bridge（`bridges.<name>`）

| 字段 | 类型 | 默认值 | 说明 |
|------|------|--------|------|
| `enable` | boolean | `true` | 启用或禁用该桥接。 |
| `remote` | string | — | 对应 `remotes` 中远端定义的名称。 |
| `proto_ver` | string | `"v4"` | MQTT 协议版本：`v3`、`v4` 或 `v5`。 |
| `clientid_prefix` | string | `"emqx-dq-<name>-"` | 自动生成 MQTT Client ID 的前缀。每个连接会追加唯一索引（例如 `emqx-dq-mybridge-0`）。可选；置空则使用默认值。 |
| `keepalive_s` | integer | `60` | MQTT keep-alive 秒数。 |
| `pool_size` | integer | `4` | 到远端 Broker 的 MQTT 连接数。 |
| `buffer_pool_size` | integer | `4` | 每个桥接的磁盘队列 buffer worker 数量。见下方告警说明。 |
| `filter_topic` | string | — | 本地主题过滤模式。支持 `+` 和 `#` 通配符。 |
| `remote_topic` | string | — | 目标主题模板。使用 `${topic}` 表示原始主题。 |
| `enqueue_timeout_ms` | integer | `5000` | 等待磁盘队列确认的最大阻塞时间（毫秒）。仅对 QoS > 0 生效；QoS 0 始终异步。 |
| `max_inflight` | integer | `32` | 每个到远端 Broker 连接的最大未确认消息数。控制从磁盘队列取批大小以及 emqtt 发送窗口。 |
| `remote_qos` | string | `"${qos}"` | 向远端 Broker 发布时使用的 QoS（`"0"`、`"1"`、`"2"`）。默认 `"${qos}"` 表示保持原消息 QoS。 |
| `remote_retain` | string | `"${retain}"` | 向远端 Broker 发布时使用的 retain 标志（`"true"`、`"false"`）。默认 `"${retain}"` 表示保持原消息 retain 标志。 |
| `max_publish_retries` | integer | `-1` | 每条消息在被丢弃前的发布重试次数。`-1` 表示无限重试。每次 PUBACK 失败或连接丢失都会消耗一次重试额度。 |

#### Remote（`remotes.<name>`）

| 字段 | 类型 | 默认值 | 说明 |
|------|------|--------|------|
| `server` | string | — | 远端 MQTT Broker 地址（`host:port`）。 |
| `username` | string | `""` | 连接远端 Broker 使用的用户名。 |
| `password` | string | `""` | 连接远端 Broker 使用的密码。 |
| `ssl.enable` | boolean | `false` | 是否启用到远端 Broker 的 SSL/TLS 连接。 |
| `ssl.verify` | string | `verify_none` | TLS 校验模式。支持：`verify_none`、`verify_peer`。 |
| `ssl.sni` | string | server hostname | TLS SNI。默认使用 server 主机名。设为 `"disable"` 可关闭 SNI。 |
| `ssl.cacertfile` | string | — | 用于校验远端 Broker 证书的 CA 证书文件。 |
| `ssl.certfile` | string | — | 双向 TLS 认证使用的客户端证书文件。 |
| `ssl.keyfile` | string | — | 双向 TLS 认证使用的客户端私钥文件。 |

#### Queue

| 字段 | 类型 | 默认值 | 说明 |
|------|------|--------|------|
| `queue.base_dir` | string | `"emqx_bridge_mqtt_dq"` | 磁盘队列段文件的基础目录。桥接名与分区索引会自动追加在后面（即 `<base_dir>/<bridge_name>/<index>`）。相对路径基于 EMQX `data_dir` 解析；绝对路径则按原样使用。 |
| `queue_seg_bytes` | string | `"100MB"` | 单个队列段文件的最大大小。 |
| `queue.max_total_bytes` | string | `"1GB"` | **每个分区**允许使用的最大磁盘队列大小。每个桥接使用 `buffer_pool_size` 个分区（默认 4），因此最坏情况下总磁盘使用量为 `buffer_pool_size` x 该值。超出后会丢弃最旧消息。 |

## 主题模板

`remote_topic` 字段支持 `${topic}` 占位符，转发时会被替换为原始发布主题。

示例：
- `remote_topic = "${topic}"`：保持原始主题不变直接转发。
- `remote_topic = "forwarded/${topic}"`：在原始主题前增加前缀。
- `remote_topic = "region1/${topic}"`：添加区域命名空间。

`remote_topic` 在消息从队列发送出去时生效。修改该字段后，受影响桥接在重启后，队列中的消息会使用新的模板。

## REST API

该插件在 EMQX plugin API 基础路径下暴露四个端点：

- `GET /api/v5/plugin_api/emqx_bridge_mqtt_dq/metrics`：Prometheus 文本格式
- `GET /api/v5/plugin_api/emqx_bridge_mqtt_dq/stats`：JSON 仪表盘快照
- `GET /api/v5/plugin_api/emqx_bridge_mqtt_dq/stats/<bridge>`：单个桥接视图
- `GET /api/v5/plugin_api/emqx_bridge_mqtt_dq/status`：插件/集群健康状态摘要

所有 JSON 端点都返回 `application/json; charset=utf-8`。

这些 JSON API 是集群聚合的。如果有节点不可用或聚合超时，API 仍会返回尽力而为的数据，但响应中会明确给出集群完整性元数据。

示例：

```bash
curl -u admin:public \
  http://127.0.0.1:18083/api/v5/plugin_api/emqx_bridge_mqtt_dq/metrics
```

```bash
curl -u admin:public \
  http://127.0.0.1:18083/api/v5/plugin_api/emqx_bridge_mqtt_dq/stats
```

### `/stats` 响应结构

`/stats` 响应体包含：

- `cluster`：集群完整性与失败节点信息
- `uptime_seconds`：响应节点中观察到的最大插件运行时长
- `summary`：所有已配置桥接的汇总统计
- `bridges`：每个已配置桥接对应一项

示例：

```json
{
  "cluster": {
    "complete": true,
    "responded_nodes": ["emqx@127.0.0.1"],
    "failed_nodes": [],
    "timeout_ms": 5000
  },
  "uptime_seconds": 123,
  "summary": {
    "bridge_count": 1,
    "running_bridge_count": 1,
    "buffered": 12,
    "backlog": 3,
    "inflight": 8,
    "enqueue": 1000,
    "dequeue": 995,
    "publish": 990,
    "drop": 5
  },
  "bridges": [
    {
      "name": "to-cloud",
      "config_state": "enabled",
      "runtime_state": "running",
      "status": "ok",
      "status_reason": null,
      "enqueue": 1000,
      "dequeue": 995,
      "publish": 990,
      "drop": 5,
      "retried_by_reason": {
        "connect_failed": 2,
        "reason_code": 3
      },
      "buffered": 12,
      "backlog": 3,
      "inflight": 8,
      "buffers": [
        {
          "bridge": "to-cloud",
          "index": 0,
          "status": "running",
          "buffered": 12
        }
      ],
      "connectors": [
        {
          "bridge": "to-cloud",
          "index": 0,
          "status": "connected",
          "backlog": 3,
          "inflight": 8
        }
      ]
    }
  ]
}
```

`GET /stats/<bridge>` 返回：

```json
{
  "cluster": {
    "complete": true,
    "responded_nodes": ["emqx@127.0.0.1"],
    "failed_nodes": [],
    "timeout_ms": 5000
  },
  "bridge": {
    "name": "to-cloud",
    "config_state": "enabled",
    "runtime_state": "running",
    "status": "ok"
  }
}
```

如果当前配置中不存在该桥接，则 API 返回 `404`。

`GET /status` 返回精简健康视图：

```json
{
  "plugin": "emqx_bridge_mqtt_dq",
  "cluster": {
    "complete": true,
    "responded_nodes": ["emqx@127.0.0.1"],
    "failed_nodes": [],
    "timeout_ms": 5000
  },
  "status": "ok",
  "bridge_count": 1
}
```

`/metrics` 端点以 Prometheus 文本格式返回集群聚合指标，例如：

- `emqx_bridge_mqtt_dq_uptime_seconds`
- `emqx_bridge_mqtt_dq_bridge_enqueue_total{bridge="..."}`
- `emqx_bridge_mqtt_dq_bridge_dequeue_total{bridge="..."}`
- `emqx_bridge_mqtt_dq_bridge_publish_total{bridge="..."}`
- `emqx_bridge_mqtt_dq_bridge_drop_total{bridge="..."}`
- `emqx_bridge_mqtt_dq_bridge_status{bridge="...",status="..."}`
- `emqx_bridge_mqtt_dq_bridge_retry_reason_total{bridge="...",reason="..."}`
- `emqx_bridge_mqtt_dq_buffer_buffered{bridge="...",index="..."}`
- `emqx_bridge_mqtt_dq_connector_backlog{bridge="...",index="..."}`
- `emqx_bridge_mqtt_dq_connector_inflight{bridge="...",index="..."}`

### 指标语义

#### Bridge 指标

- `enqueue`：进入桥接入队路径的本地消息数
- `dequeue`：从本地队列中持久化移除的消息数
- `publish`：成功发布到远端 Broker 的消息数
- `drop`：最终被判定为丢弃的排队消息数
- `retried_by_reason`：按原因分类的重试次数
- `config_state`：配置中期望的桥接状态（`enabled` 或 `disabled`）
- `runtime_state`：观测到的 worker/storage 运行状态（`running`、`degraded` 或 `purged`）
- `status`：面向运维的桥接健康状态（`ok`、`partial`、`disconnected`、`disabled`、`error`）

当前重试原因包括：

- `reason_code`：远端 Broker 返回了非成功 MQTT reason code，因此消息被重试
- `connect_failed`：连接或发布失败触发重试
- `timeout`：超时类重试
- `connection_lost`：关联客户端进程退出，导致 inflight 消息被回收后重试
- `other`：无法归类时的兜底原因

当桥接完全清空后，计数器满足：

- `enqueue = dequeue = publish + drop`

#### Buffer 指标

- `buffered`：当前该持久化队列分区中存储的消息数量
- buffer 行中的 `status`：worker 存在时为 `running`，否则为 `missing`

该 gauge 会在 `replayq:open/1` 之后立即刷新，因此即使尚无新流量，磁盘上已持久化的消息也能被观测到。

#### Connector 指标

- `backlog`：connector backlog 队列中等待分发给 `emqtt` 的消息数
- `inflight`：已经交给 `emqtt` 但仍在等待完成的消息数
- connector 行中的 `status`：`connected`、`disconnected`、`partial`、`missing` 或 `unknown`

## 配置变更行为

配置更新按桥接粒度生效：
- 已变更的桥接会重启。
- 已删除的桥接会停止。
- 被禁用的桥接会停止并清空其队列目录。
- 新桥接会启动。
- 未变更的桥接会继续运行。

插件整体不会因为每次配置更新而重启。
但每个被重启的桥接都会有一个短暂切换窗口，在此期间匹配的消息可能被丢弃。建议在低流量时段进行影响桥接的配置调整。

### 修改配置前建议

1. 确认哪些桥接会受到影响。
2. 选择低流量窗口执行。
3. 通过 Dashboard 状态和日志观察重启/重连错误。
4. 对关键链路，在变更后验证端到端消息投递。

### 修改 `queue.base_dir`

修改已启用桥接的 `queue.base_dir` 会使该桥接使用新目录重启。实际队列路径为 `<base_dir>/<bridge_name>/<index>`。旧目录**不会**被自动清理：它会作为孤立数据继续保留在磁盘中。如果旧目录不再需要，请在确认桥接已在新路径上正常运行后手动删除。

### 修改 `buffer_pool_size`

`buffer_pool_size` 控制每个桥接拥有多少个磁盘队列分区。消息会通过 `erlang:phash2(Topic, buffer_pool_size)` 被分配到各分区。修改该值会带来以下影响：

1. **缩小分区池**（例如 8 -> 4）：索引 >= 新大小的分区将不再被消费。其旧文件仍保留在 `queue.base_dir` 下，需要手动清理。

2. **扩大分区池**（例如 4 -> 8）：哈希空间会发生变化，因此此前映射到分区 N 的主题，之后可能映射到分区 M。旧分区中已排队的消息仍会继续投递（且在该分区内保持顺序），但同一主题的新消息可能进入新分区。这会打破变更前后的端到端逐主题顺序：某些旧消息可能会晚于新消息送达。

3. **桥接级掉消息窗口**：修改 `buffer_pool_size` 会导致该桥接重启，因此切换期间匹配中的消息可能丢失。

## 消息投递保证

在正常运行情况下，该插件提供**至少一次**投递；在持续故障情况下，则退化为**尽力而为**。以下场景可能发生消息丢失：

### 磁盘队列溢出

当某个队列分区超过 `queue.max_total_bytes` 时，该分区中最旧的消息会被静默丢弃，以便为新数据腾出空间。插件会周期性输出警告日志（`mqtt_dq_buffer_overflow`），而不是为每条消息单独记日志。

**缓解方式：** 增加 `queue.max_total_bytes`、增加 `buffer_pool_size` 以分散负载到更多分区，或降低消息吞吐量。

### 远端 Broker 拒绝发布

当远端 Broker 在 PUBACK（QoS 1）或 PUBREC（QoS 2）中返回非成功 reason code 时，connector 最多会重试 3 次。如果重试全部耗尽，消息会被丢弃，并输出警告日志（`mqtt_dq_publish_dropped`）。

常见拒绝 reason code 包括：

| Code | 含义（MQTT 5.0） |
|------|------------------|
| 16   | No matching subscribers |
| 128  | Unspecified error |
| 131  | Implementation specific error |
| 135  | Not authorized |
| 144  | Topic Name invalid |
| 145  | Packet identifier in use |
| 151  | Quota exceeded |

注意：reason code 0（Success）和 16（No matching subscribers）会被视为成功投递，不会触发重试。

**缓解方式：** 检查远端 Broker 的 ACL 与主题策略，并结合日志定位具体 reason code。

### 连接反复失败

每当到远端 Broker 的连接断开时，所有待确认（尚未 ACK）的消息都会损失一次重试机会。若在没有一次成功投递的情况下累计发生 3 次连接失败，则该消息会被丢弃。

例如，一条在网络故障期间发布的消息：
1. 本地入队（重试计数 = 3）。
2. 远端重连成功，消息被发送，但在 ACK 返回前再次断连（重试计数 = 2）。
3. 再次重连并发送，连接又断开（重试计数 = 1）。
4. 再次重连并发送，若再次被拒绝或连接断开（重试计数 = 0）。
5. 消息被丢弃，并记录警告日志。

**缓解方式：** 排查远端 Broker 持续不可达的原因。短暂网络抖动会被透明处理；只有持续不稳定才会触发该场景。

### 入队背压（QoS > 0 本地发布）

当 QoS 1 或 2 客户端发布了一条匹配桥接的消息时，插件会先将该消息发送到 buffer worker 的邮箱，然后最多阻塞发布会话进程 `enqueue_timeout_ms`（默认 5000 ms），等待磁盘写入确认。

当该超时触发时，消息本身**不会丢失**：它已经进入了 buffer worker 的 Erlang 邮箱，最终仍会被写入磁盘队列。该超时仅用于控制本地发布路径最多阻塞多久。

其重要性在于：`message.publish` Hook 运行在 MQTT 会话进程内部。当 Hook 阻塞时，该会话无法处理来自同一客户端的其他消息。如果 buffer worker 很慢（例如磁盘 IO 卡顿或邮箱堆积严重），这个超时机制可以防止单个慢桥接无限期阻塞客户端会话。

当超时触发时：
1. 会话进程停止等待并继续正常执行。
2. 客户端照常收到 PUBACK/PUBREC：不会显式收到发布错误。
3. 插件会记录告警日志（`mqtt_dq_enqueue_timeout`）。
4. 消息继续保留在 buffer worker 的邮箱中，待 worker 追上处理进度后写入磁盘队列。

真正的风险是间接的：如果 buffer worker 长时间跟不上，邮箱会无限增长，从而增加内存占用。这意味着桥接处理能力已经落后于输入流量。

**缓解方式：** 增加 `buffer_pool_size` 分散负载，为 `queue.base_dir` 使用更快的存储，或降低匹配主题的消息速率。

注意：QoS 0 本地发布从不阻塞：它们始终异步入队，不对发布会话施加背压。

### Bridge 重启窗口

当桥接因配置变更、插件重载或启停操作而重启时，会存在一个短暂窗口，在此期间匹配消息可能无法被捕获。

**缓解方式：** 在低流量时段执行配置变更。

### QoS 0 的 TCP 级投递

对于发送到远端 Broker 的 QoS 0 消息，connector 在消息进入本地 TCP 发送缓冲区后即认为投递成功。如果远端 Broker 在 TCP 栈接收数据之后、Broker 进程真正处理该消息之前发生崩溃，则消息可能丢失，而且 connector 不会收到任何错误反馈。

这是 MQTT QoS 0 本身的特性，并非该插件特有问题。

## 运维说明

### 持久化

缓冲消息在以下场景下依然可以保留：
- EMQX 节点重启。
- 插件重载或升级。
- 到远端 Broker 的临时网络中断。

### 队列上限

当某个分区的队列使用量超过 `queue.max_total_bytes` 时，该分区中最旧的消息会被丢弃，以便为新数据腾出空间。插件会输出警告日志。

### Pool 大小规划

每个 buffer worker 会通过 `BufferIndex rem pool_size` 被分配给一个 connector。为了让负载分布更均匀：

- `buffer_pool_size` 应当**大于或等于** `pool_size`。
- `buffer_pool_size` 最好是 `pool_size` 的**整数倍**
  （即 `buffer_pool_size mod pool_size = 0`）。

较好的例子：`pool_size = 4, buffer_pool_size = 4`（1:1），
`pool_size = 4, buffer_pool_size = 8`（2:1）。

较差的例子：`pool_size = 4, buffer_pool_size = 5`：connector 0 需要服务两个 buffer，而其他 connector 只服务一个，会导致吞吐分布不均。

如果某个 connector 断开，分配给它的 buffer worker 会暂停；待该 connector 重连后会自动恢复。

### 顺序性

在桥接配置稳定不变的情况下，可以保持逐主题顺序。如果修改了 `buffer_pool_size`，则可能按前文所述暂时影响顺序性。

### 发布者 ACK 行为（QoS 1/2）

对于匹配桥接的消息：
- 返回给发布客户端的 `PUBACK`（QoS 1）与 `PUBREC`（QoS 2）可能会被延迟，直到 EMQX 等到磁盘队列入队确认（`enqueue_timeout_ms`）。
- 即使该等待最终超时，EMQX 仍会完成客户端的发布流程。客户端不会因为磁盘队列入队超时而收到发布错误。

<!-- PLUGIN-DOWNLOADS:BEGIN (auto-generated, do not edit) -->

## 下载

各 EMQX 版本对应的插件安装包：

| EMQX 版本 | 插件版本 | 安装包 |
|---|---|---|
| 6.2.0 | 0.5.1 | [emqx_bridge_mqtt_dq-0.5.1.tar.gz](https://www.emqx.com/downloads/emqx-plugins/6.2.0/emqx_bridge_mqtt_dq-0.5.1.tar.gz) ([sha256](https://www.emqx.com/downloads/emqx-plugins/6.2.0/emqx_bridge_mqtt_dq-0.5.1.sha256)) |
| 6.2.1 | 0.5.2 | [emqx_bridge_mqtt_dq-0.5.2.tar.gz](https://www.emqx.com/downloads/emqx-plugins/6.2.1/emqx_bridge_mqtt_dq-0.5.2.tar.gz) ([sha256](https://www.emqx.com/downloads/emqx-plugins/6.2.1/emqx_bridge_mqtt_dq-0.5.2.sha256)) |
| 6.2.2 | 0.5.2 | [emqx_bridge_mqtt_dq-0.5.2.tar.gz](https://www.emqx.com/downloads/emqx-plugins/6.2.2/emqx_bridge_mqtt_dq-0.5.2.tar.gz) ([sha256](https://www.emqx.com/downloads/emqx-plugins/6.2.2/emqx_bridge_mqtt_dq-0.5.2.sha256)) |
| 6.2.3 | 0.5.2 | [emqx_bridge_mqtt_dq-0.5.2.tar.gz](https://www.emqx.com/downloads/emqx-plugins/6.2.3/emqx_bridge_mqtt_dq-0.5.2.tar.gz) ([sha256](https://www.emqx.com/downloads/emqx-plugins/6.2.3/emqx_bridge_mqtt_dq-0.5.2.sha256)) |

<!-- PLUGIN-DOWNLOADS:END -->
