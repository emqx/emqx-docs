# 带磁盘队列的 MQTT 桥接

使用此插件可将本地 MQTT 消息转发到另一个 MQTT Broker，并借助磁盘缓冲提升可靠性。

## 特性

- 每个桥接独立的磁盘缓冲。
- 当远端 Broker 不可用时自动重试。
- 支持通过 `${topic}` 进行主题重写。
- 单个插件中可配置多个桥接。
- 配置更新按桥接逐个应用（未变更的桥接保持运行）。

## 工作原理

1. 使用每个桥接的 `filter_topic` 匹配本地发布的消息。
2. 将匹配的消息追加到磁盘队列分区。
3. 将排队的消息发布到远端 Broker。
4. 若因网络/连接问题导致发布失败，则自动重试。
5. 如果某个队列分区超过 `queue.max_total_bytes`，该分区中最旧的记录将被丢弃。

## 配置

可从 EMQX Dashboard（推荐）或通过插件配置文件进行配置。

在生产环境中，建议先配置一个桥接，验证流量后再横向扩展。

### 配置文件位置

有两个相关的配置文件位置：

- 已安装插件包内自带的默认文件：
  - `0.2.0` 的 docker 安装示例：
    `/opt/emqx/plugins/emqx_bridge_mqtt_dq-0.2.0/emqx_bridge_mqtt_dq-0.2.0/priv/config.hocon`
  - `0.2.0` 的 deb/rpm 安装示例：
    `/usr/lib/emqx/plugins/emqx_bridge_mqtt_dq-0.2.0/emqx_bridge_mqtt_dq-0.2.0/priv/config.hocon`

- 通过 Dashboard 或 API 保存配置后，由 EMQX 管理的持久化插件配置文件：
  - docker：
    `/opt/emqx/data/plugins/emqx_bridge_mqtt_dq/config.hocon`
  - deb/rpm：
    `/var/lib/emqx/plugins/emqx_bridge_mqtt_dq/config.hocon`

`priv/config.hocon` 文件是随包提供的默认模板。`data/plugins/.../config.hocon`
文件是 EMQX 保存插件配置变更后使用的持久化插件配置位置。

### 快速开始（Dashboard）

1. 启用插件。
2. 在 `remotes` 下添加一个可复用的远端。
3. 在 `bridges` 下添加一个桥接。
4. 设置 `remote`、`filter_topic` 和 `remote_topic`。
5. 保存并验证远端投递。
6. 仅在完成基线验证后再调优队列和连接池设置。

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

配置文件中的任何字符串值都可以使用 `${EMQXDQ_*}` 语法引用操作系统环境变量。
只有带 `EMQXDQ_` 前缀的变量会被解析——其他 `${...}` 模式（例如 `remote_topic`
中的 `${topic}`）保持不变。整个值必须就是占位符；不支持部分插值（例如
`"prefix-${EMQXDQ_VAR}-suffix"`）。

**限制：** `${EMQXDQ_*}` 替换仅适用于接受字符串值的配置字段（例如 `server`、
`username`、`password`）。它不能用于布尔字段（`enable`）和整数字段（`pool_size`、
`keepalive_s`）。

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

如果环境变量未设置，插件会记录一条错误，并将原始的 `${EMQXDQ_...}` 字符串作为
字面值保留。这通常会导致连接失败（例如尝试连接到 `"${EMQXDQ_REMOTE_SERVER}"`），
从而使该配置错误在日志和状态 API 中都可见。

> **警告——动态配置更新与节点本地环境变量**
>
> 环境变量在解析配置的节点上于配置解析时被解析。当你通过 EMQX Dashboard、
> REST API 或 CLI 更新插件配置时，原始配置文本会被持久化，然后在集群中的
> 每个节点上重新解析。如果不同节点对所引用的环境变量有不同的取值（或缺失取值），
> 则每个节点会解析出不同的实际生效配置。
>
> 因此，除非你确定集群中每个节点都设置了相同的环境变量，否则**应避免在
> Dashboard、API 或 CLI 配置更新中使用 `${EMQXDQ_...}` 替换**。对于节点本地的
> 密钥，建议直接编辑配置文件并重新加载插件，或使用一致的密钥注入机制（例如在
> 所有节点上以相同方式挂载的 Kubernetes ConfigMaps/Secrets）。

### 配置参考

#### 顶层

| 字段     | 类型    | 默认值 | 说明                          |
|-----------|---------|---------|--------------------------------------|
| `bridges` | map     | `{}`    | 桥接名称到桥接配置的映射。 |
| `remotes` | map     | `{}`    | 可复用的远端 Broker 定义映射。 |

#### 桥接（`bridges.<name>`）

| 字段             | 类型    | 默认值 | 说明                                                                 |
|-------------------|---------|---------|-----------------------------------------------------------------------------|
| `enable`          | boolean | `true`  | 启用或禁用此桥接。                                              |
| `remote`          | string  | —       | `remotes` 下远端 Broker 定义的名称。                       |
| `proto_ver`       | string  | `"v4"`  | MQTT 协议版本：`v3`、`v4` 或 `v5`。                                |
| `clientid_prefix` | string  | `"emqx-dq-<name>-"` | 自动生成的 MQTT 客户端 ID 前缀。每个连接会追加一个唯一索引（例如 `emqx-dq-mybridge-0`）。可选——留空则使用默认值。 |
| `keepalive_s`     | integer | `60`    | MQTT 保活间隔，单位为秒。                                        |
| `pool_size`       | integer | `4`     | 到远端 Broker 的 MQTT 连接数。                            |
| `buffer_pool_size` | integer | `4`    | 每个桥接的磁盘队列缓冲工作进程数。参见下方警告。         |
| `filter_topic`    | string  | —       | 本地主题过滤模式。支持 `+` 和 `#` 通配符。                |
| `remote_topic`    | string  | —       | 目标主题模板。使用 `${topic}` 表示原始主题。              |
| `enqueue_timeout_ms` | integer | `5000` | 阻塞等待磁盘队列确认的最长时间（毫秒）。仅适用于 QoS > 0；QoS 0 始终为异步。 |
| `max_inflight`    | integer | `32`    | 到远端 Broker 的每个连接允许的最大未确认消息数。控制从磁盘队列批量弹出的大小以及 emqtt 的发送窗口。 |
| `remote_qos`      | string | `"${qos}"` | 发布到远端 Broker 的 QoS 级别（`"0"`、`"1"`、`"2"`）。默认值 `"${qos}"` 会保留原始消息的 QoS。 |
| `remote_retain`   | string | `"${retain}"` | 发布到远端 Broker 的保留（retain）标志（`"true"`、`"false"`）。默认值 `"${retain}"` 会保留原始消息的保留标志。 |
| `max_publish_retries` | integer | `-1` | 每条消息被丢弃前的发布重试次数。`-1` 表示无限重试。每次 PUBACK 失败或连接丢失都会消耗一次额度。 |

#### 远端（`remotes.<name>`）

| 字段        | 类型    | 默认值 | 说明                                             |
|--------------|---------|---------|---------------------------------------------------------|
| `server`     | string  | —       | 远端 MQTT Broker 地址（`host:port`）。               |
| `username`   | string  | `""`    | 用于向远端 Broker 认证的用户名。     |
| `password`   | string  | `""`    | 用于向远端 Broker 认证的密码。     |
| `ssl.enable` | boolean | `false` | 为到远端 Broker 的连接启用 SSL/TLS。 |
| `ssl.verify` | string | `verify_none` | TLS 校验模式。支持的取值：`verify_none`、`verify_peer`。 |
| `ssl.sni`    | string  | 服务器主机名 | TLS 服务器名称指示（SNI）。默认为服务器主机名。设置为 `"disable"` 可关闭 SNI。 |
| `ssl.cacertfile` | string | —    | 用于校验远端 Broker 证书的 CA 证书文件。 |
| `ssl.certfile` | string | —      | 用于双向 TLS 认证的客户端证书文件。  |
| `ssl.keyfile` | string | —       | 用于双向 TLS 认证的客户端私钥文件。  |

#### 队列

| 字段             | 类型   | 默认值                        | 说明                                      |
|-------------------|--------|--------------------------------|--------------------------------------------------|
| `queue.base_dir`  | string | `"emqx_bridge_mqtt_dq"` | 磁盘队列段文件的基础目录。桥接名称和分区索引会被自动追加（即 `<base_dir>/<bridge_name>/<index>`）。相对路径相对于 EMQX 的 `data_dir` 解析。绝对路径原样使用。 |
| `queue_seg_bytes` | string | `"100MB"`                      | 每个队列段文件的最大大小。              |
| `queue.max_total_bytes` | string | `"1GB"`                  | **每个分区**的最大磁盘队列大小。每个桥接使用 `buffer_pool_size` 个分区（默认 4 个），因此最坏情况下的磁盘总占用为 `buffer_pool_size` × 此值。超出时会丢弃最旧的消息。 |

## 主题模板

`remote_topic` 字段支持 `${topic}` 占位符，转发时会将其替换为原始的发布主题。

示例：
- `remote_topic = "${topic}"` —— 保持原始主题不变进行转发。
- `remote_topic = "forwarded/${topic}"` —— 添加前缀。
- `remote_topic = "region1/${topic}"` —— 添加区域命名空间。

`remote_topic` 在消息从队列发出时应用。更改此字段后，已排队的消息将在受影响的桥接
重启后使用新模板。

## REST API

该插件在 EMQX 插件 API 基础路径下暴露四个端点：

- `GET /api/v5/plugin_api/emqx_bridge_mqtt_dq/metrics` —— Prometheus 文本格式
- `GET /api/v5/plugin_api/emqx_bridge_mqtt_dq/stats` —— JSON Dashboard 快照
- `GET /api/v5/plugin_api/emqx_bridge_mqtt_dq/stats/<bridge>` —— 仅单个桥接
- `GET /api/v5/plugin_api/emqx_bridge_mqtt_dq/status` —— 插件/集群健康摘要

所有 JSON 端点均返回 `application/json; charset=utf-8`。

JSON API 为集群聚合结果。如果某个节点在聚合期间不可用或超时，API 仍会返回尽力而为的
数据，但响应中会包含明确的集群完整性元数据。

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

- `cluster`：集群完整性和失败节点信息
- `uptime_seconds`：在响应节点中观测到的最大插件运行时长
- `summary`：所有已配置桥接的合计
- `bridges`：每个已配置桥接一个条目

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

如果该桥接在当前配置中不存在，API 返回 `404`。

`GET /status` 返回一个简洁的健康视图：

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

`/metrics` 端点返回 Prometheus 文本导出格式，包含集群聚合的时间序列，例如：

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

#### 桥接指标

- `enqueue`：进入桥接入队路径而被接受的本地消息数量
- `dequeue`：从本地队列中被持久化移除的消息数量
- `publish`：成功发布到远端 Broker 的消息数量
- `drop`：最终被判定为丢弃的排队消息数量
- `retried_by_reason`：按原因细分的重试次数
- `config_state`：来自配置的期望桥接状态（`enabled` 或 `disabled`）
- `runtime_state`：观测到的工作进程/存储状态（`running`、`degraded` 或 `purged`）
- `status`：面向运维人员的桥接健康状态（`ok`、`partial`、`disconnected`、`disabled`、`error`）

当前的重试原因包括：

- `reason_code`：远端 Broker 返回了非成功的 MQTT 原因码，消息被重试
- `connect_failed`：连接或发布失败触发了重试
- `timeout`：超时特定的重试分类
- `connection_lost`：关联的客户端进程退出，正在处理中的消息被抢救出来以便重试
- `other`：未分类重试原因的兜底桶

桥接完全排空后，各计数器满足：

- `enqueue = dequeue = publish + drop`

#### 缓冲指标

- `buffered`：该持久化队列分区中当前存储的消息数量
- 缓冲行 `status`：工作进程存在时为 `running`，否则为 `missing`

该 gauge 会在 `replayq:open/1` 之后立即刷新，因此即使在新流量到达之前，也能看到磁盘上
已持久化的消息。

#### 连接器指标

- `backlog`：处于连接器积压队列中、等待分发到 `emqtt` 的消息数量
- `inflight`：已交给 `emqtt` 但仍在等待完成的消息数量
- 连接器行 `status`：`connected`、`disconnected`、`partial`、`missing` 或 `unknown`

## 配置变更行为

配置更新按桥接逐个应用：
- 变更的桥接会重启。
- 被移除的桥接会停止。
- 被禁用的桥接会停止并清除其队列目录。
- 新增的桥接会启动。
- 未变更的桥接继续运行。

并非每次配置更新都会重启整个插件。
不过，每个被重启的桥接都有一个短暂的切换窗口，在此期间匹配的消息可能被丢弃。
请在低流量时段应用会影响桥接的变更。

### 变更配置前

1. 识别哪些桥接会受影响。
2. 在低流量时段应用。
3. 监控 Dashboard 状态和日志，关注重启/重连错误。
4. 对于关键管道，在变更后验证端到端投递。

### 变更 `queue.base_dir`

在已启用的桥接上变更 `queue.base_dir` 会使桥接以新目录重启。实际的队列路径为
`<base_dir>/<bridge_name>/<index>`。旧目录**不会**被自动清除——它会作为孤立数据
留在磁盘上。如果不再需要旧目录，请在确认桥接已在新路径上运行后手动删除它。

### 变更 `buffer_pool_size`

`buffer_pool_size` 控制每个桥接存在多少个磁盘队列分区。消息通过
`erlang:phash2(Topic, buffer_pool_size)` 分配到各分区。变更此值有重要的副作用：

1. **缩小分区池**（例如 8 -> 4）：索引 >= 新大小的分区不再被消费。它们的旧文件仍
   保留在 `queue.base_dir` 下，需要手动清理。

2. **扩大分区池**（例如 4 -> 8）：哈希空间发生变化，因此原本映射到分区 N 的主题
   现在可能映射到分区 M。已在旧分区中排队的消息仍会被投递（在该分区内保持顺序），
   但同一主题的新消息可能进入不同的分区。这会在过渡期间破坏端到端的按主题顺序——
   一些旧消息可能在新消息之后才被投递。

3. **桥接范围的丢弃窗口**：变更 `buffer_pool_size` 会重启该桥接，因此在切换期间
   正在处理中的匹配消息可能被丢弃。

## 消息投递保证

该插件在正常运行下提供**至少一次**（at-least-once）投递，在持续故障下提供
**尽力而为**（best-effort）投递。在以下场景中消息可能丢失：

### 磁盘队列溢出

当某个队列分区超过 `queue.max_total_bytes` 时，该分区中最旧的消息会被静默丢弃，以为
新数据腾出空间。会周期性地（而非每条消息）发出一条警告日志（`mqtt_dq_buffer_overflow`）。

**缓解措施**：增大 `queue.max_total_bytes`，增大 `buffer_pool_size` 以将负载分散到更多
分区，或降低消息吞吐量。

### 远端 Broker 拒绝发布

当远端 Broker 在 PUBACK（QoS 1）或 PUBREC（QoS 2）中返回非成功的 MQTT 原因码时，
连接器会对该消息最多重试 3 次。如果所有重试都耗尽，消息会被丢弃并发出一条警告日志
（`mqtt_dq_publish_dropped`）。

常见的拒绝原因码包括：

| 原因码 | 含义（MQTT 5.0）              |
|------|---------------------------------|
| 16   | 无匹配的订阅者         |
| 128  | 未指定的错误                |
| 131  | 具体实现相关的错误    |
| 135  | 未授权                  |
| 144  | 主题名称无效              |
| 145  | 报文标识符正在使用中        |
| 151  | 超出配额                  |

注意：原因码 0（成功）和 16（无匹配的订阅者）被视为成功投递，不会触发重试。

**缓解措施**：检查远端 Broker 的 ACL 和主题策略。查看日志中的具体原因码。

### 反复的连接失败

每当到远端 Broker 的连接断开时，所有待处理（尚未确认）的消息都会损失一次重试机会。
在没有一次成功投递的情况下累计 3 次连接失败后，消息会被丢弃。

例如，在网络中断期间发布的一条消息：
1. 在本地排队（重试计数器 = 3）。
2. 远端重连，消息被分发——远端在 ACK 之前再次断开（重试计数器 = 2）。
3. 重连，再次分发——连接断开（重试计数器 = 1）。
4. 重连，分发——被拒绝或连接断开（重试计数器 = 0）。
5. 消息被丢弃，记录警告日志。

**缓解措施**：排查远端 Broker 为何反复不可达。瞬时的网络抖动会被透明处理；此场景需要
持续的不稳定才会发生。

### 入队背压（QoS > 0 的本地发布）

当 QoS 1 或 2 的客户端发布一条与桥接匹配的消息时，插件会将该消息发送到缓冲工作进程的
邮箱，然后阻塞发布会话进程，最长阻塞 `enqueue_timeout_ms`（默认 5000 毫秒）以等待
磁盘写入确认。

该超时触发时消息本身**不会丢失**——它已经在缓冲工作进程的 Erlang 邮箱中，最终会被写入
磁盘队列。该超时只控制本地发布路径阻塞的时长。

为何这很重要：`message.publish` 钩子运行在 MQTT 会话进程内。当钩子处于阻塞状态时，
会话无法处理来自该客户端的其他消息。如果缓冲工作进程较慢（例如磁盘 I/O 停顿或邮箱
积压严重），该超时可防止一个慢桥接无限期地拖住客户端会话。

当超时触发时：
1. 会话进程停止等待并正常继续。
2. 客户端照常收到 PUBACK/PUBREC——不会暴露任何错误。
3. 发出一条警告日志（`mqtt_dq_enqueue_timeout`）。
4. 消息仍留在缓冲工作进程的邮箱中，待工作进程赶上进度后写入磁盘队列。

风险是间接的：如果缓冲工作进程持续落后，其邮箱会无限增长，从而增加内存使用。这表明
桥接跟不上传入的消息速率。

**缓解措施**：增大 `buffer_pool_size` 以分散负载，为 `queue.base_dir` 使用更快的存储，
或降低匹配主题的消息速率。

注意：QoS 0 的本地发布从不阻塞——它们以异步方式入队，不会对发布会话施加任何背压。

### 桥接重启窗口

当桥接重启时（由于配置变更、插件重载或启用/禁用切换），会有一个短暂窗口，期间匹配的
消息可能未被捕获。

**缓解措施**：在低流量时段应用配置变更。

### QoS 0 的 TCP 层投递

对于发布到远端 Broker 的 QoS 0 消息，连接器一旦将消息送达本地 TCP 发送缓冲区即认为
投递成功。如果远端 Broker 在 TCP 栈接受数据之后、但在 Broker 处理之前崩溃，消息可能
在没有任何错误返回给连接器的情况下丢失。

这是 MQTT QoS 0 固有的特性，并非本插件特有。

## 运维注意事项

### 持久化

缓冲的消息可在以下情况下存续：
- EMQX 节点重启。
- 插件重载和升级。
- 到远端 Broker 的临时网络中断。

### 队列上限

当某个分区的队列使用量超过 `queue.max_total_bytes` 时，该分区中最旧的消息会被丢弃，以为
新数据腾出空间。会发出警告日志。

### 连接池大小设置

每个缓冲工作进程通过 `BufferIndex rem pool_size` 被分配到恰好一个连接器。为实现均匀的
负载分布：

- `buffer_pool_size` 应**大于或等于** `pool_size`。
- `buffer_pool_size` 应为 `pool_size` 的**整数倍**（即 `buffer_pool_size mod pool_size = 0`）。

良好示例：`pool_size = 4, buffer_pool_size = 4`（1:1），
`pool_size = 4, buffer_pool_size = 8`（2:1）。

不良示例：`pool_size = 4, buffer_pool_size = 5` —— 连接器 0 服务两个缓冲，其余各服务
一个，导致吞吐不均。

如果某个连接器断开，分配给它的缓冲工作进程会暂停，并在该连接器重连后自动恢复。

### 顺序

在桥接设置稳定的情况下，按主题的顺序会被保留。如果你变更 `buffer_pool_size`，顺序可能
如上文所述被暂时影响。

### 发布方 ACK 行为（QoS 1/2）

对于与桥接匹配的消息：
- 在 EMQX 等待磁盘队列入队确认（`enqueue_timeout_ms`）期间，向发布客户端返回的
  `PUBACK`（QoS 1）和 `PUBREC`（QoS 2）可能被延迟。
- 如果入队等待超时，EMQX 仍会完成客户端的发布流程。客户端不会因为磁盘队列入队超时
  而收到发布错误。

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
