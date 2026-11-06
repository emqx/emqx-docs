# 设备消息广播（Bcast）

`emqx_bcast` 插件支持后端应用通过 HTTP 向 IoT 设备发送命令、通知和其他消息。它提供产品级广播、定向批量投递和 QoS 1 离线重放能力，使应用无需维护 MQTT 连接，也无需自行构建临时离线投递队列和 MQTT PUBACK 跟踪系统。应用还可以预先注册一次 Payload，并在后续批量请求中复用其消息 ID。

该插件适用于 EMQX 企业版 6.1.6 及后续 6.1 版本。

典型场景包括向同一产品下的设备广播服务通知或配置刷新信号、向指定设备发送命令或配置更新，以及在暂时离线的设备重新连接时投递 QoS 1 命令。

## 快速开始

本示例将 `Hello` 广播给产品 `P1` 下的一个在线设备。开始前，请确保 EMQX 企业版 6.1.6 或后续 6.1 版本已运行，并已安装用于建立设备连接的 [MQTTX CLI](https://mqttx.app/cli)。

### 安装并启动插件

本快速开始使用集群管理插件方式。从[下载](#下载)一节获取安装包，将其复制到 EMQX 插件目录，并将 `<version>` 替换为下载的插件版本：

```bash
cp emqx_bcast-<version>.tar.gz "$EMQX_HOME/plugins/"
emqx ctl plugins install emqx_bcast-<version> --cluster
emqx ctl plugins start emqx_bcast-<version>
```

单节点部署无需添加 `--cluster`。也可以通过 Dashboard 安装并启动插件。两种方式的详细步骤以及通过 Dashboard 安装所需的授权，请参阅[安装和管理插件](../../plugin-management.md#安装和管理插件)。

### 配置设备标识映射

插件将每个 MQTT 客户端映射为以下标识：

- `ProductKey`：客户端的 `client_attrs.tns` 命名空间属性。
- `DeviceName`：MQTT 客户端 ID。

在 Dashboard 中进入**管理** -> **MQTT 配置** -> **客户端属性**，添加属性 `tns`，并将其表达式设置为 `nth(1, tokens(username, '-'))`。对应的配置如下，它从采用 `ProductKey-DeviceName` 格式的用户名中提取第一段作为命名空间：

```hocon
mqtt.client_attrs_init = [
  { expression = "nth(1, tokens(username, '-'))", set_as_attr = tns }
]
```

使用该配置时，以用户名 `P1-device001`、客户端 ID `device001` 连接的客户端，其 `ProductKey` 为 `P1`，`DeviceName` 为 `device001`。

有关设置客户端属性的更多信息，请参阅[设置客户端属性](../../../../develop/client-attributes/client-attributes.md#设置客户端属性)。

应用该配置后，请重新连接客户端，以便 EMQX 在客户端连接时初始化该属性。

### 创建 API 密钥

在 Dashboard 中进入**系统设置** -> **API 密钥**，创建一个角色为**管理员**的 API 密钥。保持**命名空间**开关关闭以创建全局密钥。选择**自定义受限权限**，然后选择 `plugin_api` 权限范围。请立即保存 API Key 和 Secret Key。详细步骤请参阅[创建 API 密钥](../../../api.md#创建-api-密钥)。

`plugin_api` 权限范围用于访问 `/api/v5/plugin_api/emqx_bcast/*` 下的 Bcast 运行时端点，不允许访问 `/api/v5/plugins*` 下的插件管理端点；这些端点需要 `system` 权限范围。

### 连接设备并订阅主题

运行以下命令，以用户名 `P1-device001` 连接客户端 `device001`，并订阅产品 `P1` 的默认广播主题：

```bash
mqttx sub -h 127.0.0.1 -p 1883 \
  -i device001 \
  -u P1-device001 \
  -t '/sys/broadcast/P1' \
  -q 1 -v
```

如果 MQTT 监听器启用了认证，请添加对应的密码和 TLS 选项。若要通过默认主题接收 `BatchPub` 消息，请改为订阅 `/P1/device001/user/get`。

目标客户端必须订阅投递主题。客户端仅保持连接并不足以接收消息。

### 发送并验证广播消息

保持 MQTTX 客户端运行。在另一个终端中，将 `<api_key>` 和 `<api_secret>` 替换为上一步创建的凭证，然后运行：

```bash
curl -u '<api_key>:<api_secret>' \
  -X POST 'http://127.0.0.1:18083/api/v5/plugin_api/emqx_bcast/pub' \
  -H 'Content-Type: application/json' \
  -d '{"Action":"PubBroadcast","ProductKey":"P1","MessageContent":"SGVsbG8="}'
```

`MessageContent` 必须使用 base64 编码。本示例中的 `SGVsbG8=` 解码后为 `Hello`。

EMQX 返回 HTTP `200`，响应类似于：

```json
{
  "Success": true,
  "RequestId": "550e8400-e29b-41d4-a716-446655440000",
  "MessageId": "550e8400-e29b-41d4-a716-446655440000"
}
```

MQTTX 客户端将在主题 `/sys/broadcast/P1` 上收到 Payload `Hello`。

HTTP `200` 表示 EMQX 已接受请求，不表示消息已经投递。各 QoS 等级的投递保证请参阅[投递语义](#投递语义)。

::: warning 重要提示

Bcast 将消息直接发送到客户端通道。这些消息会绕过发布鉴权和常规 MQTT 发布流水线，包括规则引擎、Schema 验证、消息转换、保留消息和延迟发布。

仅向可信调用方授予 Bcast API 访问权限，并在调用方应用中验证 Payload 和目标标识。

:::

## 消息操作

所有消息操作都使用以下端点：

```http
POST /api/v5/plugin_api/emqx_bcast/pub
```

在 JSON 请求体中设置 `Action` 字段以选择具体操作。`MessageContent` 必须使用 base64 编码。

### 向在线设备广播

`PubBroadcast` 向一个产品下所有在线且已订阅投递主题的设备发送消息。该操作不会存储消息，因此离线设备无法收到消息。

```json
{
  "Action": "PubBroadcast",
  "ProductKey": "P1",
  "MessageContent": "SGVsbG8="
}
```

插件会展开已配置的 `broadcast_topic` 模板，其默认值为 `/sys/broadcast/${productKey}`。可以通过 `TopicFullName` 指定一个具体的自定义主题。`TopicFullName` 不允许包含通配符或模板占位符。

解码后的 Payload 默认上限为 64 KiB。

### 向设备列表发送消息

`BatchPub` 向明确指定的设备列表发送消息。单个请求最多可以包含 `max_device_count` 个不重复的设备名称。默认上限为 `10,000`。

```json
{
  "Action": "BatchPub",
  "ProductKey": "P1",
  "DeviceName": ["device001", "device002"],
  "MessageContent": "SGVsbG8=",
  "Qos": 1
}
```

必须且只能使用以下一种 Payload 字段：

- `MessageContent`：内联的 base64 编码 Payload。解码后的 Payload 默认上限为 10 KiB。
- `MessageId`：由 `RegisterMessage` 或之前的 QoS 1 批量请求创建的 Payload ID。

对于 `BatchPub`，投递主题按以下优先级确定：

1. `TopicTemplateName`，并展开 `${productKey}` 和 `${deviceName}`。
2. `TopicShortName`，展开为 `/${productKey}/${deviceName}/user/${TopicShortName}`。
3. 已配置的 `batch_topic`，默认值为 `/${productKey}/${deviceName}/user/get`。

### 注册并复用消息

`RegisterMessage` 存储 Payload，以供后续 `BatchPub` 请求复用：

```json
{
  "Action": "RegisterMessage",
  "MessageContent": "SGVsbG8="
}
```

响应中包含 `MessageId`。重复注册相同内容会返回同一个 ID，并刷新其 TTL。已存储的 Payload 不可修改。

如需刷新已有消息的 TTL，请提交其 ID，而不是消息内容：

```json
{
  "Action": "RegisterMessage",
  "MessageId": "550e8400-e29b-41d4-a716-446655440000"
}
```

`MessageContent` 和 `MessageId` 互斥。

## 投递语义

`BatchPub` 的 `Qos` 字段接受 `0` 或 `1`，默认值为 `0`。

| 请求 QoS | 在线设备 | 离线设备 | 存储与确认 |
| --- | --- | --- | --- |
| `0` | 目标在线并已订阅时投递。 | 跳过。 | 不存储，也不跟踪确认。 |
| `1` | 投递消息；订阅 QoS 为 1 时跟踪消息直至收到 PUBACK。 | 设备重新连接并订阅后重放。 | 待投递消息在 `msg_ttl` 后过期，默认 TTL 为 15 天。 |

实际投递 QoS 取请求 QoS 和目标订阅 QoS 中的较小值。如果 QoS 1 请求发送到 QoS 0 订阅，插件在将消息交给客户端通道后即视为投递完成。

PUBACK 仅确认设备在 MQTT 协议层面收到消息，不表示设备应用已经处理消息或执行命令。

QoS 1 批量投递采用异步的至少一次语义。客户端必须能够容忍重复消息，尤其是在重新连接和会话接管期间。HTTP `200` 成功响应仅表示 EMQX 已接受请求并准备异步处理，并不表示消息已经存储或投递。如果接收请求的节点在存储完成前发生故障，该请求可能丢失。

待处理的 QoS 1 状态以内存方式复制到多个核心节点，因此可以承受单个核心节点故障，但无法在整个集群重启后保留。生产端必须能够在集群完全重启后重放消息。

QoS 0 消息在会话接管窗口中也可能重复。

## 配置插件

下表列出了生效的配置项及其默认值：

| 配置项 | 默认值 | 描述 |
| --- | --- | --- |
| `broadcast_topic` | `/sys/broadcast/${productKey}` | `PubBroadcast` 的主题模板。 |
| `batch_topic` | `/${productKey}/${deviceName}/user/get` | `BatchPub` 的默认主题模板。 |
| `msg_ttl` | `15d` | 已注册消息和待处理 QoS 1 投递的 TTL。修改此配置只影响配置更新后创建的记录。 |
| `cleanup_interval` | `60s` | 清理过期记录的间隔。 |
| `max_device_count` | `10000` | 单个 `BatchPub` 请求允许包含的最大设备数。 |
| `max_message_size_broadcast` | `65536` | 广播消息解码后的最大字节数。 |
| `max_message_size_batch` | `10240` | 批量消息解码后的最大字节数。 |
| `max_pending_deliveries` | `10000000` | 集群范围内待处理 QoS 1 投递数上限。 |
| `max_pending_deliveries_per_device` | `100` | 单个设备待处理 QoS 1 投递数上限。配置值会被限制在 10 到 200 之间。启动或故障切换期间，此限制以尽力而为方式执行。 |
| `delivery_pool_size` | `0` | 每个投递池的工作进程数。`0` 表示自动为每个 Erlang 调度器配置一个工作进程。修改该配置会重启投递池。 |

配置示例：

```hocon
broadcast_topic = "/sys/broadcast/${productKey}"
batch_topic = "/${productKey}/${deviceName}/user/get"
msg_ttl = 15d
cleanup_interval = 60s
max_device_count = 10000
max_message_size_broadcast = 65536
max_message_size_batch = 10240
max_pending_deliveries = 10000000
max_pending_deliveries_per_device = 100
delivery_pool_size = 0
```

使用标准插件配置 API 更新这些配置：

```http
PUT /api/v5/plugins/<name-vsn>/config
```

有关安装、启动和配置步骤，请参阅[安装和管理插件](../../plugin-management.md#安装和管理插件)。`/api/v5/plugins*` 下的 REST API 操作需要 `system` 权限范围。对于节点固定插件，仍可通过 REST API 更新配置，但不能通过 REST API 管理其生命周期。

## 调用 API

使用 EMQX API 密钥和 Secret 进行 HTTP Basic 认证：

```bash
curl -u '<api_key>:<api_secret>' \
  -X POST 'http://127.0.0.1:18083/api/v5/plugin_api/emqx_bcast/pub' \
  -H 'Content-Type: application/json' \
  -d '{"Action":"PubBroadcast","ProductKey":"P1","MessageContent":"SGVsbG8="}'
```

Dashboard 用户或 API 密钥必须具有 `plugin_api` 或 `system` 权限范围，其角色和命名空间权限也必须允许所请求的 HTTP 方法。插件 API 网关接受 `system` 权限范围，但 Bcast 运行时调用应优先使用受限的 `plugin_api` 权限范围。`plugin_api` 权限范围不允许访问 `/api/v5/plugins*` 下的插件生命周期端点。

`plugin_api` 权限范围允许访问通过插件 API 网关发布的所有端点。对于 Bcast，这些端点包括消息和投递删除操作以及指标重置操作，因此请在检查所有公开操作后再分配该权限范围。

请求成功时返回 HTTP `200`：

```json
{
  "Success": true,
  "RequestId": "550e8400-e29b-41d4-a716-446655440000",
  "MessageId": "550e8400-e29b-41d4-a716-446655440000"
}
```

错误响应采用以下结构：

```json
{
  "Success": false,
  "RequestId": "550e8400-e29b-41d4-a716-446655440000",
  "Code": "InvalidBase64",
  "ErrorMessage": "Invalid Base64 encoding"
}
```

常见错误包括：

| HTTP 状态码 | Code | 原因 |
| --- | --- | --- |
| `400` | `InvalidProductKey`、`InvalidDeviceName`、`DuplicateDeviceName` | 产品或设备标识缺失或无效，或者设备列表包含重复项。 |
| `400` | `DeviceCountExceeded`、`MessageTooLarge`、`InvalidBase64` | 请求超过配置限制，或包含无效的 base64 内容。 |
| `400` | `InvalidTopicTemplate`、`InvalidQos` | 主题字段无效，或 `Qos` 不是 `0` 或 `1`。 |
| `400` | `MessageNotFound`、`MessageIdContentConflict`、`InvalidMessageId` | 引用的消息不存在、Payload 字段冲突，或 ID 无效。 |
| `429` | `QuotaExceeded` | 已达到全局或单设备待投递数量上限。 |
| `429` | `Busy` | 本地 QoS 1 接入队列已满，请稍后重试。 |

## 管理消息和投递

管理端点仅公开元数据，不会返回消息 Payload 内容。

| 方法 | 端点 | 描述 |
| --- | --- | --- |
| `GET` | `/api/v5/plugin_api/emqx_bcast/messages?limit=100&cursor=<cursor>` | 列出已注册的消息。`limit` 的取值范围为 1 到 1000。 |
| `GET` | `/api/v5/plugin_api/emqx_bcast/messages/:messageId` | 获取消息元数据及其未完成投递数。 |
| `DELETE` | `/api/v5/plugin_api/emqx_bcast/messages/:messageId` | 删除消息及其关联的待处理投递。 |
| `GET` | `/api/v5/plugin_api/emqx_bcast/deliveries?product_key=<key>&device_name=<name>` | 列出一个设备的待处理投递。两个查询参数都必须提供。 |
| `GET` | `/api/v5/plugin_api/emqx_bcast/deliveries/:deliveryId` | 获取一条待处理投递。 |
| `DELETE` | `/api/v5/plugin_api/emqx_bcast/deliveries/:deliveryId` | 删除一条待处理投递。 |

删除操作无法撤回已经进入投递过程的消息。删除消息需要所有运行中的核心节点参与。如果某个核心节点不可用或版本不兼容，请求会返回 `500 InternalError`，并且不会删除任何记录。

## 监控 Bcast

插件通过以下端点公开 Prometheus 文本格式指标：

```http
GET /api/v5/plugin_api/emqx_bcast/metrics
```

这些指标属于节点本地指标。请抓取每个 EMQX 节点，并在集群范围内聚合指标值。QoS 1 计数器最终满足以下关系：

```text
wanted = acked + auto_acked + ttl_expired + canceled + queued + inflight
```

对应的指标名称使用 `bcast_batch_pub_qos1_` 前缀。可以通过 `queued` 和 `inflight` gauge 观察实时积压，通过 `redelivered` counter 观察重复投递尝试。

以下端点会重置所有运行节点上的指标：

```http
POST /api/v5/plugin_api/emqx_bcast/metrics/reset
```

仅在维护或测试窗口中使用该操作。只要任一节点仍有排队中或投递中的消息，该端点就会返回 `409 PendingDeliveries`。如果部分节点完成指标重置后操作失败，该端点将返回 `500 PartialReset`，并指出受影响的节点。

## 使用限制

- Bcast 不支持监听器挂载点或 `namespace_as_mountpoint`。这些功能会为存储的订阅过滤器添加前缀，导致其无法与 Bcast 未挂载的投递主题匹配。
- 滚动升级期间，连接到尚未运行该插件的节点的客户端无法接收待处理的 Bcast 投递。请先启动插件，并确认 `GET /api/v5/plugin_api/emqx_bcast/metrics` 返回 `200`，再将 MQTT 或 Bcast API 流量转发到该节点。

<!-- PLUGIN-DOWNLOADS:BEGIN (auto-generated, do not edit) -->

## 下载

各 EMQX 版本对应的插件安装包：

| EMQX 版本 | 插件版本 | 安装包 |
|---|---|---|
| 6.1.6 | 0.4.3 | [emqx_bcast-0.4.3.tar.gz](https://www.emqx.com/downloads/emqx-plugins/6.1.6/emqx_bcast-0.4.3.tar.gz) ([sha256](https://www.emqx.com/downloads/emqx-plugins/6.1.6/emqx_bcast-0.4.3.sha256)) |

<!-- PLUGIN-DOWNLOADS:END -->
