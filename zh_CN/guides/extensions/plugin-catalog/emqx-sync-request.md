# 同步请求

`emqx_sync_request` 插件允许 HTTP 服务通过 EMQX REST API 发送一条 MQTT 请求，并在同一个 HTTP 请求中获取第一条匹配的 MQTT 响应。该插件自 EMQX 企业版 5.10.5 起可用。

当后端服务需要向已连接的 MQTT 客户端发送命令或查询时，可以使用该插件。与普通消息发布 API 不同，该插件会等待并关联客户端响应、处理超时并管理并发请求。HTTP 服务无需运行 MQTT 客户端，也无需自行跟踪请求和响应的对应关系。

使用 API 前，请参照[插件管理](../plugin-management.md)安装并启动 `emqx_sync_request`。该端点仅在插件运行期间可用。

## 工作原理

请求和响应流程如下：

1. HTTP 调用方调用该 API，并在请求中提供 MQTT 请求主题、响应主题、`request_id` 和 Payload。
2. 插件找到订阅请求主题的 MQTT 客户端，并将请求直接投递给该客户端。
3. MQTT 客户端处理请求，并向响应主题发布响应。对于 MQTT 5，插件会将 `request_id` 作为请求消息的 Correlation Data。
4. 如果 MQTT 5 客户端返回该 Correlation Data，插件会根据响应主题和 Correlation Data 精确匹配响应。如果响应未包含 Correlation Data，插件会将其匹配到该响应主题最早进入等待状态的请求。此回退方式适用于 MQTT 3，以及未返回 Correlation Data 的 MQTT 5 客户端。当并发请求使用同一个响应主题时，MQTT 5 客户端应返回 Correlation Data，以确保每条响应与预期请求匹配。
5. 插件将第一条匹配的 MQTT 响应返回给 HTTP 调用方。如果超时前没有收到匹配的响应，API 返回 `504 TIMEOUT`。

请求主题必须精确匹配一个在线的非共享订阅者：

- 通配符主题过滤器不会被匹配为请求接收方。
- 共享订阅不会被接受为请求接收方。
- 如果没有在线的精确订阅者，API 返回 `404 NO_SUBSCRIBERS`。
- 如果请求主题存在共享订阅，或存在多个精确订阅者，API 返回 `409 CONFLICT`。

## 请求投递与响应处理

该插件仅在本地节点内存中保存正在处理的请求，不会持久化请求、订阅响应主题或修改 MQTT Payload。

EMQX 将请求直接投递给选定的客户端，而不经过普通 MQTT 发布流程。因此，请求不会经过规则引擎、Schema 验证、消息转换、保留消息处理或延迟发布，也不使用通用 `/publish` API。

将请求转发到其他节点和等待 MQTT 响应共用同一个 HTTP 超时时间。转发耗时会减少可用于等待响应的时间。

响应必须由连接到请求投递节点的客户端发布，通常使用接收请求的同一连接。通过其他节点发布的响应不会被匹配。

## 插件配置

以下配置控制插件在每个节点上的超时设置和资源限制。单次请求的参数请参见[请求体](#请求体)。

| 字段 | 默认值 | 描述 |
| --- | --- | --- |
| `default_timeout` | `10s` | 请求体未指定 `timeout` 时使用的默认 HTTP 等待超时时间。 |
| `max_timeout` | `60s` | 单个请求允许的最大 `timeout`。 |
| `max_inflight_requests` | `10000` | 单个节点上可同时等待响应的本地 HTTP 请求数上限。 |
| `max_payload_size` | `64KB` | MQTT 请求 Payload 和 MQTT 响应 Payload 的最大大小。 |

配置示例：

```hocon
default_timeout = "10s"
max_timeout = "60s"
max_inflight_requests = 10000
max_payload_size = "64KB"
```

通过标准插件配置 API 更新插件配置：

```http
PUT /api/v5/plugins/<name-vsn>/config
```

## 同步请求 API

调用以下端点发送 MQTT 请求并等待响应：

```http
POST /api/v5/plugin_api/emqx_sync_request/request
```

该 API 使用与其他 EMQX 管理 API 相同的认证方式。通过 Dashboard 登录获取的 Bearer Token 可以访问该 API。API 密钥必须通过 HTTP Basic 认证发送，并且需要具备 `publish` 权限范围。

### 请求体

```json
{
  "timeout": "5s",
  "request": {
    "topic": "devices/1001/request",
    "response_topic": "devices/1001/response",
    "request_id": "request-id-1",
    "qos": 0,
    "payload_encoding": "plain",
    "payload": "{\"cmd\":\"reboot\"}",
    "content_type": "application/json"
  }
}
```

| 字段 | 类型 | 是否必填 | 默认值 | 描述 |
| --- | --- | --- | --- | --- |
| `timeout` | duration string | 否 | `default_timeout` | 等待匹配 MQTT 响应的最长时间。该值必须大于 `0`，且不超过 `max_timeout`。示例：`100ms`、`5s`、`1m`。 |
| `request` | object | 是 | - | MQTT 请求参数。 |

`request` 对象包含以下字段：

| 字段 | 类型 | 是否必填 | 默认值 | 描述 |
| --- | --- | --- | --- | --- |
| `topic` | string | 是 | - | MQTT 请求主题。该字段必须是主题名，而不是主题过滤器，因此不允许包含 `+` 和 `#`。该主题必须有且仅有一个在线的非共享订阅者。 |
| `response_topic` | string | 是 | - | MQTT 响应主题。该字段也必须是不包含 `+` 或 `#` 的主题名。 |
| `request_id` | string | 是 | - | 普通字符串，用作 MQTT 5 Correlation Data，并会在 HTTP 响应中返回。最大长度为 128 字节。 |
| `qos` | integer | 否 | `0` | 请求消息的 MQTT QoS。允许值为 `0`、`1` 和 `2`。 |
| `payload_encoding` | string | 否 | `plain` | 请求 Payload 编码。允许值为 `plain` 和 `base64`。 |
| `payload` | string | 是 | - | 请求 Payload。使用 `plain` 时，该字符串的字节会作为 MQTT Payload。使用 `base64` 时，该值必须是有效的 base64，解码后的字节会作为 MQTT Payload。MQTT Payload 不得超过 `max_payload_size`。 |
| `content_type` | string | 否 | - | 请求消息的 MQTT 5 Content Type。MQTT 3 客户端不会收到该属性。 |

### 成功响应

请求成功时返回 HTTP `200`。MQTT 响应 Payload 始终以 base64 形式返回。

```json
{
  "code": "OK",
  "message": "OK",
  "response": {
    "topic": "devices/1001/response",
    "request_id": "request-id-1",
    "payload_encoding": "base64",
    "payload": "eyJyZXN1bHQiOiJvayJ9",
    "content_type": "application/json"
  }
}
```

| 字段 | 描述 |
| --- | --- |
| `code` | 固定为 `OK`。 |
| `message` | 固定为 `OK`。 |
| `response.topic` | MQTT 响应主题。 |
| `response.request_id` | HTTP 请求中的 `request_id`。 |
| `response.payload_encoding` | 固定为 `base64`。 |
| `response.payload` | base64 编码后的 MQTT 响应 Payload。 |
| `response.content_type` | 可选字段。响应 PUBLISH 中的 MQTT 5 Content Type。如果响应方未发送该属性，包括 MQTT 3 响应方，该字段会被省略。 |

### 错误响应

错误响应使用与其他 EMQX 管理 API 相同的 `code` 和 `message` 结构。

| HTTP 状态码 | Code | 含义 |
| --- | --- | --- |
| `400` | `BAD_REQUEST` | JSON 请求体无效、字段值无效、请求 Payload 过大，或 MQTT 响应 Payload 过大。 |
| `401` | `BAD_API_KEY_OR_SECRET` | API 密钥认证失败。由 EMQX 管理 API 认证逻辑返回。 |
| `403` | `UNAUTHORIZED_ROLE` | API 密钥无权调用该 API。由 EMQX 管理 API 授权逻辑返回。 |
| `404` | `NO_SUBSCRIBERS` | 请求主题没有在线的精确非共享订阅者。通配符订阅者会被忽略。 |
| `409` | `CONFLICT` | 请求主题存在共享订阅，或存在多个精确订阅者。 |
| `429` | `TOO_MANY_REQUESTS` | 本地节点已有 `max_inflight_requests` 个 HTTP 请求正在等待响应。 |
| `503` | `SERVICE_UNAVAILABLE` | 未能将请求调度到订阅者所在节点。 |
| `504` | `TIMEOUT` | 等待匹配 MQTT 响应超时。 |
| `500` | `INTERNAL_ERROR` | 非预期的服务端错误。 |

## 运维诊断

该插件提供节点本地诊断 CLI 命令：

```bash
emqx ctl sync_request status
```

输出示例：

```text
Counters since plugin start:
sync_request.requests.total: 42
sync_request.requests.succeeded: 39
sync_request.requests.failed: 3
sync_request.requests.bad_request: 1
sync_request.requests.no_subscribers: 1
sync_request.requests.conflict: 0
sync_request.requests.too_many_requests: 0
sync_request.requests.dispatch_failed: 0
sync_request.requests.timeout: 1
sync_request.requests.internal_error: 0

Current gauges:
sync_request.inflight_requests: 0
sync_request.pending_responses: 0
```

这些值不是集群范围聚合结果。该命令只读取其运行节点上的数据。在集群中，应在可能接收 HTTP 请求或投递 MQTT 响应的每个节点上运行该命令。

只有到达插件处理器的请求才会被计数。管理 API 的认证和授权失败会先由 EMQX 处理，不会进入插件。

| 指标 | 类型 | 描述 |
| --- | --- | --- |
| `sync_request.requests.total` | counter | 已处理的 HTTP 同步请求次数。 |
| `sync_request.requests.succeeded` | counter | 返回 HTTP `200` 的请求数。 |
| `sync_request.requests.failed` | counter | 返回非 `200` HTTP 状态码的请求数。 |
| `sync_request.requests.bad_request` | counter | 因 `400 BAD_REQUEST` 被拒绝的请求数。 |
| `sync_request.requests.no_subscribers` | counter | 因没有在线的精确非共享订阅者而被拒绝的请求数。 |
| `sync_request.requests.conflict` | counter | 因请求主题匹配多个订阅者或共享订阅者而被拒绝的请求数。 |
| `sync_request.requests.too_many_requests` | counter | 因达到 `max_inflight_requests` 而被拒绝的请求数。 |
| `sync_request.requests.dispatch_failed` | counter | 未能调度到订阅者所在节点的请求数。 |
| `sync_request.requests.timeout` | counter | 等待匹配 MQTT 响应超时的请求数。 |
| `sync_request.requests.internal_error` | counter | 因非预期内部错误失败的请求数。 |
| `sync_request.inflight_requests` | gauge | 当前正在等待 MQTT 响应的 HTTP 请求数。 |
| `sync_request.pending_responses` | gauge | 请求投递后创建的待响应注册数量。 |
