# 同步请求

`emqx_sync_request` 插件允许 HTTP 调用方通过 EMQX REST API 发布一条 MQTT 请求，并同步等待第一条匹配的 MQTT 响应。

当基于 HTTP 的后端服务需要向已连接的 MQTT 客户端发送命令或查询，并在同一个 HTTP 请求中获取结果时，可以使用该插件。插件负责请求投递、响应关联、超时处理和并发请求管理，因此 HTTP 调用方无需运行自己的 MQTT 客户端，也无需自行实现 MQTT 请求/响应跟踪逻辑。

## 工作原理

该插件通过插件 API 网关暴露运行时 API：

```http
POST /api/v5/plugin_api/emqx_sync_request/request
```

EMQX 收到 HTTP 请求后，插件会查找请求主题对应的在线 MQTT 订阅者，将 MQTT 请求直接投递给该订阅者，并等待匹配的响应消息。对于 MQTT 5 响应方，响应必须同时匹配响应主题和使用 `request_id` 作为值的 Correlation Data。对于不支持 Correlation Data 的 MQTT 3 响应方，响应会按响应主题的请求顺序进行匹配。

请求主题必须精确匹配一个在线的非共享订阅者：

- 通配符主题过滤器不会被匹配为请求接收方。
- 共享订阅不会被接受为请求接收方。
- 如果没有在线的精确订阅者，API 返回 `404 NO_SUBSCRIBERS`。
- 如果请求主题存在共享订阅，或存在多个精确订阅者，API 返回 `409 CONFLICT`。

## 投递语义

该插件仅在本地节点内存中保存正在处理的请求。它不会持久化请求，不会订阅响应主题，也不会修改 MQTT Payload。

请求消息通过直接会话投递发送给单个精确订阅者，不经过普通 MQTT 发布流水线。因此，请求消息不会被规则引擎、Schema 验证、消息转换、保留消息或延迟发布处理，也不会使用通用 `/publish` 路径。

HTTP 等待超时时间是远程调度和本地等待 MQTT 响应共享的同一个截止时间。远程调度耗时会计入同一个超时时间，而不会再叠加一次完整等待。

匹配响应通过投递请求的节点上的 broker `message.publish` 钩子观察。响应方应从连接到该节点的客户端发布响应，通常就是接收请求的同一个连接。从其他节点发布的响应不会被匹配。

## 配置

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

## HTTP API

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

| 指标 | 类型 | 范围 | 描述 |
| --- | --- | --- | --- |
| `sync_request.requests.total` | counter | 节点本地 | 本节点处理的 HTTP 同步请求次数。 |
| `sync_request.requests.succeeded` | counter | 节点本地 | 返回 HTTP `200` 的请求数。 |
| `sync_request.requests.failed` | counter | 节点本地 | 返回非 `200` HTTP 状态码的请求数。 |
| `sync_request.requests.bad_request` | counter | 节点本地 | 因 `400 BAD_REQUEST` 被拒绝的请求数。 |
| `sync_request.requests.no_subscribers` | counter | 节点本地 | 因没有在线的精确非共享订阅者而被拒绝的请求数。 |
| `sync_request.requests.conflict` | counter | 节点本地 | 因请求主题匹配多个订阅者或共享订阅者而被拒绝的请求数。 |
| `sync_request.requests.too_many_requests` | counter | 节点本地 | 因本节点达到 `max_inflight_requests` 而被拒绝的请求数。 |
| `sync_request.requests.dispatch_failed` | counter | 节点本地 | 未能调度到订阅者所在节点的请求数。 |
| `sync_request.requests.timeout` | counter | 节点本地 | 等待匹配 MQTT 响应超时的请求数。 |
| `sync_request.requests.internal_error` | counter | 节点本地 | 因非预期内部错误失败的请求数。 |
| `sync_request.inflight_requests` | gauge | 节点本地 | 本节点当前正在等待 MQTT 响应的 HTTP 请求数。 |
| `sync_request.pending_responses` | gauge | 节点本地 | 请求投递后创建的本地待响应注册数量。 |

<!-- PLUGIN-DOWNLOADS:BEGIN (auto-generated, do not edit) -->

## 下载

各 EMQX 版本对应的插件安装包：

| EMQX 版本 | 插件版本 | 安装包 |
|---|---|---|
| 6.1.4 | 0.1.0 | [emqx_sync_request-0.1.0.tar.gz](https://packages.emqx.io/emqx-plugins/6.1.4/emqx_sync_request-0.1.0.tar.gz) |

<!-- PLUGIN-DOWNLOADS:END -->
