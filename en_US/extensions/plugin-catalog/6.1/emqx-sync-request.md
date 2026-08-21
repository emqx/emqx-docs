# Sync Request

The `emqx_sync_request` plugin lets an HTTP caller publish one MQTT request through the EMQX REST API and wait synchronously for the first matching MQTT response.

Use this plugin when an HTTP-based backend service needs to send a command or query to a connected MQTT client and receive the result in the same HTTP request. The plugin handles request delivery, response correlation, timeout handling, and concurrent inflight requests, so the HTTP caller does not need to run its own MQTT client or implement MQTT request/response tracking.

## How It Works

The plugin exposes a runtime API through the plugin API gateway:

```http
POST /api/v5/plugin_api/emqx_sync_request/request
```

When EMQX receives the HTTP request, the plugin finds the online MQTT subscriber for the request topic, delivers the MQTT request directly to that subscriber, and waits for a matching response message. For MQTT 5 responders, the response must match both the response topic and the Correlation Data generated from `request_id`. For MQTT 3 responders, which do not support Correlation Data, responses are matched by response topic in request sequence.

Request topics must match one online, non-shared subscriber exactly:

- Wildcard topic filters are not matched as request receivers.
- Shared subscriptions are not accepted as request receivers.
- If no exact subscriber is online, the API returns `404 NO_SUBSCRIBERS`.
- If the request topic has a shared subscription or more than one exact subscriber, the API returns `409 CONFLICT`.

## Delivery Semantics

The plugin stores inflight requests in local node memory only. It does not persist requests, subscribe to response topics, or modify MQTT payloads.

Request messages are injected by direct session delivery to the single exact subscriber. They do not pass through the normal MQTT publish pipeline. As a result, request messages are not processed by the rule engine, schema validation, message transformation, retained message handling, or delayed publish, and they do not use the generic `/publish` path.

The HTTP wait timeout is a single deadline shared by remote dispatch and the local wait for the MQTT response. Remote dispatch time counts against the same timeout instead of adding a separate full wait.

Matching responses are observed through the broker `message.publish` hook on the node that delivered the request. The responder should publish the response from a client connected to that same node, typically the same connection that received the request. A response published from another node is not matched.

## Configuration

| Field | Default | Description |
| --- | --- | --- |
| `default_timeout` | `10s` | Default HTTP wait timeout when the request body omits `timeout`. |
| `max_timeout` | `60s` | Maximum allowed per-request `timeout`. |
| `max_inflight_requests` | `10000` | Maximum number of local HTTP requests waiting for responses on one node. |
| `max_payload_size` | `64KB` | Maximum MQTT request payload size and maximum MQTT response payload size. |

Example configuration:

```hocon
default_timeout = "10s"
max_timeout = "60s"
max_inflight_requests = 10000
max_payload_size = "64KB"
```

Update plugin configuration through the standard plugin configuration API:

```http
PUT /api/v5/plugins/<name-vsn>/config
```

## HTTP API

Use the same authentication methods as other EMQX management APIs. Bearer tokens obtained from Dashboard login are accepted. API keys must be sent with HTTP Basic authentication and require the `publish` scope.

### Request Body

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

| Field | Type | Required | Default | Description |
| --- | --- | --- | --- | --- |
| `timeout` | duration string | No | `default_timeout` | Maximum time to wait for a matching MQTT response. It must be greater than `0` and no greater than `max_timeout`. Examples: `100ms`, `5s`, `1m`. |
| `request` | object | Yes | - | MQTT request parameters. |

The `request` object contains the following fields:

| Field | Type | Required | Default | Description |
| --- | --- | --- | --- | --- |
| `topic` | string | Yes | - | MQTT request topic. It must be a topic name, not a topic filter, so `+` and `#` are not allowed. Exactly one non-shared subscriber must be online for this topic. |
| `response_topic` | string | Yes | - | MQTT response topic. It must also be a topic name without `+` or `#`. |
| `request_id` | string | Yes | - | Plain string used as MQTT 5 Correlation Data and echoed in the HTTP response. The maximum length is 128 bytes. |
| `qos` | integer | No | `0` | MQTT QoS for the request. Allowed values are `0`, `1`, and `2`. |
| `payload_encoding` | string | No | `plain` | Request payload encoding. Allowed values are `plain` and `base64`. |
| `payload` | string | Yes | - | Request payload. With `plain`, the string bytes are used as the MQTT payload. With `base64`, the value must be valid base64 and the decoded bytes are used as the MQTT payload. The MQTT payload must not exceed `max_payload_size`. |
| `content_type` | string | No | - | MQTT 5 Content Type for the request. MQTT 3 clients do not receive this property. |

### Success Response

A successful request returns HTTP `200`. The MQTT response payload is always returned as base64.

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

| Field | Description |
| --- | --- |
| `code` | Always `OK`. |
| `message` | Always `OK`. |
| `response.topic` | MQTT response topic. |
| `response.request_id` | The `request_id` from the HTTP request. |
| `response.payload_encoding` | Always `base64`. |
| `response.payload` | Base64-encoded MQTT response payload. |
| `response.content_type` | Optional. MQTT 5 Content Type from the response PUBLISH. This field is omitted when the responder does not send it, including MQTT 3 responders. |

### Error Responses

Errors use the same `code` and `message` response shape as other EMQX management APIs.

| HTTP Status | Code | Meaning |
| --- | --- | --- |
| `400` | `BAD_REQUEST` | Invalid JSON body, invalid field value, request payload too large, or MQTT response payload too large. |
| `401` | `BAD_API_KEY_OR_SECRET` | API key authentication failed. Returned by EMQX management API authentication. |
| `403` | `UNAUTHORIZED_ROLE` | The API key does not have permission to call this API. Returned by EMQX management API authorization. |
| `404` | `NO_SUBSCRIBERS` | No exact, non-shared subscriber is online for the request topic. Wildcard subscribers are ignored. |
| `409` | `CONFLICT` | The request topic has a shared subscription or more than one exact subscriber. |
| `429` | `TOO_MANY_REQUESTS` | The local node already has `max_inflight_requests` HTTP requests waiting for responses. |
| `503` | `SERVICE_UNAVAILABLE` | Failed to dispatch the request to the subscriber node. |
| `504` | `TIMEOUT` | Timed out waiting for a matching MQTT response. |
| `500` | `INTERNAL_ERROR` | Unexpected server-side error. |

## Operational Diagnostics

The plugin provides a node-local diagnostic CLI command:

```bash
emqx ctl sync_request status
```

Example output:

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

These values are not cluster-wide aggregates. The command reads only the node where it runs. In a cluster, run it on each node that may receive the HTTP request or deliver the MQTT response.

Only requests that reach the plugin handler are counted. Management API authentication and authorization failures are handled by EMQX before the plugin runs.

| Metric | Type | Scope | Description |
| --- | --- | --- | --- |
| `sync_request.requests.total` | counter | node-local | HTTP sync request attempts handled by this node. |
| `sync_request.requests.succeeded` | counter | node-local | Requests that returned HTTP `200`. |
| `sync_request.requests.failed` | counter | node-local | Requests that returned a non-`200` HTTP status. |
| `sync_request.requests.bad_request` | counter | node-local | Requests rejected with `400 BAD_REQUEST`. |
| `sync_request.requests.no_subscribers` | counter | node-local | Requests rejected because no exact, non-shared subscriber was online. |
| `sync_request.requests.conflict` | counter | node-local | Requests rejected because the request topic matched multiple or shared subscribers. |
| `sync_request.requests.too_many_requests` | counter | node-local | Requests rejected because this node reached `max_inflight_requests`. |
| `sync_request.requests.dispatch_failed` | counter | node-local | Requests that could not be dispatched to the subscriber node. |
| `sync_request.requests.timeout` | counter | node-local | Requests that timed out waiting for a matching MQTT response. |
| `sync_request.requests.internal_error` | counter | node-local | Requests that failed with an unexpected internal error. |
| `sync_request.inflight_requests` | gauge | node-local | Current number of HTTP requests waiting for MQTT responses on this node. |
| `sync_request.pending_responses` | gauge | node-local | Current number of local pending response registrations created after request delivery. |

<!-- PLUGIN-DOWNLOADS:BEGIN (auto-generated, do not edit) -->

## Download

Tarballs for each EMQX release:

| EMQX Version | Plugin Version | Package |
|---|---|---|
| 6.1.4 | 0.1.0 | [emqx_sync_request-0.1.0.tar.gz](https://packages.emqx.io/emqx-plugins/6.1.4/emqx_sync_request-0.1.0.tar.gz) |

<!-- PLUGIN-DOWNLOADS:END -->
