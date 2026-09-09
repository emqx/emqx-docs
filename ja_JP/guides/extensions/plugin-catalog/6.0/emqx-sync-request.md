# Sync Request

The `emqx_sync_request` plugin lets an HTTP service send an MQTT request through the EMQX REST API and receive the first matching MQTT response in the same HTTP request. The plugin is available in EMQX Enterprise 6.0.4 and later 6.0 releases.

Use this plugin when a backend service needs to send a command or query to a connected MQTT client. Unlike the standard publish API, this plugin waits for and correlates the client response, handles timeouts, and manages concurrent requests. The HTTP service does not need to run an MQTT client or track request and response pairs.

Before using the API, install and start `emqx_sync_request` as described in [Manage Plugins](../../plugin-management.md). The endpoint is available only while the plugin is running.

## How It Works

The request and response flow works as follows:

1. The HTTP caller sends an HTTP request to the API that includes the MQTT request topic, response topic, `request_id`, and payload.
2. The plugin finds the MQTT client subscribed to the request topic and delivers the request directly to that client.
3. The MQTT client processes the request and publishes a response to the response topic. For MQTT 5, the plugin includes `request_id` as Correlation Data in the request message.
4. If the MQTT 5 client returns this Correlation Data, the plugin matches the response by response topic and Correlation Data. If the response does not include Correlation Data, the plugin matches it to the oldest pending request for the response topic. This fallback applies to MQTT 3 responses and MQTT 5 responses that omit Correlation Data. When concurrent requests use the same response topic, MQTT 5 clients should return the Correlation Data to ensure that each response is matched to the intended request.
5. The plugin returns the first matching MQTT response to the HTTP caller. If no matching response arrives before the timeout, the API returns `504 TIMEOUT`.

Request topics must match one online, non-shared subscriber exactly:

- Wildcard topic filters are not matched as request receivers.
- Shared subscriptions are not accepted as request receivers.
- If no exact subscriber is online, the API returns `404 NO_SUBSCRIBERS`.
- If the request topic has a shared subscription or more than one exact subscriber, the API returns `409 CONFLICT`.

## Request Delivery and Response Handling

The plugin stores inflight requests only in the local node's memory. It does not persist requests, subscribe to response topics, or modify MQTT payloads.

EMQX delivers each request directly to the selected client instead of sending it through the normal MQTT publish pipeline. Therefore, the request is not processed by the rule engine, schema validation, message transformation, retained message handling, or delayed publishing, and it does not use the generic `/publish` API.

Forwarding a request to another node and waiting for the MQTT response share the same HTTP timeout. The forwarding time reduces the time available to wait for the response.

The response must be published by a client connected to the node that delivered the request, typically through the same connection that received it. A response published through another node is not matched.

## Plugin Configuration

These settings control the plugin-wide timeouts and resource limits on each node. Request-specific parameters are described in [Request Body](#request-body).

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

## Synchronous Request API

Call the following endpoint to send an MQTT request and wait for its response:

```http
POST /api/v5/plugin_api/emqx_sync_request/request
```

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

| Metric | Type | Description |
| --- | --- | --- |
| `sync_request.requests.total` | counter | HTTP sync request attempts handled. |
| `sync_request.requests.succeeded` | counter | Requests that returned HTTP `200`. |
| `sync_request.requests.failed` | counter | Requests that returned a non-`200` HTTP status. |
| `sync_request.requests.bad_request` | counter | Requests rejected with `400 BAD_REQUEST`. |
| `sync_request.requests.no_subscribers` | counter | Requests rejected because no exact, non-shared subscriber was online. |
| `sync_request.requests.conflict` | counter | Requests rejected because the request topic matched multiple or shared subscribers. |
| `sync_request.requests.too_many_requests` | counter | Requests rejected because `max_inflight_requests` was reached. |
| `sync_request.requests.dispatch_failed` | counter | Requests that could not be dispatched to the subscriber node. |
| `sync_request.requests.timeout` | counter | Requests that timed out waiting for a matching MQTT response. |
| `sync_request.requests.internal_error` | counter | Requests that failed with an unexpected internal error. |
| `sync_request.inflight_requests` | gauge | HTTP requests currently waiting for MQTT responses. |
| `sync_request.pending_responses` | gauge | Pending response registrations created after request delivery. |
