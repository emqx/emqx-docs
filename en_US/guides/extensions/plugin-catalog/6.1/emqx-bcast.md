# Device Message Broadcasting (Bcast)

The `emqx_bcast` plugin enables backend applications to deliver commands, notifications, and other messages to IoT devices through HTTP. It supports product-wide broadcasts, targeted batch delivery, and QoS 1 offline replay, so applications do not need to maintain MQTT connections or build their own temporary offline-delivery queues and MQTT PUBACK tracking systems. Applications can also register a payload once and reuse its message ID in later batch requests.

This plugin is available in EMQX Enterprise 6.1.6 and later releases in the 6.1 series.

Typical use cases include broadcasting service notices or configuration refresh signals to devices in a product, sending commands or configuration updates to selected devices, and delivering QoS 1 commands when temporarily offline devices reconnect.

## Quick Start

This example broadcasts `Hello` to an online device in product `P1`. It assumes that EMQX Enterprise 6.1.6 or a later 6.1 release is running and that [MQTTX CLI](https://mqttx.app/cli) is available for the device connection.

### Install and Start the Plugin

This Quick Start uses the cluster-managed plugin mode. Download the package listed in [Download](#download), copy it to the EMQX plugins directory, and replace `<version>` with the downloaded plugin version:

```bash
cp emqx_bcast-<version>.tar.gz "$EMQX_HOME/plugins/"
emqx ctl plugins install emqx_bcast-<version> --cluster
emqx ctl plugins start emqx_bcast-<version>
```

For a single-node deployment, omit `--cluster`. You can also install and start the plugin from Dashboard. For details about both methods, including the authorization required for Dashboard installation, see [Install and Manage Plugins](../../plugin-management.md#install-and-manage-plugins).

### Configure Device Identity Mapping

The plugin maps each MQTT client to the following identifiers:

- `ProductKey`: The client's `client_attrs.tns` namespace attribute.
- `DeviceName`: The MQTT client ID.

In Dashboard, go to **Management** -> **MQTT Settings** -> **Client Attributes**, add the attribute `tns`, and set its expression to `nth(1, tokens(username, '-'))`. The equivalent configuration derives the namespace from the first segment of a username in the `ProductKey-DeviceName` format:

```hocon
mqtt.client_attrs_init = [
  { expression = "nth(1, tokens(username, '-'))", set_as_attr = tns }
]
```

With this configuration, a client that connects with username `P1-device001` and client ID `device001` has `ProductKey` `P1` and `DeviceName` `device001`.

For more information about setting client attributes, see [Set Client Attributes](../../../../develop/client-attributes/client-attributes.md#set-client-attributes).

Reconnect the client after applying this setting so that EMQX initializes the attribute when the client connects.

### Create an API Key

In Dashboard, go to **System** -> **API Keys** and create an API key with the **Administrator** role. Leave **Namespace** disabled to create a global key. Select **Custom Restricted Permissions**, and then select the `plugin_api` scope. Save the API key and secret immediately. For detailed steps, see [Create API Keys](../../../api.md#create-api-keys).

The `plugin_api` scope authorizes Bcast runtime endpoints under `/api/v5/plugin_api/emqx_bcast/*`. It does not authorize plugin management endpoints under `/api/v5/plugins*`, which require the `system` scope.

### Connect and Subscribe a Device

Run the following command to connect `device001` with username `P1-device001` and subscribe to the default broadcast topic for product `P1`:

```bash
mqttx sub -h 127.0.0.1 -p 1883 \
  -i device001 \
  -u P1-device001 \
  -t '/sys/broadcast/P1' \
  -q 1 -v
```

If your MQTT listener requires authentication, add the corresponding password and TLS options. To receive `BatchPub` messages on the default topic, subscribe the device to `/P1/device001/user/get` instead.

The target client must subscribe to the delivery topic. The plugin does not deliver a message merely because the client is connected.

### Send and Verify a Broadcast

Keep the MQTTX client running. In another terminal, replace `<api_key>` and `<api_secret>` with the credentials created in the previous step, and run:

```bash
curl -u '<api_key>:<api_secret>' \
  -X POST 'http://127.0.0.1:18083/api/v5/plugin_api/emqx_bcast/pub' \
  -H 'Content-Type: application/json' \
  -d '{"Action":"PubBroadcast","ProductKey":"P1","MessageContent":"SGVsbG8="}'
```

`MessageContent` must be base64-encoded. In this example, `SGVsbG8=` decodes to `Hello`.

EMQX returns HTTP `200` with a response similar to the following:

```json
{
  "Success": true,
  "RequestId": "550e8400-e29b-41d4-a716-446655440000",
  "MessageId": "550e8400-e29b-41d4-a716-446655440000"
}
```

The MQTTX client receives the payload `Hello` on topic `/sys/broadcast/P1`.

HTTP `200` means that EMQX accepted the request. It does not confirm delivery. For the delivery guarantees of each QoS level, see [Delivery Semantics](#delivery-semantics).

::: warning Important Notice

Bcast sends messages directly to client channels. These messages bypass publish authorization checks and the normal MQTT publish pipeline, including the rule engine, schema validation, message transformation, retained messages, and delayed publishing.

Grant access to the Bcast APIs only to trusted callers, and validate payloads and target identifiers in the calling application.

:::

## Message Actions

All message actions use the following endpoint:

```http
POST /api/v5/plugin_api/emqx_bcast/pub
```

Set the `Action` field in the JSON request body to select an operation. `MessageContent` must be base64-encoded.

### Broadcast to Online Devices

`PubBroadcast` sends a message to all online subscribers to the delivery topic within one product. It does not store the message, so offline devices do not receive it.

```json
{
  "Action": "PubBroadcast",
  "ProductKey": "P1",
  "MessageContent": "SGVsbG8="
}
```

The plugin expands the configured `broadcast_topic` template, whose default value is `/sys/broadcast/${productKey}`. You can set `TopicFullName` to a concrete custom topic. Wildcards and template placeholders are not allowed in `TopicFullName`.

The default maximum decoded payload size is 64 KiB.

### Send to a Device List

`BatchPub` sends a message to an explicit list of devices. A request can target up to `max_device_count` unique device names. The default limit is `10,000`.

```json
{
  "Action": "BatchPub",
  "ProductKey": "P1",
  "DeviceName": ["device001", "device002"],
  "MessageContent": "SGVsbG8=",
  "Qos": 1
}
```

Specify exactly one of the following payload fields:

- `MessageContent`: An inline base64-encoded payload. The default maximum decoded payload size is 10 KiB.
- `MessageId`: The ID of a payload created by `RegisterMessage` or a previous QoS 1 batch request.

For `BatchPub`, the delivery topic is selected in the following order:

1. `TopicTemplateName`, with `${productKey}` and `${deviceName}` expanded.
2. `TopicShortName`, expanded as `/${productKey}/${deviceName}/user/${TopicShortName}`.
3. The configured `batch_topic`, which defaults to `/${productKey}/${deviceName}/user/get`.

### Register and Reuse a Message

`RegisterMessage` stores a payload for reuse by later `BatchPub` requests:

```json
{
  "Action": "RegisterMessage",
  "MessageContent": "SGVsbG8="
}
```

The response contains a `MessageId`. Registering identical content returns the same ID and refreshes its TTL. The stored payload is immutable.

To refresh the TTL of an existing message, submit its ID instead of its content:

```json
{
  "Action": "RegisterMessage",
  "MessageId": "550e8400-e29b-41d4-a716-446655440000"
}
```

`MessageContent` and `MessageId` are mutually exclusive.

## Delivery Semantics

The `Qos` field of `BatchPub` accepts `0` or `1` and defaults to `0`.

| Requested QoS | Online Devices | Offline Devices | Storage and Confirmation |
| --- | --- | --- | --- |
| `0` | Delivered when the target is online and subscribed. | Skipped. | No storage or acknowledgement tracking. |
| `1` | Delivered and tracked until PUBACK when the subscription QoS is 1. | Stored and replayed after the device reconnects and subscribes. | Pending deliveries expire after `msg_ttl`, which defaults to 15 days. |

The effective delivery QoS is the lower of the requested QoS and the target subscription QoS. If a QoS 1 request reaches a QoS 0 subscription, the plugin considers the delivery complete after handing the message to the client channel.

A PUBACK confirms receipt at the MQTT protocol level. It does not confirm that the device application has processed the message or executed a command.

QoS 1 batch delivery is asynchronous and at least once. Clients must tolerate duplicate messages, especially around reconnects and session takeovers. A successful HTTP `200` response means that EMQX accepted the request for asynchronous processing. It does not mean that the message has been stored or delivered. If the accepting node fails before storage completes, the request can be lost.

Pending QoS 1 state is replicated in memory across core nodes. It can survive the failure of one core node, but not a full cluster restart. The producer must be able to replay messages after a complete cluster restart.

QoS 0 messages can also be duplicated during a session takeover window.

## Configure the Plugin

The following table lists the active settings and their defaults:

| Setting | Default | Description |
| --- | --- | --- |
| `broadcast_topic` | `/sys/broadcast/${productKey}` | Topic template for `PubBroadcast`. |
| `batch_topic` | `/${productKey}/${deviceName}/user/get` | Default topic template for `BatchPub`. |
| `msg_ttl` | `15d` | TTL for registered messages and pending QoS 1 deliveries. Changes apply only to records created after the update. |
| `cleanup_interval` | `60s` | Interval for removing expired records. |
| `max_device_count` | `10000` | Maximum number of devices in one `BatchPub` request. |
| `max_message_size_broadcast` | `65536` | Maximum decoded broadcast payload size in bytes. |
| `max_message_size_batch` | `10240` | Maximum decoded batch payload size in bytes. |
| `max_pending_deliveries` | `10000000` | Cluster-wide limit for pending QoS 1 deliveries. |
| `max_pending_deliveries_per_device` | `100` | Per-device limit for pending QoS 1 deliveries. Values are clamped to the range from 10 through 200. Enforcement is best effort during startup or failover. |
| `delivery_pool_size` | `0` | Worker count for each delivery pool. `0` automatically sets one worker per Erlang scheduler. Changing this setting restarts the pools. |

Example configuration:

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

Use the standard plugin configuration API to update these settings:

```http
PUT /api/v5/plugins/<name-vsn>/config
```

For installation, startup, and configuration procedures, see [Install and Manage Plugins](../../plugin-management.md#install-and-manage-plugins). REST API operations under `/api/v5/plugins*` require the `system` scope. For a node-pinned plugin, configuration updates remain supported, but REST APIs cannot manage its lifecycle.

## Call the API

Use HTTP Basic authentication with an EMQX API key and secret:

```bash
curl -u '<api_key>:<api_secret>' \
  -X POST 'http://127.0.0.1:18083/api/v5/plugin_api/emqx_bcast/pub' \
  -H 'Content-Type: application/json' \
  -d '{"Action":"PubBroadcast","ProductKey":"P1","MessageContent":"SGVsbG8="}'
```

The Dashboard user or API key must have the `plugin_api` or `system` scope, and its role and namespace permissions must allow the requested HTTP method. The plugin API gateway accepts `system`, but prefer the restricted `plugin_api` scope for Bcast runtime calls. The `plugin_api` scope does not authorize plugin lifecycle endpoints under `/api/v5/plugins*`.

The `plugin_api` scope grants access to every endpoint published through the plugin API gateway. For Bcast, this includes message and delivery deletion and metrics reset endpoints, so assign the scope only after reviewing all exposed operations.

A successful request returns HTTP `200`:

```json
{
  "Success": true,
  "RequestId": "550e8400-e29b-41d4-a716-446655440000",
  "MessageId": "550e8400-e29b-41d4-a716-446655440000"
}
```

An error response uses the following shape:

```json
{
  "Success": false,
  "RequestId": "550e8400-e29b-41d4-a716-446655440000",
  "Code": "InvalidBase64",
  "ErrorMessage": "Invalid Base64 encoding"
}
```

Common errors include:

| HTTP Status | Code | Cause |
| --- | --- | --- |
| `400` | `InvalidProductKey`, `InvalidDeviceName`, `DuplicateDeviceName` | A product or device identifier is missing or invalid, or the device list contains duplicates. |
| `400` | `DeviceCountExceeded`, `MessageTooLarge`, `InvalidBase64` | A request exceeds a configured limit or contains invalid base64. |
| `400` | `InvalidTopicTemplate`, `InvalidQos` | A topic field is invalid or `Qos` is not `0` or `1`. |
| `400` | `MessageNotFound`, `MessageIdContentConflict`, `InvalidMessageId` | A referenced message is absent, the payload fields conflict, or the ID is invalid. |
| `429` | `QuotaExceeded` | A global or per-device pending-delivery limit was reached. |
| `429` | `Busy` | The local QoS 1 intake queue is full. Retry later. |

## Manage Messages and Deliveries

The management endpoints expose metadata but never return message payload content.

| Method | Endpoint | Description |
| --- | --- | --- |
| `GET` | `/api/v5/plugin_api/emqx_bcast/messages?limit=100&cursor=<cursor>` | List registered messages. `limit` must be between 1 and 1000. |
| `GET` | `/api/v5/plugin_api/emqx_bcast/messages/:messageId` | Get message metadata and its outstanding delivery count. |
| `DELETE` | `/api/v5/plugin_api/emqx_bcast/messages/:messageId` | Delete a message and its associated pending deliveries. |
| `GET` | `/api/v5/plugin_api/emqx_bcast/deliveries?product_key=<key>&device_name=<name>` | List pending deliveries for one device. Both query parameters are required. |
| `GET` | `/api/v5/plugin_api/emqx_bcast/deliveries/:deliveryId` | Get one pending delivery. |
| `DELETE` | `/api/v5/plugin_api/emqx_bcast/deliveries/:deliveryId` | Delete one pending delivery. |

Deletion cannot recall a delivery that is already in flight. Deleting a message requires all running core nodes to participate. If a core is unavailable or incompatible, the request returns `500 InternalError` and does not delete any record.

## Monitor Bcast

The plugin exposes Prometheus text metrics at:

```http
GET /api/v5/plugin_api/emqx_bcast/metrics
```

Metrics are node-local. Scrape every EMQX node and aggregate values across the cluster. The QoS 1 counters eventually satisfy the following relationship:

```text
wanted = acked + auto_acked + ttl_expired + canceled + queued + inflight
```

The corresponding metric names use the `bcast_batch_pub_qos1_` prefix. Watch the `queued` and `inflight` gauges for live backlog, and the `redelivered` counter for duplicate delivery attempts.

The following endpoint resets metrics on every running node:

```http
POST /api/v5/plugin_api/emqx_bcast/metrics/reset
```

Use it only during a maintenance or test window. It returns `409 PendingDeliveries` while any node has queued or in-flight deliveries. If the reset fails after some nodes have reset their metrics, the endpoint returns `500 PartialReset` and identifies the affected nodes.

## Limitations

- Bcast does not support listener mountpoints or `namespace_as_mountpoint`. These features prefix stored subscription filters, which prevents Bcast's unmounted delivery topics from matching them.
- During rolling upgrades, clients connected to a node where the plugin is not running cannot receive pending Bcast deliveries. Start the plugin and verify that `GET /api/v5/plugin_api/emqx_bcast/metrics` returns `200` before directing MQTT or Bcast API traffic to that node.

<!-- PLUGIN-DOWNLOADS:BEGIN (auto-generated, do not edit) -->

## Download

Tarballs for each EMQX release:

| EMQX Version | Plugin Version | Package |
|---|---|---|
| 6.1.6 | 0.4.3 | [emqx_bcast-0.4.3.tar.gz](https://www.emqx.com/downloads/emqx-plugins/6.1.6/emqx_bcast-0.4.3.tar.gz) ([sha256](https://www.emqx.com/downloads/emqx-plugins/6.1.6/emqx_bcast-0.4.3.sha256)) |

<!-- PLUGIN-DOWNLOADS:END -->
