# Client Tags and Group-Based Rate Limiting

Starting from EMQX version 4.4.33, EMQX introduces a client tagging feature. This feature allows you to assign tags to clients based on business logic and apply differentiated rate-limiting policies to clients under different tags.

This module relies heavily on [HTTP Authentication/ACL](./http_authentication.md), using the data returned from the authentication interface to assign tags to clients. This document details the working mechanism, configuration, and management of client tags.

## How It Works

The core principle of client tagging is “group first, then rate limit.” EMQX automatically assigns clients to different groups based on the `tag` returned by the HTTP authentication service, and then applies independent rate-limiting policies to each group.

The full workflow is as follows:

1. **Create Tags**: Tags are pre-created in the EMQX Dashboard, each with its own rate-limiting strategy. A built-in `default` tag exists and cannot be deleted. It is used for clients that do not match any tag.
2. **Client Connection**: A client initiates a connection request.
3. **HTTP Authentication**: EMQX sends client information to the external HTTP authentication service.
4. **Tag Assignment**: The authentication service includes tag information in its response based on custom business logic.
5. **Apply Policy**: EMQX parses the tag from the response, associates the client with the corresponding tag, and enforces the rate-limiting policy configured for that tag.

### Tag Assignment Logic

- If HTTP authentication is successful (status code `200`) and the `client_attrs.tag` in the response matches a pre-created tag, the client is assigned to that tag.
- If the `tag` field is missing or the value does not match any existing tag, the client is automatically assigned to the `default` tag.
- If HTTP authentication fails (non-`200` status code), the client connection is denied, and no tag is assigned.

::: tip Note

You must create tags in the Dashboard first; only then will tags returned by the HTTP service take effect. The rate-limiting policy for the `default` tag can also be customized to control default client behavior.

If the HTTP Authentication module is not enabled, all clients are automatically assigned to the `default` tag.

:::

## HTTP Authentication Interface Specification

To enable EMQX to recognize client tags, your HTTP authentication service must comply with the following interface specification.

When authentication is successful, the HTTP service should return a 200 status code and a response body containing a JSON object. The `client_attrs.tag` field in the object specifies the client’s tag.

**Example Response Body:**

```json
{
  "result": "allow",
  "client_attrs": {
    "tag": "group_a"
  }
}
```

**Field Descriptions:**

- `result`: `string` type. Currently reserved for EMQX 5.0 compatibility and does not affect authentication results. EMQX determines authentication success based solely on HTTP status code.
- `client_attrs`: `object` type. Stores additional client attributes.
  - `tag`: `string` type. Specifies the client’s tag name. EMQX uses this value to group the client.

## Rate Limiting Policies and Handling

EMQX provides fine-grained rate-limiting strategies per tag, allowing you to control resource usage per client group.

### Configurable Limits

| Limit Type                        | Unit         | Description                                                                          |
|-----------------------------------|--------------|--------------------------------------------------------------------------------------|
| Sent Message TPS Limit            | Messages/sec | Limits the number of messages a client can publish per second (TPS).                 |
| Subscribe Message TPS Limit       | Messages/sec | Limits the number of messages EMQX can deliver to a client per second.               |
| Sent Traffic Limit (bytes/s)      | Bytes/sec    | Limits the message traffic a client can publish per second.                          |
| Subscribe Traffic Limit (bytes/s) | Bytes/sec    | Limits the message traffic a client can deliver per second.                          |
| Maximum QoS Level                 | 0, 1, 2      | Limits the maximum QoS level that clients can use when publishing or subscribing to messages. |

::: tip
Apart from QoS level restrictions, if a restriction item has a value of empty or 0, it means that no restriction is imposed on that item.
:::

### Enforcement Mechanism

When a client exceeds the rate limits configured for its assigned tag, EMQX handles it differently based on the type of limit:

- **For publishing clients**:
  - **QoS Limit**: If a client tries to publish with a QoS level higher than allowed, the message is dropped.
  - **Rate/Byte Limit**: If the client exceeds the message or byte rate limit, EMQX applies backpressure, temporarily blocking data reads from the client’s socket to slow down publishing. This behaves similarly to [EMQX’s built-in rate limiting](../advanced/rate-limit.md) but does not disconnect the client.
- **For subscribing clients**:
  - **Rate/Byte Limit**: If the message delivery rate exceeds the configured limit, subsequent messages are dropped to prevent the client from being overwhelmed.

::: tip Note

For performance reasons, EMQX does not enqueue messages that exceed rate limits. They are dropped immediately.
See: [Inflight Window and Message Queue](../advanced/inflight-window-and-message-queue.md).

Due to batch processing of message delivery, if the delivery rate or byte limit is set too low, rate limiting may be triggered frequently, causing the actual delivery rate to fall below the configured limit.

:::

## Manage Client Tags via Dashboard

You can manage client tags and their rate-limiting policies conveniently through the EMQX Dashboard.

1. Open the EMQX Dashboard in your browser.
2. In the left menu, click **Modules**.
3. Click **Add Module** on the page, then select and add **Client Tag Management** from the list.
4. Once added, a **Client Tag Management** module will appear in the module list. Click **Manage** to enter the Client Tag Management page.

### Client Tags

Under the **Client Tags** tab, all created tags are displayed in a list, including the built-in `default` tag. You can:

- **View**: See tag names and the number of currently associated clients.
- **Create**: Click **Add** to add new tags and configure rate-limiting policies. For detailed configuration descriptions, see [Configurable Limits](#configurable-limits).
- **Manage**: Edit rate-limiting strategies or view the client list under a tag by clicking the **View Client List** button.
  - All clients under this tag will have their rate limiting configuration updated immediately during the next rate limiting check.
- **Delete**: Delete a tag that is no longer needed.
  - After a tag is deleted, all clients that were originally under that tag will update their own tags to the `default` and immediately apply the rate limiting configuration of the `default` tag.

### Client Search

Under the **Client Search** tab, you can view all online clients assigned to different tags. The list supports pagination, and you can click a client ID to view detailed information for troubleshooting and monitoring. You can quickly locate clients or tags by client ID or tag name.

## Monitoring and Logs

To assist with monitoring and debugging group-based rate limiting, EMQX provides relevant metrics and logging support:

- **Prometheus Metrics**: EMQX exposes metrics for messages dropped due to rate limiting, which can be integrated with monitoring and alerting systems.
- **Logs**: When rate limiting is triggered, EMQX logs the event. The logging system supports **sampling** to avoid performance degradation during traffic spikes.
