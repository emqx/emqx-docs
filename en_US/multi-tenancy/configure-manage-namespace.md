# Configure and Manage Namespaces

You can configure and manage namespaces using the Dashboard and REST API, including setting session limits, rate limits, and managing connected clients.

## Namespace Rate Limits

Namespace configuration mainly includes maximum session limits and rate limiters. Before configuring rate limiters, it is recommended to understand the different types of rate limiters available for namespaces and their scope of effect.

For details on how to configure specific options, see [Configure and Manage Namespaces via Dashboard](#configure-and-manage-namespaces-via-dashboard).

Namespace rate limiters can be used to control message traffic and bandwidth usage for clients within a specific namespace. They can work together with existing EMQX rate-limiting mechanisms (such as zone-level or listener-level rate limiters), depending on the type of rate limiter configured.

### Rate Limiter Types

There are two types of rate limiters available for managed namespaces:

#### Tenant Rate Limiter

The tenant rate limiter allocates shared tokens across all clients within the same namespace.

When this limiter is enabled:

- The limit applies to the entire namespace
- It works together with existing zone-level rate limiters
- Clients must satisfy both the zone-level and namespace-level limits

This type is suitable for scenarios where the overall traffic of a tenant needs to be controlled.

#### Client Rate Limiter

The client rate limiter allocates dedicated tokens to each client within a namespace.

When this limiter is enabled:

- The limit applies to individual clients
- It overrides listener-level rate limiters
- Listener-level rate limits are ignored, and only the namespace client rate limiter is applied

This type is suitable for scenarios that require fine-grained control over individual client behavior.

### Supported Limiting Dimensions

Both tenant and client rate limiters support the following dimensions:

- **Message rate limit**: The maximum number of messages that a client or tenant can publish within a specified period
- **Byte throughput limit**: The maximum effective payload size that can be transmitted within a specified period

:::

For more details about the rate-limiting mechanism, see [Rate Limiting](../rate-limit/rate-limit.md).

:::

## Configure and Manage Namespaces via Dashboard

In the Dashboard’s left-side menu, go to **Management** -> **Namespace**. On the **Namespace** page, you can manage namespaces and clients connected to each namespace.

By default, the namespace list only shows explicitly created namespaces. You can toggle the switch at the top left of the page to show both explicitly created namespaces and those automatically created by EMQX from the `client_attrs.tns` attribute.

::: tip Note

Automatically created namespaces cannot be edited in the Dashboard.

:::

### Configure a Namespace via Dashboard

You can configure a namespace when creating it, or edit it later. To edit an existing namespace, click **Edit** in the **Actions** column for that namespace.

1. In the **Create Namespace** dialog, complete the following configuration:

   - **Max Sessions**: By default, this is set to `infinity` (unlimited). If enabled, you can set a specific number to limit the maximum number of sessions, preventing too many clients from occupying resources in a single namespace. When setting the max sessions, ensure it aligns with your cluster capacity to avoid rejected connections due to a low limit.

   - **Tenant Limiter**: This configuration controls the traffic for all clients within the namespace. For instance, when multiple clients share the same infrastructure, tenant rate limits ensure fair bandwidth distribution. By default, this is disabled. If enabled, you can configure the following rate limits:

     ::: tip

     For more details on this configuration, refer to the tooltips in the Dashboard.

     :::

     - **Packet Publish Rate**: Limits the bytes the current tenant can send to EMQX per second.
     - **Packet Publish Burst**: Allows additional bytes to be sent during bursts.
     - **Messages Publish Rate**: Limits the maximum number of messages a tenant can send per second.
     - **Messages Publish Burst**: Allows additional messages to be sent during bursts.

   - **Client Limiter**: This configuration controls traffic for individual clients. Client rate limiters are exclusive to each client, meaning the rate limit for one client won’t affect others. By default, this is disabled. If enabled, you can configure the following rate limits:

     ::: tip

     For more details on this configuration, refer to the tooltips in the Dashboard.

     :::

     - **Packet Publish Rate**: Limits the bytes a client can send to EMQX per second.
     - **Packet Publish Burst**: Allows additional bytes to be sent during bursts.
     - **Messages Publish Rate**: Limits the maximum number of messages a client can send per second.
     - **Messages Publish Burst**: Allows additional messages to be sent during bursts.

2. After completing the configuration, click **Create**. The new namespace will appear in the list.

### Manage Namespace Clients

To view clients connected to a specific namespace, click **Clients** in the **Actions** column. You can also choose to bulk disconnect clients.

## Configure and Manage Namespaces via REST API

::: tip

To view request and response schemas that match the current EMQX instance version, open `/api-spec.html` on the Dashboard listener, for example, `http://localhost:18083/api-spec.html`.

:::

### List Namespaces via REST API

EMQX provides two endpoints for listing namespaces with details, depending on which namespaces you need:

| Endpoint | Scope | Config included |
| -------- | ----- | --------------- |
| `GET /mt/ns_list_details` | All namespaces (auto-created and explicitly created) | No |
| `GET /mt/managed_ns_list_details` | Explicitly created (managed) namespaces only | Yes |

Both endpoints support the same query parameters:

| Parameter | Type | Default | Description |
| --------- | ---- | ------- | ----------- |
| `last_ns` | String | `""` | Pagination cursor. Pass the `name` of the last item from the previous page to retrieve the next page. |
| `limit` | Integer | `100` | Maximum number of namespaces to return per page. |

#### List All Namespaces

`GET /mt/ns_list_details` returns all namespaces, including those auto-created from client connection metadata. Each item contains `name` and `created_at` only, with no configuration fields.

**Response Example**

```json
[
  { "name": "ns1", "created_at": 1747917753 },
  { "name": "ns2", "created_at": 1747917754 }
]
```

#### List Managed Namespaces with Configuration

`GET /mt/managed_ns_list_details` returns only explicitly created namespaces and includes each namespace's current configuration inline. A management UI can use this endpoint to render a full list with configuration data in a single request.

**Response Example**

```json
[
  {
    "name": "ns1",
    "created_at": 1747917753,
    "config": {
      "session": {
        "max_sessions": 100
      },
      "limiter": {
        "tenant": {
          "bytes": { "rate": "20MB/10s", "burst": "300MB/1m" },
          "messages": { "rate": "5000/1s", "burst": "60/1m" }
        },
        "client": {
          "bytes": { "rate": "10MB/10s", "burst": "200MB/1m" },
          "messages": { "rate": "3000/1s", "burst": "40/1m" }
        }
      }
    }
  },
  {
    "name": "ns2",
    "created_at": 1747917754,
    "config": {}
  }
]
```

Each item contains:
- `name`: The namespace identifier.
- `created_at`: Unix timestamp (seconds) of when the namespace was created.
- `config`: The namespace configuration. An empty object (`{}`) indicates no configuration has been applied. For a full description of config fields, see [Configure a Namespace via REST API](#configure-a-namespace-via-rest-api).

To retrieve the full configuration of a specific namespace, use `GET /mt/ns/<namespace>/config`.

### Configure a Namespace via REST API

After the namespace is created, it can be configured using the `PUT /mt/ns/<namespace>/config` API.

Use this endpoint to set rate limits, session limits, and other namespace-specific settings.

#### Configuration Example


This example configures a namespace using the REST API. Suppose you want to configure some specific rate limits for clients in the `ns1` namespace. You also want to limit the maximum number of concurrent sessions allowed in this namespace.

##### Create the Namespace

Before applying any configuration, ensure the namespace is explicitly created:

```bash
# No request body is needed
POST /mt/ns/ns1
```

::: tip Important Notice

If clients connect to a namespace before it is explicitly created, they will not inherit configurations such as rate limiters applied later. To enforce new settings, those clients must be manually disconnected and reconnected.

:::

##### Configure Rate Limits and Session Limits

Once the namespace is created, apply the configuration using:

```
PUT /mt/ns/ns1/config
```

**Request body:**

```json
{
  "limiter": {
    "client": {
      "bytes": {
        "rate": "10MB/10s",
        "burst": "200MB/1m"
      },
      "messages": {
        "rate": "3000/1s",
        "burst": "40/30s"
      }
    },
    "tenant": {
      "bytes": {
        "rate": "20MB/10s",
        "burst": "300MB/1m"
      },
      "messages": {
        "rate": "5000/1s",
        "burst": "60/30s"
      }
    }
  },
  "session": {
    "max_sessions": 100
  }
}
```

This configuration applies both client-specific and shared tenant-wide rate limits and sets a maximum of 100 sessions for the namespace.

##### Disable Namespace Rate Limiters

If you want to remove rate limiting entirely, you can update the configuration again and set the limiter types to `"disabled"`:

```
PUT /mt/ns/ns1/config
```

**Request body:**

```json
{
  "limiter": {
    "client": "disabled",
    "tenant": "disabled"
  }
}
```

## Delete and Clean Up a Namespace

Deleting a managed namespace permanently removes the namespace and its associated configuration. Starting from EMQX 6.1.4, EMQX also asynchronously removes namespace-scoped data from the built-in database. This data includes password-based authentication users, SCRAM users, and authorization rules. EMQX removes authentication users from every user group in the deleted namespace without affecting the global namespace or other namespaces. After the cleanup completes, recreating a namespace with the same name does not restore the deleted users or authorization rules.

::: tip Note

Deleting a managed namespace automatically starts disconnecting all clients currently connected through it. To avoid unexpected client interruption, disconnect active clients before deleting the namespace.

:::
### Delete via Dashboard

To delete a namespace, click **Delete** in the **Actions** column. After confirming, the namespace will be permanently deleted.

### Delete via REST API

To remove a namespace and its associated configuration, use the `DELETE /mt/ns/<namespace>` API.

### Recover from an Interrupted Deletion

Starting from EMQX 6.1.4, use `emqx ctl mt purge_ns <namespace>` as a last resort when a previous namespace deletion was interrupted and left data behind. The command attempts to clean up the namespace data even if the namespace no longer exists. If the namespace still exists, the command also deletes it.

::: warning Important Notice

Running this command for an existing namespace permanently deletes the namespace and its data. Use the Dashboard or REST API for routine namespace deletion. Use `purge_ns` only to recover from an incomplete deletion, and do not rerun it after a namespace with the same name has been recreated.

:::

For command syntax, output, and error handling, see [`mt purge_ns`](../admin/cli.md#mt).
