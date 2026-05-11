# Configure and Manage Namespaces

You can configure and manage namespaces using either the Dashboard or the REST API.

## Rate Limiters for a Namespace

Namespace configuration mainly includes setting a maximum session count and rate limiters. Before configuring rate limiters, it's important to understand the types and purposes of rate limiters in a namespace. For specific configuration options, refer to [Configure and Manage Namespaces via Dashboard](#configure-and-manage-namespaces-via-dashboard).

You can configure rate limiters for each namespace to control traffic and message flow for specific client groups. These namespace-level limiters work alongside EMQX’s existing rate limiters (for zones and listeners), depending on the type used.

### Types of Rate Limiters

In a managed namespace, there are two types of rate limiters:

**Tenant rate limiters**: Assign tokens that are **shared** across all clients within a namespace (NS). When this type of limiter is configured, it composes with any existing zone-level rate limiters, meaning both the zone and the namespace tenant rate limiters apply to clients simultaneously.

**Client rate limiters**: Assign tokens that are **dedicated** to each client within the NS. When this type of limiter is configured, it replaces any existing listener-level rate limiters, meaning the listener rate limiters are ignored while the namespace client limiter takes effect.

Both limiter types can define limits for:

- **Message rate limits**: The maximum number of messages a client or tenant can publish over a given time period.
- **Byte throughput limits**: The maximum allowed size for message payloads over time.

::: tip

For more details, refer to the [Rate Limit](../rate-limit.md) documentation.

:::

## Configure and Manage Namespaces via Dashboard

In the Dashboard’s left-side menu, go to **Management** -> **Namespace**. On the **Namespace** page, you can view, edit, or delete namespaces and manage clients connected to each namespace.

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

     - **Data Publish Rate**: Limits the bytes the current tenant can send to EMQX per second.
     - **Data Publish Burst**: Allows additional bytes to be sent during bursts.
     - **Messages Publish Rate**: Limits the maximum number of messages a tenant can send per second.
     - **Messages Publish Burst**: Allows additional messages to be sent during bursts.

   - **Client Limiter**: This configuration controls traffic for individual clients. Client rate limiters are exclusive to each client, meaning the rate limit for one client won’t affect others. By default, this is disabled. If enabled, you can configure the following rate limits:

     ::: tip

     For more details on this configuration, refer to the tooltips in the Dashboard.

     :::

     - **Data Publish Rate**: Limits the bytes a client can send to EMQX per second.
     - **Data Publish Burst**: Allows additional bytes to be sent during bursts.
     - **Messages Publish Rate**: Limits the maximum number of messages a client can send per second.
     - **Messages Publish Burst**: Allows additional messages to be sent during bursts.

2. After completing the configuration, click **Create**. The new namespace will appear in the list.

### Delete a Namespace via Dashboard

To delete a namespace, click **Delete** in the **Actions** column. After confirming, the namespace will be permanently deleted.

::: tip Note

Before deleting a namespace, ensure that all active clients associated with the namespace are properly disconnected.

:::

To view clients connected to a specific namespace, click **Clients** in the **Actions** column. You can also choose to bulk disconnect clients.

## Configure and Manage Namespaces via REST API

::: tip

Always check the corresponding Swagger API documentation for detailed and up-to-date request and response endpoint schemas. These are served by the Dashboard listeners at `/api-docs`.

:::

### Configure a Namespace via REST API

After the namespace is created, it can be configured using the `PUT /mt/ns/<namespace>/config` API.

Use this endpoint to set rate limits, session limits, and other namespace-specific settings. For example configurations, see the [Configuration Example](#configuration-example).

#### Configuration Example


This example configures a namespace using the [REST API](../api.md). Suppose you want to configure some specific rate limits for clients in the `ns1` namespace. You also want to limit the maximum number of concurrent sessions allowed in this namespace.

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

### Delete a Namespace via REST API

To remove a namespace and its associated configuration, you can use the `DELETE /mt/ns/<namespace>` API.

::: tip Note

Before deleting a namespace, ensure that all active clients associated with the namespace are properly disconnected. EMQX provides an API to bulk kick all sessions under a namespace, and this process should be triggered automatically when deleting a managed namespace.

:::