# Namespace

Starting from EMQX 5.9.0, the Namespace feature allows users to logically group MQTT clients and apply traffic limits within a single EMQX cluster. This feature enables scalable deployments where multiple client groups (such as business units, applications, or customers) share the same infrastructure while remaining logically separated.

::: tip Note

This feature is referred to as Namespace in EMQX 5.9, even though it follows multi-tenancy design principles.

:::

## What Is a Namespace

A Namespace is a logical boundary in EMQX used to group MQTT clients and apply isolation policies. It is identified by the special client attribute `tns` (tenant namespace), which can be extracted from connection metadata such as the username or Server Name Indication (SNI).

Below features are automatically available for namespaces:

- Per-namespace client count limits and connection tracking
- Logging metadata enrichment with namespace information
- Rate limiting per namespace

Please note, you do not automatically get isolations such as client ID override and topic prefixing (mountpoint) by enabling namespace. This is because not all use cases require these isolations. Please refer to [Isolations](#isolations) for more details.

## Enable Namespace

To enable namespace features, EMQX must first know how to determine which namespace a client belongs to. This is done by setting the special `tns` (tenant namespace) client attribute.

You can extract the `tns` attribute from connection metadata, such as the client’s username, SNI, or other fields.

For example, to use the client's username as the namespace identifier, you can use the following configuration:

```hcl
mqtt.client_attrs_init = [{expression = username, set_as_attr = tns}]
```

## Isolations

EMQX is highly flexible and provides multiple ways to achieve different isolations even before the namespace feature.

- **Client ID override:**
  To allow clients to connect EMQX with the same client ID in different namespaces, you can set an override rule for client ID. For example, `mqtt.clientid_override="concat([client_attrs.tns, '-', clientid])"` will add the namespace to the client ID as a prefix.

- **Topic isolation using mountpoint:**
  To allow clients to subscribe and publish the same topic name in different namespaces, you can set `listener.{TYPE}.{NAME}.mountpoint="${client_attrs.tns}/"` to prefix the topic name.

As of 5.9, namespace only applies to MQTT clients, the Dashboard and REST API are not isolated by namespace. It is however in our roadmap to unify administration namespace with MQTT namespace in future releases. See [Multi-tenancy Roadmap](#multi-tenancy-roadmap) for more details.

### Methods for Creating a Namespace

There are two methods for creating namespaces: explicit creation and automatic creation.

1. **Explicit Namespace Creation**
   You can manually create a namespace through the Dashboard or REST API. Explicitly created namespaces can be directly managed, edited, and deleted.

   **Use Case**: This method is ideal when you need full control over which namespaces exist and wish to manage them individually.

2. **Automatic Namespace Creation via EMQX Extracting `client_attrs.tns`**
   When clients connect, EMQX can automatically create namespaces based on the client’s `client_attrs.tns` attribute. This method is usually used in scenarios where manual namespace creation is not necessary and is suitable for large-scale automated deployments.

   **Use Case**: This approach works best for environments where client connections are trusted, and each tenant or client requires an automatically generated, independent namespace.

   ::: tip Note

   Automatically created namespaces cannot be edited in the Dashboard. They are generated either through configuration files or automatically based on client metadata.

   :::

    :::: tip Note
    Automatic namespace creation is only enabled when `multi_tenancy.allow_only_managed_namespaces = false`.
    ::::
   With these two methods, you can choose the most appropriate way to create namespaces based on your needs. Explicit creation is ideal for environments requiring strict control, while automatic creation is suitable for dynamic environments with reduced manual intervention.

## Quick Start: Configure Rate Limiter per Namespace

You can configure per-namespace rate limiters to control traffic and message flow for specific client groups. These limiters work in conjunction with EMQX's existing rate limiters for zones and listeners, depending on the type of limiter configured.

### Limiter Types

In a managed namespace, there are two types of rate limiters:

**Tenant rate limiters**: Assign tokens that are **shared** across all clients within a namespace (NS). When this type of limiter is configured, it composes with any existing zone-level rate limiters, meaning both the zone and the namespace tenant rate limiters apply to clients simultaneously.

**Client rate limiters**: Assign tokens that are **exclusive** to each client within the NS. When this type of limiter is configured, it replaces any existing listener-level rate limiters, meaning the listener rate limiters are ignored while the namespace client limiter takes effect.

Both limiter types can define limits for:

- **Message rate limits**: The maximum number of messages a client or tenant can publish over a given time period.
- **Byte throughput limits**: The maximum allowed size for message payloads over time.

::: tip

For more details, refer to the [Rate Limit](../rate-limit/rate-limit.md) documentation.

:::

### Example of Configuring a Namespace via REST API

This example configures a namespace using the [REST API](../admin/api.md). Suppose you want to configure some specific rate limits for clients in the `ns1` namespace. You also want to limit the maximum number of concurrent sessions allowed in this namespace.

#### Create the Namespace

Before applying any configuration, ensure the namespace is explicitly created:

```bash
# No request body is needed
POST /mt/ns/ns1
```

::: tip Important Notice

If clients connect to a namespace before it is explicitly created, they will not inherit configurations such as rate limiters applied later. To enforce new settings, those clients must be manually disconnected and reconnected.

:::

#### Configure Rate Limits and Session Limits

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

#### Disable Namespace Rate Limiters

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

## Manage Namespace

You can configure and manage namespaces through the Dashboard and REST API.

### Manage Namespaces via Dashboard

In the Dashboard, click **Manage** -> **Namespace** in the left menu. On the **Namespaces** page, you can view, create, edit, and delete namespaces, as well as manage clients connected to namespaces.

By default, the namespace list only shows explicitly created namespaces. You can toggle the switch at the top left of the page to show both explicitly created namespaces and those automatically created by EMQX from the `client_attrs.tns` attribute.

::: tip

Automatically created namespaces cannot be edited in the Dashboard.

:::

#### Create a Namespace

1. Click **Create** in the top right corner of the **Namespaces** page.

2. In the **Create Namespace** dialog, complete the following configuration:

   - **Namespace**: Enter the namespace name.

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

3. After completing the configuration, click **Create**. The new namespace will appear in the list.

#### Edit and Delete Namespaces

To edit the settings for a specific namespace, click **Edit** in the **Actions** column for that namespace.

To delete a namespace, click **Delete** in the **Actions** column. After confirming, the namespace will be permanently deleted.

::: tip Note

Before deleting a namespace, ensure that all active clients associated with the namespace are properly disconnected.

:::

To view clients connected to a specific namespace, click **Clients** in the **Actions** column. You can also choose to bulk disconnect clients.

### Manage Namespaces via REST API

::: tip

Always check the corresponding Swagger API documentation for detailed and up-to-date request and response endpoint schemas. These are served by the Dashboard listeners at `/api-docs`.

:::

### Create a Namespace

Before applying namespace-specific configuration, the namespace must be explicitly created using the `POST /mt/ns/<namespace>` API. Replace `<namespace>` with the desired namespace ID. No request body is required.

### Configure Namespace

After the namespace is created, it can be configured using the `PUT /mt/ns/<namespace>/config` API.

Use this endpoint to set rate limits, session limits, and other namespace-specific settings. For example configurations, see the [Quick Start](#quick-start-configure-rate-limiter-per-namespace) section below.

### Delete a Namespace

To remove a namespace and its associated configuration, you can use the `DELETE /mt/ns/<namespace>` API.

::: tip Note

Before deleting a namespace, ensure that all active clients associated with the namespace are properly disconnected. EMQX provides an API to bulk kick all sessions under a namespace, and this process should be triggered automatically when deleting a managed namespace.

:::

## Multi-tenancy Roadmap

- Unify administration namespace with MQTT namespace.
- Isolation for Rules, Actions/Sources, and Connectors.
- Isolation for builtin DB authentication.
- Isolation for builtin DB authorization.
- Isolation for retained messages quota.
- Isolation for Prometheus metrics.
