# Namespace

Starting from EMQX 5.9.0, the Namespace feature allows users to logically group MQTT clients and apply traffic limits within a single EMQX cluster. This feature enables scalable deployments where multiple client groups (such as business units, applications, or customers) share the same infrastructure while remaining logically separated.

::: tip Note

This feature is referred to as Namespace in EMQX 5.9, even though it follows multi-tenancy design principles.

:::

## What Is a Namespace

A Namespace in EMQX Enterprise is a mechanism used for logical isolation and resource management of MQTT clients. It allows users to divide clients from different businesses or tenants into separate namespaces within a shared EMQX cluster, achieving isolation in connections, messages, quotas, and more.

Namespaces are identified by a special client attribute `tns` (tenant namespace), which can be extracted from the client's connection metadata, such as the username or Server Name Indication (SNI).

> **Typical use cases include**: multiple business units sharing a cluster within an enterprise, tenant-level resource isolation management, centralized access control, etc.

### What Namespaces Can Achieve

- **Logical Isolation of Clients and Messages**

  Namespaces enable you to logically separate clients across different tenants by isolating client IDs and topic spaces.

  ::: tip Note

  Enabling namespaces does not automatically apply client ID overrides or topic prefixes. These features must be manually configured. See [Isolation Mechanisms](#isolation-mechanisms) for details.

  :::

- **Tenant-Level Quotas and Connection Control**

  You can define limits on the number of concurrent connections and message publish rates for each namespace, helping to ensure fair usage and system stability.

- **Enhanced Logging and Operational Visibility**

  Logs automatically include the namespace identifier (`tns`), making it easier to trace client activity, detect issues, and perform tenant-level diagnostics.

- **Namespace-Based Resource Monitoring**

  Namespaces provide a clean boundary for collecting metrics such as connection count and message throughput per tenant, essential for capacity planning and operational insight.

### Isolation Mechanisms

EMQX offers high flexibility and has supported various isolation methods even before the namespace feature. The namespace feature provides a unified tenant identifier field (`client_attrs.tns`), allowing configurations like client ID and topic mount points to be organized and managed around unified tenant information.

However, note that isolation strategies still require **manual configuration** by users based on business needs; the system will not automatically enable client ID or topic isolation features.

- **Client ID Override**

  If you want clients in different namespaces to use the same client ID to connect to EMQX, you can set a client ID override rule. For example:

  ```
  mqtt.clientid_override = "concat([client_attrs.tns, '-', clientid])"
  ```

​       This rule adds the namespace as a prefix to the client ID to avoid conflicts.

- **Topic Isolation Using Mount Points**

  If clients in different namespaces need to publish or subscribe to the same topic names without affecting each other, you can use mount points to automatically add namespace prefixes:

  ```
  listener.{TYPE}.{NAME}.mountpoint = "${client_attrs.tns}/"
  ```

  This setting adds a namespace prefix to the topic name.

As of version 5.9, namespaces are only applicable to MQTT clients. The Dashboard and REST API are not yet isolated based on namespaces. EMQX plans to implement unified management namespaces and MQTT namespaces in future versions. For details, see the [Multi-Tenancy Roadmap](#multi-tenancy-roadmap).

## Enable Namespaces

To enable the namespace feature, you must first tell EMQX how to determine which namespace a client belongs to. This is done by setting a special client attribute called `tns` (tenant namespace).

### Enable Namespaces via Configuration File

You can extract the `tns` attribute from the connection metadata, such as the client's username, SNI, or other fields.

For example, to use the client's username as the namespace identifier, you can apply the following configuration:

```
mqtt.client_attrs_init = [{expression = username, set_as_attr = tns}]
```

### Enable Namespaces via Dashboard

You can also enable namespaces using the EMQX Dashboard:

1. Navigate to **Management** -> **MQTT Settings** -> **General** tab, and locate the **Client Attributes** section.
2. Click **Add**, and fill in the following information:
   - **Attribute**: `tns`
   - **Attribute Expression**: For example, if you want to use the client's username as the namespace identifier, enter `username`. You may also use other variables. For more information on attribute expressions, refer to [Set Client Attributes](../client-attributes/client-attributes.md#set-client-attributes).
3. Click **Save Changes**.

## Create Namespaces

There are two ways to create a namespace in EMQX: explicit creation and automatic creation.

### Explicit Namespace Creation

You can manually create namespaces through the Dashboard or the REST API. Explicitly created namespaces can be directly managed, edited, and deleted.

**Use case**: Recommended when you need explicit control over which namespaces exist and require fine-grained management.

#### Create a Namespace via Dashboard

To quickly create a namespace in the Dashboard:

1. In the Dashboard's left menu, go to **Management** -> **Namespace**.
2. Click **Create**. In the **Create Namespace** dialog that appears, enter the name of the namespace. Leave other options at their default values. For details on configuring maximum session count and rate limits, see [Configure and Manage Namespaces via Dashboard](#configure-and-manage-namespaces-via-dashboard).
3. Click **Create**. The newly created namespace will appear in the list.

#### Create a Namespace via REST API

Before applying any namespace-specific configurations, you must explicitly create the namespace using the following API:

```
POST /mt/ns/<namespace>
```

Replace `<namespace>` with the desired namespace ID. No request body is required.

### Automatic Namespace Creation

When a client connects, EMQX can automatically create a namespace by extracting the `client_attrs.tns` attribute from the client's metadata. This method is typically used in large-scale, automated environments where manual creation is impractical.

**Use case**: Ideal for trusted client connection environments where each tenant or client should be assigned a separate namespace without manual setup.

::: tip Note
Automatically created namespaces **cannot be edited** via the Dashboard. They are only generated through configuration or extracted automatically from client metadata.
:::

::: tip Note
Automatic namespace creation is enabled only when the following configuration is set:

```
multi_tenancy.allow_only_managed_namespaces = false
```

:::

With these two approaches, you can flexibly choose how to create namespaces based on your needs.

- **Explicit creation** is suitable for tightly controlled environments.
- **Automatic creation** is better for dynamic, large-scale deployments that benefit from reduced manual intervention.

## Configure and Manage Namespaces

You can configure and manage namespaces using either the Dashboard or the REST API.

### Rate Limiters for a Namespace

Namespace configuration mainly includes setting a maximum session count and rate limiters. Before configuring rate limiters, it's important to understand the types and purposes of rate limiters in a namespace. For specific configuration options, refer to [Configure and Manage Namespaces via Dashboard](#configure-and-manage-namespaces-via-dashboard).

You can configure rate limiters for each namespace to control traffic and message flow for specific client groups. These namespace-level limiters work alongside EMQX’s existing rate limiters (for zones and listeners), depending on the type used.

#### Types of Rate Limiters

In a managed namespace, there are two types of rate limiters:

**Tenant rate limiters**: Assign tokens that are **shared** across all clients within a namespace (NS). When this type of limiter is configured, it composes with any existing zone-level rate limiters, meaning both the zone and the namespace tenant rate limiters apply to clients simultaneously.

**Client rate limiters**: Assign tokens that are **dedicated** to each client within the NS. When this type of limiter is configured, it replaces any existing listener-level rate limiters, meaning the listener rate limiters are ignored while the namespace client limiter takes effect.

Both limiter types can define limits for:

- **Message rate limits**: The maximum number of messages a client or tenant can publish over a given time period.
- **Byte throughput limits**: The maximum allowed size for message payloads over time.

::: tip

For more details, refer to the [Rate Limit](../rate-limit/rate-limit.md) documentation.

:::

### Configure and Manage Namespaces via Dashboard

In the Dashboard’s left-side menu, go to **Management** -> **Namespace**. On the **Namespace** page, you can view, edit, or delete namespaces and manage clients connected to each namespace.

By default, the namespace list only shows explicitly created namespaces. You can toggle the switch at the top left of the page to show both explicitly created namespaces and those automatically created by EMQX from the `client_attrs.tns` attribute.

::: tip Note

Automatically created namespaces cannot be edited in the Dashboard.

:::

#### Configure a Namespace

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

#### Delete a Namespace

To delete a namespace, click **Delete** in the **Actions** column. After confirming, the namespace will be permanently deleted.

::: tip Note

Before deleting a namespace, ensure that all active clients associated with the namespace are properly disconnected.

:::

To view clients connected to a specific namespace, click **Clients** in the **Actions** column. You can also choose to bulk disconnect clients.

### Configure and Manage Namespaces via REST API

::: tip

Always check the corresponding Swagger API documentation for detailed and up-to-date request and response endpoint schemas. These are served by the Dashboard listeners at `/api-docs`.

:::

#### Configure a Namespace

After the namespace is created, it can be configured using the `PUT /mt/ns/<namespace>/config` API.

Use this endpoint to set rate limits, session limits, and other namespace-specific settings. For example configurations, see the [Quick Start](#quick-start-configure-rate-limiter-per-namespace) section below.

<details>
  <summary><b>Example of Configuring a Namespace via REST API</b></summary>


This example configures a namespace using the [REST API](../admin/api.md). Suppose you want to configure some specific rate limits for clients in the `ns1` namespace. You also want to limit the maximum number of concurrent sessions allowed in this namespace.

###### Create the Namespace

Before applying any configuration, ensure the namespace is explicitly created:

```bash
# No request body is needed
POST /mt/ns/ns1
```

::: tip Important Notice

If clients connect to a namespace before it is explicitly created, they will not inherit configurations such as rate limiters applied later. To enforce new settings, those clients must be manually disconnected and reconnected.

:::

###### Configure Rate Limits and Session Limits

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

###### Disable Namespace Rate Limiters

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

</details>

#### Delete a Namespace

To remove a namespace and its associated configuration, you can use the `DELETE /mt/ns/<namespace>` API.

::: tip Note

Before deleting a namespace, ensure that all active clients associated with the namespace are properly disconnected. EMQX provides an API to bulk kick all sessions under a namespace, and this process should be triggered automatically when deleting a managed namespace.

:::

## Quick Start: Experience Namespaces

This section guides you through using the [MQTTX client](https://mqttx.app) to connect to EMQX and quickly experience the core capabilities of the namespace feature: tenant identification, client isolation, and topic isolation.

### Enable the `tns` Attribute for Namespace Identification

1. First, configure a client attribute in `emqx.conf` to extract the namespace (tenant identifier) from the username:

   ```
   mqtt.client_attrs_init = [{expression = "nth(1, tokens(username, '-'))", set_as_attr = tns}]
   ```

   > Example: If a client connects with the username `tenantA-user1`, EMQX will extract `tenantA` as the namespace (`tns`).

   Alternatively, you can configure this in the Dashboard:

   <img src="./assets/enable_namespace.png" alt="enable_namespace" style="zoom:67%;" />

2. Create an MQTT client connection using MQTTX, simulating tenant `tenantA`, and set the username to `tenantA-user1`. Connect the client to EMQX.

3. Go to the **Namespace** page in the Dashboard and disable the **View Explicitly Created Namespace Only** toggle. You should see the automatically created namespace `tenantA`.

   Click **Clients** in the **Actions** column to view the client connected to this namespace.

   ![namespace_client](./assets/namespace_client.png)

### Configure and Verify Namespace Isolation

1. To isolate client IDs and topics between namespaces, add the following configuration to `emqx.conf`:

   ```
   mqtt.clientid_override = "concat([client_attrs.tns, '-', clientid])"
   listener.tcp.default.mountpoint = "${client_attrs.tns}/"
   ```

   This configuration will:

   - Automatically prepend the tenant prefix to the client ID to avoid conflicts.
   - Automatically prepend the namespace prefix to topic names for topic-level isolation between tenants.

   You can also set this up in the Dashboard:

   <img src="./assets/clientid_override.png" alt="clientid_override" style="zoom:67%;" />
   <img src="./assets/listener_mountpoint.png" alt="listener_mountpoint" style="zoom:67%;" />

2. Use MQTTX to create two MQTT client connections to simulate two tenants: `tenantA` and `tenantB`.

   ##### Client A (Tenant: tenantA):

   | Parameter | Value           |
   | --------- | --------------- |
   | Client ID | `client1`       |
   | Username  | `tenantA-user1` |
   | Subscribe | `test/topic`    |

   ##### Client B (Tenant: tenantB):

   | Parameter | Value           |
   | --------- | --------------- |
   | Client ID | `client1`       |
   | Username  | `tenantB-user2` |
   | Publish   | `test/topic`    |
   
3. Use Client B to publish a message. Verify the result in MQTTX and the EMQX Dashboard:

   - Although both clients use the same client ID (`client1`), due to the prefix rule, they connect as `tenantA-client1` and `tenantB-client1`, avoiding conflicts.
   - Even though both clients use the same topic (`test/topic`), Client A will **not receive** messages published by Client B because they are isolated by namespace.
   - In the **Monitoring** -> **Clients** page:
     - Client A's subscribed topic appears as `tenantA/test/topic`.
     - Client B's published topic appears as `tenantB/test/topic`.

## Multi-Tenancy Roadmap

- Unify management namespaces and MQTT namespaces.
- Implement isolation for rules, actions/data sources, and connectors.
- Implement isolation for built-in database authentication.
- Implement isolation for built-in database authorization.
- Implement quota isolation for retained messages.
- Implement isolation for Prometheus metrics.
