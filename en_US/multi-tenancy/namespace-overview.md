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

- **Admin User Isolation**
   
  Starting from EMQX 6.0, namespaces are extended to Dashboard, CLI, and API users through [namespaced roles](../dashboard/system.md/#namespaced-roles).
  
  - Admin users can be created with roles restricted to a specific namespace, e.g., `ns:team_a::administrator`.
  - Namespaced users only see and operate on resources within their assigned namespace.
   - Cluster-level configurations not yet namespace-aware are visible but read-only for namespaced users, and only modifiable by global administrators.
   - This ensures secure, tenant-specific administrative access alongside data isolation.
   
- **Multi-Tenant Management**

  System administrators can manage multiple namespaces within the same cluster, while each tenant operates in a self-contained environment with isolated resources and user permissions.

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

## Multi-Tenancy Roadmap

The following features are being rolled out progressively:

- Unify management namespaces and MQTT namespaces.
- Implement isolation for built-in database authentication.
- Implement isolation for built-in database authorization.
- Implement quota isolation for retained messages.
- Implement isolation for Prometheus metrics.

::: tip Update

As of EMQX 6.0, isolation for Rules, Actions, Sources, and Connectors has been fully implemented and is no longer part of the roadmap.
:::

## What's Next

Now that you understand what namespaces are and what they can achieve, here are the next steps to start using them in EMQX:

- **[Create Namespaces](./create-namespace.md)**
  Learn how to create namespaces explicitly via the Dashboard or REST API, or automatically based on client metadata.
- **[Configure and Manage Namespaces](./configure-manage-namespace.md)**
  Set rate limits and session quotas using either the Dashboard or REST API.
- **[Quick Start: Experience Namespaces](./namespace-quick-start.md)**
  Follow a hands-on guide using MQTTX to quickly try out namespace-based client and topic isolation.
