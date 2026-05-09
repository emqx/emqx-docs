# Create Namespaces

There are two ways to create a namespace in EMQX: explicit creation and automatic creation. With these two approaches, you can flexibly choose how to create namespaces based on your needs.

- **Explicit creation** is suitable for tightly controlled environments.
- **Automatic creation** is better for dynamic, large-scale deployments that benefit from reduced manual intervention.

## Explicit Namespace Creation

You can manually create namespaces through the Dashboard or the REST API. Explicitly created namespaces can be directly managed, edited, and deleted.

**Use case**: Recommended when you need explicit control over which namespaces exist and require fine-grained management.

### Create a Namespace via Dashboard

To quickly create a namespace in the Dashboard:

1. In the Dashboard's left menu, go to **Management** -> **Namespace**.
2. Click **Create**. In the **Create Namespace** dialog that appears, enter the name of the namespace. Leave other options at their default values. For details on configuring maximum session count and rate limits, see [Configure and Manage Namespaces via Dashboard](./configure-manage-namespace).
3. Click **Create**. The newly created namespace will appear in the list.

### Create a Namespace via REST API

Before applying any namespace-specific configurations, you must explicitly create the namespace using the following API:

```
POST /mt/ns/<namespace>
```

Replace `<namespace>` with the desired namespace ID. No request body is required.

## Automatic Namespace Creation

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
