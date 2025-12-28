# Create Namespaces

There are two ways to create a namespace in EMQX: explicit creation and automatic creation. With these two approaches, you can flexibly choose how to create namespaces based on your needs.

- **Explicit creation** is suitable for tightly controlled environments.
- **Automatic creation** is better for dynamic, large-scale deployments that benefit from reduced manual intervention.

## Explicitly Create a Namespace

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

## Automatically Create a Namespace

In EMQX, namespaces can also be created automatically when clients connect.

Automatic namespace creation is not based directly on the `client_attrs.tns` field itself, but instead depends on the **Namespace Source** configuration.

When a client connects, EMQX evaluates the configured namespace source rule to derive the `tns` (tenant namespace) attribute from the client connection metadata. If the corresponding namespace does not already exist, EMQX automatically creates it.

**Typical use cases**: This approach is suitable when client connections originate from trusted sources, the namespace identifier can be reliably derived from connection metadata, and namespaces need to be created dynamically for a large number of tenants or business units.

::: tip Note

Automatically created namespaces are read-only and cannot be edited in the Dashboard.

Their configuration (such as session limits or rate limits) cannot be modified, as these namespaces are generated automatically based on the namespace source rules.

:::

::: tip Note

Automatic namespace creation is only allowed when `multi_tenancy.allow_only_managed_namespaces = false`.

When this configuration is set to `true`, only explicitly created namespaces are allowed, and automatic namespace creation is disabled.

You can also control this behavior from the Dashboard using the **[Allow Only Explicitly Created Namespaces](./namespace-global-settings#allow-only-explicitly-created-namespaces)** setting.

:::
