# Namespace

Starting from EMQX 5.9.0, the Namespace feature allows users to logically group MQTT clients and apply traffic limits within a single EMQX cluster. This feature enables scalable deployments where multiple client groups (such as business units, applications, or customers) share the same infrastructure while remaining logically separated.

The Namespace feature in EMQX consists of two parts:

- **MQTT client namespace** — logical grouping of MQTT clients (by username, SNI, or other connection metadata) with per-namespace quotas, rate limits, and isolation knobs for client IDs and topics.
- **Admin user namespace** — Dashboard, CLI, and API users scoped to a specific namespace via [namespaced roles](../dashboard/system.md/#namespaced-roles), so delegated administrators only see and operate on resources within their assigned namespace. Available since EMQX 6.0.

::: warning Trusted Deployments Only

The **admin user namespace** is intended for trusted internal deployments, such as separating teams or business units within one organization, to reduce the risk of accidentally changing each other's configurations. It does **not** provide strong isolation guarantees and is **not** a security boundary for public or untrusted multi-tenant deployments. If you are considering admin user namespaces for a public multi-tenant deployment, please contact EMQ sales: this use case is not currently supported out of the box.

For the **MQTT client namespace**, isolation between clients across namespaces is opt-in and must be explicitly configured. When clients across namespaces are not mutually trusted, see [Isolation Mechanisms](#isolation-mechanisms) for the required client ID overrides and topic mount points.

:::

Beginning with EMQX 6.1, namespace-related capabilities have been enhanced without changing their original semantics. These enhancements simplify multi-tenant isolation configuration and unify the behavior of topic isolation.

## What Is a Namespace

A Namespace in EMQX Enterprise is a mechanism used for logical isolation and resource management of MQTT clients. It allows users to divide clients from different businesses or tenants into separate namespaces within a shared EMQX cluster, achieving isolation in connections, messages, quotas, and more.

A namespace is identified by a special client attribute named `tns` (tenant namespace). This attribute is not created automatically; instead, it must be derived from client connection metadata, such as the username or Server Name Indication (SNI), through configuration.

A namespace becomes effective once it is created, regardless of whether it is created explicitly via the Dashboard or REST API, or automatically during client connection based on a defined rule.

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
  
  Starting from EMQX 6.0, namespaces are extended to Dashboard, CLI, and API users through [namespaced roles](../dashboard/system.md/#namespaced-roles). See the [Trusted Deployments Only](#namespace) notice at the top of this page for the trust model and required safeguards.

  - Admin users can be created with roles restricted to a specific namespace, e.g., `ns:team_a::administrator`.
  - Namespaced users only see and operate on resources within their assigned namespace.
  - Cluster-level configurations not yet namespace-aware are visible but read-only for namespaced users, and only modifiable by global administrators.
  - Starting from EMQX 6.0.4, namespaced administrators can [manage API keys within their own namespace](../admin/api.md#manage-api-keys-as-a-namespaced-administrator). Global keys and keys in other namespaces remain inaccessible.
  - This ensures secure, tenant-specific administrative access alongside data isolation.
- **Multi-Tenant Management**

  System administrators can manage multiple namespaces within the same cluster, while each tenant operates in a self-contained environment with isolated resources and user permissions.

## Operational Security for Admin Namespaces

Delegated namespace administrators can configure outbound targets such as connectors, bridges, and actions. Without additional controls, this could allow unintended access to internal or sensitive network destinations.

Enable `rule_engine.ssrf` where available to validate HTTP and MQTT connector targets when a connector configuration is tested, created, or updated. Starting from EMQX 6.0.4, the policy does not cover other connector types or runtime connections. Add egress controls on the EMQX hosts to enforce an outbound network boundary across connector types:

- Allow outbound access only to approved destinations, such as identity providers (IdPs), webhooks, or connector backends.
- Deny access to instance metadata services, loopback addresses, link-local addresses, and internal management networks unless explicitly required. Typical metadata endpoints to block include `100.100.100.200`, `169.254.169.253`, `169.254.169.254`, and `fd00:ec2::254`.
- Review firewall rules whenever you add new integrations or management features that initiate outbound HTTP or TCP connections.

For details, see [Mitigate SSRF with Rule Engine Policy and Firewall Rules](../deploy/cluster/security.md#mitigate-ssrf-with-rule-engine-policy-and-firewall-rules).

## Isolation Mechanisms

EMQX is highly flexible and supports multiple isolation mechanisms even before namespaces were introduced.

Namespaces provide a unified tenant identifier (`client_attrs.tns`) that allows Client IDs, topic mountpoints, and related configurations to be organized around a consistent tenant context.

However, isolation policies still need to be explicitly configured based on business requirements. EMQX does not automatically enable Client ID or topic isolation when namespaces are enabled.

### Client ID Override

::: warning Required for Untrusted Multi-Tenant Deployments

If clients from different namespaces are not mutually trusted (for example, when each namespace represents an external customer or a separate organization), you **must** configure `mqtt.clientid_override`. Without it, a client in one namespace can reuse another tenant's client ID, kicking it offline, hijacking its persistent session, or causing a denial-of-service for that tenant. Authentication does not prevent this: session takeover happens at the connection layer before ACLs apply.

Pair this with [topic isolation using mountpoints](#topic-isolation-using-mountpoints) so that topic-level access cannot cross namespace boundaries either.

:::

To allow clients in different namespaces to use the same Client ID, you can configure a Client ID override rule. For example:

```hocon
mqtt.clientid_override = "concat([client_attrs.tns, '-', clientid])"
```

This rule prefixes the Client ID with the namespace to avoid conflicts.

### Topic Isolation Using Mountpoints

If clients in different namespaces need to publish or subscribe to the same topic names without interfering with each other, a mountpoint can be used to automatically prefix topics with the namespace.

In EMQX 6.0 and earlier, mountpoints were typically configured at the listener level, for example:

```hocon
listener.{TYPE}.{NAME}.mountpoint = "${client_attrs.tns}/"
```

In environments with multiple listeners, this required repetitive configuration.

Starting from EMQX 6.1, namespaces can be used as a unified topic mountpoint. Once a namespace is successfully identified, EMQX internally applies `{namespace}/` as the topic prefix, achieving the same isolation effect as listener mountpoints without requiring per-listener configuration.

To maintain backward compatibility, authorization (ACL) checks do not include the mountpoint prefix by default.

From EMQX 6.1 onward, you can enable this behavior by setting:

```hocon
authorization.include_mountpoint = true
```

This allows authorization backends to receive topics with the mountpoint prefix.

### Rule Namespace Isolation

When `rule_engine.limit_selects_in_namespace` is enabled (the default), this setting prevents messages and client-related events from another namespace from triggering a namespaced rule. EMQX identifies the client namespace by the `client_attrs.tns` client attribute. Starting from EMQX 6.1.5, the setting also confines the output topic of the rule's Republish action to the rule's namespace.

After rendering the topic template, EMQX prepends `<namespace>/` if the rendered topic does not already start with `<namespace>/`. A topic that already starts with `<namespace>/` remains unchanged. Global rules and deployments with `rule_engine.limit_selects_in_namespace = false` continue to publish to the rendered topic without adding the rule namespace.

This Republish behavior does not depend on `mqtt.namespace_as_mountpoint`. The setting does not restrict clients from publishing or subscribing across namespaces. To isolate client topic access, configure topic isolation using mount points and authorization rules.

## Multi-Tenancy Capability Support

Namespaces are the core building block of EMQX multi-tenancy. Introduced in EMQX 5.9 and enhanced in 6.1, namespaces now support tenant isolation across multiple subsystems. The current support status is as follows:

- **Unified management and MQTT namespaces** (6.0)

  The management plane (Dashboard, CLI, APIs) and the MQTT data plane share the same namespace model.

- **Isolation for built-in database authentication** (6.1)

  Authentication data stored in the built-in database can be isolated by namespace.

- **Isolation for built-in database authorization** (6.1)

  Authorization rules can be scoped to specific namespaces.

- **Prometheus metrics isolation** (6.1)

  Metrics can be exposed and aggregated by namespace, enabling better observability in multi-tenant environments.

- **Retained message quota isolation**

  Resource usage related to retained messages can be limited per namespace.

In addition, starting from EMQX 6.0, namespace isolation has been fully implemented for rules, actions, sources, and connectors, and is no longer part of the future roadmap.

## What's Next

Now that you understand what namespaces are and what they can achieve, here are the next steps to start using them in EMQX:

- **[Create Namespaces](./create-namespace.md)**: Learn how to create namespaces explicitly via the Dashboard or REST API, or automatically based on client metadata.
- **[Configure and Manage Namespaces](./configure-manage-namespace.md)**: Set rate limits and session quotas using either the Dashboard or REST API.
- **[Namespace Global Settings](./namespace-global-settings.md)**: Configure cluster-wide namespace behaviors, including namespace resolution, isolation mechanisms, topic mount points, and authorization handling.
- **[Quick Start: Experience Namespaces](./namespace-quick-start.md)**: Follow a hands-on guide using MQTTX to try out namespace-based client and topic isolation quickly.
