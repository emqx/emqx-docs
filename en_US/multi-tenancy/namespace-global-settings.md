# Global Namespace Settings

In EMQX 6.1, in addition to configuring individual namespace instances, a set of global namespace settings is available to control how namespaces are identified, how isolation behaviors are applied, and how topics and authorization are handled.

These settings apply cluster-wide and affect all namespaces and client connections. They are typically configured before enabling and using namespace-related features.

Global namespace settings can be managed through the Dashboard at: **Management** -> **Namespace** -> **Settings**.

::: tip Note

To preserve backward compatibility, most global namespace settings in EMQX 6.1, such as Client ID Isolation, Namespace as Mountpoint, and Mount Prefix for Authorization, are disabled by default.

To enable the corresponding isolation capabilities, you must explicitly turn them on under **Namespace Related Configurations**.

:::

![Global namespace settings, including denied namespace names](./assets/namespace_global_settings.png)

## Allow Only Explicitly Created Namespaces

This setting controls whether clients are allowed to connect only to namespaces that have been explicitly created. It corresponds to `multi_tenancy.allow_only_managed_namespaces` in the configuration file.

When this setting is enabled, EMQX validates the client’s namespace during the connection process and decides whether to allow or reject the connection.

- **Enabled**:
  - Clients whose resolved namespace has not been explicitly created via the Dashboard or REST API will be denied connection.
  - Clients whose namespace cannot be resolved (for example, when the namespace source is not configured or does not produce a valid value) will also be denied connection.
- **Disabled**:
  - Clients are allowed to connect to namespaces that have not been explicitly created.
  - If a namespace source is configured, EMQX may automatically create namespaces as needed.

::: tip Note

Before enabling this setting, ensure that **Take Namespace From** is properly configured and that all valid clients can successfully resolve an explicitly created namespace. Otherwise, clients may be rejected because their namespace cannot be resolved or has not been explicitly created.

When **After Authentication** mode is selected under **When to Resolve Namespace**, pre-authentication namespace checks are skipped. The check against explicitly created namespaces runs after authentication completes instead.

:::

## Default Max Number of Sessions

This setting defines the default maximum number of concurrent sessions for newly created namespaces.

- **Enabled**:
  - Newly created namespaces automatically inherit this maximum session limit.
- **Disabled**:
  - Newly created namespaces have no session limit (`infinity`) by default.

This setting applies only to namespaces created after the configuration takes effect. Existing namespaces are not affected and must be updated individually if needed.

## Denied Namespace Names

Starting from EMQX 6.3.0, `multi_tenancy.deny_namespaces` specifies names that cannot be used as namespace identifiers. The restriction applies to Dashboard user roles, API keys, namespace creation and bulk imports through the management API, and client namespace assignment through `client_attrs.tns`.

The default list is `["global", "undefined", "null", "none"]`. These names can be confused with internal identifiers in logs and Dashboard output.

To edit the list in the Dashboard:

1. Go to **Management** -> **Namespace** -> **Settings**.
2. In **Denied Namespace Names**, add or remove names as needed. Clear all entries to disable the name restriction.
3. Click **Confirm** to apply the changes.

You can also configure the list in `etc/base.hocon`. The following example shows the default value:

```hocon
multi_tenancy.deny_namespaces = ["global", "undefined", "null", "none"]
```

A custom list replaces the default list. Include any default names you still want to deny. Set `multi_tenancy.deny_namespaces = []` to disable the name restriction. For configuration file precedence, see [Config Override Rules](../configuration/configuration.md#config-override-rules).

EMQX rejects a client connection with `not_authorized` if its resolved namespace is in the list, even when **Allow Only Explicitly Created Namespaces** is disabled. This restriction does not prevent clients without a namespace from connecting when `multi_tenancy.allow_only_managed_namespaces = false`.

::: warning Important Notice

The default list rejects names accepted before EMQX 6.3.0. EMQX does not automatically migrate namespaces that use these names. Before upgrading, change the affected namespace names or adjust `multi_tenancy.deny_namespaces` to allow them.

:::

## When to Resolve Namespace

This setting controls at which point in the connection lifecycle EMQX resolves the client’s namespace identifier.

EMQX supports two modes, selectable via the **When to Resolve Namespace** radio button in the Dashboard:

- **Before Authentication** (default): The namespace expression is evaluated before the authentication chain runs, using only connection metadata available at that point (such as `username`, `clientid`, or `cert_common_name`). This corresponds to configuring `tns` via `mqtt.client_attrs_init` in the configuration file.
- **After Authentication**: The namespace expression is evaluated after the full authentication chain completes. In addition to the standard connection metadata, `client_attrs.*` values are available, including any attributes returned by the authentication backend (for example, a `tag` field from an HTTP auth response). This corresponds to configuring `multi_tenancy.post_auth_tns_expression` in the configuration file.

::: tip

If **After Authentication** is configured, EMQX uses the post-authentication expression to assign the namespace. A pre-authentication `tns` value is not used as a fallback if that expression renders empty or fails. See [Empty or Failed Post-authentication Expressions](#empty-or-failed-post-authentication-expressions).

:::

### Interaction with Allow Only Explicitly Created Namespaces

When **After Authentication** mode is selected, pre-authentication namespace checks are skipped entirely, even if **Allow Only Explicitly Created Namespaces** is enabled. All enforcement (whether the resolved namespace exists, quota checks) is deferred until after authentication completes and the final namespace value is known.

## Take Namespace From

This setting specifies the Variform expression EMQX uses to derive the client’s namespace identifier (`client_attrs.tns`).

The expression is evaluated at the point in the connection lifecycle determined by the **When to Resolve Namespace** setting:

- In **Before Authentication** mode, only standard connection metadata is available: `username`, `clientid`, `cert_common_name`, and other pre-auth attributes.
- In **After Authentication** mode, `client_attrs.*` is also available, including attributes merged in from the authentication result.

::: tip

The **Take Namespace From** expression uses Variform syntax. For details on available functions, see [Variform Expressions](../configuration/configuration.md#variform-expressions).

:::

This configuration is a prerequisite for the following features:

- Automatic namespace creation
- Namespace-based topic isolation
- Namespace-based Client ID isolation
- Namespace-level session limits and rate limits

If **Take Namespace From** is not configured, no `tns` attribute will be generated. In this case, clients will not be associated with any namespace, and all namespace-related isolation and control features will remain inactive.

### Examples

#### Before Authentication

Extract the namespace from the username:

```text
nth(1, tokens(username, '-'))
```

With this configuration, a client connecting with the username `tenantA-user1` has `tenantA` assigned as its namespace identifier before authentication runs.

#### After Authentication

Use a `tag` attribute returned by an HTTP auth backend:

```text
client_attrs.tag
```

With a fallback in case the auth backend does not return a tag:

```text
coalesce(client_attrs.tag, username)
```

With this configuration, EMQX waits for the authentication chain to complete, then reads the `tag` value from the merged `client_attrs` and assigns it as the namespace identifier.

### Empty or Failed Post-authentication Expressions

When `multi_tenancy.post_auth_tns_expression` is configured but evaluates to an empty string or fails, EMQX handles the connection as follows. Evaluation failures also produce a warning log.

1. If the pre-authentication `client_attrs.tns` value is in `multi_tenancy.deny_namespaces`, EMQX rejects the connection with `not_authorized`.
2. Otherwise, EMQX treats the client as having no namespace:
   - If `multi_tenancy.allow_only_managed_namespaces = true`, EMQX rejects the connection with `not_authorized`.
   - If `multi_tenancy.allow_only_managed_namespaces = false`, EMQX removes any pre-authentication `tns` value and allows the client to connect without a namespace.

## Client ID Isolation

Client ID isolation prevents conflicts when clients in different namespaces use the same Client ID.

EMQX identifies sessions globally by the effective Client ID, not by a combination of namespace and Client ID. Client ID isolation therefore makes the effective Client ID globally unique, typically by adding the namespace as a prefix. Clients continue to send their original Client IDs, while EMQX uses the overridden IDs internally as the effective Client IDs.

### Choose a Client ID Override Mechanism

Choose a mechanism based on where the namespace information comes from and whether the effective Client ID must contain it:

- If the namespace is available before authentication, configure `mqtt.clientid_override`. EMQX evaluates this expression after `mqtt.client_attrs_init` and before authentication, so it can use attributes initialized by `mqtt.client_attrs_init`, including `client_attrs.tns`.
- If the namespace comes from authentication results and the effective Client ID must include it, configure the [authentication backend to return `clientid_override`](../access-control/authn/authn.md#override-client-ids-from-authentication-results). The returned value must contain the complete new Client ID. The `mqtt.clientid_override` expression cannot use attributes returned by an authentication backend or a namespace generated by `multi_tenancy.post_auth_tns_expression`.
- If `multi_tenancy.post_auth_tns_expression` sets the namespace but the effective Client ID does not need to include it, no Client ID override is required only when clients already use globally unique Client IDs.

Use only one Client ID override mechanism for a connection. If both mechanisms are configured, an authentication-result override runs later and replaces the Client ID produced by `mqtt.clientid_override`. In either case, ensure that the resulting Client ID is globally unique.

### How EMQX Applies Client ID Overrides

EMQX determines the effective Client ID in the following order:

1. Initialize client attributes with `mqtt.client_attrs_init`.
2. Evaluate `mqtt.clientid_override` before authentication.
3. Authenticate the client and apply a non-empty `clientid_override` returned in the successful authentication result.
4. Evaluate `multi_tenancy.post_auth_tns_expression`.
5. Open the client session with the effective Client ID.

EMQX does not evaluate `mqtt.clientid_override` again or automatically add a namespace obtained after authentication to the Client ID. If a successful authentication result omits `clientid_override` or returns an empty value, EMQX keeps the previously determined Client ID.

### Configure Pre-Authentication Client ID Isolation

When Client ID Isolation is enabled in the Dashboard, EMQX configures `mqtt.clientid_override` and automatically populates a recommended expression:

```
concat([client_attrs.tns, '-', clientid])
```

::: warning Important Notice

Starting from EMQX 6.3.0, if the `mqtt.clientid_override` expression raises an error or renders an empty string, EMQX logs an error and rejects the connection. MQTT 5.0 clients receive CONNACK reason code `0x85` (`Client Identifier not valid`), and MQTT 3.1 and 3.1.1 clients receive return code `2`. EMQX does not fall back to the Client ID supplied by the client.

Before upgrading, verify that every connecting client can render the configured expression to a non-empty string. Fix the expression or required client data for any client that cannot do so.

:::

With this configuration:

- Clients in different namespaces can safely use the same Client ID.
- The internally used Client ID always includes the namespace prefix.

This expression is provided as an example for namespaces resolved before authentication. You may customize it to suit your business requirements, as long as the resulting Client ID remains globally unique.

### Example Behavior

Assume a namespace source has been configured to extract the namespace from the username:

```
nth(1, tokens(username, '-'))
```

Client ID isolation is enabled using the default expression:

```
concat([client_attrs.tns, '-', clientid])
```

#### Client Connection Details

| Client | Username      | Client ID |
| ------ | ------------- | --------- |
| A      | tenantA-user1 | client1   |
| B      | tenantB-user2 | client1   |

#### Internally Used Client IDs

| Namespace | Original Client ID | Actual Client ID |
| --------- | ------------------ | ---------------- |
| tenantA   | client1            | tenantA-client1  |
| tenantB   | client1            | tenantB-client1  |

## Namespace as Mountpoint

When enabled, EMQX uses the client’s namespace as a topic mountpoint after the namespace has been successfully resolved. This enables namespace-level topic isolation.

If a listener already has a `mountpoint` configured, this setting is ignored and the listener-level configuration takes precedence.

### Behavior

After **Namespace as Mountpoint** is enabled, EMQX isolates topics as follows:

- During `PUBLISH`, `SUBSCRIBE`, `UNSUBSCRIBE`, and Will message processing:
  - EMQX automatically prepends `{namespace}/` to topics internally.
- When delivering messages to clients:
  - The namespace prefix is automatically stripped.
- From the client’s perspective:
  - Published and subscribed topic names remain unchanged.
  - Clients are not aware of the namespace prefix.

### Example

Assume the client belongs to namespace `n1` and **Namespace as Mountpoint** is enabled.

#### Client-side Behavior

- Client subscribes to: `sensors/#`
- Client publishes to: `sensors/data`

#### Internal Processing in EMQX

- Broker registers the subscription as: `n1/sensors/#`
- Broker routes messages using: `n1/sensors/data`
- Message is delivered to the client as: `sensors/data`

As a result:

- Namespace prefixes are used only internally.
- Clients always interact with original topic names.
- Clients in different namespaces using the same topics do not receive each other’s messages.

## Mount Prefix for Authorization

This setting controls whether the topic mountpoint prefix is added to target topics and topic filters before authorization (ACL) checks are performed.

The mountpoint prefix typically comes from the namespace when **Namespace as Mountpoint** is enabled, and follows the format:

```
{namespace}/
```

### Behavior

When **Mount Prefix for Authorization** is enabled:

- EMQX prepends the topic mountpoint to the target topic or topic filter before matching ACL rules or authorization backends.
- Authorization checks are performed using the prefixed topic.

This behavior applies to the following operations:

- `PUBLISH`
- `SUBSCRIBE`
- `UNSUBSCRIBE`
- Will messages

### Example

Assume the following configuration is enabled:

- **Namespace as Mountpoint**
- **Mount Prefix for Authorization**
- Client namespace: `n1`

#### Client Action

The client attempts to subscribe to:

```
sensors/#
```

#### Topic Used for Authorization

During authorization, EMQX evaluates `n1/sensors/#`.  Therefore, the corresponding ACL rule must be defined as `n1/sensors/#` rather than `sensors/#`.

### Recommendation

When **Namespace as Mountpoint** is enabled for topic isolation, it is recommended to enable **Mount Prefix for Authorization** as well. This ensures that authorization checks are performed against the same topic names used internally by the broker, avoiding inconsistencies between authorization results and actual message routing.
