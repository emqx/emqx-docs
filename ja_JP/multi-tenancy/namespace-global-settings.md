# Global Namespace Settings

In EMQX 6.1, in addition to configuring individual namespace instances, a set of global namespace settings is available to control how namespaces are identified, how isolation behaviors are applied, and how topics and authorization are handled.

These settings apply cluster-wide and affect all namespaces and client connections. They are typically configured before enabling and using namespace-related features.

Global namespace settings can be managed through the Dashboard at: **Management -> Namespaces -> Settings**.

::: tip Note

To preserve backward compatibility, most global namespace settings in EMQX 6.1, such as Client ID Isolation, Namespace as Mountpoint, and Mount Prefix for Authorization, are disabled by default.

To enable the corresponding isolation capabilities, you must explicitly turn them on under **Namespace Related Configurations**.

:::

![namespace_global_settings](./assets/namespace_global_settings.png)

## Allow Only Explicitly Created Namespaces

This setting controls whether clients are allowed to connect only to namespaces that have been explicitly created.

When this setting is enabled, EMQX validates the client’s namespace during the connection process and decides whether to allow or reject the connection.

- **Enabled**:
  - Clients whose resolved namespace has not been explicitly created via the Dashboard or REST API will be denied connection.
  - Clients whose namespace cannot be resolved (for example, when the namespace source is not configured or does not produce a valid value) will also be denied connection.
- **Disabled**:
  - Clients are allowed to connect to namespaces that have not been explicitly created.
  - If a namespace source is configured, EMQX may automatically create namespaces as needed.

::: tip

Before enabling this setting, ensure that **Namespace Source** is properly configured and that all valid clients can successfully resolve a namespace.

Otherwise, clients may be rejected because their namespace cannot be resolved.

:::

## Default Max Number of Sessions

This setting defines the default maximum number of concurrent sessions for newly created namespaces.

- **Enabled**:
  - Newly created namespaces automatically inherit this maximum session limit.
- **Disabled**:
  - Newly created namespaces have no session limit (`infinity`) by default.

This setting applies only to namespaces created after the configuration takes effect. Existing namespaces are not affected and must be updated individually if needed.

## Namespace Source

The namespace source defines how EMQX determines the namespace to which a client belongs.

When a client connects, EMQX evaluates the configured namespace source rule and extracts a namespace identifier from the client’s connection metadata. The extracted value is stored in the client attribute `client_attrs.tns`.

This configuration is a prerequisite for the following features:

- Automatic namespace creation
- Namespace-based topic isolation
- Namespace-based Client ID isolation
- Namespace-level session limits and rate limits

If no namespace source is configured, clients will not be assigned to any namespace, and all namespace-related isolation and control features will remain inactive.

### Example

The following example extracts the namespace from the client username:

```
nth(1, tokens(username, '-'))
```

With this configuration:

- A client connects using the username `tenantA-user1`.
- The namespace source rule extracts `tenantA`.
- `tenantA` becomes the namespace identifier for the client.

::: tip

Namespace source rules are defined using Variform expressions. For details on the syntax and available functions, see [Variform Expressions](../configuration/configuration.md#variform-expressions).

:::

## Client ID Isolation

Client ID isolation prevents conflicts when clients in different namespaces use the same Client ID.

When enabled, EMQX internally prefixes the Client ID with the client’s namespace, while the original Client ID provided by the client remains unchanged.

When Client ID Isolation is enabled, the Dashboard automatically populates a recommended default expression:

```
concat([client_attrs.tns, '-', clientid])
```

With this configuration:

- Clients in different namespaces can safely use the same Client ID.
- The internally used Client ID always includes the namespace prefix.

This expression is provided as an example. You may customize it to suit your business requirements, as long as the resulting Client ID remains globally unique.

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