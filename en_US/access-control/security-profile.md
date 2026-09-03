# Security Profiles

Since version 6.3, EMQX supports a node-wide security profile. The profile selects a set of security-related default behaviors. EMQX provides two profiles:

- `legacy` (default): Keeps the default behaviors of earlier EMQX versions.
- `hardened`: Applies strict, secure-by-default behaviors.

EMQX 7.0 is planned to use `hardened` by default. Use `hardened` for initial deployments. Before migrating a deployment from `legacy`, review the behavior changes below.

## Select a Profile

The profile is selected by the `EMQX_SECURITY_PROFILE` environment variable. Set it in the `emqx.env` file, which the `emqx` command sources on every invocation:

- rpm and deb installs: `/etc/emqx/emqx.env`
- Docker image: `/opt/emqx/etc/emqx.env`
- tar.gz installs: `etc/emqx.env`

Uncomment the `EMQX_SECURITY_PROFILE` line in the file and set the value:

```bash
EMQX_SECURITY_PROFILE=hardened
```

Then restart the node.

Values in `emqx.env` override variables inherited from the environment, and package upgrades keep edits to the file. The variable can also be set in the environment directly, for example with `docker run -e EMQX_SECURITY_PROFILE=hardened`, or with `export EMQX_SECURITY_PROFILE=hardened` before a foreground start.

EMQX reads the variable once at boot, before it parses the configuration files, so the profile cannot be set in a configuration file. Valid values are `legacy`, `hardened`, or empty (which selects the default). Any other value stops the node from starting. Set the same value on every node in a cluster.

::: tip
The security profile only changes default behaviors. Most of the behaviors listed below can also be configured individually, regardless of the selected profile.
:::

## Verify the Active Profile

In the Dashboard, click **Monitoring** -> **Cluster Overview** -> **Nodes** and check the **Security Profile** column. The column shows whether each node started with the `legacy` or `hardened` profile. The value is fixed at startup and can change only after the node restarts.

Compare the values across all running nodes to verify that the cluster uses a consistent profile. Stopped nodes and nodes running versions earlier than 6.3 do not report a security profile.

## Behavior Changes in the Hardened Profile

The `hardened` profile changes the following behaviors compared to `legacy`.

### Node and Cluster Security

- **Known insecure Erlang cookies are rejected.** A node does not start if it uses the built-in default Erlang cookie or the commonly used sample value `emqxsecretcookie`. Configure a non-default `node.cookie` or set `EMQX_NODE__COOKIE` before starting EMQX. Use the same cookie on every node in the cluster.

### Listener Exposure

The `hardened` profile uses the following default bind addresses unless you override them with `node.default_listener_address`, a node-level setting for listeners without an explicit bind address:

- **MQTT listeners bind to loopback by default.** MQTT TCP, SSL, WebSocket, secure WebSocket and QUIC listeners with an omitted or port-only `bind` listen on the loopback interface only. Configure an explicit bind address, for example `bind = "0.0.0.0:1883"`, to accept external connections.
- **The Dashboard HTTP listener binds to loopback by default.** The Dashboard HTTP listener with an omitted or port-only `bind` listens on the loopback interface only. Configure an explicit bind address to accept external connections.

This setting also applies to gateway listeners, whose default bind address is not changed by the security profile. See [Default Listener Address](#default-listener-address) for supported values and configuration details.

### Authentication

- **Explicit authentication is required.** Clients are denied when no authenticators are configured, or when all authenticators are disabled. To explicitly allow anonymous access on a listener, set the listener's `enable_authn = false`.
- **Authentication backend failures deny access.** Authenticator backend errors, malformed backend responses, errors evaluating authenticator preconditions, and unavailable JWT verification keys deny the client instead of continuing to the next authenticator. Set `authentication_settings.ignore_backend_failures = true` to allow fallback to later authenticators.
- **Missing JWTs are not ignored by the JWT authenticator.** The JWT authenticator denies clients that omit the configured JWT field. Set the authenticator's `on_missing_jwt = ignore` to allow those clients to continue to the next authenticator.
- **Non-JWT credentials must skip JWT authenticators in mixed authentication chains.** A JWT authenticator fails authentication when it receives a malformed JWT. When a JWT authenticator and a later password-based authenticator read a JWT or a password from the same field, for example `password`, set the JWT authenticator's `precondition = "is_jwt(password)"` so that plain passwords continue to the next authenticator.
- **Outbound JWKS TLS is verified.** The JWT authenticator verifies peer certificates and hostnames when it fetches keys from a JWKS HTTPS endpoint. Endpoints with untrusted certificates become unavailable. Set `ssl.verify = verify_none` on a specific JWKS endpoint to disable verification.

### Authorization

- **Authorization backend failures deny the operation.** Authorization backend errors, malformed rules, and template evaluation errors deny the publish or subscribe operation instead of continuing to later sources or falling back to the no-match behavior. Set `authorization.ignore_backend_failures = true` to ignore backend failures and proceed to the next authorization source.
- **Forbidden characters in authorization topic-template substitutions deny the operation.** By default, a substituted value that contains `/`, `+`, or `#` causes the rule to return no match under `legacy` and to deny the operation under `hardened`. For example, a client with client ID `i/am/+/good/#` matched against the rule `{allow, all, all, ["t/${clientid}/#"]}.`. Allow individual characters with `authorization.topic_template_allow.slash`, `authorization.topic_template_allow.plus`, or `authorization.topic_template_allow.hash`.
- **The default file authorization source is deny-by-default.** The default `acl.conf` ends with the rule `{allow, {security_profile, legacy}}.`, which allows the operation under the `legacy` profile and does not apply under `hardened`. Under `hardened`, operations that match no rule fall through to `authorization.no_match`, which defaults to `deny`. Change the final rule to `{allow, all}.` for permissive behavior. The `{security_profile, legacy}` and `{security_profile, hardened}` conditions can be used in any `acl.conf` rule, including within `and` and `or` expressions, to apply custom rules only under the selected profile.
- **Internal subscriptions are authorized.** Subscriptions made by features such as Auto Subscribe undergo topic validation, authorization, capability checks, and subscribe hooks. Privileged management force-subscribe operations continue to bypass MQTT authorization.

### Delayed Publishing

- **Delayed messages are reauthorized when replayed.** EMQX checks the current publish authorization rules and ban records by using the authorization context saved when the message was scheduled. A message that was authorized when scheduled can be dropped when replayed.

::: warning Important Notice
Under the `hardened` profile, EMQX drops pending delayed messages created before an upgrade because they do not contain an authorization context. The `legacy` profile continues to replay these messages.
:::

### Extensions

- **Access-control hook failures deny the request.** Exceptions raised from authentication or authorization hooks interrupt processing and deny the request. This is particularly important for custom authentication and authorization provided by plugins or the ExHook extension.
- **ExHook `message.publish` failures deny publishing.** If no ExHook server is available, or an ExHook server whose `failed_action` is `deny` fails while processing `message.publish`, EMQX prevents the message from being published. Under `legacy`, the same failure does not block publishing.
- **Plugin installation requires a package digest.** `emqx ctl plugins allow <Name-Vsn>` requires the `sha256:<hex>` argument. The grant binds the plugin package to that digest, and EMQX installs an upload only when its bytes match. A grant without a digest is refused, including one that a cluster peer sends. Under `legacy`, the argument stays optional.

### Dashboard

- **Default Dashboard credentials are not accepted.** Local Dashboard accounts with the default password `public` cannot log in. This includes administrator accounts created before an upgrade. Change the password before switching to the `hardened` profile.

## Default Listener Address

The `node.default_listener_address` configuration option sets the address for listener binds that have no explicit address, that is bare-port binds such as `bind = 1883`. It applies to MQTT listeners, gateway listeners, and the Dashboard HTTP listener. An explicit `IP:port` bind always wins.

EMQX determines the default address locally on each node and applies it when the listener starts. The configured `bind` value remains unchanged: a port-only bind does not become a persisted `IP:port` value. Nodes with the same listener configuration can therefore listen on different addresses.

Use this option to control listener exposure independently of the security profile. For example, to keep the `hardened` profile and bind defaulted listeners to all network interfaces, add the following to the node's `emqx.conf`:

```hocon
node.default_listener_address = "all"
```

Valid values:

| Value | Bind address |
|---|---|
| `loopback` | `127.0.0.1`. The Dashboard binds `::1` instead when its `inet6` option is set. |
| `nodename` | The host part of the Erlang node name, after the `@`. When it is an IP address, EMQX binds it. Otherwise EMQX resolves it at boot and binds the first IPv4 address, or the first IPv6 address when no IPv4 address resolves. |
| `all` | All network interfaces. With the default IPv4 configuration, the address is `0.0.0.0`. The address family depends on the listener configuration. |
| An IP address | The literal address, for example `192.168.1.10` or `::1`. On most systems `::` accepts both IPv4 and IPv6 connections; the operating system's `bindv6only` setting decides. |
| A hostname | Resolved at boot, for example `broker1.example.com`. |

When the option is not set, the security profile decides the default address for MQTT listeners and the Dashboard HTTP listener: `legacy` binds all interfaces, and `hardened` binds loopback. Gateway listeners bind all interfaces under either profile.

The option is node-local. EMQX reads it once at boot, so changing it requires a node restart. It can also be set with the `EMQX_NODE__DEFAULT_LISTENER_ADDRESS` environment variable.

::: tip
The official Docker image's entrypoint sets `EMQX_NODE__DEFAULT_LISTENER_ADDRESS=all` when the variable is unset or empty, because a container's loopback interface is not reachable through published ports. With this default, listeners whose binds specify only a port listen on all network interfaces under either profile, allowing access through published container ports. To override it, explicitly set the environment variable to another supported value. Listener binds with an explicit IP address remain unchanged.
:::

## Restore Backups Across Security Profiles

A data backup records the security profile of the node from which it was exported. By default, EMQX rejects a backup whose recorded security profile is `legacy`, as well as a backup without this metadata, when it is imported into a `hardened` node. Importing into a `legacy` node is not affected.

Review the differences between the profiles before overriding this protection. For the compatibility rules and override methods, see [Backup and Restore](../operations/backup-restore.md#security-profile-compatibility).

## Rolling Upgrade

All nodes in a cluster must use the same security profile. When profiles differ between nodes, access-control decisions depend on which node a client connects to. Nodes running versions before 6.3 always behave as `legacy`.

When performing a rolling upgrade from a version before 6.3:

1. Do not set `EMQX_SECURITY_PROFILE=hardened` on the upgraded nodes. Leave the variable unset, or set it to `legacy`, so that upgraded nodes behave the same as the nodes that still run the old version.
2. Complete the rolling upgrade on every node.
3. Switch the cluster to `hardened` afterwards by following the migration steps below.

## Migration

To move an existing deployment from `legacy` to `hardened`:

1. Review each behavior change above and apply the explicit configuration where the strict default does not fit the deployment.
2. Configure a non-default Erlang cookie and verify that every node in the cluster uses the same value.
3. Verify that listeners and the Dashboard have explicit bind addresses when they must accept external connections, or set `node.default_listener_address` for all defaulted listeners at once.
4. Verify that every node has authentication configured, or that anonymous access is explicitly enabled where intended.
5. Change any Dashboard account that still uses the default password.
6. Before enabling `hardened` after an upgrade, wait for pending delayed messages created before the upgrade to be replayed, or accept that EMQX will drop them.
7. Set `EMQX_SECURITY_PROFILE=hardened` on all nodes and restart them one at a time.

To retain the previous behavior, set `EMQX_SECURITY_PROFILE=legacy` or leave the variable unset.
