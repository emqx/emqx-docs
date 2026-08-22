# Security Profiles

Since version 6.3, EMQX supports a node-wide security profile. The profile selects a set of security-related default behaviors. EMQX provides two profiles:

- `legacy` (default): Keeps the default behaviors of earlier EMQX versions.
- `hardened`: Applies strict, secure-by-default behaviors.

EMQX 7.0 is planned to use `hardened` by default. Use `hardened` for initial deployments. Before migrating a deployment from `legacy`, review the behavior changes below.

## Select a Profile

Set the `EMQX_SECURITY_PROFILE` environment variable before starting the node:

```bash
export EMQX_SECURITY_PROFILE=hardened
```

For nodes installed from rpm or deb packages and managed by systemd, set the variable in the `emqx` service. Run `systemctl edit emqx` and add the following lines to the override file:

```ini
[Service]
Environment=EMQX_SECURITY_PROFILE=hardened
```

Then restart the node with `systemctl restart emqx`.

EMQX reads the variable once at boot. Valid values are `legacy`, `hardened`, or empty (which selects the default). Any other value stops the node from starting. Set the same value on every node in a cluster.

::: tip
The security profile only changes default behaviors. Most of the behaviors listed below can also be configured individually, regardless of the selected profile.
:::

## Behavior Changes in the Hardened Profile

The `hardened` profile changes the following behaviors compared to `legacy`.

### Node and Cluster Security

- **Known insecure Erlang cookies are rejected.** A node does not start if it uses the built-in default Erlang cookie or the commonly used sample value `emqxsecretcookie`. Configure a non-default `node.cookie` or set `EMQX_NODE__COOKIE` before starting EMQX. Use the same cookie on every node in the cluster.

### Listener Exposure

- **MQTT listeners bind to loopback by default.** MQTT TCP, SSL, WebSocket, secure WebSocket and QUIC listeners with an omitted or port-only `bind` listen on the loopback interface only. Configure an explicit bind address, for example `bind = "0.0.0.0:1883"`, to accept external connections.
- **The Dashboard HTTP listener binds to loopback by default.** The Dashboard HTTP listener with an omitted or port-only `bind` listens on the loopback interface only. Configure an explicit bind address to accept external connections.

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

### Dashboard

- **Default Dashboard credentials are not accepted.** Local Dashboard accounts with the default password `public` cannot log in. This includes administrator accounts created before an upgrade. Change the password before switching to the `hardened` profile.
- **SAML signatures are verified.** SAML single sign-on requires signatures on both the response envelope and the assertion. Configure `idp_signs_envelopes` and `idp_signs_assertions` to match the identity provider.

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
3. Verify that listeners and the Dashboard have explicit bind addresses when they must accept external connections.
4. Verify that every node has authentication configured, or that anonymous access is explicitly enabled where intended.
5. Change any Dashboard account that still uses the default password.
6. Before enabling `hardened` after an upgrade, wait for pending delayed messages created before the upgrade to be replayed, or accept that EMQX will drop them.
7. Set `EMQX_SECURITY_PROFILE=hardened` on all nodes and restart them one at a time.

To retain the previous behavior, set `EMQX_SECURITY_PROFILE=legacy` or leave the variable unset.
