# Load Secrets from a File

Many EMQX configuration fields hold sensitive values: SSL listener key passphrases, bridge and connector passwords, OIDC client secrets, S3 secret access keys, API keys, and similar. To avoid embedding these values directly in `emqx.conf` or in API requests, EMQX supports a `file://` URL prefix for secret-typed fields. EMQX reads the secret from the referenced file at startup and at every configuration reload.

## Syntax

For any field documented as a secret (or whose dashboard tooltip mentions the `file://` option), use the form:

```text
file://<path-to-file>
```

The path can be absolute or relative to the EMQX working directory. The file's entire content is used as the secret value, with one transformation:

- Trailing whitespace is stripped. Any trailing newline, carriage return, space, or tab characters are removed. The leading and inner content is taken verbatim.

Examples:

```hocon
# SSL listener key passphrase loaded from a file
listeners.ssl.default.ssl_options.password = "file://etc/certs/key-passphrase"

# MQTT bridge password loaded from a file
bridges.mqtt.upstream.password = "file:///run/secrets/upstream-mqtt-password"
```

## Cluster Considerations

When running EMQX in a cluster, every node that reads the configuration must be able to resolve the file path:

- The file must exist on every EMQX node that loads the configuration. The same path resolves to a per-node file; EMQX does not copy the file between nodes.
- The file's content should match across nodes; otherwise different nodes will use different secret values for the same configuration field.
- For configuration set via the Dashboard or REST API, the change propagates as a `file://...` string to all nodes, and each node then opens its own copy of the file.

A common pattern is to provision the secret file via your deployment tooling (Kubernetes Secrets, Ansible, configuration management) before EMQX starts, with the same path on every node.

## Where It Applies

The `file://` convention works wherever the configuration schema uses the secret type. Notable examples:

- **SSL/TLS listeners**: `listeners.<type>.<name>.ssl_options.password` (the key passphrase). See [Enable SSL/TLS](../network/emqx-mqtt-tls.md).
- **Bridges and connectors**: passwords, API keys, secret access keys, JWT tokens, and service account JSON credentials such as `service_account_json`.
- **Cluster Linking**: `cluster.links[].password`.
- **Dashboard SSO (OIDC)**: `dashboard.sso.oidc.secret`.
- **License**: `license.key` (the license string itself). See [License Configuration](../configuration/license.md).
- **AI completion**: `ai.completion_profile.api_key`.

The Dashboard tooltips for these fields indicate that they support the `file://` format.

## Load the Node Cookie from a File

Starting from EMQX 6.3.0, `node.cookie` and its environment-variable override, `EMQX_NODE__COOKIE`, accept `file://`. This is an explicit exception to the default behavior of `string` fields.

To avoid storing the node cookie in plain text in `emqx.conf`, set `node.cookie` to a file URL:

```hocon
node.cookie = "file:///run/secrets/emqx-cookie"
```

Alternatively, set `EMQX_NODE__COOKIE`:

```bash
export EMQX_NODE__COOKIE='file:///run/secrets/emqx-cookie'
```

The path can reference a regular file or a FIFO (named pipe). EMQX resolves the node cookie once during boot. Configuration reloads do not read the file or FIFO again.

When you use a FIFO, the orchestrator must write the cookie to the FIFO on every boot before invoking any other `emqx` command, such as `emqx ctl`. Commands invoked after the node starts obtain the cookie from the running node instead of reading the FIFO again.

The startup script removes trailing newline characters from the file content. The node fails to start if the referenced path does not exist, the file is empty, or the resolved cookie contains a backslash, single quote, double quote, or space.

EMQX passes the resolved cookie directly to the Erlang VM without writing it to the generated `data/configs/vm.*.args` file. In clustered deployments, provision the file or FIFO on every node and ensure that each node reads the same cookie. For more information, see [Set Node Cookie](../deploy/cluster/security.md#set-node-cookie).

## Logging and Redaction

EMQX redacts the values of secret-typed fields in logs and HTTP API responses. For a `file://` value, EMQX logs the file path but not its contents. The resolved secret value is never logged.

## When Not to Use `file://`

Except for explicitly documented fields such as `node.cookie`, a plain `string` field treats a `file://` value as a literal string, not as a file reference. Check the schema type and field documentation before assuming that `file://` is supported.
