# Load Secrets from a File

Many EMQX configuration fields hold sensitive values: SSL listener key passphrases, bridge and connector passwords, OIDC client secrets, S3 secret access keys, API keys, and similar. To avoid embedding these values directly in `emqx.conf` or in API requests, EMQX supports a `file://` URL prefix for secret-typed fields. EMQX reads the secret from the referenced file at startup and at every configuration reload.

## Syntax

For any field documented as a secret (or whose dashboard tooltip mentions the `file://` option), use the form:

```bash
file://<path-to-file>
```

The path can be absolute or relative to the EMQX working directory. The file's entire content is used as the secret value, with one transformation:

- **Trailing whitespace is stripped.** Any trailing newline, carriage return, space, or tab characters are removed. The leading and inner content is taken verbatim.

Examples:

```bash
# SSL listener key passphrase loaded from a file
listeners.ssl.default.ssl_options.password = "file://etc/certs/key-passphrase"

# MQTT bridge password loaded from a file
bridges.mqtt.upstream.password = "file:///run/secrets/upstream-mqtt-password"
```

## Cluster Considerations

When running EMQX in a cluster, every node that reads the configuration must be able to resolve the file path:

- The file **must exist on every EMQX node** that loads the configuration. The same path resolves to a per-node file; EMQX does not copy the file between nodes.
- The file's content should match across nodes; otherwise different nodes will use different secret values for the same configuration field.
- For configuration set via the Dashboard or REST API, the change propagates as a `file://...` string to all nodes — each node then opens its own copy of the file.

A common pattern is to provision the secret file via your deployment tooling (Kubernetes Secrets, Ansible, configuration management) before EMQX starts, with the same path on every node.

## Where It Applies

The `file://` convention works wherever the configuration schema uses the secret type. Notable examples:

- **SSL/TLS listeners** — `listeners.<type>.<name>.ssl_options.password` (the key passphrase). See [Enable SSL/TLS](../network/emqx-mqtt-tls.md).
- **Bridges and connectors** — passwords, API keys, secret access keys, JWT tokens, etc.
- **Cluster Linking** — `cluster.links[].password`.
- **Dashboard SSO (OIDC)** — `dashboard.sso.oidc.secret`.
- **License** — `license.key` (the license string itself). See [License Configuration](./license.md).
- **AI completion** — `ai.completion_profile.api_key`.

If a particular field accepts `file://`, the field's dashboard tooltip or this documentation will say so explicitly.

## Logging and Redaction

EMQX redacts secret values from logs and HTTP API responses. When a secret is configured as a `file://...` URL, EMQX logs the path itself (not the file's content), so operators can still verify which file the node is reading. The secret value extracted from the file is never logged.

## When Not to Use `file://`

If a configuration field is plain `string` (not a secret-typed field), prefixing the value with `file://` will be treated as a literal string, not as a file reference. Check the schema type for each field before assuming `file://` is supported.
