---
description: "Configure TLS pre-shared key authentication in EMQX and verify accepted and rejected handshakes separately from MQTT authentication."
---

# Authenticate TLS Connections with a Pre-Shared Key

Configure a TLS listener that looks up a pre-shared key by client identity. Verify both a successful handshake and rejection of an unknown identity or incorrect key before sending MQTT traffic.

This feature authenticates the TLS transport; the MQTT authentication chain and topic authorization still need an explicit policy.

## Before You Begin

Use a client TLS stack that supports the selected TLS 1.2 PSK cipher suite. Confirm the actual cipher list supported by both the EMQX runtime and client; a client that supports TLS 1.3 resumption PSKs does not necessarily support this external PSK lookup feature.

This procedure selects `RSA-PSK-AES128-GCM-SHA256`, one of the documented RSA-PSK suites. RSA-PSK still requires an RSA server certificate and its private key. For a certificate-free PSK suite, confirm runtime/client cipher compatibility first rather than removing the certificate from an RSA-PSK configuration. See [RFC 4279, RSA-PSK](https://www.rfc-editor.org/rfc/rfc4279.html#section-4).

Save the existing listener configuration and use a dedicated test listener. Obtain a randomly generated shared secret and provision it securely to the approved client and EMQX. Do not reuse the illustrative identity across real devices with a shared fleet-wide secret.

## Create the PSK File

On the EMQX node, create a protected file such as `data/psk_file.txt`. Each line has one identity and secret separated by `:`; do not include a heading or comment line:

```text
psk-device-001:replace-with-a-random-test-secret
```

Replace the sample secret before use. The file importer treats the bytes after the separator as the shared secret, rather than interpreting a hex-looking string as hexadecimal key bytes. Configure the client with exactly those bytes. If the client accepts only a hexadecimal PSK argument, hex-encode the literal secret bytes for that argument.

PSK material is input to the TLS key schedule; do not infer its required length solely from the AES cipher's name. Meet your TLS implementation's PSK limits and entropy requirements. Protect the file so only the EMQX service and authorized administrators can read it, and provision its path in the runtime/container where EMQX reads configuration.

## Enable PSK Authentication

Add the following group to `base.hocon`, following the [configuration workflow](../../guides/configuration/configuration.md). Set `init_file` to the PSK file's path as seen by the EMQX process:

```hocon
psk_authentication {
  enable = true
  init_file = "data/psk_file.txt"
}
```

## Configure the TLS Listener

Add a dedicated TLS listener with the selected RSA-PSK cipher suite:

```hocon
listeners.ssl.psk_devices {
  bind = "0.0.0.0:8884"
  ssl_options {
    certfile = "etc/certs/rsa-server-cert.pem"
    keyfile = "etc/certs/rsa-server-key.pem"
    versions = ["tlsv1.2"]
    ciphers = ["RSA-PSK-AES128-GCM-SHA256"]
  }
}
```

These are configuration groups to merge into your managed configuration, not a replacement for all existing settings. The example binds a separate port to avoid changing an existing 8883 listener. Place the RSA server certificate and private key at the configured paths on every node that serves this listener.

This example restricts the listener to TLS 1.2 and the specified RSA-PSK cipher suite so that a successful handshake verifies this PSK configuration rather than negotiating another TLS authentication method. Keep TLS 1.3 disabled for this example. TLS 1.0 and 1.1 are unnecessary for this TLS 1.2 path.

Apply the configuration through the [configuration workflow](../../guides/configuration/configuration.md), then check node/listener status and logs for PSK file or cipher errors. See [listener configuration](enable.md) and the [configuration reference](https://docs.emqx.com/en/enterprise/v6.3.1/hocon/) for additional settings.

## Verify the Connection

Set the broker hostname, port `8884`, TLS version 1.2, matching cipher, PSK identity `psk-device-001`, and the exact shared-secret bytes from the [PSK file](#create-the-psk-file). For RSA-PSK, configure the trusted server CA and server-hostname verification as required by your client library.

Do not send the PSK identity as an MQTT password and expect TLS to use it. The PSK identity belongs to the TLS handshake. After the handshake, the client sends MQTT CONNECT with any username/password or other credentials required by your [authentication policy](../authentication/index.md). PSK transport authentication does not automatically create a topic authorization policy or a particular MQTT client ID.

Check the negotiated protocol and cipher, then verify the following cases:

| Test | Expected Checkpoint |
| --- | --- |
| Configured identity, exact key, matching cipher and TLS version | TLS handshake succeeds; check the negotiated protocol and cipher |
| Configured identity, incorrect key | TLS handshake fails |
| Unknown PSK identity | TLS handshake fails |
| Client offers only TLS 1.3 or no common PSK suite | TLS handshake fails. |
| Valid PSK, invalid MQTT credentials when the chain requires them | TLS succeeds, but MQTT authentication fails |

Use a fresh TLS connection for each case. Do not reuse an existing connection to test a changed key. Then connect with the intended MQTT identity and verify permitted and denied topic operations using a non-superuser policy.

## Update PSK Data

Imported identities and secrets are stored in a persistent database. Importing a file adds identities or overwrites the secret for an existing identity; it does not replace the complete database. Removing an identity from the file and importing it again does not delete the stored identity. Restarting EMQX does not clear these persisted records either, so neither operation should be treated as key revocation.

A file edit by itself is not evidence that the active key database has been reloaded. Verify new handshakes after every key change. Deleting the initialization file alone is not a verified revocation procedure.

## Troubleshooting and Cleanup

For handshake failure, compare the exact identity, raw key bytes versus hex text, runtime file location and permissions, TLS version, common cipher, and RSA certificate/key requirements. Check both TLS logs and EMQX startup/import errors. If the handshake succeeds but MQTT CONNECT fails, inspect the separate MQTT authentication chain.

Disconnect test clients and remove the dedicated listener or restore its saved settings. Disable the PSK feature only if no other listener or client depends on it. Do not assume that these steps remove imported PSK records; see [Update PSK Data](#update-psk-data). If test keys must be revoked, confirm the supported removal procedure for your deployment and verify that new connections using those keys are rejected.
