---
description: "Configure an MQTT TLS or mutual TLS listener in EMQX and verify certificate trust and message delivery."
---

# Enable TLS and Mutual TLS

Configure an MQTT TLS listener and verify a message exchange with certificate validation enabled.

## Before You Begin

Prepare a server certificate chain, its private key, and a client trust bundle. The server certificate's Subject Alternative Name (SAN) must include the DNS name or IP address clients use. For local testing, follow [Create a private test CA](certificates.md#create-a-private-test-ca); its certificate includes `localhost` and `127.0.0.1`.

Use paths readable by the EMQX process on every node that uses the listener. A host path is not automatically available inside a container. Map the TLS port through Docker or expose it through your load balancer and permit it through the firewall. The usual MQTT TLS port is `8883`; confirm the effective [listener configuration](../../guides/configuration/listener.md).

## Choose a Termination Point

| Deployment | Configure |
| --- | --- |
| Client connects directly to EMQX | EMQX presents the server certificate and handles TLS. Size and measure handshake load on the broker. |
| Load balancer terminates TLS | The load balancer presents the certificate. Decide whether its backend connection to EMQX also uses TLS, and configure trusted identity forwarding separately. |

Terminating TLS at a proxy does not automatically encrypt the backend hop or make the original client certificate available to EMQX. See [load balancing](../../guides/cluster/lb.md).

## Enable Server Authentication

With server authentication, clients verify EMQX's certificate. EMQX does not require a client certificate; use [MQTT authentication](../authentication/index.md) when clients must supply credentials.

EMQX provides a default SSL listener on port `8883`. Replace its bundled test certificate before exposing the listener outside an isolated test deployment.

1. In the EMQX Dashboard, open **Management** -> **Listeners** and edit the SSL listener.
2. Under **Certificate Source**, choose how to provide the server certificate:
   - **Enter Manually**: Set **TLS Cert** to the server certificate chain file path and **TLS Key** to its private key file path. Both files must be readable by EMQX on each relevant node.
   - **Select from Managed Certs**: Select the bundle's **Namespace** (`global` for a global bundle) and **Managed Cert Bundle Name**. If needed, click **Create Managed Certs** and follow [Create a Bundle in the Dashboard](certificates.md#create-a-bundle-in-the-dashboard). For multiple certificates, see [SNI selection](certificates.md#multiple-certificates-with-sni).
3. Leave **Verify Peer** and **Force Verify Peer Certificate** disabled for server-only authentication.
4. Click **Update** to save the listener configuration. Then [verify a message exchange](#verify-a-message-exchange).

The equivalent path-based configuration is:

```hocon
listeners.ssl.default {
  bind = "0.0.0.0:8883"
  ssl_options {
    certfile = "etc/certs/server.crt"
    keyfile = "etc/certs/server.key"
    verify = verify_none
    fail_if_no_peer_cert = false
  }
}
```

`verify_none` here controls whether **EMQX verifies the client**. The client must still verify EMQX. Do not combine it with `fail_if_no_peer_cert = true`; EMQX rejects that combination.

For file-managed deployments, place the settings in `base.hocon` and apply the [configuration workflow](../../guides/configuration/configuration.md). Dashboard/API overrides can take precedence over that file. Plan any required restart and check the effective listener afterward.

## Verify a Message Exchange

Install [MQTTX CLI](https://mqttx.app/cli). On your computer, start a subscriber and wait for its subscription to succeed:

```bash
mqttx sub -h 127.0.0.1 -p 8883 -l mqtts -V 5.0 -i tls-sub --reconnect-period 0 \
  --ca rootCA.crt -t docs/tls -q 1
```

In another terminal, publish through the same TLS endpoint:

```bash
mqttx pub -h 127.0.0.1 -p 8883 -l mqtts -V 5.0 -i tls-pub --reconnect-period 0 \
  --ca rootCA.crt -t docs/tls -q 1 -m 'hello over TLS'
```

Expect the subscriber to print topic `docs/tls` and payload `hello over TLS`. Add your configured MQTT credentials to both clients when authentication is enabled. Use the actual hostname and trust bundle outside this local example.

## Require Mutual TLS

Issue a client certificate and key, and provide EMQX with the CA that signs client certificates. In the Dashboard, edit the same SSL listener:

1. For **Enter Manually**, set **CA Cert** to the trusted client CA bundle file path. For **Select from Managed Certs**, select a bundle that includes the client CA in addition to the server chain and key; see [Create a Bundle in the Dashboard](certificates.md#create-a-bundle-in-the-dashboard).
2. Enable **Verify Peer** and **Force Verify Peer Certificate**.
3. Click **Update** to save the listener configuration.

The equivalent path-based configuration is:

```hocon
listeners.ssl.default {
  bind = "0.0.0.0:8883"
  ssl_options {
    certfile = "etc/certs/server.crt"
    keyfile = "etc/certs/server.key"
    cacertfile = "etc/certs/rootCA.crt"
    verify = verify_peer
    fail_if_no_peer_cert = true
  }
}
```

Both clients now need their certificate and key. Start this subscriber, then run the publisher below in another terminal:

```bash
mqttx sub -h 127.0.0.1 -p 8883 -l mqtts -V 5.0 -i tls-sub --reconnect-period 0 \
  --ca rootCA.crt --cert client.crt --key client.key \
  -t docs/tls -q 1
```

```bash
mqttx pub -h 127.0.0.1 -p 8883 -l mqtts -V 5.0 -i tls-pub --reconnect-period 0 \
  --ca rootCA.crt --cert client.crt --key client.key \
  -t docs/tls -q 1 -m 'hello with mTLS'
```

Expect `hello with mTLS`. Repeat without `--cert` and `--key`: the TLS connection must fail. A valid client certificate does not by itself define its MQTT username, Client ID, or topic permissions; see [X.509 authentication](../../guides/access-control/authn/x509.md).

## Use Managed Certificates

Create a bundle before referencing it. This global bundle replaces the three path fields above and must contain the server chain, key, and client CA for mutual TLS:

```hocon
listeners.ssl.default.ssl_options {
  managed_certs = { bundle_name = "mqtt-server" }
  verify = verify_peer
  fail_if_no_peer_cert = true
}
```

Omit `namespace` for a global bundle. For a tenant bundle, add its explicit `namespace`, such as `tenant-a`. See [managed certificates and SNI](certificates.md#multiple-certificates-with-sni) for selection among multiple domains.

## TLS Settings Reference

| Setting Under `ssl_options` | Behavior |
| --- | --- |
| `verify` | `verify_peer` validates client certificates; `verify_none` does not. |
| `fail_if_no_peer_cert` | `true` requires a client certificate and must be paired with `verify_peer`. With `verify_peer` and `false`, a certificate is optional, but a supplied certificate is validated. |
| `versions` | Default TLS versions are `tlsv1.3` and `tlsv1.2`. MQTT SSL listeners use TLS; DTLS belongs to supported UDP gateways. |
| `ciphers` | Optional allowed cipher suites; select suites compatible with your TLS version and certificate. See [PSK](psk.md) for its separate requirements. |
| `depth` | Maximum certificate chain depth; default `10`. |
| `password` | Passphrase for an encrypted key. A `file://` secret reference avoids an inline passphrase; the file must exist on each relevant node. See [Load Secrets from a File](../../guides/configuration/secret-from-file.md) for file handling and trailing whitespace rules. |
| `handshake_timeout` | Default `15s`. Check network and certificate errors before increasing it. |
| `enable_crl_check` | Client certificate [revocation checks](crl.md), disabled by default. |
| `ocsp` | Server certificate [OCSP stapling](ocsp.md), disabled by default. |

### TLS Session Tickets

`session_tickets` defaults to `disabled`. `stateless` enables TLS 1.3 tickets without retaining client certificate information in the resumed connection; `stateless_with_cert` retains that information for certificate-dependent authentication. Configure a non-empty secret in `node.tls_stateless_tickets_seed` before enabling either mode, and manage the seed consistently for nodes intended to accept the same tickets. These ticket modes apply only to TLS 1.3. TLS 1.2 session reuse is controlled separately by `ssl_options.reuse_sessions` and is disabled by default (`false`).

These modes do not enable client early data (0-RTT). Send MQTT packets after the TLS handshake completes. Validate reconnects and certificate-dependent identity handling before enabling resumption in production.

## Troubleshoot and Clean Up

| Symptom | Action |
| --- | --- |
| Hostname or IP does not match | Connect with a name in the SAN or reissue the certificate with the correct SAN. Keep hostname verification enabled. |
| Unknown CA / incomplete chain | Give the client the correct root trust bundle and include intermediate certificates after the server leaf certificate. |
| Client certificate required | Present a valid client certificate and key, or deliberately change the listener's client-certificate policy. |
| TLS succeeds, MQTT is rejected | Check MQTT credentials, Client ID policy, and topic authorization. |
| File cannot be read | Check container mounts, ownership, permissions, and paths on each node. |

Stop the test subscribers with Ctrl-C. Keep production private keys out of example directories and remove disposable test certificates when the test listener no longer references them.
