---
description: "Create test certificates, manage certificate bundles in EMQX, and rotate certificates with new-connection verification."
---

# Create, Manage, and Rotate TLS Certificates

Prepare certificates for MQTT TLS, WSS, Dashboard HTTPS, or outbound services. EMQX supports PEM file paths and, from version 6.1, reusable managed bundles.

## Choose a Certificate Source

Use a public CA when clients should trust a public hostname through their existing trust stores. Use an enterprise/private CA when you control trust distribution to devices and services. A private CA can support production deployments when its issuance, key protection, revocation, and renewal processes meet your requirements.

The certificates shipped in `etc/certs` are test material. Replace them before exposing a production endpoint. A certificate's SAN, intended usage, trust chain, and private key matter to TLS interoperability; organization validation alone does not establish these properties.

## Create a Private Test CA

These OpenSSL 3 commands create a disposable local CA and a server certificate for `localhost` and `127.0.0.1`. Run them in a new private directory. The CA private key stays with the issuer; do not copy it to EMQX or clients.

```bash
umask 077
mkdir emqx-tls-test
cd emqx-tls-test
openssl genrsa -aes256 -out rootCA.key 3072
openssl req -x509 -new -sha256 -days 3650 \
  -key rootCA.key -out rootCA.crt -subj '/CN=EMQX Test CA' \
  -addext 'basicConstraints=critical,CA:TRUE' \
  -addext 'keyUsage=critical,keyCertSign,cRLSign'
```

OpenSSL prompts for the CA key's passphrase. This CA certificate is self-signed; the leaf certificates below are signed by that CA.

## Issue a Server Certificate

Replace the SAN values when using a real hostname. The Common Name identifies the subject; it is not a substitute for the hostname/IP SAN.

```bash
openssl genrsa -out server.key 2048
openssl req -new -key server.key -out server.csr -subj '/CN=localhost'
cat > server.ext <<'EOF'
basicConstraints=critical,CA:FALSE
keyUsage=critical,digitalSignature,keyEncipherment
extendedKeyUsage=serverAuth
subjectAltName=DNS:localhost,IP:127.0.0.1
EOF
openssl x509 -req -in server.csr -CA rootCA.crt -CAkey rootCA.key \
  -CAcreateserial -out server.crt -days 365 -sha256 -extfile server.ext
openssl verify -CAfile rootCA.crt -verify_hostname localhost server.crt
```

Expect `server.crt: OK`. If an intermediate CA issues the server certificate, put the leaf first and intermediate certificates after it in the server chain file. Clients retain the trusted root separately.

## Issue a Client Certificate

For mutual TLS, issue a unique client identity with the `clientAuth` extended key usage:

```bash
openssl genrsa -out client.key 2048
openssl req -new -key client.key -out client.csr -subj '/CN=device-001'
cat > client.ext <<'EOF'
basicConstraints=critical,CA:FALSE
keyUsage=critical,digitalSignature
extendedKeyUsage=clientAuth
EOF
openssl x509 -req -in client.csr -CA rootCA.crt -CAkey rootCA.key \
  -CAcreateserial -out client.crt -days 365 -sha256 -extfile client.ext
openssl verify -CAfile rootCA.crt -purpose sslclient client.crt
```

Give the client its own certificate/key and the server CA trust bundle. Give EMQX the server certificate/key and the CA bundle for validating clients. Mapping a certificate subject to an MQTT identity is a separate [X.509 configuration](../../guides/access-control/authn/x509.md).

## Path-Based Certificates

Reference files through `certfile`, `keyfile`, and, when peer verification is required, `cacertfile`. Files must be readable by EMQX at the configured path on each relevant node. Mount them into containers explicitly.

| File | Purpose |
| --- | --- |
| Server chain | Leaf certificate followed by intermediate certificates. |
| Server key | Private key matching the leaf; restrict read access. |
| CA bundle | Trust anchors for verifying peers, not a replacement for the server chain. |

Use [the TLS listener example](enable.md#enable-server-authentication) to apply them. Manage file deployment and renewal through your own certificate tooling.

## Managed Certificates

A bundle has a name and an optional tenant namespace. It can be reused by MQTT SSL/WSS listeners, Dashboard HTTPS, and supported TLS connectors. EMQX stores bundle material under `data/certs2/`; manage it through the Dashboard/API rather than modifying internal files.

| File in a Bundle | API Field | Purpose |
| --- | --- | --- |
| `key.pem` | `key` | Private key. |
| `chain.pem` | `chain` | Leaf and intermediate certificate chain. |
| `ca.pem` | `ca` | Optional CA trust bundle for peer verification. |
| `key-password.pem` | `key_password` | Optional passphrase for an encrypted private key. |

### Create a Bundle in the Dashboard

1. Open **Management** -> **Certificates** and select **Create**.
2. Enter a name. Leave the namespace switch off for a global bundle, or select a tenant namespace you are authorized to manage.
3. Provide the TLS certificate chain and matching key. Add the CA bundle for mutual TLS or an outbound connection that validates peers, and the key passphrase if needed.
4. Save, then select the bundle in the listener or connector.

Global administrators can manage global and tenant bundles. Namespace-scoped users can manage bundles within their own namespace. Use the namespace selector to find an existing bundle, edit it, or inspect dependencies before deletion.

### Manage Bundles Through the API

The API accepts **JSON fields containing PEM strings**. Prefix these routes with `/api/v5` and authenticate with an API key authorized for the operation.

| Operation | Global Route | Tenant Route |
| --- | --- | --- |
| Create or update | `POST /certs/global/name/{name}` | `POST /certs/ns/{namespace}/name/{name}` |
| List bundles | `GET /certs/global/list` | `GET /certs/ns/{namespace}/list` |
| List available file kinds | `GET /certs/global/name/{name}` | `GET /certs/ns/{namespace}/name/{name}` |
| Delete bundle or file | `DELETE /certs/global/name/{name}` | `DELETE /certs/ns/{namespace}/name/{name}` |

Install Python 3 if it is not available. In the directory containing `server.key`, `server.crt`, and `rootCA.crt` from the examples above (`emqx-tls-test`), run the following command to create `bundle.json` without placing the private key in a command argument:

```bash
python3 <<'PY'
import json
import os
from pathlib import Path
os.umask(0o077)
Path("bundle.json").write_text(json.dumps({
    "key": Path("server.key").read_text(),
    "chain": Path("server.crt").read_text(),
    "ca": Path("rootCA.crt").read_text(),
}))
PY
```

Stay in the same directory and set `EMQX_API_KEY` to your API key identifier. Curl prompts for the API secret:

```bash
curl --fail-with-body --user "$EMQX_API_KEY" \
  --write-out '\nHTTP %{http_code}\n' \
  -H 'Content-Type: application/json' \
  --data-binary @bundle.json \
  http://127.0.0.1:18083/api/v5/certs/global/name/mqtt-server
```

Expect `HTTP 204`. Use HTTPS for a remote management endpoint. Remove `bundle.json` after the request because it contains private key material. For an encrypted key, include `key_password` in the JSON. The API also accepts `acc_key` for account-key material; it is not required for this manually issued TLS bundle.

To delete one file, append `?kind=key`, `chain`, `ca`, `key_password`, or `acc_key` to the DELETE route. Since EMQX 6.1.4, deletion is refused while any configuration references the bundle, including when deleting an individual file. A `400` response contains `referencing_configs` grouped by namespace. Update every listed dependency and retry; `force_delete=true` does not bypass this check.

## Multiple Certificates with SNI

Create both bundles first, then configure the listener:

```hocon
listeners.ssl.default.ssl_options {
  managed_certs = [
    { bundle_name = "default-cert", sni = "mqtt.example.com" },
    { namespace = "tenant-a", bundle_name = "tenant-cert", sni = "mqtt.tenant-a.example.com" }
  ]
  verify = verify_none
  fail_if_no_peer_cert = false
}
```

EMQX selects the entry matching the client's SNI; if there is no match, it uses the first entry. Omit `namespace` for a global bundle. Configure each certificate's SAN for its hostname, and test both matching names and a client that sends no SNI. This is certificate selection, not tenant authorization. OCSP stapling cannot be combined with a managed-certificate selection list.

## Rotate and Verify

1. Inventory all listeners and connectors that use the paths or bundle. Record the current certificate fingerprint and expiry, and retain the previous material securely for rollback.
2. Validate the replacement chain, SANs, key match, and client trust before applying it. For a CA change, distribute compatible trust anchors before switching the server certificate.
3. For file-based certificates, replace the referenced files consistently on each node. For a managed bundle, update its contents through the Dashboard/API; the update affects every resource that references that bundle.
4. Establish **new** TLS connections to each node and hostname. Inspect the presented certificate and verify an application message. Certificate caches and existing outbound connections can delay when a replacement is observed; an existing MQTT connection does not repeat its handshake just because files changed.
5. If verification fails, restore the previous matching certificate/key and recheck new connections. Remove old material only after the rollout and rollback window are complete.

EMQX can reload certificate files without a broker restart. Do not treat a nominal cache interval as a guarantee that every existing connection has adopted a replacement. Test the behavior of the specific listener or connector during your rotation procedure.

Continue with [TLS listener setup](enable.md), [client revocation](crl.md), or [server OCSP stapling](ocsp.md).
